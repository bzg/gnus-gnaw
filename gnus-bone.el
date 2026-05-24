;;; gnus-bone.el --- highlight BARK reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: news, mail
;; URL: https://codeberg.org/bzg/gnus-bone

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; M-x gnus-bone RET will limit summary to BARK reports + highlight
;; M-x gnus-bone-highlight RET will highlight BARK reports (no limit)
;; M-x gnus-bone-topic RET filters highlighted reports by topic
;; M-x gnus-bone-clear RET will unhighlight and disable auto-rehighlight
;; M-x gnus-bone-update-cache RET will force update of remote reports
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x gnus-bone-mark-read   RET — toggle :read-at on current article
;; M-x gnus-bone-mark-todo   RET — toggle :todo flag on current article
;; M-x gnus-bone-mark-sticky RET — toggle :sticky flag on current article
;;
;; The annotation gains a leading mark column: '!' = :todo, '*' = :sticky,
;; 'r' = :read-at (without flag).
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)

(declare-function gnus-summary-article-number "gnus-sum")
(declare-function gnus-summary-article-header "gnus-sum")
(declare-function gnus-summary-limit "gnus-sum")
(declare-function gnus-summary-pop-limit "gnus-sum")
(declare-function mail-header-id "nnheader")

(defvar url-http-response-status)

(defgroup gnus-bone nil
  "Highlight BARK reports in Gnus summary buffers."
  :group 'gnus)

(defcustom gnus-bone-reports-source nil
  "Path or URL to a BARK reports.json file.
If nil, load sources configured in config.edn under `gnus-bone-config-dir'."
  :type '(choice (const :tag "Use config.edn sources" nil)
                 (string :tag "Local path or URL"))
  :group 'gnus-bone)

(defcustom gnus-bone-config-dir "~/.config/bone"
  "Directory containing bone configuration and state/cache files."
  :type 'directory
  :group 'gnus-bone)

(defvar gnus-bone-addresses nil
  "List of user email addresses loaded from config.")

(defface gnus-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in Gnus summary."
  :group 'gnus-bone)

(defface gnus-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'gnus-bone)

(defconst gnus-bone-supported-bark-format "0.9.1"
  "Minimum supported BONE reports.json bark-format.")

(defun gnus-bone--uri-to-path (uri)
  "Convert file:// URI to local path, otherwise return URI."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun gnus-bone--read-edn-strings (text key)
  "Extract list of string values for vector KEY in EDN TEXT."
  (when (string-match
         (concat (regexp-quote key) "[[:space:]]*\\[\\([^][]*\\)\\]")
         text)
    (let ((body (match-string 1 text))
          (pos 0)
          (acc nil))
      (while (string-match "\"\\([^\"]*\\)\"" body pos)
        (push (match-string 1 body) acc)
        (setq pos (match-end 0)))
      (nreverse acc))))

(defun gnus-bone--read-edn-source-urls (text)
  "Extract list of :url strings from :sources in EDN TEXT."
  (when (string-match
         ":sources[[:space:]]*\\[\\(\\(?:[^][]\\|\\[[^][]*\\]\\)*\\)\\]"
         text)
    (let ((body (match-string 1 text))
          (pos 0)
          (acc nil))
      (while (string-match ":url[[:space:]]*\"\\([^\"]+\\)\"" body pos)
        (push (match-string 1 body) acc)
        (setq pos (match-end 0)))
      (nreverse acc))))

(defun gnus-bone--load-config ()
  "Load config file and return plist (:addresses :sources)."
  (let* ((file (expand-file-name "config.edn" gnus-bone-config-dir))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (while (re-search-forward "^[ \t]*;.*$" nil t)
                   (replace-match ""))
                 (buffer-string)))
         (addresses (gnus-bone--read-edn-strings text ":addresses"))
         (sources   (gnus-bone--read-edn-source-urls text)))
    (setq gnus-bone-addresses addresses)
    (list :addresses addresses :sources sources)))

(defun gnus-bone--load-sources ()
  "Return list of reports.json paths or URLs."
  (mapcar #'gnus-bone--uri-to-path
          (if gnus-bone-reports-source
              (list gnus-bone-reports-source)
            (plist-get (gnus-bone--load-config) :sources))))

(defun gnus-bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun gnus-bone--java-hash (str)
  "Calculate Java String hashCode of STR as an unsigned 32-bit integer."
  (let ((h 0)
        (len (length str)))
    (dotimes (i len)
      (setq h (logand (+ (* h 31) (aref str i)) #xffffffff)))
    h))

(defun gnus-bone--source-to-cache-file (src)
  "Return cache file path for remote source SRC."
  (let* ((h (format "%08x" (gnus-bone--java-hash src)))
         (safe (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" src))
         (prefix (substring safe 0 (min 80 (length safe)))))
    (expand-file-name
     (concat "cache/reports/" prefix "-" h ".json")
     gnus-bone-config-dir)))

(defun gnus-bone--fetch-json-from-url (url)
  "Synchronously fetch JSON from URL."
  (let ((buf (url-retrieve-synchronously url t)))
    (unless buf (error "gnus-bone: failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (when (and (bound-and-true-p url-http-response-status)
                     (>= url-http-response-status 400))
            (error "gnus-bone: HTTP error %d from %s" url-http-response-status url))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "gnus-bone: malformed HTTP response from %s" url))
          (let ((json-object-type 'alist)
                (json-array-type 'list))
            (json-read)))
      (kill-buffer buf))))

(defun gnus-bone--write-json-to-file (data file)
  "Write JSON DATA to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (json-encode data))))

(defun gnus-bone--read-json (source)
  "Read JSON from SOURCE, using local cache for remote URLs if available."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (if (gnus-bone--http-url-p source)
        (let ((cache-file (gnus-bone--source-to-cache-file source)))
          (if (file-exists-p cache-file)
              (json-read-file cache-file)
            (let ((data (gnus-bone--fetch-json-from-url source)))
              (gnus-bone--write-json-to-file data cache-file)
              data)))
      (json-read-file source))))

(defun gnus-bone--normalize-mid (mid)
  "Ensure MID has angle brackets."
  (if (string-match-p "^<.*>$" mid)
      mid
    (concat "<" mid ">")))

(defun gnus-bone--extract-open-reports (source)
  "Extract open reports from SOURCE."
  (let* ((data (gnus-bone--read-json source))
         (fv (alist-get 'bark-format data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv gnus-bone-supported-bark-format))
      (message "gnus-bone: %s has format %s, min supported is %s"
               source fv gnus-bone-supported-bark-format))
    (dolist (r reports result)
      (let ((mid          (alist-get 'message-id r))
            (status       (alist-get 'status r))
            (type         (alist-get 'type r))
            (acked        (alist-get 'acked r))
            (owned        (alist-get 'owned r))
            (closed       (alist-get 'closed r))
            (close-reason (alist-get 'close-reason r))
            (priority     (alist-get 'priority r))
            (votes        (alist-get 'votes r))
            (deadline     (alist-get 'deadline r))
            (topic        (alist-get 'topic r))
            (subject      (alist-get 'subject r))
            (from         (alist-get 'from r))
            (from-name    (alist-get 'from-name r))
            (date         (alist-get 'date r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-")))))
                (norm-mid (gnus-bone--normalize-mid mid)))
            (push (cons norm-mid (list :type (or type "bug")
                                       :flags flags
                                       :priority (or priority 0)
                                       :votes votes
                                       :deadline deadline
                                       :topic topic
                                       :subject subject
                                       :from from
                                       :from-name from-name
                                       :date date))
                  result)))))))

(defun gnus-bone--load-all-open-reports ()
  "Collect open report pairs from all sources, tolerating failures."
  (let ((result nil))
    (dolist (source (gnus-bone--load-sources))
      (condition-case err
          (setq result (append result (gnus-bone--extract-open-reports source)))
        (error
         (message "gnus-bone: failed loading source %s: %s"
                  source (error-message-string err)))))
    result))

(defun gnus-bone-update-cache ()
  "Force-refresh the local cache from remote JSON sources."
  (interactive)
  (let ((sources (gnus-bone--load-sources))
        (count 0))
    (dolist (source sources)
      (when (gnus-bone--http-url-p source)
        (message "gnus-bone: updating cache for %s..." source)
        (condition-case err
            (let ((data (gnus-bone--fetch-json-from-url source))
                  (cache-file (gnus-bone--source-to-cache-file source)))
              (gnus-bone--write-json-to-file data cache-file)
              (setq count (1+ count))
              (message "gnus-bone: cache updated for %s" source))
          (error
           (message "gnus-bone: failed updating %s: %s"
                    source (error-message-string err))))))
    (message "gnus-bone: cache update finished (%d updated)." count)))

;; --- EDN reader/writer for ~/.config/bone/state.edn -----------------------

(defun gnus-bone--edn-skip-ws ()
  (skip-chars-forward " \t\n\r,"))

(defun gnus-bone--edn-read ()
  "Read one EDN value at point."
  (gnus-bone--edn-skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "gnus-bone EDN: unexpected EOF"))
     ((eq c ?\") (read (current-buffer)))
     ((eq c ?:)  (gnus-bone--edn-read-keyword))
     ((eq c ?\{) (gnus-bone--edn-read-map))
     ((eq c ?\[) (gnus-bone--edn-read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                            (and d (>= d ?0) (<= d ?9)))))
      (gnus-bone--edn-read-number))
     (t (gnus-bone--edn-read-symbol)))))

(defun gnus-bone--edn-read-keyword ()
  (forward-char 1)
  (let ((start (1- (point))))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (intern (buffer-substring-no-properties start (point)))))

(defun gnus-bone--edn-read-symbol ()
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (pcase (buffer-substring-no-properties start (point))
      ("nil"   nil)
      ("true"  t)
      ("false" nil)
      (s       (intern s)))))

(defun gnus-bone--edn-read-number ()
  (let ((start (point)))
    (skip-chars-forward "0-9.eE+-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun gnus-bone--edn-read-map ()
  (forward-char 1)
  (let ((acc nil))
    (gnus-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (gnus-bone--edn-read)))
        (gnus-bone--edn-skip-ws)
        (push (cons k (gnus-bone--edn-read)) acc))
      (gnus-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun gnus-bone--edn-read-vector ()
  (forward-char 1)
  (let ((acc nil))
    (gnus-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (gnus-bone--edn-read) acc)
      (gnus-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun gnus-bone--edn-write-string (s)
  "Format string S as an EDN string."
  (format "%S" s))

(defun gnus-bone--edn-write-value (v)
  (cond
   ((stringp v)  (gnus-bone--edn-write-string v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((consp v)    (gnus-bone--edn-write-entry v))
   (t (error "gnus-bone EDN: cannot serialize %S" v))))

(defun gnus-bone--edn-write-entry (entry)
  "Format entry as an EDN map."
  (if (null entry) "{}"
    (concat "{"
            (mapconcat (lambda (kv)
                         (concat (gnus-bone--edn-write-value (car kv))
                                 " "
                                 (gnus-bone--edn-write-value (cdr kv))))
                       entry ", ")
            "}")))

;; --- State file I/O -------------------------------------------------------

(defun gnus-bone--read-state ()
  "Read state file."
  (let ((file (expand-file-name "state.edn" gnus-bone-config-dir)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (gnus-bone--edn-skip-ws)
            (when (eq (char-after) ?{)
              (gnus-bone--edn-read-map)))
        (error
         (message "gnus-bone: cannot parse %s: %s"
                  file (error-message-string err))
         nil)))))

(defun gnus-bone--write-state (state)
  "Write STATE to state file."
  (let ((file (expand-file-name "state.edn" gnus-bone-config-dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{")
        (let ((first t))
          (dolist (kv state)
            (if first (setq first nil) (insert "\n "))
            (insert (gnus-bone--edn-write-string (car kv)))
            (insert " ")
            (insert (gnus-bone--edn-write-entry (cdr kv)))))
        (insert "}\n")))))

;; --- State transitions ----------------------------------------------------

(defun gnus-bone--iso-now ()
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun gnus-bone--author-string (info)
  "Build author string from INFO."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string= n ""))) (concat n " <" e ">"))
     (e e)
     (n n))))

(defun gnus-bone--enrich-entry (existing info)
  "Refresh metadata from INFO in EXISTING."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (let ((v (plist-get info (car pair))))
        (when v
          (setf (alist-get (cdr pair) entry) v))))
    (let ((author (gnus-bone--author-string info)))
      (when author
        (setf (alist-get :author entry) author)))
    entry))

(defun gnus-bone--state-put (state mid entry)
  "Set MID to ENTRY in STATE, keeping order."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun gnus-bone--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun gnus-bone--alist-dissoc (alist key)
  "Remove KEY from ALIST copy."
  (assq-delete-all key (copy-alist alist)))

(defun gnus-bone--alist-assoc (alist key value)
  "Set KEY to VALUE in ALIST copy."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defun gnus-bone--apply-transition (state action mid info)
  "Apply ACTION transition for MID in STATE."
  (let* ((base (gnus-bone--enrich-entry (cdr (assoc mid state)) info))
         (flag (alist-get :flag base))
         (new
          (pcase action
            (:read   (if (alist-get :read-at base)
                         (gnus-bone--alist-dissoc base :read-at)
                       (gnus-bone--alist-assoc  base :read-at
                                                 (gnus-bone--iso-now))))
            (:todo   (if (eq flag :todo)
                         (gnus-bone--alist-dissoc base :flag)
                       (gnus-bone--alist-assoc  base :flag :todo)))
            (:sticky (if (eq flag :sticky)
                         (gnus-bone--alist-dissoc base :flag)
                       (gnus-bone--alist-assoc  base :flag :sticky))))))
    (if (and (null (alist-get :flag    new))
             (null (alist-get :read-at new)))
        (gnus-bone--state-delete state mid)
      (gnus-bone--state-put state mid new))))

(defun gnus-bone--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (read (cdr (assq :read-at entry))))
    (cond
     ((eq flag :todo)   "!")
     ((eq flag :sticky) "*")
     (read              "r")
     (t                 " "))))

(defvar gnus-bone-votes-width 7
  "Fixed width for votes column.")

(defvar gnus-bone-deadline-width 5
  "Fixed width for deadline column.")

(defun gnus-bone--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun gnus-bone--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun gnus-bone--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
  (let* ((mark     (gnus-bone--mark-prefix entry))
         (type     (gnus-bone--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (days     (gnus-bone--deadline-days deadline))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if days (format "D%+d" days) ""))
         (dl-pad   (string-pad dl-str gnus-bone-deadline-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str gnus-bone-votes-width))
         (tag       (concat mark " " type " " flags " " pri-str " "
                             dl-pad votes-pad)))
    tag))

(defun gnus-bone--for-each-summary-mid (fn)
  "Map FN over article numbers and MIDs in summary buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((article (gnus-summary-article-number))
             (header  (and (numberp article) (> article 0)
                           (gnus-summary-article-header article)))
             (mid     (and header (mail-header-id header))))
        (when mid (funcall fn article mid)))
      (forward-line 1))))

(defun gnus-bone--build-mid-map (reports &optional value-fn)
  "Build map of normalized MIDs to report info."
  (let ((id-map (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (car r)
               (if value-fn (funcall value-fn r) (cdr r))
               id-map))
    id-map))

(defun gnus-bone--apply-overlays (reports)
  "Apply overlays for BARK REPORTS in the current summary buffer."
  (remove-overlays (point-min) (point-max) 'gnus-bone t)
  (let ((id-map (gnus-bone--build-mid-map reports))
        (state  (gnus-bone--read-state)))
    (gnus-bone--for-each-summary-mid
     (lambda (_article mid)
       (let ((info (gethash mid id-map)))
         (when info
           (let* ((entry   (cdr (assoc mid state)))
                  (bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (gnus-bone--annotation info entry))
                  (ann-len (length ann-str))
                  (p3      (= 3 (plist-get info :priority)))
                  (face    (if p3 '(gnus-bone-face bold) 'gnus-bone-face))
                  (ov-bg   (make-overlay bol eol))
                  (tag-len (+ ann-len 1))
                  (start   (max bol (- eol tag-len)))
                  (ov-ann  (make-overlay start eol)))
             (overlay-put ov-bg 'face face)
             (overlay-put ov-bg 'gnus-bone t)
             (overlay-put ov-ann 'display
                          (propertize (concat " " ann-str)
                                      'face 'gnus-bone-annotation-face))
             (overlay-put ov-ann 'gnus-bone t))))))))

(defvar-local gnus-bone--active-reports nil
  "Buffer-local cache of BARK reports for auto-rehighlighting.")

(defun gnus-bone--rehighlight (&rest _args)
  "Re-apply overlays on summary buffer updates."
  (when gnus-bone--active-reports
    (gnus-bone--apply-overlays gnus-bone--active-reports)))

(defun gnus-bone--enable-hooks ()
  "Enable hooks in current summary buffer."
  (add-hook 'gnus-summary-prepared-hook #'gnus-bone--rehighlight nil t)
  (add-hook 'gnus-summary-update-hook #'gnus-bone--rehighlight nil t))

(defun gnus-bone--disable-hooks ()
  "Disable hooks in current summary buffer."
  (remove-hook 'gnus-summary-prepared-hook #'gnus-bone--rehighlight t)
  (remove-hook 'gnus-summary-update-hook #'gnus-bone--rehighlight t))

(defun gnus-bone--activate (reports &optional limit-articles)
  "Activate reports, optionally limiting summary first."
  (when limit-articles (gnus-summary-limit limit-articles))
  (gnus-bone-clear)
  (setq gnus-bone--active-reports reports)
  (gnus-bone--apply-overlays reports)
  (gnus-bone--enable-hooks))

(defun gnus-bone-highlight ()
  "Highlight summary lines of open BARK reports."
  (interactive)
  (let ((reports (gnus-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (gnus-bone--activate reports)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun gnus-bone--matching-articles (reports)
  "Get article numbers matching REPORTS."
  (let ((id-map (gnus-bone--build-mid-map reports (lambda (_) t)))
        (articles nil))
    (gnus-bone--for-each-summary-mid
     (lambda (article mid)
       (when (gethash mid id-map)
         (push article articles))))
    (nreverse articles)))

(defun gnus-bone ()
  "Limit summary to open BARK reports and highlight them."
  (interactive)
  (let ((reports (gnus-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (let ((articles (gnus-bone--matching-articles reports)))
        (if (null articles)
            (message "No matching articles in this summary.")
          (gnus-bone--activate reports articles)
          (message "Limited to %d BARK reports." (length articles)))))))

(defun gnus-bone--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort (copy-sequence topics) #'string<)))

(defun gnus-bone--filter-by-topic (reports topic)
  "Reports matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

(defun gnus-bone-topic ()
  "Limit summary to reports of selected topic and highlight them."
  (interactive)
  (let* ((reports (gnus-bone--load-all-open-reports))
         (topics  (gnus-bone--collect-topics reports))
         (topic   (completing-read "BARK topic: " topics nil t)))
    (if (string= topic "")
        (message "No topic selected.")
      (let* ((filtered (gnus-bone--filter-by-topic reports topic))
             (articles (and filtered (gnus-bone--matching-articles filtered))))
        (if (null articles)
            (message "No matching articles for topic \"%s\"." topic)
          (gnus-bone--activate filtered articles)
          (message "Limited to %d BARK reports for topic \"%s\"."
                   (length articles) topic))))))

;; --- Marking commands -----------------------------------------------------

(defun gnus-bone--current-mid ()
  "Current article's normalized MID, or nil."
  (let ((article (gnus-summary-article-number)))
    (when (and (numberp article) (> article 0))
      (let* ((header (gnus-summary-article-header article))
             (raw    (and header (mail-header-id header))))
        (when raw
          (gnus-bone--normalize-mid raw))))))

(defun gnus-bone--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc mid reports)))

(defun gnus-bone--refresh-overlays ()
  "Re-apply overlays in summary buffer."
  (when gnus-bone--active-reports
    (gnus-bone--apply-overlays gnus-bone--active-reports)))

(defun gnus-bone--action-on-p (state mid action)
  "Check if ACTION is set for MID in STATE."
  (let ((entry (cdr (assoc mid state))))
    (pcase action
      (:read   (cdr (assq :read-at entry)))
      (:todo   (eq (cdr (assq :flag entry)) :todo))
      (:sticky (eq (cdr (assq :flag entry)) :sticky)))))

(defun gnus-bone--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or gnus-bone--active-reports
                      (gnus-bone--load-all-open-reports)))
         (mid     (and reports (gnus-bone--current-mid)))
         (info    (and mid (gnus-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current article is not a BARK report: %s" mid))
     (t
      (let* ((state (gnus-bone--read-state))
             (new   (gnus-bone--apply-transition state action mid info)))
        (gnus-bone--write-state new)
        (gnus-bone--refresh-overlays)
        (message "%s" (if (gnus-bone--action-on-p new mid action)
                          on-msg off-msg)))))))

(defun gnus-bone-mark-read ()
  "Toggle :read-at timestamp for current report."
  (interactive)
  (gnus-bone--mark :read "Marked read" "Unmarked read"))

(defun gnus-bone-mark-todo ()
  "Toggle :todo flag for current report."
  (interactive)
  (gnus-bone--mark :todo "Marked TODO" "Unmarked TODO"))

(defun gnus-bone-mark-sticky ()
  "Toggle :sticky flag for current report."
  (interactive)
  (gnus-bone--mark :sticky "Marked STICKY" "Unmarked STICKY"))

(defun gnus-bone-clear ()
  "Remove all gnus-bone overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-bone t)
  (setq gnus-bone--active-reports nil)
  (gnus-bone--disable-hooks))

(provide 'gnus-bone)
;;; gnus-bone.el ends here
