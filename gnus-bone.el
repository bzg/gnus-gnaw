;;; gnus-bone.el --- highlight BARK reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
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
;; M-x gnus-bone-clear RET will unhighlight and disable auto-rehighlight
;;
;;; Code:

(require 'json)
(require 'cl-lib)

(declare-function gnus-summary-article-number "gnus-sum")
(declare-function gnus-summary-article-header "gnus-sum")
(declare-function gnus-summary-limit "gnus-sum")
(declare-function gnus-summary-pop-limit "gnus-sum")
(declare-function mail-header-id "nnheader")

(defvar gnus-bone-config-file "~/.config/bone/config.edn"
  "Path to bone config.edn.
The file is an EDN map with at least these keys:
  :addresses  vector of email addresses belonging to the user
  :sources    vector of maps, each with a :url key pointing at a
              reports.json (local file:// URI or http(s) URL).")

(defvar gnus-bone-addresses nil
  "List of user email addresses, loaded from `gnus-bone-config-file'.
Populated by `gnus-bone--load-config'.")

(defface gnus-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in Gnus summary.
Lighter variant of `hl-line' to avoid clashing."
  :group 'gnus-bone)

(defface gnus-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations (type, flags, priority, votes)."
  :group 'gnus-bone)

(defconst gnus-bone-supported-bark-format "0.4.0"
  "Minimum supported BONE reports.json bark-format.")

(defun gnus-bone--uri-to-path (uri)
  "Convert a file:// URI to a local path; pass other URIs through unchanged."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun gnus-bone--read-edn-strings (text key)
  "Return list of strings inside the vector after KEY in EDN TEXT.
KEY is a string like \":addresses\".  Only top-level double-quoted
strings within the matched [...] block are returned."
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
  "Return list of :url strings from the :sources vector in EDN TEXT."
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
  "Load `gnus-bone-config-file' and return a plist (:addresses :sources).
Also sets `gnus-bone-addresses' as a side effect."
  (let* ((file (expand-file-name gnus-bone-config-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (addresses (gnus-bone--read-edn-strings text ":addresses"))
         (sources   (gnus-bone--read-edn-source-urls text)))
    (setq gnus-bone-addresses addresses)
    (list :addresses addresses :sources sources)))

(defun gnus-bone--load-sources ()
  "Return list of reports.json paths/URLs from `gnus-bone-config-file'."
  (mapcar #'gnus-bone--uri-to-path
          (plist-get (gnus-bone--load-config) :sources)))

(defun gnus-bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun gnus-bone--read-json (source)
  "Read JSON from SOURCE, a local path or HTTP(S) URL."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (if (gnus-bone--http-url-p source)
        (let ((buf (url-retrieve-synchronously source t)))
          (unless buf (error "gnus-bone: failed to fetch %s" source))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (unless (re-search-forward "\n\n" nil t)
                  (error "gnus-bone: malformed HTTP response from %s" source))
                (json-read))
            (kill-buffer buf)))
      (json-read-file source))))

(defun gnus-bone--extract-open-reports (source)
  "Extract report plists for open reports from SOURCE.
SOURCE may be a local file path or an HTTP(S) URL.
Each entry is (MESSAGE-ID . (:type T :flags F :priority P :votes V)).
A report is open when its status is >= 4."
  (let* ((data (gnus-bone--read-json source))
         (fv (alist-get 'bark-format data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv gnus-bone-supported-bark-format))
      (message "gnus-bone: %s has bark-format %s, minimum supported is %s"
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
            (topic        (alist-get 'topic r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-"))))))
            (push (cons mid (list :type (or type "bug")
                                  :flags flags
                                  :priority (or priority 0)
                                  :votes votes
                                  :deadline deadline
                                  :topic topic))
                  result)))))))

(defun gnus-bone--load-all-open-reports ()
  "Collect open (message-id . plist) pairs from all sources."
  (mapcan #'gnus-bone--extract-open-reports (gnus-bone--load-sources)))

(defun gnus-bone--normalize-mid (mid)
  "Ensure MID is bracketed."
  (if (string-match-p "^<.*>$" mid)
      mid
    (concat "<" mid ">")))

(defvar gnus-bone-votes-width 7
  "Fixed width for the votes column (e.g. \"[1/1]  \" or \"       \").
Increase if you expect votes like [12/34].")

(defvar gnus-bone-deadline-width 5
  "Fixed width for the deadline column (e.g. \"D-2  \" or \"     \").")

(defun gnus-bone--type-letter (type)
  "Return a single-letter abbreviation for report TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun gnus-bone--deadline-days (deadline)
  "Return days until DEADLINE (a \"YYYY-MM-DD\" string), or nil."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun gnus-bone--annotation (info)
  "Build a fixed-width annotation string from report INFO plist."
  (let* ((type     (gnus-bone--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (days    (gnus-bone--deadline-days deadline))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if days (format "D%+d" days) ""))
         (dl-pad   (string-pad dl-str gnus-bone-deadline-width))
         (votes-str (if votes
                        (format "[%s]" votes)
                      ""))
         (votes-pad (string-pad votes-str gnus-bone-votes-width))
         (tag       (concat type " " flags " " pri-str " " dl-pad votes-pad)))
    tag))

(defun gnus-bone--for-each-summary-mid (fn)
  "Walk every summary line, calling FN with (ARTICLE-NUMBER . MESSAGE-ID).
FN is only called for lines with a valid article and message-id."
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
  "Build a hash-table mapping normalized message-ids from REPORTS.
VALUE-FN, when given, is called on each (mid . plist) entry to
produce the hash value; defaults to the plist (cdr)."
  (let ((id-map (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (gnus-bone--normalize-mid (car r))
               (if value-fn (funcall value-fn r) (cdr r))
               id-map))
    id-map))

(defun gnus-bone--apply-overlays (reports)
  "Apply overlays for REPORTS, a list of (message-id . plist).
The annotation replaces the rightmost columns of each line,
so it is always visible regardless of margins."
  (let ((id-map (gnus-bone--build-mid-map reports)))
    (gnus-bone--for-each-summary-mid
     (lambda (_article mid)
       (let ((info (gethash mid id-map)))
         (when info
           (let* ((bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (gnus-bone--annotation info))
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
  "Buffer-local cache of BARK reports for auto-rehighlighting.
Set by `gnus-bone' and `gnus-bone-highlight', cleared by `gnus-bone-clear'.")

(defun gnus-bone--rehighlight (&rest _args)
  "Re-apply BARK overlays after summary buffer changes.
Intended for `gnus-summary-prepared-hook' and `gnus-summary-update-hook'."
  (when gnus-bone--active-reports
    (gnus-bone--apply-overlays gnus-bone--active-reports)))

(defun gnus-bone--enable-hooks ()
  "Enable auto-rehighlighting hooks in the current summary buffer."
  (add-hook 'gnus-summary-prepared-hook #'gnus-bone--rehighlight nil t)
  (add-hook 'gnus-summary-update-hook #'gnus-bone--rehighlight nil t))

(defun gnus-bone--disable-hooks ()
  "Disable auto-rehighlighting hooks in the current summary buffer."
  (remove-hook 'gnus-summary-prepared-hook #'gnus-bone--rehighlight t)
  (remove-hook 'gnus-summary-update-hook #'gnus-bone--rehighlight t))

(defun gnus-bone--activate (reports &optional limit-articles)
  "Clear previous state, apply overlays for REPORTS, enable hooks.
When LIMIT-ARTICLES is non-nil, limit summary to those articles first."
  (when limit-articles (gnus-summary-limit limit-articles))
  (gnus-bone-clear)
  (setq gnus-bone--active-reports reports)
  (gnus-bone--apply-overlays reports)
  (gnus-bone--enable-hooks))

(defun gnus-bone-highlight ()
  "Highlight summary lines whose message-id appears in open BARK reports.
Highlighting persists across `A T' and other summary updates."
  (interactive)
  (let ((reports (gnus-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (gnus-bone--activate reports)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun gnus-bone--matching-articles (reports)
  "Return article numbers in current summary matching REPORTS."
  (let ((id-map (gnus-bone--build-mid-map reports (lambda (_) t)))
        (articles nil))
    (gnus-bone--for-each-summary-mid
     (lambda (article mid)
       (when (gethash mid id-map)
         (push article articles))))
    (nreverse articles)))

(defun gnus-bone ()
  "Limit Gnus summary to open BARK reports, then highlight them.
Highlighting persists across `A T' and other summary updates.
Use `gnus-summary-pop-limit' (\\[gnus-summary-pop-limit]) to restore."
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
  "Return sorted list of unique topics from REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (when-let ((topic (plist-get (cdr r) :topic)))
        (cl-pushnew topic topics :test #'equal)))
    (sort (copy-sequence topics) #'string<)))

(defun gnus-bone--filter-by-topic (reports topic)
  "Return REPORTS whose :topic equals TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                     reports))

(defun gnus-bone-topic ()
  "Like `gnus-bone', but limited to a single topic.
Completes over topics found in the BARK JSON sources."
  (interactive)
  (let* ((reports (gnus-bone--load-all-open-reports))
         (topics  (gnus-bone--collect-topics reports))
         (topic   (completing-read "BARK topic: " topics nil t)))
    (if (string-empty-p topic)
        (message "No topic selected.")
      (let* ((filtered (gnus-bone--filter-by-topic reports topic))
             (articles (and filtered (gnus-bone--matching-articles filtered))))
        (if (null articles)
            (message "No matching articles for topic \"%s\"." topic)
          (gnus-bone--activate filtered articles)
          (message "Limited to %d BARK reports for topic \"%s\"."
                   (length articles) topic))))))

(defun gnus-bone-clear ()
  "Remove all gnus-bone overlays and disable auto-rehighlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-bone t)
  (setq gnus-bone--active-reports nil)
  (gnus-bone--disable-hooks))

(provide 'gnus-bone)
;;; gnus-bone.el ends here
