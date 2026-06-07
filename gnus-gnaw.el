;;; gnus-gnaw.el --- Highlight BONE reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: news, mail
;; URL: https://codeberg.org/bzg/gnus-gnaw
;; Version: 0.13.0
;; Package-Requires: ((emacs "28.1") (gnaw "0.1"))

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
;; This library is not actively maintained, it is shared as a proof of
;; concept.  If you want to maintain and develop it, please contact me.
;;
;; M-x gnus-gnaw RET will limit summary to BONE reports + highlight
;; M-x gnus-gnaw-highlight RET will highlight BONE reports (no limit)
;; M-x gnus-gnaw-topic RET filters highlighted reports by topic
;; M-x gnus-gnaw-clear RET will unhighlight and disable auto-rehighlight
;; M-x gnaw-update RET will force update of the remote reports cache
;;
;; The following commands toggle gnaw's local marks (kept in
;; ~/.config/gnaw/state.edn so they are shared with the gnaw CLI):
;;
;; M-x gnus-gnaw-mark-sticky RET — toggle the sticky mark (keep visible)
;; M-x gnus-gnaw-mark-skip RET — toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; gnus-gnaw builds on the `gnaw' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the Gnus presentation and commands.
;;
;;; Code:

(require 'gnaw)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)

(declare-function gnus-summary-article-number "gnus-sum")
(declare-function gnus-summary-article-header "gnus-sum")
(declare-function gnus-summary-limit "gnus-sum")
(declare-function gnus-summary-pop-limit "gnus-sum")
(declare-function mail-header-id "nnheader")

(defgroup gnus-gnaw nil
  "Highlight BONE reports in Gnus summary buffers."
  :group 'gnus)

(defface gnus-gnaw-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BONE reports in Gnus summary."
  :group 'gnus-gnaw)

(defface gnus-gnaw-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'gnus-gnaw)

;;; Annotation rendering

(defun gnus-gnaw--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip            "_")
     (t               " "))))

(defvar gnus-gnaw-votes-width 7
  "Fixed width for votes column.")

(defvar gnus-gnaw-deadline-width 5
  "Fixed width for deadline column.")

(defun gnus-gnaw--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun gnus-gnaw--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun gnus-gnaw--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
  (let* ((mark     (gnus-gnaw--mark-prefix entry))
         (type     (gnus-gnaw--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (days     (gnus-gnaw--deadline-days deadline))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if days (format "D%+d" days) ""))
         (dl-pad   (string-pad dl-str gnus-gnaw-deadline-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str gnus-gnaw-votes-width))
         (tag       (concat mark " " type " " flags " " pri-str " "
                             dl-pad votes-pad)))
    tag))

;;; Summary overlays

(defun gnus-gnaw--for-each-summary-mid (fn)
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

(defun gnus-gnaw--build-mid-map (reports &optional value-fn)
  "Build a hash of normalized MIDs to report info for REPORTS.
Use VALUE-FN to compute each value when given, else the report info."
  (let ((id-map (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (car r)
               (if value-fn (funcall value-fn r) (cdr r))
               id-map))
    id-map))

(defun gnus-gnaw--apply-overlays (reports)
  "Apply overlays for BONE REPORTS in the current summary buffer."
  (remove-overlays (point-min) (point-max) 'gnus-gnaw t)
  (let ((id-map (gnus-gnaw--build-mid-map reports))
        (state  (gnaw-read-state)))
    (gnus-gnaw--for-each-summary-mid
     (lambda (_article mid)
       (let ((info (gethash mid id-map)))
         (when info
           (let* ((entry   (cdr (assoc mid state)))
                  (bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (gnus-gnaw--annotation info entry))
                  (ann-len (length ann-str))
                  (p3      (= 3 (plist-get info :priority)))
                  (face    (if p3 '(gnus-gnaw-face bold) 'gnus-gnaw-face))
                  (ov-bg   (make-overlay bol eol))
                  (tag-len (+ ann-len 1))
                  (start   (max bol (- eol tag-len)))
                  (ov-ann  (make-overlay start eol)))
             (overlay-put ov-bg 'face face)
             (overlay-put ov-bg 'gnus-gnaw t)
             (overlay-put ov-ann 'display
                          (propertize (concat " " ann-str)
                                      'face 'gnus-gnaw-annotation-face))
             (overlay-put ov-ann 'gnus-gnaw t))))))))

(defvar-local gnus-gnaw--active-reports nil
  "Buffer-local cache of BONE reports for auto-rehighlighting.")

(defun gnus-gnaw--rehighlight (&rest _args)
  "Re-apply overlays on summary buffer updates."
  (when gnus-gnaw--active-reports
    (gnus-gnaw--apply-overlays gnus-gnaw--active-reports)))

(defun gnus-gnaw--enable-hooks ()
  "Enable hooks in current summary buffer."
  (add-hook 'gnus-summary-prepared-hook #'gnus-gnaw--rehighlight nil t)
  (add-hook 'gnus-summary-update-hook #'gnus-gnaw--rehighlight nil t))

(defun gnus-gnaw--disable-hooks ()
  "Disable hooks in current summary buffer."
  (remove-hook 'gnus-summary-prepared-hook #'gnus-gnaw--rehighlight t)
  (remove-hook 'gnus-summary-update-hook #'gnus-gnaw--rehighlight t))

(defun gnus-gnaw--activate (reports &optional limit-articles)
  "Activate REPORTS, optionally limiting summary to LIMIT-ARTICLES first."
  (when limit-articles (gnus-summary-limit limit-articles))
  (gnus-gnaw-clear)
  (setq gnus-gnaw--active-reports reports)
  (gnus-gnaw--apply-overlays reports)
  (gnus-gnaw--enable-hooks))

;;; Commands

(defun gnus-gnaw-highlight ()
  "Highlight summary lines of open BONE reports."
  (interactive)
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (gnus-gnaw--activate reports)
      (message "Highlighted %d BONE reports." (length reports)))))

(defun gnus-gnaw--matching-articles (reports)
  "Get article numbers matching REPORTS."
  (let ((id-map (gnus-gnaw--build-mid-map reports (lambda (_) t)))
        (articles nil))
    (gnus-gnaw--for-each-summary-mid
     (lambda (article mid)
       (when (gethash mid id-map)
         (push article articles))))
    (nreverse articles)))

(defun gnus-gnaw ()
  "Limit summary to open BONE reports and highlight them."
  (interactive)
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (let ((articles (gnus-gnaw--matching-articles reports)))
        (if (null articles)
            (message "No matching articles in this summary.")
          (gnus-gnaw--activate reports articles)
          (message "Limited to %d BONE reports." (length articles)))))))

(defun gnus-gnaw--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort (copy-sequence topics) #'string<)))

(defun gnus-gnaw--filter-by-topic (reports topic)
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

(defun gnus-gnaw-topic ()
  "Limit summary to reports of selected topic and highlight them."
  (interactive)
  (let* ((reports (gnaw-reports))
         (topics  (gnus-gnaw--collect-topics reports)))
    (cond
     ((null reports) (message "No open BONE reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BONE topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (gnus-gnaw--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (let ((articles (gnus-gnaw--matching-articles filtered)))
            (if (null articles)
                (message "No matching articles for topic \"%s\"." topic)
              (gnus-gnaw--activate filtered articles)
              (message "Limited to %d BONE reports for topic \"%s\"."
                       (length articles) topic))))))))))

;;; Marking commands

(defun gnus-gnaw--current-mid ()
  "Current article's normalized MID, or nil."
  (let ((article (gnus-summary-article-number)))
    (when (and (numberp article) (> article 0))
      (let* ((header (gnus-summary-article-header article))
             (raw    (and header (mail-header-id header))))
        (when raw
          (gnaw-normalize-mid raw))))))

(defun gnus-gnaw--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc mid reports)))

(defun gnus-gnaw--refresh-overlays ()
  "Re-apply overlays in summary buffer."
  (when gnus-gnaw--active-reports
    (gnus-gnaw--apply-overlays gnus-gnaw--active-reports)))

(defun gnus-gnaw--mark (action on-msg off-msg)
  "Toggle ACTION mark on the current report, showing ON-MSG or OFF-MSG."
  (let* ((reports (or gnus-gnaw--active-reports (gnaw-reports)))
         (mid     (and reports (gnus-gnaw--current-mid)))
         (info    (and mid (gnus-gnaw--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BONE reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current article is not a BONE report: %s" mid))
     (t
      (let ((on (gnaw-toggle-mark mid info action)))
        (gnus-gnaw--refresh-overlays)
        (message "%s" (if on on-msg off-msg)))))))

(defun gnus-gnaw-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (gnus-gnaw--mark :sticky "Marked sticky" "Unmarked sticky"))

(defun gnus-gnaw-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (gnus-gnaw--mark :skip "Skipped" "Unskipped"))

(defun gnus-gnaw-clear ()
  "Remove all gnus-gnaw overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-gnaw t)
  (setq gnus-gnaw--active-reports nil)
  (gnus-gnaw--disable-hooks))

;; --- Cache update hooks ----------------------------------------------------

(defun gnus-gnaw--refresh-all-buffers ()
  "Re-apply overlays in active summary buffers from the refreshed cache."
  (let ((reports (gnaw-reports)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'gnus-summary-mode)
                   gnus-gnaw--active-reports)
          (setq gnus-gnaw--active-reports reports)
          (gnus-gnaw--apply-overlays reports))))))

(add-hook 'gnaw-after-update-hook #'gnus-gnaw--refresh-all-buffers)

(provide 'gnus-gnaw)
;;; gnus-gnaw.el ends here
