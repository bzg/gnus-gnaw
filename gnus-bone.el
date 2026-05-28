;;; gnus-bone.el --- Highlight BARK reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: news, mail
;; URL: https://codeberg.org/bzg/gnus-bone
;; Version: 0.13.0
;; Package-Requires: ((emacs "28.1") (bone "0.1"))

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
;; M-x bone-update RET will force update of the remote reports cache
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x gnus-bone-mark-sticky RET — toggle the sticky mark (keep visible)
;; M-x gnus-bone-mark-skip RET — toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; gnus-bone builds on the `bone' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the Gnus presentation and commands.
;;
;;; Code:

(require 'bone)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)

(declare-function gnus-summary-article-number "gnus-sum")
(declare-function gnus-summary-article-header "gnus-sum")
(declare-function gnus-summary-limit "gnus-sum")
(declare-function gnus-summary-pop-limit "gnus-sum")
(declare-function mail-header-id "nnheader")

(defgroup gnus-bone nil
  "Highlight BARK reports in Gnus summary buffers."
  :group 'gnus)

(defface gnus-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in Gnus summary."
  :group 'gnus-bone)

(defface gnus-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'gnus-bone)

;;; Annotation rendering

(defun gnus-bone--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip            "_")
     (t               " "))))

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

;;; Summary overlays

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
  "Build a hash of normalized MIDs to report info for REPORTS.
Use VALUE-FN to compute each value when given, else the report info."
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
        (state  (bone-read-state)))
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
  "Activate REPORTS, optionally limiting summary to LIMIT-ARTICLES first."
  (when limit-articles (gnus-summary-limit limit-articles))
  (gnus-bone-clear)
  (setq gnus-bone--active-reports reports)
  (gnus-bone--apply-overlays reports)
  (gnus-bone--enable-hooks))

;;; Commands

(defun gnus-bone-highlight ()
  "Highlight summary lines of open BARK reports."
  (interactive)
  (let ((reports (bone-reports)))
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
  (let ((reports (bone-reports)))
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
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

(defun gnus-bone-topic ()
  "Limit summary to reports of selected topic and highlight them."
  (interactive)
  (let* ((reports (bone-reports))
         (topics  (gnus-bone--collect-topics reports)))
    (cond
     ((null reports) (message "No open BARK reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BARK topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (gnus-bone--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (let ((articles (gnus-bone--matching-articles filtered)))
            (if (null articles)
                (message "No matching articles for topic \"%s\"." topic)
              (gnus-bone--activate filtered articles)
              (message "Limited to %d BARK reports for topic \"%s\"."
                       (length articles) topic))))))))))

;;; Marking commands

(defun gnus-bone--current-mid ()
  "Current article's normalized MID, or nil."
  (let ((article (gnus-summary-article-number)))
    (when (and (numberp article) (> article 0))
      (let* ((header (gnus-summary-article-header article))
             (raw    (and header (mail-header-id header))))
        (when raw
          (bone-normalize-mid raw))))))

(defun gnus-bone--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc mid reports)))

(defun gnus-bone--refresh-overlays ()
  "Re-apply overlays in summary buffer."
  (when gnus-bone--active-reports
    (gnus-bone--apply-overlays gnus-bone--active-reports)))

(defun gnus-bone--mark (action on-msg off-msg)
  "Toggle ACTION mark on the current report, showing ON-MSG or OFF-MSG."
  (let* ((reports (or gnus-bone--active-reports (bone-reports)))
         (mid     (and reports (gnus-bone--current-mid)))
         (info    (and mid (gnus-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current article is not a BARK report: %s" mid))
     (t
      (let ((on (bone-toggle-mark mid info action)))
        (gnus-bone--refresh-overlays)
        (message "%s" (if on on-msg off-msg)))))))

(defun gnus-bone-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (gnus-bone--mark :sticky "Marked sticky" "Unmarked sticky"))

(defun gnus-bone-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (gnus-bone--mark :skip "Skipped" "Unskipped"))

(defun gnus-bone-clear ()
  "Remove all gnus-bone overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-bone t)
  (setq gnus-bone--active-reports nil)
  (gnus-bone--disable-hooks))

;; --- Cache update hooks ----------------------------------------------------

(defun gnus-bone--refresh-all-buffers ()
  "Re-apply overlays in active summary buffers from the refreshed cache."
  (let ((reports (bone-reports)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'gnus-summary-mode)
                   gnus-bone--active-reports)
          (setq gnus-bone--active-reports reports)
          (gnus-bone--apply-overlays reports))))))

(add-hook 'bone-after-update-hook #'gnus-bone--refresh-all-buffers)

(provide 'gnus-bone)
;;; gnus-bone.el ends here
