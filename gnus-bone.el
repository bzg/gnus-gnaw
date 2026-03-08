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

(defvar gnus-bone-sources-file "~/.config/bone/sources.json"
  "Path to bone sources.json, a JSON array of reports.json URIs.")

(defface gnus-bone-face
  '((((background light)) :background "#dddddd")
    (((background dark))  :background "#bbbbbb"))
  "Subtle highlight for BARK reports in Gnus summary.
Lighter variant of `hl-line' to avoid clashing."
  :group 'gnus-bone)

(defface gnus-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations (type, flags, priority, votes)."
  :group 'gnus-bone)

(defconst gnus-bone-supported-format-version "0.1.0"
  "Supported BONE reports.json format-version.")

(defun gnus-bone--uri-to-path (uri)
  "Convert a file:// URI to a local path."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun gnus-bone--load-sources ()
  "Return list of reports.json paths from `gnus-bone-sources-file'."
  (let ((json-array-type 'list))
    (mapcar #'gnus-bone--uri-to-path
            (json-read-file (expand-file-name gnus-bone-sources-file)))))

(defun gnus-bone--extract-open-reports (reports-file)
  "Extract report plists for open reports from REPORTS-FILE.
Each entry is (MESSAGE-ID . (:type T :flags F :priority P :votes V)).
A report is open when its status is >= 4."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (data (json-read-file reports-file))
         (fv (alist-get 'format-version data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (not (equal fv gnus-bone-supported-format-version)))
      (message "gnus-bone: %s has format-version %s, supported is %s"
               reports-file fv gnus-bone-supported-format-version))
    (dolist (r reports result)
      (let ((mid      (alist-get 'message-id r))
            (status   (alist-get 'status r))
            (type     (alist-get 'type r))
            (flags    (alist-get 'flags r))
            (priority (alist-get 'priority r))
            (votes    (alist-get 'votes r)))
        (when (and mid (numberp status) (>= status 4))
          (push (cons mid (list :type (or type "bug")
                                :flags (or flags "---")
                                :priority (or priority 0)
                                :votes votes))
                result))))))

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

(defun gnus-bone--annotation (info)
  "Build a fixed-width annotation string from report INFO plist."
  (let* ((type     (gnus-bone--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (pri-str  (format "P%d" priority))
         (votes-str (if votes
                        (format "[%s]" votes)
                      ""))
         (votes-pad (format (format "%%-%ds" gnus-bone-votes-width) votes-str))
         (tag       (concat type " " flags " " pri-str " " votes-pad)))
    tag))

(defun gnus-bone--apply-overlays (reports)
  "Apply overlays for REPORTS, a list of (message-id . plist).
The annotation replaces the rightmost columns of each line,
so it is always visible regardless of margins."
  (let ((id-map (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (gnus-bone--normalize-mid (car r)) (cdr r) id-map))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((article (gnus-summary-article-number))
               (header  (and (numberp article)
                             (> article 0)
                             (gnus-summary-article-header article)))
               (mid     (and header (mail-header-id header)))
               (info    (and mid (gethash mid id-map))))
          (when info
            (let* ((bol     (line-beginning-position))
                   (eol     (line-end-position))
                   (ann-str (gnus-bone--annotation info))
                   (ann-len (length ann-str))
                   (p3      (= 3 (plist-get info :priority)))
                   (face    (if p3 '(gnus-bone-face bold) 'gnus-bone-face))
                   ;; Background overlay on full line
                   (ov-bg   (make-overlay bol eol))
                   ;; Find where to start the annotation: ann-len + 1 gap
                   ;; chars before eol, but don't go before bol
                   (tag-len (+ ann-len 1))
                   (start   (max bol (- eol tag-len)))
                   (ov-ann  (make-overlay start eol)))
              (overlay-put ov-bg 'face face)
              (overlay-put ov-bg 'gnus-bone t)
              (overlay-put ov-ann 'display
                           (propertize (concat " " ann-str)
                                       'face 'gnus-bone-annotation-face))
              (overlay-put ov-ann 'gnus-bone t))))
        (forward-line 1)))))

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

(defun gnus-bone-highlight ()
  "Highlight summary lines whose message-id appears in open BARK reports.
Highlighting persists across `A T' and other summary updates."
  (interactive)
  (let ((reports (gnus-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (setq gnus-bone--active-reports reports)
      (gnus-bone--apply-overlays reports)
      (gnus-bone--enable-hooks)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun gnus-bone--matching-articles (reports)
  "Return article numbers in current summary matching REPORTS."
  (let ((id-map (make-hash-table :test 'equal))
        (articles nil))
    (dolist (r reports)
      (puthash (gnus-bone--normalize-mid (car r)) t id-map))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((article (gnus-summary-article-number))
               (header  (and (numberp article)
                             (> article 0)
                             (gnus-summary-article-header article)))
               (mid     (and header (mail-header-id header))))
          (when (and mid (gethash mid id-map))
            (push article articles)))
        (forward-line 1)))
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
          (gnus-summary-limit articles)
          (gnus-bone-clear)
          (setq gnus-bone--active-reports reports)
          (gnus-bone--apply-overlays reports)
          (gnus-bone--enable-hooks)
          (message "Limited to %d BARK reports." (length articles)))))))

(defun gnus-bone-clear ()
  "Remove all gnus-bone overlays and disable auto-rehighlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-bone t)
  (setq gnus-bone--active-reports nil)
  (gnus-bone--disable-hooks))

(provide 'gnus-bone)
;;; gnus-bone.el ends here
