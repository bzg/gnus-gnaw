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
;; M-x gnus-bone-highlight RET will highlight BARK reports
;; M-x gnus-bone-clear RET will unhighlight BARK reports
;;
;;; Code:

(require 'json)

(defvar gnus-bone-sources-file "~/.config/bone/sources.json"
  "Path to bone sources.json, a JSON array of reports.json URIs.")

(defface gnus-bone-bug-face
  '((t :foreground "red" :weight bold))
  "Face for BONE bug reports in Gnus summary."
  :group 'gnus-bone)

(defface gnus-bone-patch-face
  '((t :foreground "blue" :weight bold))
  "Face for BONE patch reports in Gnus summary."
  :group 'gnus-bone)

(defface gnus-bone-request-face
  '((t :foreground "orange" :weight bold))
  "Face for BONE request reports in Gnus summary."
  :group 'gnus-bone)

(defface gnus-bone-acked-face
  '((t :slant italic))
  "Additional face for acked BONE reports."
  :group 'gnus-bone)

(defface gnus-bone-owned-face
  '((t :weight ultra-bold))
  "Additional face for owned BONE reports."
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
  "Extract (message-id type flags) lists for open reports from REPORTS-FILE.
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
      (let ((mid    (alist-get 'message-id r))
            (status (alist-get 'status r))
            (type   (alist-get 'type r))
            (flags  (alist-get 'flags r)))
        (when (and mid (numberp status) (>= status 4))
          (push (list mid (or type "bug") (or flags "---")) result))))))

(defun gnus-bone--load-all-open-reports ()
  "Collect open (message-id type flags) lists from all sources."
  (mapcan #'gnus-bone--extract-open-reports (gnus-bone--load-sources)))

(defun gnus-bone--type-face (type)
  "Return the face for report TYPE."
  (pcase type
    ("patch"   'gnus-bone-patch-face)
    ("request" 'gnus-bone-request-face)
    (_         'gnus-bone-bug-face)))

(defun gnus-bone--flags-faces (flags)
  "Return extra faces based on FLAGS string."
  (let ((extra '()))
    (when (and (>= (length flags) 1) (eq (aref flags 0) ?a))
      (push 'gnus-bone-acked-face extra))
    (when (and (>= (length flags) 2) (eq (aref flags 1) ?o))
      (push 'gnus-bone-owned-face extra))
    extra))

(defun gnus-bone--normalize-mid (mid)
  "Ensure MID is bracketed."
  (if (string-match-p "^<.*>$" mid)
      mid
    (concat "<" mid ">")))

(defun gnus-bone--apply-overlays (reports)
  "Apply overlays for REPORTS, a list of (message-id type flags)."
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
            (let* ((type  (car info))
                   (flags (cadr info))
                   (face  (gnus-bone--type-face type))
                   (extra (gnus-bone--flags-faces flags))
                   (ov    (make-overlay (line-beginning-position)
                                        (line-end-position))))
              (overlay-put ov 'face (cons face extra))
              (overlay-put ov 'gnus-bone t))))
        (forward-line 1)))))

(defun gnus-bone-highlight ()
  "Highlight summary lines whose message-id appears in open BONE reports."
  (interactive)
  (let ((reports (gnus-bone--load-all-open-reports)))
    (when reports
      (gnus-bone--apply-overlays reports))))

(defun gnus-bone-clear ()
  "Remove all gnus-bone overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-bone t))
