;;; moom-font.el --- A module for resizing Japanese fonts for Moom

;; Copyright (C) 2017-2021 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 1.4.0
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/Moom
;; Twitter: @takaxp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is an additional module for Moom.el

;;; Change Log:

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defcustom moom-font-ja-scale 1.2
  "The default value to scale JP fonts."
  :type 'float
  :group 'moom)

(defcustom moom-font-ascii-scale 1.0
  "The default value to scale ASCII fonts."
  :type 'float
  :group 'moom)

(defcustom moom-font-table nil
  "Font table."
  :type '(repeat (list (integer :tag "Font size in point")
                       (integer :tag "Font width in pixels")))
  :group 'moom)

(defcustom moom-font-verbose nil
  "Show responses from `moom`."
  :type 'boolean
  :group 'moom)

(defcustom moom-font-before-resize-hook nil
  "Hook runs before resizing font size."
  :type 'hook
  :group 'moom)

(defcustom moom-font-after-resize-hook nil
  "Hook runs after resizing font size."
  :type 'hook
  :group 'moom)

(defvar moom-font--init-size 12
  "The default value to set font size.")
(defvar moom-font--size moom-font--init-size
  "Current font size.")
(defvar moom-font--pause nil)
(defvar moom-font--ascii nil)
(defvar moom-font--ja nil)

(defun moom-font--update-rescale-alist (key value)
  "Update `face-font-rescale-alist'.
If KEY exists in `face-font-rescale-alist', delete it before updating the list.
VALUE is a new value to re-scale the font in KEY."
  (setq face-font-rescale-alist
        (delete (assoc key face-font-rescale-alist) face-font-rescale-alist))
  (add-to-list 'face-font-rescale-alist `(,key . ,value)))

(defun moom-font--change-size (&optional arg)
  "Core function to change font size.
If `ARG' is nil, the default size is used."
  (unless moom-font--pause
    (when arg
      (setq moom-font--size arg))
    (moom-font--update-rescale-alist
     (concat ".*" moom-font--ascii ".*") moom-font-ascii-scale)
    (moom-font--update-rescale-alist
     (concat ".*" moom-font--ja ".*") moom-font-ja-scale)
    (set-fontset-font nil 'ascii
                      (font-spec :family moom-font--ascii
                                 :size moom-font--size))
    (let ((spec (font-spec :family moom-font--ja
                           :size moom-font--size)))
      (set-fontset-font nil 'japanese-jisx0208 spec)
      (set-fontset-font nil 'katakana-jisx0201 spec)
      (set-fontset-font nil 'japanese-jisx0212 spec)
      (set-fontset-font nil '(#x0080 . #x024F) spec)
      (set-fontset-font nil '(#x0370 . #x03FF) spec)
      (set-fontset-font nil 'mule-unicode-0100-24ff spec))))

(defun moom-font--extract-font-size (xlfd)
  "Try to identify the font size.
Return an integer value extracted from XLFD if possible, otherwise return nil."
  (when (stringp xlfd)
    (let ((size
           (string-to-number
            (if (string-match
                 "^-[^-]+-[^-]+-[^-]+-[^-]+-[^-]+-[^-]+-\\([^-]+\\)-.*$" xlfd)
                (match-string 1 xlfd) "0"))))
      (if (> size 0) size nil))))

(defun moom-font--extract-family-name (xlfd)
  "Try to identify the font name.
Return a font name extracted from XLFD if possible, otherwise return nil."
  (when (stringp xlfd)
    (let* ((name (when (string-match "^-[^-]+-\\([^-]+\\)-.*$" xlfd)
                   (match-string 1 xlfd))))
      (if (and name (x-list-fonts name)) name nil))))

(defun moom-font--find-size (width table)
  "Return font size associated with WIDTH by looking up the TABLE."
  (when table
    (car (rassoc (list width) table))))

(defun moom-font--find-width (size table)
  "Return font width associated with SIZE by looking up the TABLE."
  (when table
    (cdr (assoc size table))))

(defun moom-font--generate-font-table (&optional begin end)
  "Generate a font table.
If BEGIN is nil, use 5 as the default value.
If END is nil, use 50 as the default value."
  (let ((moom-font-table nil))
    (cl-loop
     for pt from (or begin 5) to (or end 50)
     do
     (moom-font-resize pt)
     (push (list moom-font--size (frame-char-width)) moom-font-table))
    (let ((buffer "*moom-font*"))
      (with-current-buffer (get-buffer-create buffer)
        (erase-buffer)
        (insert ";; 1. M-x eval-buffer\n")
        (insert ";; 2. Paste the following configurations into your init.el.\n")
        (insert "(with-eval-after-load \"moom-font\"\n")
        (insert (format "  (setq moom-scaling-gradient (/ (float %d) %d))\n"
                        (nth 0 (car moom-font-table))
                        (nth 1 (car moom-font-table))))
        (insert (format "  (setq moom-font-table (quote %s)))" moom-font-table))
        (goto-char 0)
        (switch-to-buffer buffer)))))

(defun moom-font--font-exists-p (font-family)
  "Check given FONT-FAMILY exists."
  (when window-system
    (let ((result (and (fboundp 'x-list-fonts)
                       (x-list-fonts font-family))))
      (if result
          (when moom-font-verbose
            (message "[moom-font] \"%s\" is available." font-family))
        (warn "[moom-font] \"%s\" is NOT installed in your system."
              font-family))
      result)))

;;;###autoload
(defun moom-font-ascii (font &optional plist)
  "Set ASCII font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied."
  (when (moom-font--font-exists-p font)
    (setq moom-font--ascii font)
    (let ((font-size (plist-get plist :size)))
      (when font-size
        (setq moom-font--size font-size)))
    (let ((rescale (plist-get plist :scale)))
      (when rescale
        (setq moom-font-ascii-scale rescale)))
    (when (plist-get plist :immediate)
      (run-hooks 'moom-font-before-resize-hook)
      (moom-font--change-size)
      (run-hooks 'moom-font-after-resize-hook))))

;;;###autoload
(defun moom-font-ja (font &optional plist)
  "Set Japanese font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied."
  (when (moom-font--font-exists-p font)
    (setq moom-font--ja font)
    (let ((rescale (plist-get plist :scale)))
      (when rescale
        (setq moom-font-ja-scale rescale)))
    (when (plist-get plist :immediate)
      (moom-font--change-size))))

;;;###autoload
(defun moom-font-resize (&optional n width)
  "Resize font.
`frame-width' will be updated accordingly.
Optional argument N specifies the target font size.
If WIDTH is non-nil, ensure an appropriate font size so that
the actual pixel width will not exceed the WIDTH."
  (interactive "nSize: ")
  (run-hooks 'moom-font-before-resize-hook)
  (unless moom-font--pause
    (moom-font--change-size
     (setq moom-font--size (or n moom-font--init-size)))
    (when (and width
               (< width (frame-pixel-width)))
      (when moom-font-verbose
        (message "[moom-font] Font size is changed from %s to %s."
                 moom-font--size (1- moom-font--size)))
      (moom-font--change-size
       (setq moom-font--size (1- moom-font--size))))
    (when moom-font-verbose
      (message "[moom-font] %s" moom-font--size)))
  (run-hooks 'moom-font-after-resize-hook))

;;;###autoload
(defun moom-font-size-reset ()
  "Reset font to the initial size."
  (interactive)
  (run-hooks 'moom-font-before-resize-hook)
  (moom-font--change-size
   (setq moom-font--size moom-font--init-size))
  (when moom-font-verbose
    (message "[moom-font] %s" moom-font--size))
  (run-hooks 'moom-font-after-resize-hook))

;;;###autoload
(defun moom-font-increase (&optional inc)
  "Increase font size.
Optional argument INC specifies an increasing step."
  (interactive)
  (run-hooks 'moom-font-before-resize-hook)
  (unless moom-font--pause
    (setq moom-font--size
          (+ moom-font--size
             (if (and (integerp inc) (> inc 0))
                 inc 1)))
    (moom-font--change-size moom-font--size)
    (when moom-font-verbose
      (message "[moom-font] +%d: %s"
               (if (integerp inc) inc 1) moom-font--size)))
  (run-hooks 'moom-font-after-resize-hook))

;;;###autoload
(defun moom-font-decrease (&optional dec)
  "Decrease font size.
Optional argument DEC specifies a decreasing step."
  (interactive)
  (run-hooks 'moom-font-before-resize-hook)
  (unless moom-font--pause
    (setq moom-font--size
          (- moom-font--size
             (if (and (integerp dec)
                      (> dec 0)
                      (> moom-font--size dec))
                 dec 1)))
    (when (< moom-font--size 1)
      (setq moom-font--size 1))
    (when (and moom-font-verbose
               (> moom-font--size 0))
      (message "[moom-font] -%d: %s"
               (if (integerp dec) dec 1) moom-font--size))
    (moom-font--change-size moom-font--size))
  (run-hooks 'moom-font-after-resize-hook))

;;;###autoload
(defun moom-font-print-name-at-point ()
  "Print font family name at point."
  (interactive)
  (if (eq (point) (point-max))
      (message "[moom-font] Not on a character. Move cursor, and try again.")
    (let* ((xlfd-name (font-xlfd-name (font-at (point))))
           (family-name (moom-font--extract-family-name xlfd-name)))
      (if family-name
          (message
           "[moom-font] It's \"%s\", %s[pt].\n[moom-font] Call `moom-font-ja' or `moom-font-ascii' with \"%s\"."
           family-name moom-font--size family-name)
        (message
         "[moom-font] Failed to detect the font family name from \"%s\"."
         xlfd-name)))))

;; init
(when (and window-system
           (fboundp 'x-list-fonts))
  (let* ((default-font (face-font 'default nil ?A))
         (size
          (moom-font--extract-font-size default-font))
         (ascii-font
          (moom-font--extract-family-name default-font))
         (ja-font
          (moom-font--extract-family-name (face-font 'default nil ?あ))))
    (when size
      (setq moom-font--size
            (setq moom-font--init-size size)))
    ;; Apply font if found. Otherwise, use the default ASCII or Japanese font.
    (if ascii-font
        (moom-font-ascii ascii-font)
      (cond ((memq window-system '(ns mac))
             (moom-font-ascii "Monaco"))
            ((eq window-system 'w32)
             (moom-font-ascii "ＭＳ ゴシック"))
            ((eq window-system 'x)
             (moom-font-ascii "TakaoGothic"))))
    (if ja-font
        (moom-font-ja ja-font)
      (cond ((memq window-system '(ns mac))
             (moom-font-ja "Osaka"))
            ((eq window-system 'w32)
             (moom-font-ja "ＭＳ ゴシック" '(:size 1.0)))
            ((eq window-system 'x)
             (moom-font-ja "TakaoGothic" '(:size 1.0)))))))

(provide 'moom-font)

;;; moom-font.el ends here
