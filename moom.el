;;; moom.el --- Commands to control frame size and position

;; Copyright (C) 2017 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 0.9.0
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/Moom
;; Package-Requires: ((frame-cmds "0"))
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

;; This package provides a set of tools to control frame size, position, and font size.

;;; Change Log:

;;; Code:

(require 'frame-cmds)
(eval-when-compile (require 'cl-lib))

(defgroup moom nil
  "A tool to control frame size, position, and font size."
  :group 'convenience)

(defcustom moom-move-frame-pixel-menubar-offset 22
  "Offset of the menubar. The default height is 22 for MacOSX"
  :type 'integer
  :group 'moom)

(defcustom moom-move-frame-pixel-offset '(0 . 0)
  "Offset of the center position"
  :type 'sexp
  :group 'moom)

(defcustom moom-auto-move-frame-to-center nil
  "Toggle status of moving frame to center"
  :type 'boolean
  :group 'moom)

(defcustom moom-min-frame-height 16
  "The minimum height"
  :type 'integer
  :group 'moom)

(defcustom moom-fullscreen-font-size 24
  "Font size will be used for fullscreen"
  :type 'integer
  :group 'moom)

(defcustom moom-init-line-spacing line-spacing
  "The default value to set line-spacing"
  :type 'float
  :group 'moom)

(defcustom moom-min-line-spacing 0.1
  "The minimum value for line spacing"
  :type 'float
  :group 'moom)

(defcustom moom-max-line-spacing 0.8
  "The maximum value for line spacing"
  :type 'float
  :group 'moom)

(defcustom moom-init-font-size 12
  "The default value to set font size"
  :type 'integer
  :group 'moom)

(defcustom moom-verbose nil
  "Show responses from `moom`"
  :type 'boolean
  :group 'moom)

(defcustom moom-frame-width-single 80
  "The width of the current frame as the default value"
  :type 'integer
  :group 'moom)

(defcustom moom-frame-width-double 163
  "The width of the current frame (double size)"
  :type 'integer
  :group 'moom)

(defcustom moom-horizontal-shifts '(40 40)
  "Distance to move the frame horizontally"
  :type '(choice (integer :tag "common value for left and right")
                 (list (integer :tag "value for left")
                       (integer :tag "value for right")))
  :group 'moom)

(defun moom--make-frame-height-ring ()
  ""
  (let ((max-height (moom-max-frame-height)))
    (moom-make-height-ring
     ;; Specify Maximum, Minimum, 50%, and 75% values
     (cons max-height
           (sort (list
                  (max (moom-min-frame-height) (/ max-height 4))
                  (/ max-height 2)
                  (* 3 (/ max-height 4)))
                 '<)))))

(defvar moom--target-frame-width moom-frame-width-single)
(defun moom--set-font-size (&optional arg)
  (let* ((font-size (or arg moom--target-font-size))
         (frame-width moom--target-frame-width)
         (ja-font-scale 1.2)
         (ja-font "Migu 2M")
         (ascii-font "Monaco"))

    (set-fontset-font nil 'ascii (font-spec :family ascii-font :size font-size))
    (let ((spec (font-spec :family ja-font :size font-size)))
      (set-fontset-font nil 'japanese-jisx0208 spec)
      (set-fontset-font nil 'katakana-jisx0201 spec)
      (set-fontset-font nil 'japanese-jisx0212 spec)
      (set-fontset-font nil '(#x0080 . #x024F) spec)
      (set-fontset-font nil '(#x0370 . #x03FF) spec)
      (set-fontset-font nil 'mule-unicode-0100-24ff spec))
    (setq face-font-rescale-alist
          `((".*Migu.*" . ,ja-font-scale)))
    (set-frame-width (selected-frame) frame-width)
    (moom-reset-frame-height (frame-height))))

(defvar moom--target-font-size moom-init-font-size)

;;;###autoload
(defun moom-set-font-size-input (n)
  (interactive "nSize: ")
  (setq moom--target-font-size n)
  (moom--set-font-size moom--target-font-size)
  (when moom-verbose
    (message "0: %s" moom--target-font-size))
  (moom--make-frame-height-ring)
  (moom-reset-frame-height (moom-max-frame-height)))

;;;###autoload
(defun moom-increase-font-size (&optional inc)
  "Increase font size"
  (interactive)
  (setq moom--target-font-size
        (+ moom--target-font-size
           (if (and (integerp inc) (> inc 0))
               inc 1)))
  (moom--set-font-size moom--target-font-size)
  (when moom-verbose
    (message "+%d: %s" inc moom--target-font-size))
  (moom--make-frame-height-ring)
  (moom-open-height-ring))

;;;###autoload
(defun moom-decrease-font-size (&optional dec)
  "Decrease font size"
  (interactive)
  (when (and (integerp dec)
             (> dec 0)
             (> moom--target-font-size dec))
    (setq moom--target-font-size (- moom--target-font-size dec)))
  (when (and moom-verbose
             (> moom--target-font-size 0))
    (message "-%d: %s" dec moom--target-font-size))
  (moom--set-font-size moom--target-font-size)
  (moom--make-frame-height-ring)
  (moom-open-height-ring))

;;;###autoload
(defun moom-reset-font-size ()
  "Reset font size"
  (interactive)
  (moom--set-font-size moom-init-font-size)
  (setq moom--target-font-size moom-init-font-size)
  (when moom-verbose
    (message "0: %s" moom--target-font-size))
  (moom--make-frame-height-ring)
  (moom-open-height-ring)
  (run-hooks 'moom-reset-font-size-hook))

;;;###autoload
(defun moom-cycle-line-spacing ()
  "Change line-spacing value between a range"
  (interactive)
  (if (< line-spacing moom-max-line-spacing)
      (setq line-spacing (+ line-spacing 0.1))
    (setq line-spacing moom-min-line-spacing))
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun moom-reset-line-spacing ()
  "Reset the defaut value for line spacing"
  (interactive)
  (setq line-spacing moom-init-line-spacing)
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun moom-toggle-auto-move-frame-to-center ()
  "Change whether move the frame to center automatically"
  (interactive)
  (cond (moom-auto-move-frame-to-center
         (setq moom-auto-move-frame-to-center nil)
         (when moom-verbose
           (message "Toggle auto move OFF")))
        (t
         (setq moom-auto-move-frame-to-center t)
         (when moom-verbose
           (message "Toggle auto move ON")))))

;;;###autoload
(defun moom-move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (+ (car moom-move-frame-pixel-offset)
                         (/ (- (display-pixel-width) (frame-pixel-width)) 2))
                      (frame-parameter (selected-frame) 'top)))

;;;###autoload
(defun moom-move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      (+ (cdr moom-move-frame-pixel-offset)
                         (/ (- (display-pixel-height)
                               (frame-pixel-height)) 2))))

;;;###autoload
(defun moom-move-frame-to-edge-top ()
  "Move the current frame to the top of the window display"
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      0))

;;;###autoload
(defun moom-move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display
   If you find the frame is NOT moved to the bottom exactly,
   Please set `moom-move-frame-pixel-menubar-offset'.
   22 is the default value for MacOSX"
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      (- (- (display-pixel-height) (frame-pixel-height))
                         moom-move-frame-pixel-menubar-offset)))

;;;###autoload
(defun moom-move-frame-to-edge-right ()
  "Move the current frame to the right edge of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (- (display-pixel-width) (frame-pixel-width))
                      (frame-parameter (selected-frame) 'top)))

;;;###autoload
(defun moom-move-frame-to-edge-left ()
  "Move the current frame to the left edge of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      0
                      (frame-parameter (selected-frame) 'top)))

;;;###autoload
(defun moom-move-frame-to-center ()
  "Move the current frame to the center of the window display."
  (interactive)
  (let
      ((prev-pos-x (frame-parameter (selected-frame) 'left))
       (prev-pos-y (frame-parameter (selected-frame) 'top))
       (center-pos-x
        (+ (car moom-move-frame-pixel-offset)
           (/ (- (display-pixel-width) (frame-pixel-width)) 2)))
       (center-pos-y
        (+ (cdr moom-move-frame-pixel-offset)
           (/ (- (display-pixel-height) (frame-pixel-height)) 2))))
    (set-frame-position (selected-frame) center-pos-x center-pos-y)
    (when moom-verbose
      (message "Frame move: from (%s, %s) to (%s, %s)"
               prev-pos-x
               prev-pos-y
               (frame-parameter (selected-frame) 'left)
               (frame-parameter (selected-frame) 'top)))))

;;;###autoload
(defun moom-move-frame-with-user-specify (&optional arg)
  "Move the frame to somewhere (default: 0,0).
   Use prefix to specify the destination position."
  (interactive "P")
  (let ((pos-x 0)
        (pos-y moom-move-frame-pixel-menubar-offset))
    (when arg
      (setq pos-x (string-to-number
                   (read-from-minibuffer
                    (format "X: from %s to "
                            (frame-parameter (selected-frame) 'left)))))
      (setq pos-y (string-to-number
                   (read-from-minibuffer
                    (format "Y: from %s to "
                            (frame-parameter (selected-frame) 'top))))))
    (set-frame-position (selected-frame) pos-x pos-y)
    (when moom-verbose
      (message "Frame move: (%s, %s)"
               (frame-parameter (selected-frame) 'left)
               (frame-parameter (selected-frame) 'top)))))

;;;###autoload
(defun moom-max-frame-height ()
  "Return the maximum height based on screen size."
  (interactive)
  (/ (- (display-pixel-height) (* 2 moom-move-frame-pixel-menubar-offset))
     (frame-char-height)))

;;;###autoload
(defun moom-min-frame-height ()
  "Return the minimum height of frame"
  (interactive)
  moom-min-frame-height)

;;;###autoload
(defun moom-reset-frame-height (new-height)
  "Reset the hight of the current frame."
  (interactive
   (list (string-to-number
          (read-string "New Height: " (number-to-string (frame-height))))))
  (let ((min-height (moom-min-frame-height))
        (max-height (moom-max-frame-height)))
    (when (> new-height max-height)
      (setq new-height max-height)
      (when moom-verbose
        (message "Force set the height %s." new-height)))
    (when (< new-height min-height)
      (setq new-height min-height)
      (when moom-verbose
        (message "Force set the height %s." new-height)))
    (let ((height (floor new-height)))
      (set-frame-height (selected-frame) height))))

(defvar moom-after-fullscreen-hook nil "")
(defvar moom-reset-font-size-hook nil "")

;;;###autoload
(defun moom-fit-frame-to-fullscreen ()
  "Change font size and expand height to fit full. Add an appropriate function to `moom-after-fullscreen-hook' if the frame move to specific position."
  (interactive)
  (setq moom--target-font-size moom-fullscreen-font-size)
  (moom--set-font-size moom--target-font-size)
  (moom-reset-frame-height (moom-max-frame-height))
  (run-hooks 'moom-after-fullscreen-hook))

(defvar moom--height-ring nil)
(defun moom-make-height-ring (heights)
  "Cycle change the height of the current frame."
  (setq moom--height-ring (copy-sequence heights)))

;;;###autoload
(defun moom-open-height-ring ()
  ""
  (interactive)
  (moom-reset-frame-height (car moom--height-ring))
  (setq moom--height-ring
        (append (cdr moom--height-ring)
                (list (car moom--height-ring)))))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame origin, and frame size in mini buffer."
  (interactive)
  (message "Font: %spt | Origin: (%s,\t%s) | Frame: (%d, %d)"
           moom--target-font-size
           (frame-parameter (selected-frame) 'left)
           (frame-parameter (selected-frame) 'top)
           (frame-width)
           (frame-height)))

;;;###autoload
(defun moom-move-frame-right (&optional N FRAME)
  ""
  (interactive)
  (move-frame-right
   (or N
       (cond ((integerp moom-horizontal-shifts)
              moom-horizontal-shifts)
             ((listp moom-horizontal-shifts)
              (nth 1 moom-horizontal-shifts))
             (t
              (error (format "%s is wrong value." moom-horizontal-shifts)))))
   FRAME))

;;;###autoload
(defun moom-move-frame-left (&optional N FRAME)
  ""
  (interactive)
  (move-frame-left
   (or N
       (cond ((integerp moom-horizontal-shifts)
              moom-horizontal-shifts)
             ((listp moom-horizontal-shifts)
              (nth 0 moom-horizontal-shifts))
             (t
              (error (format "%s is wrong value." moom-horizontal-shifts)))))
   FRAME))

;;;###autoload
(defun moom-change-frame-width-single ()
  "Change the frame width to double"
  (interactive)
  (setq moom--target-frame-width moom-frame-width-single)
  (set-frame-width (selected-frame) moom-frame-width-single))

;;;###autoload
(defun moom-change-frame-width-double ()
  "Change the frame width to double"
  (interactive)
  (setq moom--target-frame-width moom-frame-width-double)
  (set-frame-width (selected-frame) moom-frame-width-double))

;;;###autoload
(defun moom-version ()
  "The release version of Moom."
  (interactive)
  (let ((moom-release "0.9.0"))
    (message "Moom: v%s" moom-release)))

;; init call
(moom--make-frame-height-ring)

(provide 'moom)

;;; moom.el ends here
