;;; moom.el --- A tool to control frame size, position, and font size
;;
;; Copyright (C) 2017 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Repository: nil
;; Keywords: frame-cmds, frame, size, position
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'frame-cmds)
(eval-when-compile (require 'cl-lib))

(defcustom move-frame-pixel-menubar-offset 22
  "Offset of the menubar. The default height is 22 for MacOSX"
  :type 'integer
  :group 'taka-frame-control)

(defcustom move-frame-pixel-offset '(0 . 0)
  "Offset of the center position"
  :type 'sexp
  :group 'takaxp-frame-control)

(defcustom auto-move-frame-to-center nil
  "Toggle status of moving frame to center"
  :type 'boolean
  :group 'takaxp-frame-control)

(defcustom min-frame-height 16
  "The minimum height"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom fullscreen-fontsize 24
  "Font size will be used for fullscreen"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom init-line-spacing line-spacing
  "The default value to set line-spacing"
  :type 'float
  :group 'takaxp-frame-control)

(defcustom min-line-spacing 0.1
  "The minimum value for line spacing"
  :type 'float
  :group 'takaxp-frame-control)

(defcustom max-line-spacing 0.8
  "The maximum value for line spacing"
  :type 'float
  :group 'takaxp-frame-control)

(defcustom init-font-size 12
  "The default value to set font size"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom moom-verbose nil
  "Show responses from `moom`"
  :type 'boolean
  :group 'takaxp-frame-control)

(defvar target-font-size init-font-size)
(defun make-frame-height-ring ()
  ""
  (let ((max-height (max-frame-height)))
    (moom-make-height-ring
     ;; 最大，最小，最大の50%，最大の75% を指定
     (cons max-height
           (sort (list
                  (max (min-frame-height) (/ max-height 4))
                  (/ max-height 2)
                  (* 3 (/ max-height 4))) '<)))))

(defvar target-frame-width 80)
(defun set-font-size (arg)
  (let* ((font-size arg)
         (frame-width target-frame-width)
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
    (reset-frame-height (frame-height))))

;;;###autoload
(defun set-font-size-input (n)
  (interactive "nSize: ")
  (setq target-font-size n)
  (set-font-size target-font-size)
  (when moom-verbose
    (message "0: %s" target-font-size))
  (make-frame-height-ring)
  (reset-frame-height (max-frame-height)))

;;;###autoload
(defun increase-font-size (&optional inc)
  "Increase font size"
  (interactive)
  (setq target-font-size
        (+ target-font-size
           (if (and (integerp inc) (> inc 0))
               inc 1)))
  (set-font-size target-font-size)
  (when moom-verbose
    (message "+%d: %s" inc target-font-size))
  (make-frame-height-ring)
  (moom-open-height-ring))

;;;###autoload
(defun decrease-font-size (&optional dec)
  "Decrease font size"
  (interactive)
  (when (and (integerp dec)
             (> dec 0)
             (> target-font-size dec))
    (setq target-font-size (- target-font-size dec)))
  (when (and moom-verbose
             (> target-font-size 0))
    (message "-%d: %s" dec target-font-size))
  (set-font-size target-font-size)
  (make-frame-height-ring)
  (moom-open-height-ring))

;;;###autoload
(defun reset-font-size ()
  "Reset font size"
  (interactive)
  (set-font-size init-font-size)
  (setq target-font-size init-font-size)
  (when moom-verbose
    (message "0: %s" target-font-size))
  (make-frame-height-ring)
  (moom-open-height-ring))

;;;###autoload
(defun cycle-line-spacing ()
  "Change line-spacing value between a range"
  (interactive)
  (if (< line-spacing max-line-spacing)
      (setq line-spacing (+ line-spacing 0.1))
    (setq line-spacing min-line-spacing))
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun reset-line-spacing ()
  "Reset the defaut value for line spacing"
  (interactive)
  (setq line-spacing init-line-spacing)
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun toggle-auto-move-frame-to-center ()
  "Change whether move the frame to center automatically"
  (interactive)
  (cond (auto-move-frame-to-center
         (setq auto-move-frame-to-center nil)
         (when moom-verbose
           (message "Toggle auto move OFF")))
        (t
         (setq auto-move-frame-to-center t)
         (when moom-verbose
           (message "Toggle auto move ON")))))

;;;###autoload
(defun move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (+ (car move-frame-pixel-offset)
                         (/ (- (display-pixel-width) (frame-pixel-width)) 2))
                      (frame-parameter (selected-frame) 'top)))

;;;###autoload
(defun move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      (+ (cdr move-frame-pixel-offset)
                         (/ (- (display-pixel-height)
                               (frame-pixel-height)) 2))))

;;;###autoload
(defun move-frame-to-edge-top ()
  "Move the current frame to the top of the window display"
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      0))

;;;###autoload
(defun move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display
   If you find the frame is NOT moved to the bottom exactly,
   Please set `move-frame-pixel-menubar-offset'.
   22 is the default value for MacOSX"
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      (- (- (display-pixel-height) (frame-pixel-height))
                         move-frame-pixel-menubar-offset)))

;;;###autoload
(defun move-frame-to-center ()
  "Move the current frame to the center of the window display."
  (interactive)
  (let
      ((prev-pos-x (frame-parameter (selected-frame) 'left))
       (prev-pos-y (frame-parameter (selected-frame) 'top))
       (center-pos-x
        (+ (car move-frame-pixel-offset)
           (/ (- (display-pixel-width) (frame-pixel-width)) 2)))
       (center-pos-y
        (+ (cdr move-frame-pixel-offset)
           (/ (- (display-pixel-height) (frame-pixel-height)) 2))))
    (set-frame-position (selected-frame) center-pos-x center-pos-y)
    (when moom-verbose
      (message "Frame move: from (%s, %s) to (%s, %s)"
               prev-pos-x
               prev-pos-y
               (frame-parameter (selected-frame) 'left)
               (frame-parameter (selected-frame) 'top)))))

;;;###autoload
(defun move-frame-with-user-specify (&optional arg)
  "Move the frame to somewhere (default: 0,0).
   Use prefix to specify the destination position."
  (interactive "P")
  (let ((pos-x 0)
        (pos-y move-frame-pixel-menubar-offset))
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
(defun max-frame-height ()
  "Return the maximum height based on screen size."
  (interactive)
  (/ (- (display-pixel-height) 64) (frame-char-height)))

;;;###autoload
(defun min-frame-height ()
  "Return the minimum height of frame"
  (interactive)
  min-frame-height)

;;;###autoload
(defun reset-frame-height (new-height)
  "Reset the hight of the current frame."
  (interactive
   (list (string-to-number
          (read-string "New Height: " (number-to-string (frame-height))))))
  (let ((min-height (min-frame-height))
        (max-height (max-frame-height)))
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

;;;###autoload
(defun fit-frame-to-fullscreen ()
  "Change font size and expand height to fit full"
  (interactive)
  (setq target-font-size fullscreen-fontsize)
  (set-font-size target-font-size)
  (reset-frame-height (max-frame-height))
  (run-hooks 'moom-after-fullscreen-hook))

(defvar moom-height-ring nil)
(defun moom-make-height-ring (heights)
  "Cycle change the height of the current frame."
  (setq moom-height-ring (copy-sequence heights)))

;;;###autoload
(defun moom-open-height-ring ()
  ""
  (interactive)
  (reset-frame-height (car moom-height-ring))
  (setq moom-height-ring
        (append (cdr moom-height-ring)
                (list (car moom-height-ring)))))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame origin, and frame size in mini buffer."
  (interactive)
  (message "Font: %spt | Origin: (%s, %s) | Frame: (%d, %d)"
           target-font-size
           (frame-parameter (selected-frame) 'left)
           (frame-parameter (selected-frame) 'top)
           (frame-width)
           (frame-height)))

(provide 'moom)
