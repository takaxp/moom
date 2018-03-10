;;; moom.el --- Commands to control frame size and position

;; Copyright (C) 2017-2018 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 0.9.3
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/Moom
;; Package-Requires: ((emacs "25") (frame-cmds "0"))
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
  "Offset of the menubar.
The default height is 22 for macOS."
  :type 'integer
  :group 'moom)

(defcustom moom-move-frame-pixel-offset '(0 . 0)
  "Offset of the center position."
  :type 'sexp
  :group 'moom)

(defcustom moom-auto-move-frame-to-center nil
  "Toggle status of moving frame to center."
  :type 'boolean
  :group 'moom)

(defcustom moom-min-frame-height 16
  "The minimum height."
  :type 'integer
  :group 'moom)

(defcustom moom-fullscreen-font-size 24
  "Font size will be used for fullscreen."
  :type 'integer
  :group 'moom)

(defcustom moom-init-line-spacing line-spacing
  "The default value to set ‘line-spacing’."
  :type 'float
  :group 'moom)

(defcustom moom-min-line-spacing 0.1
  "The minimum value for line spacing."
  :type 'float
  :group 'moom)

(defcustom moom-max-line-spacing 0.8
  "The maximum value for line spacing."
  :type 'float
  :group 'moom)

(defcustom moom-frame-width-single 80
  "The width of the current frame as the default value."
  :type 'integer
  :group 'moom)

(defcustom moom-frame-width-double 163
  "The width of the current frame (double size)."
  :type 'integer
  :group 'moom)

(defcustom moom-horizontal-shifts '(40 40)
  "Distance to move the frame horizontally."
  :type '(choice (integer :tag "common value for left and right")
                 (list (integer :tag "value for left")
                       (integer :tag "value for right")))
  :group 'moom)

(defcustom moom-verbose nil
  "Show responses from \"moom\"."
  :type 'boolean
  :group 'moom)

(defcustom moom-after-fullscreen-hook nil
  "Hook for fullscreen."
  :type 'hook
  :group 'moom)

(defvar moom-font-module (require 'moom-font nil t)
  "A flag to check the availability of `moom-font'.")
(defvar moom--height-ring nil)
(defvar moom--frame-width moom-frame-width-single)

(defun moom--min-frame-height ()
  "Return the minimum height of frame."
  moom-min-frame-height)

(defun moom--max-frame-width ()
  "Return the maximum width based on screen size."
  (let ((ns-frame-margin 2))
    (/ (- (display-pixel-width)
          (+ (frame-parameter nil 'left-fringe)
             (frame-parameter nil 'right-fringe)
             (* 2 ns-frame-margin)))
       (frame-char-width))))

(defun moom--max-frame-height ()
  "Return the maximum height based on screen size."
  (/ (- (display-pixel-height)
        (+ (nthcdr 2 (assoc 'title-bar-size (frame-geometry)))
           moom-move-frame-pixel-menubar-offset))
     (frame-char-height)))

(defun moom--update-frame-height-ring ()
  "Open ring after re-creating ring."
  (moom-open-height-ring t))

(defun moom--make-frame-height-ring ()
  "Create ring to change frame height."
  (let ((max-height (moom--max-frame-height)))
    (moom-make-height-ring
     ;; Specify Maximum, Minimum, 50%, and 75% values
     (cons max-height
           (sort (list
                  (max (moom--min-frame-height) (/ max-height 4))
                  (/ max-height 2)
                  (* 3 (/ max-height 4)))
                 '<)))))

;;;###autoload
(defun moom-fullscreen-font-size ()
  "Return the maximum font-size for full screen."
  (if window-system
      (let ((ns-frame-margin 2)
            (scale (if (boundp 'moom-font-ja-scale)
                       moom-font-ja-scale
                     1.0)))
        (floor (/ (- (display-pixel-width)
                     (+ (frame-parameter nil 'left-fringe)
                        (frame-parameter nil 'right-fringe)
                        (* 2 ns-frame-margin)))
                  (* (/ 80 2) scale))))
    moom-fullscreen-font-size))

;;;###autoload
(defun moom-fit-frame-to-fullscreen ()
  "Change font size and expand frame width and height to fit full.
Add appropriate functions to `moom-after-fullscreen-hook'
in order to move the frame to specific position."
  (interactive)
  (if (not (fboundp 'moom-font-resize))
      (moom-change-frame-height (moom--max-frame-height))
    (let ((f (moom-fullscreen-font-size)))
      (moom-font-resize f)
      (when (< (display-pixel-width)
               (frame-pixel-width))
        (moom-font-resize (1- f))))
    (moom-change-frame-width (moom--max-frame-width)))
  (run-hooks 'moom-after-fullscreen-hook))

;;;###autoload
(defun moom-cycle-line-spacing ()
  "Change ‘line-spacing’ value between a range."
  (interactive)
  (if (< line-spacing moom-max-line-spacing)
      (setq line-spacing (+ line-spacing 0.1))
    (setq line-spacing moom-min-line-spacing))
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun moom-reset-line-spacing ()
  "Reset the defaut value for line spacing."
  (interactive)
  (setq line-spacing moom-init-line-spacing)
  (when moom-verbose
    (message "%.1f" line-spacing)))

;;;###autoload
(defun moom-move-frame-right (&optional N FRAME)
  "Move it N times `frame-char-width', where N is the prefix arg.
In Lisp code, FRAME is the frame to move."
  (interactive)
  (move-frame-right
   (or N
       (cond ((integerp moom-horizontal-shifts)
              moom-horizontal-shifts)
             ((listp moom-horizontal-shifts)
              (nth 1 moom-horizontal-shifts))
             (t
              (error (format "%s is wrong value." moom-horizontal-shifts)))))
   FRAME)
  )

;;;###autoload
(defun moom-move-frame-left (&optional N FRAME)
  "Move it N times `frame-char-width', where N is the prefix arg.
In Lisp code, FRAME is the frame to move."
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
(defun moom-toggle-auto-move-frame-to-center ()
  "Change whether move the frame to center automatically."
  (interactive)
  (setq moom-auto-move-frame-to-center
        (not moom-auto-move-frame-to-center))
  (when moom-verbose
    (if moom-auto-move-frame-to-center
        (message "Toggle auto move ON")
      (message "Toggle auto move OFF"))))

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
  "Move the current frame to the top of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (frame-parameter (selected-frame) 'left)
                      0))

;;;###autoload
(defun moom-move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display.
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
  (let ((prev-pos-x (frame-parameter (selected-frame) 'left))
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
Use prefix to specify the destination position by ARG."
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
(defun moom-open-height-ring (&optional force)
  "Change frame height and update the ring.
If FORCE non-nil, generate ring again with new values."
  (interactive)
  (when (or (not moom--height-ring)
            force)
    (moom--make-frame-height-ring))
  (moom-change-frame-height (car moom--height-ring))
  (setq moom--height-ring
        (append (cdr moom--height-ring)
                (list (car moom--height-ring)))))

;;;###autoload
(defun moom-change-frame-height (new-height)
  "Change the hight of the current frame.
Argument NEW-HEIGHT specifies new frame height."
  (interactive
   (list (string-to-number
          (read-string "New Height: " (number-to-string (frame-height))))))
  (let ((min-height (moom--min-frame-height))
        (max-height (moom--max-frame-height)))
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

;;;###autoload
(defun moom-make-height-ring (heights)
  "Cycle change the height of the current frame.
Argument HEIGHTS specifies a secuece of frame heights."
  (setq moom--height-ring (copy-sequence heights)))

;;;###autoload
(defun moom-change-frame-width (&optional width)
  "Change the frame width by the `width' argument.
If WIDTH is not provided, `moom-frame-width-single' will be used."
  (interactive)
  (let ((width (or width
                   moom-frame-width-single)))
    (setq moom--frame-width width)
    (set-frame-width (selected-frame) width)))

;;;###autoload
(defun moom-change-frame-width-single ()
  "Change the frame width to single."
  (interactive)
  (moom-change-frame-width))

;;;###autoload
(defun moom-change-frame-width-double ()
  "Change the frame width to double."
  (interactive)
  (moom-change-frame-width moom-frame-width-double))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame origin, and frame size in mini buffer."
  (interactive)
  (message
   (format "Font: %spt | Origin: (%d, %d) | Frame: (%d, %d) | Pix: (%d, %d)"
           (if (boundp 'moom-font--size) moom-font--size "**")
           (frame-parameter (selected-frame) 'left)
           (frame-parameter (selected-frame) 'top)
           (frame-width)
           (frame-height)
           (frame-pixel-width)
           (frame-pixel-height))))

;;;###autoload
(defun moom-version ()
  "The release version of Moom."
  (interactive)
  (let ((moom-release "0.9.3"))
    (message "Moom: v%s" moom-release)))

;; init call
(moom--make-frame-height-ring)

;; JP-font module
(when moom-font-module
  (add-hook 'moom-font-resize-hook #'moom--update-frame-height-ring))

(provide 'moom)

;;; moom.el ends here
