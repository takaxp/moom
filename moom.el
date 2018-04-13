;;; moom.el --- Commands to control frame size and position  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 0.9.8
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/Moom
;; Package-Requires: ((emacs "25.1"))
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

(eval-when-compile (require 'cl-lib))

(defgroup moom nil
  "A tool to control frame size, position, and font size."
  :group 'convenience)

(defcustom moom-move-frame-pixel-menubar-offset 23
  "Offset of the menubar.
The default height is 23 for macOS."
  :type 'integer
  :group 'moom)

(defcustom moom-move-frame-pixel-offset '(0 . 0)
  "Offset of the center position."
  :type 'sexp
  :group 'moom)

(defcustom moom-min-frame-height 16
  "The minimum height."
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

(defcustom moom-horizontal-shifts '(200 200)
  "Distance to move the frame horizontally."
  :type '(choice (integer :tag "common value for left and right")
                 (list (integer :tag "value for left")
                       (integer :tag "value for right")))
  :group 'moom)

(defcustom moom-verbose t
  "Show responses from \"moom\"."
  :type 'boolean
  :group 'moom)

(defcustom moom-before-fullscreen-hook nil
  "Hook runs before changing to fullscreen."
  :type 'hook
  :group 'moom)

(defcustom moom-after-fullscreen-hook nil
  "Hook runs after changing to fullscreen."
  :type 'hook
  :group 'moom)

(defcustom moom-resize-frame-height-hook nil
  "Hook runs after resizing the frame height."
  :type 'hook
  :group 'moom)

(defvar moom--frame-width moom-frame-width-single)
(defvar moom--height-ring nil)
(defvar moom--height-steps 4)
(defvar moom--font-module-p (require 'moom-font nil t))

(defun moom--frame-internal-width ()
  "Width of internal objects.
Including fringes and border."
  (if window-system
      (+ (frame-parameter nil 'left-fringe)
         (frame-parameter nil 'right-fringe)
         (frame-parameter nil 'scroll-bar-width)
         (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))
    0)) ;; TODO check this by terminal

(defun moom--frame-internal-height ()
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (if window-system
      (+ (nthcdr 2 (assoc 'title-bar-size (frame-geometry)))
         (unless (eq window-system 'mac) ;; TODO check others {x, w32}
           (nthcdr 2 (assoc 'menu-bar-size (frame-geometry))))
         (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))
    0)) ;; TODO check this by terminal

(defun moom--max-frame-pixel-width ()
  "Return the maximum width on pixel base."
  (- (display-pixel-width)
     (moom--frame-internal-width)))

(defun moom--max-frame-pixel-height ()
  "Return the maximum height on pixel base."
  (- (display-pixel-height)
     (moom--frame-internal-height)
     moom-move-frame-pixel-menubar-offset))

(defun moom--max-half-frame-pixel-height ()
  "Return the half of maximum height on pixel base."
  (floor (/ (- (display-pixel-height)
               moom-move-frame-pixel-menubar-offset
               (* 2 (moom--frame-internal-height)))
            2.0)))

(defun moom--max-frame-width ()
  "Return the maximum width based on screen size."
  (floor (/ (moom--max-frame-pixel-width)
            (frame-char-width))))

(defun moom--max-frame-height ()
  "Return the maximum height based on screen size."
  (floor (/ (moom--max-frame-pixel-height)
            (frame-char-height))))

(defun moom--min-frame-height ()
  "Return the minimum height of frame."
  moom-min-frame-height)

(defun moom--make-frame-height-ring ()
  "Create ring to change frame height."
  (let ((max-height (moom--max-frame-height))
        (min-height (moom--min-frame-height))
        (steps moom--height-steps)
        (heights nil))
    (while (> (setq steps (1- steps)) 0)
      (cl-pushnew (max min-height
                       (/ (* steps max-height) moom--height-steps))
                  heights))
    (cl-pushnew (max min-height max-height) heights)
    (setq moom--height-ring (copy-sequence heights))))

(defun moom--font-size (pixel-width)
  "Return an appropriate font-size based on PIXEL-WIDTH."
  (let ((scale (if moom--font-module-p moom-font-ja-scale 1.0)))
    (floor (/ (- pixel-width (moom--frame-internal-width))
              (* (/ 80 2) scale))))) ;; FIXME

(defun moom--fullscreen-font-size ()
  "Return the maximum font-size for full screen."
  (if window-system
      (moom--font-size (display-pixel-width))
    12)) ;; FIXME, use face-attribute

(defun moom--pos-x (posx)
  "Extract value form POSX."
  (if (listp posx)
      (nth 1 posx)
    posx))

(defvar moom--last-status nil)
(defun moom--save-last-status ()
  "Store the last frame position, size, and font-size."
  (setq moom--last-status
        `(("font-size" . ,(if moom--font-module-p moom-font--size nil))
          ("left" . ,(moom--pos-x (frame-parameter (selected-frame) 'left)))
          ("top" . ,(frame-parameter (selected-frame) 'top))
          ("width" . ,(frame-width))
          ("height" . ,(frame-height))
          ("pixel-width" . ,(frame-pixel-width))
          ("pixel-height" . ,(frame-pixel-height))))
  (when moom-verbose
    (moom-print-status)))

(defun moom--fill-display (area)
  "Move the frame to AREA.
Font size will be changed appropriately.
AREA would be 'top, 'bottom, 'left, 'right, 'topl, 'topr, 'botl, and 'botr."
  (interactive)
  (moom--save-last-status)
  (let* ((align-width (display-pixel-width))
         (pixel-width
          (- (floor (/ align-width 2.0))
             (moom--frame-internal-width)))
         (pixel-height (moom--max-half-frame-pixel-height))
         (pos-x 0)
         (pos-y 0))
    ;; Region
    (cond ((memq area '(top bottom))
           (setq pixel-width (moom--max-frame-pixel-width)))
          ((memq area '(left right))
           (setq pixel-height (moom--max-frame-pixel-height))
           (setq align-width (floor (/ align-width 2.0))))
          ((memq area '(topl topr botl botr))
           (setq align-width (floor (/ align-width 2.0))))
          (nil t))
    ;; Font size
    (when moom--font-module-p
      (moom-font-resize (moom--font-size align-width) align-width))
    ;; Position
    (when (memq area '(right topr botr))
      (setq pos-x (ceiling (/ (display-pixel-width) 2.0))))
    (when (memq area '(bottom botl botr))
      (setq pos-y
            (+ moom-move-frame-pixel-menubar-offset
               (ceiling (/ (- (display-pixel-height)
                              moom-move-frame-pixel-menubar-offset)
                           2.0)))))
    (when (memq area '(top bottom left right topl topr botl botr))
      (set-frame-position (selected-frame) pos-x pos-y)
      (set-frame-size (selected-frame) pixel-width pixel-height t)))
  (when moom-verbose
    (moom-print-status)))

(defun moom--shift-amount (direction)
  "Extract shift amount from `moom-horizontal-shifts' based on DIRECTION."
  (let ((index (cond ((eq direction 'left) 0)
                     ((eq direction 'right) 1)
                     (t 0))))
    (cond ((integerp moom-horizontal-shifts)
           moom-horizontal-shifts)
          ((listp moom-horizontal-shifts)
           (nth index moom-horizontal-shifts))
          (t
           (error (format "%s is wrong value." moom-horizontal-shifts))))))

;;;###autoload
(defun moom-fit-frame-to-fullscreen ()
  "Change font size and expand frame width and height to fit full.
Add appropriate functions to `moom-before-fullscreen-hook'
in order to move the frame to specific position."
  (interactive)
  (run-hooks 'moom-before-fullscreen-hook)
  (when moom--font-module-p
    (moom-font-resize (moom--fullscreen-font-size)
                      (display-pixel-width)))
  (set-frame-size (selected-frame)
                  (moom--max-frame-pixel-width)
                  (moom--max-frame-pixel-height) t)
  (when moom-verbose
    (moom-print-status))
  (run-hooks 'moom-after-fullscreen-hook))

(defvar moom--maximized nil)
;;;###autoload
(defun moom-toggle-frame-maximized ()
  "Toggle fullscreen."
  (interactive)
  (if (setq moom--maximized (not moom--maximized))
      (progn
        (moom--save-last-status)
        (moom-fit-frame-to-fullscreen)
        (moom-move-frame))
    (moom-restore-last-status)))

;;;###autoload
(defun moom-fill-top ()
  "Fill upper half of screen."
  (interactive)
  (moom--fill-display 'top))

;;;###autoload
(defun moom-fill-bottom ()
  "Fill lower half of screen."
  (interactive)
  (moom--fill-display 'bottom))

;;;###autoload
(defun moom-fill-left ()
  "Fill left half of screen."
  (interactive)
  (moom--fill-display 'left))

;;;###autoload
(defun moom-fill-right ()
  "Fill right half of screen."
  (interactive)
  (moom--fill-display 'right))

;;;###autoload
(defun moom-fill-top-left ()
  "Fill top left quarter of screen."
  (interactive)
  (moom--fill-display 'topl))

;;;###autoload
(defun moom-fill-top-right ()
  "Fill top right quarter of screen."
  (interactive)
  (moom--fill-display 'topr))

;;;###autoload
(defun moom-fill-bottom-left ()
  "Fill bottom left quarter of screen."
  (interactive)
  (moom--fill-display 'botl))

;;;###autoload
(defun moom-fill-bottom-right ()
  "Fill bottom right quarter of screen."
  (interactive)
  (moom--fill-display 'botr))

;;;###autoload
(defun moom-fill-band (&optional direction)
  "Fill screen by band region.
DIRECTION would be 'horizontal or 'vertical."
  (interactive)
  (when (cond ((memq direction '(vertical nil))
               (moom--fill-display 'left) t)
              ((eq direction 'horizontal)
               (moom--fill-display 'top) t)
              (t nil))
    (moom-move-frame-to-center)))

;;;###autoload
(defun moom-cycle-line-spacing ()
  "Change ‘line-spacing’ value between a range."
  (interactive)
  (if (< line-spacing moom-max-line-spacing)
      (setq line-spacing (+ line-spacing 0.1))
    (setq line-spacing moom-min-line-spacing))
  (when moom-verbose
    (message "[Moom] %.1f" line-spacing)))

;;;###autoload
(defun moom-reset-line-spacing ()
  "Reset the defaut value for line spacing."
  (interactive)
  (setq line-spacing moom-init-line-spacing)
  (when moom-verbose
    (message "[Moom] %.1f" line-spacing)))

;;;###autoload
(defun moom-move-frame-right (&optional pixel)
  "PIXEL move the current frame to the right."
  (interactive)
  (let* ((pos-x (moom--pos-x (frame-parameter (selected-frame) 'left)))
         (pos-y (frame-parameter (selected-frame) 'top))
         (new-pos-x (+ pos-x (or pixel
                                 (moom--shift-amount 'right)))))
    (when (>= new-pos-x (display-pixel-width))
      (setq new-pos-x (- new-pos-x
                         (display-pixel-width)
                         (frame-pixel-width))))
    (set-frame-position (selected-frame) new-pos-x pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-left (&optional pixel)
  "PIXEL move the current frame to the left."
  (interactive)
  (let* ((pos-x (moom--pos-x (frame-parameter (selected-frame) 'left)))
         (pos-y (frame-parameter (selected-frame) 'top))
         (new-pos-x (- pos-x (or pixel
                                 (moom--shift-amount 'left)))))
    (when (<= new-pos-x (* -1 (frame-pixel-width)))
      (setq new-pos-x (+ new-pos-x
                         (display-pixel-width)
                         (frame-pixel-width))))
    (set-frame-position (selected-frame) new-pos-x pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (+ (car moom-move-frame-pixel-offset)
                         (/ (- (display-pixel-width)
                               (frame-pixel-width))
                            2))
                      (frame-parameter (selected-frame) 'top))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (moom--pos-x (frame-parameter (selected-frame) 'left))
                      (+ (cdr moom-move-frame-pixel-offset)
                         (/ (- (display-pixel-height)
                               (frame-pixel-height))
                            2)))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-top ()
  "Move the current frame to the top of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (moom--pos-x (frame-parameter (selected-frame) 'left))
                      0)
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display.
If you find the frame is NOT moved to the bottom exactly,
Please set `moom-move-frame-pixel-menubar-offset'."
  (interactive)
  (set-frame-position (selected-frame)
                      (moom--pos-x (frame-parameter (selected-frame) 'left))
                      (- (- (display-pixel-height)
                            (frame-pixel-height))
                         moom-move-frame-pixel-menubar-offset))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-right ()
  "Move the current frame to the right edge of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      (- (display-pixel-width) (frame-pixel-width))
                      (frame-parameter (selected-frame) 'top))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-left ()
  "Move the current frame to the left edge of the window display."
  (interactive)
  (set-frame-position (selected-frame)
                      0
                      (frame-parameter (selected-frame) 'top))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-center ()
  "Move the current frame to the center of the window display."
  (interactive)
  (let ((center-pos-x
         (+ (car moom-move-frame-pixel-offset)
            (/ (- (display-pixel-width) (frame-pixel-width)) 2)))
        (center-pos-y
         (+ (cdr moom-move-frame-pixel-offset)
            (/ (- (display-pixel-height) (frame-pixel-height)) 2))))
    (set-frame-position (selected-frame) center-pos-x center-pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame (&optional arg)
  "Move the frame to somewhere (default: 0,0).
Use prefix to specify the destination position by ARG."
  (interactive "P")
  (let ((pos-x 0)
        (pos-y moom-move-frame-pixel-menubar-offset))
    (when arg
      (setq pos-x (moom--pos-x
                   (string-to-number
                    (read-from-minibuffer
                     (format "X: from %s to "
                             (frame-parameter (selected-frame) 'left))))))
      (setq pos-y (string-to-number
                   (read-from-minibuffer
                    (format "Y: from %s to "
                            (frame-parameter (selected-frame) 'top))))))
    (set-frame-position (selected-frame) pos-x pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-open-height-ring ()
  "Change frame height and update the ring."
  (interactive)
  (unless moom--height-ring
    (moom--make-frame-height-ring))
  (let ((height (car moom--height-ring)))
    (cond ((equal height (moom--max-frame-height))
           (set-frame-height (selected-frame)
                             (moom--max-frame-pixel-height) nil t))
          ((equal height (/ (moom--max-frame-height) 2))
           (set-frame-height (selected-frame)
                             (moom--max-half-frame-pixel-height) nil t))
          (t (moom-change-frame-height height))))
  (setq moom--height-ring
        (append (cdr moom--height-ring)
                (list (car moom--height-ring))))
  (when moom-verbose
    (moom-print-status))
  (run-hooks 'moom-resize-frame-height-hook))

;;;###autoload
(defun moom-change-frame-height (&optional frame-height)
  "Change the hight of the current frame.
Argument FRAME-HEIGHT specifies new frame height."
  (interactive
   (list (string-to-number
          (read-string "New Height: " (number-to-string (frame-height))))))
  (when (not frame-height)
    (setq frame-height moom-min-frame-height))
  (let ((min-height (moom--min-frame-height))
        (max-height (moom--max-frame-height)))
    (when (> frame-height max-height)
      (setq frame-height max-height)
      (when moom-verbose
        (message "[Moom] Force set the height %s." frame-height)))
    (when (< frame-height min-height)
      (setq frame-height min-height)
      (when moom-verbose
        (message "[Moom] Force set the height %s." frame-height)))
    (set-frame-height (selected-frame) (floor frame-height)))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-change-frame-width (&optional frame-width)
  "Change the frame width by the FRAME-WIDTH argument.
If WIDTH is not provided, `moom-frame-width-single' will be used."
  (interactive)
  (let ((width (or frame-width
                   moom-frame-width-single)))
    (setq moom--frame-width width)
    (set-frame-width (selected-frame) width))
  (when moom-verbose
    (moom-print-status)))

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
(defun moom-change-frame-width-half-again ()
  "Change the frame width to half as large again as single width."
  (interactive)
  (moom-change-frame-width (floor (* 1.5 moom-frame-width-single))))

;;;###autoload
(defun moom-reset ()
  "Reset associated parameters."
  (interactive)
  (let ((moom--font-module-p (require 'moom-font nil t)))
    (moom--save-last-status)
    (moom-restore-last-status moom--init-status)
    (moom--make-frame-height-ring)))

;;;###autoload
(defun moom-update-height-steps (arg)
  "Change number of steps of the height ring by ARG.
The default step is 4."
  (when (and (integerp arg)
             (> arg 1))
    (setq moom--height-steps arg)
    (moom--make-frame-height-ring)))

;;;###autoload
(defun moom-restore-last-status (&optional status)
  "Restore the last frame position, size, and font-size.
STATUS is a list storing font, position, region, and pixel-region."
  (interactive)
  (when status
    (setq moom--last-status status))
  (when moom--font-module-p
    (moom-font-resize (cdr (assoc "font-size" moom--last-status))))
  (set-frame-position (selected-frame)
                      (cdr (assoc "left" moom--last-status))
                      (cdr (assoc "top" moom--last-status)))
  (set-frame-size (selected-frame)
                  (cdr (assoc "width" moom--last-status))
                  (cdr (assoc "height" moom--last-status)))
  (set-frame-size (selected-frame)
                  (- (cdr (assoc "pixel-width" moom--last-status))
                     (moom--frame-internal-width))
                  (+ (- (cdr (assoc "pixel-height" moom--last-status))
                        (* 2 (cdr (assoc 'internal-border-width
                                         (frame-geometry))))))
                  t)
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-toggle-font-module ()
  "Toggle `moom--font-module-p'.
When `moom--font-module-p' is nil, font size is fixed except for `moom-reset' even if \"moom-font.el\" is loaded."
  (interactive)
  (if (require 'moom-font nil t)
      (progn
        (setq moom--font-module-p (not moom--font-module-p))
        (when moom-verbose
          (message
           (concat "[Moom] Using font module ... "
                   (if moom--font-module-p "ON" "OFF")))))
    (when moom-verbose
      (message "[Moom] moom-font.el is NOT installed."))))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame origin, and frame size in mini buffer."
  (interactive)
  (message
   (format
    "[Moom] Font: %spt | Frame: c(%d, %d) p(%d, %d) | Origin: (%d, %d)"
    (if moom--font-module-p moom-font--size "**")
    (frame-width)
    (frame-height)
    (frame-pixel-width)
    (frame-pixel-height)
    (moom--pos-x (frame-parameter (selected-frame) 'left))
    (frame-parameter (selected-frame) 'top))))

;;;###autoload
(defun moom-version ()
  "The release version of Moom."
  (interactive)
  (let ((moom-release "0.9.8"))
    (message "[Moom] v%s" moom-release)))

;; init call
(moom--make-frame-height-ring)
(moom--save-last-status)
(defvar moom--init-status moom--last-status)

;; JP-font module
(when moom--font-module-p
  (add-hook 'moom-font-after-resize-hook #'moom--make-frame-height-ring))

(provide 'moom)

;;; moom.el ends here
