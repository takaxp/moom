;;; moom.el --- Commands to control frame position and size -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 1.2.4
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

;; This package provides a set of commands to control frame position and size.
;; The font size in buffers will be changed with synchronization of the updated
;; frame geometry so that the frame width could be maintained at 80 as default.
;;
;; Now make your dominant hand FREE from your mouse by Moom.
;;
;; Install:
;;  - Get moom.el and moom-font.el from MELPA or Github.
;;
;; Setup:
;;  - After installing, activate Moom by (moom-mode 1) in your init.el.
;;
;; Keybindings:
;;  - The moom-mode-map is available.
;;  - To see more details and examples, go https://github.com/takaxp/moom.
;;

;;; Change Log:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'moom-font nil t))

(defgroup moom nil
  "Commands to control frame position and size."
  :group 'convenience)

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
  :type '(choice (integer :tag "Common value for left and right")
                 (list (integer :tag "Value for left")
                       (integer :tag "Value for right")))
  :group 'moom)

(defcustom moom-fill-band-options '(:direction vertical :range 50.0)
  "Band direction and range to fill screen.
If DIRECTION is horizontal, the frame height is limited based on RANGE.
If it is vertical, then the frame width will be limited based on RANGE.
If the type of RANGE is float, range is calculated in percent.
Note that even if DIRECTION is vertical and RANGE is 100.0, the screen
may not be covered entirely because this package prioritize keeping
the column at `moom-frame-width-single'.  Instead, if the value is integer,
the screen will be covered precisely by the specified value in pixels.
  :direction    symbol, horizontal or vertical
  :range        float(%) or integer(px)."
  :type 'plist
  :group 'moom)

(defcustom moom-scaling-gradient (/ 5.0 3)
  "Gradient factor between font size and actual font pixel width.
This parameter is used to calculate the font pixel width when resizing
the frame width to keep the column 80. It depends on Font.
For instance,
1.66 or (5.0/3.0) : Menlo, Monaco
2.00 or (5.0/2.5) : Inconsolata
."
  :type 'float
  :group 'moom)

(defcustom moom-verbose nil
  "Show responses from \"moom\"."
  :type 'boolean
  :group 'moom)

(defcustom moom-lighter "Moom"
  "Package name in mode line."
  :type 'string
  :group 'moom)

(defcustom moom-before-fill-screen-hook nil
  "Hook runs before changing to frame maximized."
  :type 'hook
  :group 'moom)

(defcustom moom-after-fill-screen-hook nil
  "Hook runs after changing to frame maximized."
  :type 'hook
  :group 'moom)

(defcustom moom-resize-frame-height-hook nil
  "Hook runs after resizing the frame height."
  :type 'hook
  :group 'moom)

(defvar moom-mode-map
  (let ((map (make-sparse-keymap)))
    ;; No keybindings are configured as default. It's open for users.
    map)
  "The keymap for `moom'.")

(defvar moom--init-status nil)
(defvar moom--font-module-p (require 'moom-font nil t))
(defvar moom--frame-width moom-frame-width-single)
(defvar moom--height-list nil)
(defvar moom--height-steps 4)
(defvar moom--last-status nil)
(defvar moom--maximized nil)
(defvar moom--screen-margin nil)
(defvar moom--fill-minimum-range 256)

(defun moom--setup ()
  "Init function."
  (moom--make-frame-height-list)
  (moom--save-last-status)
  (setq moom--init-status moom--last-status)
  ;; JP-font module
  (when moom--font-module-p
    (add-hook 'moom-font-after-resize-hook #'moom--make-frame-height-list)))

(defun moom--abort ()
  "Abort."
  (moom-reset-line-spacing)
  (moom-reset))

(defun moom--lighter ()
  "Lighter."
  (when moom-lighter
    (concat " " moom-lighter)))

(defun moom--frame-internal-width ()
  "Width of internal objects.
Including fringes and border."
  (if window-system
      (+ (frame-parameter nil 'left-fringe)
         (frame-parameter nil 'right-fringe)
         (frame-parameter nil 'scroll-bar-width)
         (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))
    0)) ;; TODO check this by terminal

(defun moom--internal-border-height ()
  "Height of internal border within `frame-pixel-height'."
  (* 2 (cdr (assoc 'internal-border-width (frame-geometry)))))

(defun moom--frame-internal-height ()
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (if window-system
      (+ (nthcdr 2 (assoc 'title-bar-size (frame-geometry)))
         (nthcdr 2 (assoc 'tool-bar-size (frame-geometry)))
         (if (memq window-system '(x w32))
             (nthcdr 2 (assoc 'menu-bar-size (frame-geometry))) 0)
         (moom--internal-border-height))
    0)) ;; TODO check this by terminal

(defun moom--frame-pixel-width ()
  "Return frame width in pixels."
  (let ((edges (frame-edges)))
    (- (nth 2 edges)
       (nth 0 edges))))

(defun moom--frame-pixel-height ()
  "Return frame height in pixels."
  (let ((edges (frame-edges)))
    (- (nth 3 edges)
       (nth 1 edges))))

(defun moom--max-frame-pixel-width ()
  "Return the maximum width on pixel base."
  (- (display-pixel-width)
     (moom--frame-internal-width)
     (nth 2 moom--screen-margin)
     (nth 3 moom--screen-margin)))

(defun moom--max-frame-pixel-height ()
  "Return the maximum height on pixel base."
  (- (display-pixel-height)
     (moom--frame-internal-height)
     (nth 0 moom--screen-margin)
     (nth 1 moom--screen-margin)))

(defun moom--max-half-frame-pixel-height ()
  "Return the half of maximum height on pixel base."
  (floor (/ (- (display-pixel-height)
               (nth 0 moom--screen-margin)
               (nth 1 moom--screen-margin)
               (* 2 (moom--frame-internal-height)))
            2.0)))

(defun moom--frame-width ()
  "Return frame width."
  (/ (- (moom--frame-pixel-width)
        (moom--frame-internal-width))
     (frame-char-width)))

(defun moom--frame-height ()
  "Return frame height."
  (/ (- (moom--frame-pixel-height)
        (moom--internal-border-height))
     (frame-char-height)))

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

(defun moom--make-frame-height-list ()
  "Create an internal ring to change frame height."
  (let ((max-height (moom--max-frame-height))
        (min-height (moom--min-frame-height))
        (steps moom--height-steps)
        (heights nil))
    (while (> (setq steps (1- steps)) 0)
      (cl-pushnew (max min-height
                       (/ (* steps max-height) moom--height-steps))
                  heights))
    (cl-pushnew (max min-height max-height) heights)
    (setq moom--height-list (copy-sequence heights))))

(defun moom--cycle-frame-height-list ()
  "Rotate `moom--height-list'."
  (setq moom--height-list
        (append (cdr moom--height-list)
                (list (car moom--height-list)))))

(defun moom--font-size (pixel-width)
  "Return an appropriate font-size based on PIXEL-WIDTH.
If `moom-font-table' is non-nil, the returned value will be more precisely
calculated."
  (let* ((font-width (/ (float (- pixel-width (moom--frame-internal-width)))
                        moom-frame-width-single))
         (scaled-width (/ font-width
                          (if moom--font-module-p
                              moom-font-ascii-scale 1.0)))
         (font-size (when moom--font-module-p
                      (moom-font--find-size
                       (floor scaled-width) moom-font-table))))
    (if font-size font-size
      (floor (* (if (< scaled-width 1) 1 scaled-width)
                moom-scaling-gradient)))))

(defun moom--font-resize (font-size &optional pixel-width)
  "Resize font.
Resize the current font to FONT-SIZE.
If PIXEL-WIDTH is non-nil, ensure an appropriate font size so that
the actual pixel width will not exceed the WIDTH."
  (when moom--font-module-p
    (set-frame-width nil moom-frame-width-single)
    (moom-font-resize font-size pixel-width)))

(defun moom--fullscreen-font-size ()
  "Return the maximum font-size for full screen."
  (if window-system
      (moom--font-size (+ (moom--max-frame-pixel-width)
                          (moom--frame-internal-width)))
    12)) ;; FIXME, use face-attribute

(defun moom--pos-x (posx &optional bounds)
  "Extract a value from POSX.
If BOUNDS is t, the frame will be controlled not to run over the screen."
  (when (listp posx)
    (setq posx (nth 1 posx)))
  (if (and bounds
           (not (eq window-system 'ns))) ;; TODO: support others if possible
      (let ((bounds-left 0)
            (bounds-right (- (display-pixel-width)
                             (moom--frame-pixel-width))))
        (cond ((< posx bounds-left) bounds-left)
              ((> posx bounds-right) bounds-right)
              (t posx)))
    posx))

(defun moom--pos-y (posy &optional bounds)
  "Extract a value from POSY.
If BOUNDS is t, the frame will be controlled not to run over the screen."
  (when (listp posy)
    (setq posy (nth 1 posy)))
  (if (and bounds
           (not (eq window-system 'ns))) ;; TODO: support others if possible
      (let ((bounds-top 0)
            (bounds-bottom (- (display-pixel-height)
                              (moom--frame-pixel-height))))
        (cond ((< posy bounds-top) bounds-top)
              ((> posy bounds-bottom) bounds-bottom)
              (t posy)))
    posy))

(defun moom--horizontal-center ()
  "Horizontal center position."
  (+ (nth 2 moom--screen-margin)
     (floor (/ (+ (moom--max-frame-pixel-width)
                  (moom--frame-internal-width))
               2.0))))


(defun moom--vertical-center ()
  "Vertical center position."
  (+ (nth 0 moom--screen-margin)
     (floor (/ (+ (moom--max-frame-pixel-height)
                  (moom--frame-internal-height))
               2.0))))

(defun moom--horizontal-center-pos ()
  "Left edge position when centering."
  (+ (car moom-move-frame-pixel-offset)
     (moom--horizontal-center)
     (let ((scroll (frame-parameter nil 'scroll-bar-width)))
       (if (> scroll 0) ;; TBD
           scroll
         (frame-parameter nil 'left-fringe)))
     (- (/ (+ (moom--frame-pixel-width)
              (moom--frame-internal-width)
              (- (moom--internal-border-height)))
           2))))

(defun moom--vertical-center-pos ()
  "Top edge position when centering."
  (+ (cdr moom-move-frame-pixel-offset)
     (moom--vertical-center)
     (- (/ (+ (moom--frame-pixel-height)
              (moom--frame-internal-height)
              (- (moom--internal-border-height)))
           2))))

(defun moom--save-last-status ()
  "Store the last frame position, size, and font-size."
  (setq moom--last-status
        `(("font-size" . ,(if moom--font-module-p moom-font--size nil))
          ("left" . ,(moom--pos-x (frame-parameter nil 'left)))
          ("top" . ,(frame-parameter nil 'top))
          ("width" . ,(moom--frame-width))
          ("height" . ,(moom--frame-height))
          ("pixel-width" . ,(moom--frame-pixel-width))
          ("pixel-height" . ,(moom--frame-pixel-height)))))

(defun moom--fill-display (area)
  "Move the frame to AREA.
Font size will be changed appropriately.
AREA would be 'top, 'bottom, 'left, 'right, 'topl, 'topr, 'botl, and 'botr."
  (interactive)
  (moom--save-last-status)
  (let* ((align-width (+ (moom--max-frame-pixel-width)
                         (moom--frame-internal-width)))
         (pixel-width
          (- (floor (/ align-width 2.0))
             (moom--frame-internal-width)))
         (pixel-height (moom--max-half-frame-pixel-height))
         (pos-x (nth 2 moom--screen-margin))
         (pos-y (nth 0 moom--screen-margin)))
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
    (moom--font-resize (moom--font-size align-width) align-width)
    ;; Position
    (when (memq area '(right topr botr))
      (setq pos-x (moom--horizontal-center)))
    (when (memq area '(bottom botl botr))
      (setq pos-y (moom--vertical-center)))
    (when (memq area '(top bottom left right topl topr botl botr))
      (set-frame-position nil
                          (moom--pos-x pos-x)
                          (moom--pos-y pos-y))
      (set-frame-size nil pixel-width pixel-height t)))
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

(defun moom--default-screen-margin ()
  "Calculate screen margins."
  (let ((geometry (frame-monitor-geometry))
        (workarea (frame-monitor-workarea)))
    (list (- (nth 1 workarea) (nth 1 geometry))
          (- (+ (nth 1 geometry) (nth 3 geometry))
             (+ (nth 1 workarea) (nth 3 workarea)))
          (- (nth 2 geometry) (nth 2 workarea))
          (- (+ (nth 0 geometry) (nth 2 geometry))
             (+ (nth 0 workarea) (nth 2 workarea))))))

;;;###autoload
(defun moom-fill-screen ()
  "Expand frame width and height to fill the screen.
The font size in buffers will be increased so that the frame width could be
maintained at 80. Add appropriate functions to `moom-before-fill-screen-hook'
in order to move the frame to specific position."
  (interactive)
  (run-hooks 'moom-before-fill-screen-hook)
  (moom--font-resize (moom--fullscreen-font-size)
                     (+ (moom--max-frame-pixel-width)
                        (moom--frame-internal-width)))
  (set-frame-size nil
                  (moom--max-frame-pixel-width)
                  (moom--max-frame-pixel-height) t)
  (when moom-verbose
    (moom-print-status))
  (run-hooks 'moom-after-fill-screen-hook))

;;;###autoload
(defun moom-toggle-frame-maximized ()
  "Toggle frame maximized."
  (interactive)
  (if (setq moom--maximized (not moom--maximized))
      (progn
        (moom--save-last-status)
        (moom-fill-screen)
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
(defun moom-fill-band (&optional plist)
  "Fill screen by band region.
If PLIST is nil, `moom-fill-band-options' is used."
  (interactive)
  (let* ((values (or plist moom-fill-band-options))
         (direction (plist-get values :direction))
         (range (plist-get values :range))
         (band-pixel-width nil)
         (band-pixel-height nil))
    (cond ((memq direction '(vertical nil))
           (setq band-pixel-width
                 (if (floatp range)
                     (floor (/ (* range (+ (moom--max-frame-pixel-width)
                                           (moom--frame-internal-width)))
                               100.0))
                   range))
           (when (< band-pixel-width moom--fill-minimum-range)
             (setq band-pixel-width moom--fill-minimum-range)
             (warn "Range was changed since given value is too small."))
           (moom--font-resize
            (moom--font-size band-pixel-width) band-pixel-width)
           ;; Update band-pixel-width for `set-frame-size'
           (setq band-pixel-width
                 (- (if (and moom--font-module-p
                             (floatp range))
                        (moom--frame-pixel-width)
                      band-pixel-width)
                    (moom--frame-internal-width)))
           (setq band-pixel-height (moom--max-frame-pixel-height)))
          ((eq direction 'horizontal)
           (setq band-pixel-height
                 (- (if (floatp range)
                        (floor (/ (* (+ (moom--max-frame-pixel-height)
                                        (moom--frame-internal-height))
                                     range)
                                  100.0))
                      range)
                    (moom--frame-internal-height)))
           (moom--font-resize (moom--fullscreen-font-size)
                              (+ (moom--max-frame-pixel-width)
                                 (moom--frame-internal-width)))
           ;; (set-frame-width nil moom-frame-width-single)
           ;; (when (and moom--font-module-p
           ;;            (floatp range))
           ;;   (setq band-pixel-width (- (moom--frame-pixel-width)
           ;;                             (moom--frame-internal-width))))
           (setq band-pixel-width (moom--max-frame-pixel-width))))
    (when (and band-pixel-width
               band-pixel-height)
      (set-frame-size nil band-pixel-width band-pixel-height t)
      (moom-move-frame-to-center))))

;;;###autoload
(defun moom-cycle-line-spacing ()
  "Change `line-spacing’ value between a range."
  (interactive)
  (if (< line-spacing moom-max-line-spacing)
      (setq line-spacing (+ line-spacing 0.1))
    (setq line-spacing moom-min-line-spacing))
  (when moom-verbose
    (message "[Moom] %.1f" line-spacing)))

;;;###autoload
(defun moom-reset-line-spacing ()
  "Reset to the defaut value for line spacing."
  (interactive)
  (setq line-spacing moom-init-line-spacing)
  (when moom-verbose
    (message "[Moom] %.1f" line-spacing)))

;;;###autoload
(defun moom-move-frame-right (&optional pixel)
  "PIXEL move the current frame to right."
  (interactive)
  (let* ((pos-x (moom--pos-x (frame-parameter nil 'left)))
         (pos-y (frame-parameter nil 'top))
         (new-pos-x (moom--pos-x (+ pos-x (or pixel
                                              (moom--shift-amount 'right))) t)))
    (when (>= new-pos-x (display-pixel-width))
      (setq new-pos-x (- new-pos-x
                         (display-pixel-width)
                         (moom--frame-pixel-width)
                         (- (moom--shift-amount 'right)))))
    (set-frame-position nil new-pos-x pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-left (&optional pixel)
  "PIXEL move the current frame to left."
  (interactive)
  (let* ((pos-x (moom--pos-x (frame-parameter nil 'left)))
         (pos-y (frame-parameter nil 'top))
         (new-pos-x (moom--pos-x (- pos-x (or pixel
                                              (moom--shift-amount 'left))) t)))
    (when (<= new-pos-x (- (moom--frame-pixel-width)))
      (setq new-pos-x (+ new-pos-x
                         (display-pixel-width)
                         (moom--frame-pixel-width)
                         (- (moom--shift-amount 'left)))))
    (set-frame-position nil new-pos-x pos-y))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the screen."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (moom--horizontal-center-pos) t)
                      (moom--pos-y (frame-parameter nil 'top) t))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the screen."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (frame-parameter nil 'left) t)
                      (moom--pos-y (moom--vertical-center-pos) t))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-top ()
  "Move the current frame to the top of the screen.
If you find the frame is NOT moved to the top exactly,
please configure the margins by `moom-screen-margin'."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (frame-parameter nil 'left))
                      (nth 0 moom--screen-margin))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-bottom ()
  "Move the current frame to the top of the screen.
If you find the frame is NOT moved to the bottom exactly,
please configure the margins by `moom-screen-margin'."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (frame-parameter nil 'left))
                      (- (display-pixel-height)
                         (moom--frame-pixel-height)
                         (moom--frame-internal-height)
                         (- (moom--internal-border-height))
                         (nth 1 moom--screen-margin)))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-right ()
  "Move the current frame to the right edge of the screen."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (- (display-pixel-width)
                                      (moom--frame-pixel-width)
                                      (nth 3 moom--screen-margin)))
                      (frame-parameter nil 'top))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-edge-left ()
  "Move the current frame to the left edge of the screen."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (nth 2 moom--screen-margin))
                      (frame-parameter nil 'top))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame-to-center ()
  "Move the current frame to the center of the screen."
  (interactive)
  (set-frame-position nil
                      (moom--pos-x (moom--horizontal-center-pos))
                      (moom--pos-y (moom--vertical-center-pos) t))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-move-frame (&optional arg)
  "Move the frame to somewhere (default: '(0 0)).
When ARG is a list like '(10 10), move the frame to the position.
When ARG is a single number like 10, shift the frame horizontally +10 pixel.
When ARG is nil, then move to the default position '(0 0)."
  (interactive)
  (let ((pos-x (nth 2 moom--screen-margin))
        (pos-y (nth 0 moom--screen-margin)))
    (cond ((not arg) t) ;; (0, 0)
          ((numberp arg) ;; horizontal shift
           (setq pos-x (+ pos-x arg))
           (setq pos-y (frame-parameter nil 'top)))
          ((listp arg) ;; move to '(x, y)
           (setq pos-x (+ pos-x (nth 0 arg)))
           (setq pos-y (+ pos-y (nth 1 arg)))))
    (set-frame-position nil
                        (moom--pos-x pos-x)
                        (moom--pos-y pos-y)))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-cycle-frame-height ()
  "Change frame height and update the internal ring.
If you find the frame is NOT changed as expected,
please configure the margins by `moom-screen-margin'."
  (interactive)
  (unless moom--height-list
    (moom--make-frame-height-list))
  (let ((height (car moom--height-list)))
    (when (equal height (moom--frame-height))
      (moom--cycle-frame-height-list)
      (setq height (car moom--height-list)))
    (cond ((equal height (moom--max-frame-height))
           (moom-expand-height))
          ((equal height (/ (moom--max-frame-height) 2))
           (moom-change-frame-height (moom--max-half-frame-pixel-height) t))
          (t (moom-change-frame-height height))))
  (moom--cycle-frame-height-list)
  (run-hooks 'moom-resize-frame-height-hook))

;;;###autoload
(defun moom-expand-height ()
  "Expand frame height to fill screen vertically without changing frame width."
  (moom-change-frame-height (moom--max-frame-pixel-height) t))

;;;###autoload
(defun moom-change-frame-height (&optional frame-height pixelwise)
  "Change the hight of the current frame.
Argument FRAME-HEIGHT specifies new frame height.
If PIXELWISE is non-nil, the frame height will be changed by pixel value."
  (interactive
   (list (read-number "New Height: " (moom--frame-height))))
  (when (not frame-height)
    (setq frame-height moom-min-frame-height))
  (if pixelwise
      (set-frame-height nil frame-height nil pixelwise)
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
      (set-frame-height nil (floor frame-height))))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-change-frame-width (&optional frame-width)
  "Change the frame width by the FRAME-WIDTH argument.
This function does not effect font size.
If FRAME-WIDTH is nil, `moom-frame-width-single' will be used."
  (interactive
   (list (read-number "New Width: " (moom--frame-width))))
  (let ((width (or frame-width
                   moom-frame-width-single)))
    (setq moom--frame-width width)
    (set-frame-width nil width))
  (when moom-verbose
    (moom-print-status)))

;;;###autoload
(defun moom-change-frame-width-single ()
  "Change the frame width to single.
This function does not effect font size."
  (interactive)
  (moom-change-frame-width))

;;;###autoload
(defun moom-change-frame-width-double ()
  "Change the frame width to double.
This function does not effect font size."
  (interactive)
  (moom-change-frame-width moom-frame-width-double))

;;;###autoload
(defun moom-change-frame-width-half-again ()
  "Change the frame width to half as large again as single width.
This function does not effect font size."
  (interactive)
  (moom-change-frame-width (floor (* 1.5 moom-frame-width-single))))

;;;###autoload
(defun moom-reset ()
  "Reset associated parameters."
  (interactive)
  (let ((moom--font-module-p (require 'moom-font nil t)))
    (setq moom--maximized nil)
    (moom--save-last-status)
    (moom-restore-last-status moom--init-status)
    (moom--make-frame-height-list)))

;;;###autoload
(defun moom-update-height-steps (arg)
  "Change number of steps of the height ring by ARG.
The default step is 4."
  (when (and (integerp arg)
             (> arg 1))
    (setq moom--height-steps arg)
    (moom--make-frame-height-list)))

;;;###autoload
(defun moom-screen-margin (margins &optional fill)
  "Change top, bottom, left, and right margin by provided MARGINS.
MARGINS shall be a list consists of 4 integer variables like '(23 0 0 0).
If FILL is non-nil, the frame will cover the screen with given margins."
  (when (and (listp margins)
             (eq (length margins) 4))
    (setq moom--screen-margin margins)
    (moom-reset)
    (when fill
      (moom-toggle-frame-maximized))))

;;;###autoload
(defun moom-restore-last-status (&optional status)
  "Restore the last frame position, size, and font-size.
STATUS is a list consists of font size, frame position, frame region, and pixel-region."
  (interactive)
  (when status
    (setq moom--last-status status))
  (moom--font-resize (cdr (assoc "font-size" moom--last-status)) nil)
  (set-frame-position nil
                      (cdr (assoc "left" moom--last-status))
                      (cdr (assoc "top" moom--last-status)))
  (set-frame-size nil
                  (cdr (assoc "width" moom--last-status))
                  (cdr (assoc "height" moom--last-status)))
  (set-frame-size nil
                  (- (cdr (assoc "pixel-width" moom--last-status))
                     (moom--frame-internal-width))
                  (+ (- (cdr (assoc "pixel-height" moom--last-status))
                        (moom--internal-border-height)))
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
        (setq moom-font--pause (not moom--font-module-p))
        (if moom--font-module-p
            (add-hook 'moom-font-after-resize-hook
                      #'moom--make-frame-height-list)
          (remove-hook 'moom-font-after-resize-hook
                       #'moom--make-frame-height-list))
        (when moom-verbose
          (message
           (concat "[Moom] Using font module ... "
                   (if moom--font-module-p "ON" "OFF")))))
    (when moom-verbose
      (message "[Moom] moom-font.el is NOT installed."))))

;;;###autoload
(defun moom-generate-font-table ()
  "Generate a font table.
The last frame position and size will be restored."
  (interactive)
  (if (not (fboundp 'moom-font--generate-font-table))
      (warn "moom-font.el is NOT installed.")
    (moom--save-last-status)
    (moom-font--generate-font-table)
    (moom-restore-last-status)))

;;;###autoload
(defun moom-recommended-keybindings (options)
  "Apply pre defined keybindings.
OPTIONS is a list of moom API types.  If you want to set all recommemded
keybindings, put the following code in your init.el.
 (with-eval-after-load \"moom\"
   (moom-recommended-keybindings 'all))
'all is identical to '(move fit expand fill font reset).
If you give only '(reset) as the argument, then \\[moom-reset] is activated.
The keybindings will be assigned when Emacs runs in GUI."
  (when window-system
    (when (eq 'all options)
      (setq options '(move fit expand fill font reset)))
    (when (memq 'move options)
      (define-key moom-mode-map (kbd "M-0") 'moom-move-frame)
      (define-key moom-mode-map (kbd "M-1") 'moom-move-frame-left)
      (define-key moom-mode-map (kbd "M-2") 'moom-move-frame-to-center)
      (define-key moom-mode-map (kbd "M-3") 'moom-move-frame-right))
    (when (memq 'fit options)
      (define-key moom-mode-map (kbd "M-<f1>") 'moom-move-frame-to-edge-left)
      (define-key moom-mode-map (kbd "M-<f3>") 'moom-move-frame-to-edge-right)
      (define-key moom-mode-map (kbd "<f1>") 'moom-move-frame-to-edge-top)
      (define-key moom-mode-map (kbd "S-<f1>") 'moom-move-frame-to-edge-bottom))
    (when (memq 'expand options)
      (define-key moom-mode-map (kbd "<f2>") 'moom-cycle-frame-height)
      (define-key moom-mode-map (kbd "C-c f s") 'moom-change-frame-width-single)
      (define-key moom-mode-map (kbd "C-c f d") 'moom-change-frame-width-double)
      (define-key moom-mode-map (kbd "C-c f a") 'moom-change-frame-width-half-again))
    (when (memq 'fill options)
      (define-key moom-mode-map (kbd "C-c f f t") 'moom-fill-top)
      (define-key moom-mode-map (kbd "C-c f f b") 'moom-fill-bottom)
      (define-key moom-mode-map (kbd "C-c f f l") 'moom-fill-left)
      (define-key moom-mode-map (kbd "C-c f f r") 'moom-fill-right)
      (define-key moom-mode-map (kbd "C-c f f 1") 'moom-fill-top-left)
      (define-key moom-mode-map (kbd "C-c f f 2") 'moom-fill-top-right)
      (define-key moom-mode-map (kbd "C-c f f 3") 'moom-fill-bottom-left)
      (define-key moom-mode-map (kbd "C-c f f 4") 'moom-fill-bottom-right)
      (define-key moom-mode-map (kbd "C-c f f m") 'moom-fill-band)
      (define-key moom-mode-map (kbd "M-<f2>") 'moom-toggle-frame-maximized))
    (when (memq 'font options)
      (define-key moom-mode-map (kbd "C--") 'moom-font-decrease)
      (define-key moom-mode-map (kbd "C-=") 'moom-font-increase)
      (define-key moom-mode-map (kbd "C-0") 'moom-font-size-reset))
    (when (memq 'reset options)
      (define-key moom-mode-map (kbd "C-c C-0") 'moom-reset))
    (when (and moom-verbose
               options)
      (message "[Moom] Key defined for APIs of %s." options))))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame size and origin in mini buffer."
  (interactive)
  (let ((message-log-max nil))
    (message
     (format
      "[Moom] Font: %spt(%dpx) | Frame: c(%d, %d) p(%d, %d) | Origin: (%s, %s)"
      (if moom--font-module-p moom-font--size "**")
      (frame-char-width)
      (moom--frame-width)
      (moom--frame-height)
      (moom--frame-pixel-width)
      (moom--frame-pixel-height)
      (moom--pos-x (frame-parameter nil 'left))
      (moom--pos-y (frame-parameter nil 'top))))))

;;;###autoload
(defun moom-version ()
  "The release version of Moom."
  (interactive)
  (let ((moom-release "1.2.4"))
    (message "[Moom] v%s" moom-release)))

;;;###autoload
(define-minor-mode moom-mode
  "Toggle the minor mode `moom-mode'.
This mode provides a set of commands to control frame position and size.
The font size in buffers will be changed with synchronization of the updated
frame geometry so that the frame width could be maintained at 80.

No keybindings are configured as default but recommended as follows:

(with-eval-after-load \"moom\"
  (define-key moom-mode-map (kbd \"M-0\") 'moom-move-frame) ;; move to top-left
  (define-key moom-mode-map (kbd \"M-1\") 'moom-move-frame-left)
  (define-key moom-mode-map (kbd \"M-2\") 'moom-move-frame-to-center)
  (define-key moom-mode-map (kbd \"M-3\") 'moom-move-frame-right)
  (define-key moom-mode-map (kbd \"<f2>\") 'moom-cycle-frame-height)
  (define-key moom-mode-map (kbd \"M-<f2>\") 'moom-toggle-frame-maximized)
  (define-key moom-mode-map (kbd \"C-c f s\") 'moom-change-frame-width-single)
  (define-key moom-mode-map (kbd \"C-c f d\") 'moom-change-frame-width-double))

You can use `moom-recommended-keybindings' to apply recommended keybindings in your init.el.

To see more details and examples, please visit https://github.com/takaxp/moom.
"
  :init-value nil
  :lighter (:eval (moom--lighter))
  :keymap moom-mode-map
  :global t
  :require 'moom
  :group 'moom
  (when window-system
    (if moom-mode
        (moom--setup)
      (moom--abort))))

;; Setting default value
(unless moom--screen-margin
  (setq moom--screen-margin (moom--default-screen-margin)))

(provide 'moom)

;;; moom.el ends here
