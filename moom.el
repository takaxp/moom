;;; moom.el --- Commands to control frame position and size -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 1.6.0
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
;;  - Get moom.el and moom-font.el from MELPA or GitHub.
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

(defcustom moom-frame-width-double (+ (* 2 moom-frame-width-single) 3)
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

(defcustom moom-display-line-numbers-width 6
  "Width to expand the frame for function `global-display-line-numbers-mode'.
For function `display-line-numbers-mode',
- set this variable to 0, and also
- set `moom-frame-width-single' more than 80 (e.g. 86)"
  :group 'moom
  :type 'integer)

(defcustom moom-command-with-centering '(split delete)
  "List of flags that specifies whether centerize the frame after changing the frame width."
  :type '(repeat
          (choice
           (const :tag "Split window" :value split)
           (const :tag "Delete windows" :value delete)
           (const :tag "Change frame width single" :value single)
           (const :tag "Change frame width double" :value double)
           (const :tag "Change frame width half again" :value half-again)))
  :group 'moom)

(defcustom moom-user-margin '(0 0 0 0)
  "User specified margin to adjust to the local environment."
  :type '(list (integer :tag "Top margin")
               (integer :tag "Bottom margin")
               (integer :tag "Left margin")
               (integer :tag "Right margin"))
  :group 'moom)

(defcustom moom-multi-monitors-support t
  "If non-nil, multiple monitors support is enabled."
  :type 'boolean
  :group 'moom)

(defcustom moom-use-font-module t
  "If non-nil, font module will not integrated even if the module is available.
Configure this variable before activating moom mode.
`moom-toggle-font-module' could be also useful."
  :type 'boolean
  :group 'moom)

(defcustom moom-command-history-length 100
  "Number of commands to remember in history."
  :type 'integer
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

(defcustom moom-split-window-hook nil
  "Hook runs after splitting window horizontally."
  :type 'hook
  :group 'moom)

(defcustom moom-delete-window-hook nil
  "Hook runs after deleting window horizontally."
  :type 'hook
  :group 'moom)

(defcustom moom-before-setup-hook nil
  "Hook runs before enabling this package."
  :type 'hook
  :group 'moom)

(defcustom moom-after-select-monitor-hook nil
  "Hook runs after selecting and jumping to a monitor."
  :type 'hook
  :group 'moom)

(defvar moom-mode-map
  (let ((map (make-sparse-keymap)))
    ;; No keybindings are configured as default. It's open for users.
    map)
  "The keymap for `moom'.")

(defvar moom--init-status nil)
(defvar moom--internal-border-width 0)
(defvar moom--font-module-p (require 'moom-font nil t))
(defvar moom--frame-width 80)
(defvar moom--height-list nil)
(defvar moom--height-steps 4)
(defvar moom--last-status nil)
(defvar moom--maximized nil)
(defvar moom--screen-margin nil)
(defvar moom--fill-minimum-range 256)
(defvar moom--frame-resize-pixelwise nil)
(defvar moom--virtual-grid nil)
(defvar moom--screen-grid nil)
(defvar moom--print-status t)
(defvar moom--common-margin
  (cond ((memq window-system '(ns mac)) '(23 0 0 0))
        (t '(0 0 0 0))))
(defvar moom--pos-options '(:grid nil :bound nil)) ;; {screen,virtual}, {nil,t}
(defvar moom--local-margin (cond ((eq window-system 'w32) '(0 9 -16 16))
                                 ((eq window-system 'x) '(-19 0 0 0))
                                 (t '(0 0 0 0))))
(defvar moom--autoreset-hooks '(menu-bar-mode-hook
                                tool-bar-mode-hook tab-bar-mode-hook
                                horizontal-scroll-bar-mode-hook
                                scroll-bar-mode-hook))
(defvar moom--last-monitor nil)
(defvar moom--display-border '(0 0))
(defvar moom--command-history nil)
(defvar moom--non-interactive-history nil)

(defun moom--setup ()
  "Init function."
  (run-hooks 'moom-before-setup-hook)
  (setq moom--font-module-p (when moom-use-font-module
                              (require 'moom-font nil t)))
  (setq moom-font--pause (not moom--font-module-p))
  (moom-identify-current-monitor)
  (unless moom--virtual-grid
    (setq moom--virtual-grid (moom--virtual-grid)))
  (unless moom--screen-grid
    (setq moom--screen-grid (moom--screen-grid)))
  (setq moom--internal-border-width
        (alist-get 'internal-border-width (frame-geometry)))
  (unless (eq (setq moom--frame-width moom-frame-width-single) 80)
    (set-frame-width nil moom--frame-width))
  (moom--make-frame-height-list)
  (moom--save-last-status)
  (setq moom--init-status moom--last-status)
  (setq moom--frame-resize-pixelwise frame-resize-pixelwise
        frame-resize-pixelwise t)
  ;; JP-font module
  (when moom--font-module-p
    (add-hook 'moom-font-after-resize-hook #'moom--make-frame-height-list)
    (add-hook 'moom-font-after-resize-hook #'moom--stay-in-region))
  ;; display-line-numbers-mode
  (when (fboundp 'global-display-line-numbers-mode)
    (add-hook 'global-display-line-numbers-mode-hook
              #'moom--update-frame-display-line-numbers))
  (mapcar (lambda (hook) (add-hook hook #'moom-reset))
          moom--autoreset-hooks))

(defun moom--abort ()
  "Abort."
  (moom-reset-line-spacing)
  (moom-reset)
  (setq frame-resize-pixelwise moom--frame-resize-pixelwise)
  (setq moom--screen-margin nil
        moom--virtual-grid nil
        moom--screen-grid nil)
  (when (fboundp 'global-display-line-numbers-mode)
    (remove-hook 'global-display-line-numbers-mode-hook
                 #'moom--update-frame-display-line-numbers))
  (mapcar (lambda (hook) (remove-hook hook #'moom-reset))
          moom--autoreset-hooks))

(defun moom--reload ()
  "Reload."
  (moom--abort)
  (moom--setup))

(defun moom--lighter ()
  "Lighter."
  (when moom-lighter
    (concat " " moom-lighter)))

(defun moom--frame-internal-width ()
  "Width of internal objects.
Including fringes and border."
  (if window-system
      (let ((fp (frame-parameters)))
        (+ (alist-get 'right-fringe fp)
           (alist-get 'left-fringe fp)
           (if (get-scroll-bar-mode)
               (alist-get 'scroll-bar-width fp)
             0)
           (* 2 moom--internal-border-width)))
    0)) ;; TODO check this by terminal

(defun moom--internal-border-height ()
  "Height of internal border within `frame-pixel-height'."
  0)

(defun moom--frame-internal-height ()
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (if window-system
      (let ((geometry (frame-geometry)))
        (+ (cdr (alist-get 'title-bar-size geometry))
           (cdr (alist-get 'tool-bar-size geometry))
           (if (memq window-system '(x w32))
               (cdr (alist-get 'menu-bar-size geometry)) 0)
           (moom--internal-border-height)))
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
    (setq moom--height-list (copy-sequence heights))
    ;; Skip the current height
    (when (eq (car moom--height-list)
              (moom--frame-height))
      (moom--cycle-frame-height-list))))

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

(defun moom--virtual-grid ()
  "Alignment for pointing frame left and top position on `set-frame-position'."
  (cond ((eq window-system 'w32) '(-16 0))
        ((and (eq window-system 'x)
              (version< emacs-version "26.0")) '(0 27))
        ((memq window-system '(ns mac)) '(0 0))
        ((eq window-system 'x) '(10 8))
        (t '(0 0))))

(defun moom--screen-grid ()
  "Alignment of frame left and top position on monitor."
  (cond ((eq window-system 'w32) '(8 0))
        ((and (eq window-system 'x)
              (version< emacs-version "26.0")) '(10 -19))
        ((memq window-system '(ns mac)) '(0 0))
        ((eq window-system 'x) '(0 0))
        (t '(0 0))))

(defun moom--frame-left ()
  "Return outer left position."
  (let ((left (frame-parameter nil 'left)))
    (+ (if (listp left) (nth 1 left) left)
       (nth 0 moom--virtual-grid)
       (nth 0 moom--screen-grid))))

(defun moom--frame-top ()
  "Return outer top position."
  (let ((top (frame-parameter nil 'top)))
    (+ (if (listp top) (nth 1 top) top)
       (nth 1 moom--virtual-grid)
       (nth 1 moom--screen-grid))))

(defun moom--pos-x (posx &optional options)
  "Return aligned POSX for `set-frame-position'.
OPTIONS controls grid and bound.  See `moom--pos-options'."
  (when (listp posx)
    (setq posx (nth 1 posx)))
  (setq posx (- posx (nth 0 moom--screen-grid)))
  (when (eq window-system 'w32)
    (setq posx (+ posx 16))) ;; FIXME
  (when (or (plist-get options :bound)
            (not (memq window-system '(ns mac w32)))) ;; TODO: support others if possible
    (let ((bounds-left (- (nth 0 moom--last-monitor)
                          (nth 0 moom--screen-grid)))
          (bounds-right (+ (nth 0 moom--last-monitor)
                           (nth 2 moom--last-monitor)
                           (- (moom--frame-pixel-width)))))
      (setq posx (cond ((< posx bounds-left) bounds-left)
                       ((> posx bounds-right) bounds-right)
                       (t posx)))))
  (unless (plist-get options :grid)
    (setq options (if (eq window-system 'ns)
                      '(:grid screen) '(:grid virtual))))
  (cond ((eq (plist-get options :grid) 'virtual)
         (let ((pw (- (nth 0 moom--display-border)
                      (moom--frame-pixel-width))))
           (if (< posx 0)
               (- posx (+ pw (nth 0 moom--virtual-grid)))
             posx)))
        ((eq (plist-get options :grid) 'screen)
         posx)
        (t
         posx)))

(defun moom--pos-y (posy &optional options)
  "Return aligned POSY for `set-frame-position'.
OPTIONS controls grid and bound.  See `moom--pos-options'."
  (when (listp posy)
    (setq posy (nth 1 posy)))
  (setq posy (- posy (nth 1 moom--screen-grid)))
  (when (or (plist-get options :bound)
            (not (memq window-system '(ns mac w32)))) ;; TODO: support others if possible
    (let ((bounds-top (- (nth 1 moom--last-monitor)
                         (nth 1 moom--screen-grid)))
          (bounds-bottom (+ (nth 1 moom--last-monitor)
                            (nth 3 moom--last-monitor)
                            (- (moom--frame-pixel-height)))))
      (setq posy (cond ((< posy bounds-top) bounds-top)
                       ((> posy bounds-bottom) bounds-bottom)
                       (t posy)))))
  (unless (plist-get options :grid)
    (setq options (if (eq window-system 'ns)
                      '(:grid screen) '(:grid virtual))))
  (cond ((eq (plist-get options :grid) 'virtual)
         (let ((ph (- (nth 1 moom--display-border)
                      (moom--frame-pixel-height))))
           (if (< posy 0)
               (- posy (+ ph (nth 1 moom--virtual-grid)))
             posy)))
        ((eq (plist-get options :grid) 'screen)
         posy)
        (t
         posy)))

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
       (if (and (> scroll 0) (get-scroll-bar-mode)) ;; TBD
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
          ("left" . ,(moom--pos-x (moom--frame-left)))
          ("top" . ,(moom--frame-top))
          ("width" . ,(moom--frame-width))
          ("height" . ,(moom--frame-height))
          ("pixel-width" . ,(moom--frame-pixel-width))
          ("pixel-height" . ,(moom--frame-pixel-height)))))

(defun moom--fill-display (area)
  "Move the frame to AREA.
Font size will be changed appropriately.
AREA would be 'top, 'bottom, 'left, 'right, 'topl, 'topr, 'botl, and 'botr."
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
    (let ((moom-font-before-resize-hook nil)
          (moom-font-after-resize-hook nil))
      (moom--font-resize (moom--font-size align-width) align-width))
    ;; Position
    (when (memq area '(right topr botr))
      (setq pos-x (moom--horizontal-center)))
    (when (memq area '(bottom botl botr))
      (setq pos-y (moom--vertical-center)))
    (when (memq area '(top bottom left right topl topr botl botr))
      (let ((flag (memq window-system '(ns mac w32))))
        (unless flag
          (set-frame-size nil pixel-width pixel-height t))
        (when (and (not moom--font-module-p)
                   (eq window-system 'w32))
          (set-frame-width nil moom-frame-width-single)) ;; FIXME
        (set-frame-position nil
                            (moom--pos-x pos-x)
                            (moom--pos-y pos-y))
        (when flag
          (set-frame-size nil pixel-width pixel-height t)))))
  (moom--make-frame-height-list)
  (moom-print-status))

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

(defun moom--frame-monitor-attribute (attribute &optional frame x y)
  "Return the value of ATTRIBUTE on FRAME's monitor.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y).

Taken from frame.el in 26.1 to support previous Emacs versions, 25.1 or later."
  (if (and (numberp x)
           (numberp y))
      (cl-loop for monitor in (display-monitor-attributes-list)
               for geometry = (alist-get 'geometry monitor)
               for min-x = (pop geometry)
               for min-y = (pop geometry)
               for max-x = (+ min-x (pop geometry))
               for max-y = (+ min-y (car geometry))
               when (and (<= min-x x)
                         (< x max-x)
                         (<= min-y y)
                         (< y max-y))
               return (alist-get attribute monitor))
    (alist-get attribute (frame-monitor-attributes frame))))

(defun moom--frame-monitor-geometry (&optional frame x y)
  "Return the geometry of FRAME's monitor.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y).

Taken from frame.el in 26.1 to support previous Emacs versions, 25.1 or later."
  (moom--frame-monitor-attribute 'geometry frame x y))

(defun moom--frame-monitor-workarea (&optional frame x y)
  "Return the workarea of FRAME's monitor.

If X and Y are both numbers, then ignore the value of FRAME; the
monitor is determined to be the physical monitor that contains
the pixel coordinate (X, Y).

Taken from frame.el in 26.1 to support previous Emacs versions, 25.1 or later."
  (moom--frame-monitor-attribute 'workarea frame x y))

(defun moom--default-screen-margin ()
  "Calculate default screen margins."
  (let ((geometry (moom--frame-monitor-geometry))
        (workarea (moom--frame-monitor-workarea)))
    (setq moom--screen-margin
          (list (- (nth 1 workarea) (nth 1 geometry))
                (- (+ (nth 1 geometry) (nth 3 geometry))
                   (+ (nth 1 workarea) (nth 3 workarea)))
                (- (nth 0 workarea) (nth 0 geometry))
                (- (+ (nth 0 geometry) (nth 2 geometry))
                   (+ (nth 0 workarea) (nth 2 workarea)))))
    ;; Update
    (moom--merge-screen-margin moom--local-margin)))

(defun moom--merge-screen-margin (margin)
  "Add MARGIN to `moom--screen-margin'."
  (setq moom--screen-margin
        (list (+ (nth 0 margin) (nth 0 moom--screen-margin))
              (+ (nth 1 margin) (nth 1 moom--screen-margin))
              (+ (nth 2 margin) (nth 2 moom--screen-margin))
              (+ (nth 3 margin) (nth 3 moom--screen-margin)))))

(defun moom--update-frame-display-line-numbers ()
  "Expand frame width by `moom-display-line-numbers-width'.
This feature is available when function `global-display-line-numbers-mode'
is utilized."
  (if (numberp moom-display-line-numbers-width)
      (unless (eq moom-display-line-numbers-width 0)
        (let ((flag (if global-display-line-numbers-mode 1 -1)))
          (setq moom-frame-width-single
                (+ moom-frame-width-single
                   (* flag moom-display-line-numbers-width)))
          (setq moom-frame-width-double
                (+ (* 2 moom-frame-width-single) 3))
          (set-frame-width nil
                           (+ (frame-width)
                              (* flag moom-display-line-numbers-width)))))
    (user-error "Unexpected value of `moom-display-line-numbers-width'"))
  (moom-print-status))

(defun moom--centerize-p (arg)
  "Return nil when `ARG' is not listed in `moom-command-with-centering'."
  (memq arg (if (listp moom-command-with-centering)
                moom-command-with-centering
              (list moom-command-with-centering))))

(defun moom--stay-in-region (&optional target-width)
  "Shift the frame to try to keep the frame staying in the display.
The frame width may be specified with TARGET-WIDTH."
  (let ((shift (- (+ (* (frame-char-width) (or target-width
                                               (moom--frame-width)))
                     (moom--frame-internal-width)
                     (moom--pos-x (moom--frame-left)))
                  (- (display-pixel-width)
                     (nth 3 moom--screen-margin)))))
    ;; Left side border
    (when (< (moom--pos-x (moom--frame-left)) shift)
      (setq shift (moom--pos-x (moom--frame-left))))
    ;; Right side border
    (when (> shift 0)
      (moom-move-frame-left shift))))

(defun moom--update-display-border ()
  "Update right and bottom border for negative coordinate value on windows-nt."
  (let ((dma-list (display-monitor-attributes-list))
        (rb 0)
	      (lb 0))
    (dotimes (i (length dma-list))
      (let ((gm (alist-get 'geometry (nth i dma-list))))
	      (when (< (nth 0 gm) 0)
	        (setq rb (nth 0 gm)))
	      (when (< (nth 1 gm) 0)
	        (setq lb (nth 1 gm)))))
    (setq moom--display-border
          (list (+ (display-pixel-width) rb)
	              (+ (display-pixel-height) lb)))))

(defun moom--current-monitor-id ()
  "Provide id of the current monitor."
  (let ((dma-list (display-monitor-attributes-list))
	      (id 0))
    (dotimes (i (length dma-list))
      (when (equal (alist-get 'geometry (nth i dma-list)) moom--last-monitor)
	      (setq id i)))
    id))

(defun moom--read-user-margin ()
  "Read new user margin."
  (mapcar #'string-to-number
          (split-string
           (read-string "New user margin: "
                        (mapconcat #'identity (mapcar
                                               #'number-to-string
                                               moom-user-margin) " ")))))

(defun moom--add-command-history (status)
  "Add the last STATUS to `moom--command-history' for undo feature."
  (unless (equal status (car moom--command-history))
    (push status moom--command-history)
    (when (> (length moom--command-history) moom-command-history-length)
      (nbutlast moom--command-history))))

;;;###autoload
(defun moom-undo (&optional index)
  "Undo.
If INDEX is non-nil, revert to the provided id of history."
  (interactive)
  (unless index
    (setq index 0))
  (if (and (numberp index)
           (> (length moom--command-history) index))
      (let ((previous (nth index moom--command-history)))
        (moom-restore-last-status previous)
        (setq moom--command-history (nthcdr (1+ index) moom--command-history))
        (when moom-verbose
          (message "[moom] %s undo available" (length moom--command-history))))
    (message "[moom] No further undo")))

;;;###autoload
(defun moom-identify-current-monitor (&optional shift)
  "Update `moom--screen-margin' to identify and focus on the current monitor.
SHIFT can control the margin, if needed.
If SHIFT is nil, `moom--common-margin' will be applied."
  (interactive)
  (let ((geometry (moom--frame-monitor-geometry))
        (workarea (moom--frame-monitor-workarea)))
    (if (not (and moom-multi-monitors-support
                  (> (length (display-monitor-attributes-list)) 1)))
        (setq moom--screen-margin (moom--default-screen-margin))
      (setq moom--screen-margin
            (let ((shift (or shift moom--common-margin)))
              (list (+ (nth 1 workarea)
                       (nth 0 shift))
                    (+ (- (display-pixel-height)
                          (nth 1 workarea)
                          (nth 3 workarea))
                       (nth 1 shift))
                    (+ (nth 0 workarea)
                       (nth 2 shift))
                    (+ (- (display-pixel-width)
                          (nth 0 workarea)
                          (nth 2 workarea))
                       (nth 3 shift)))))
      (moom--merge-screen-margin moom--local-margin))
    (moom--merge-screen-margin moom-user-margin)
    (moom--update-display-border)
    (unless (equal moom--last-monitor geometry)
      (setq moom--last-monitor geometry)
      (when moom-verbose
        (message "[moom] The current monitor has been changed to %s" geometry))
      (moom--make-frame-height-list)))
  (when (eq window-system 'mac) ;; To avoid visual error
    (redraw-frame)))

;;;###autoload
(defun moom-print-monitors ()
  "Print available monitors with index number.
`moom-jump-to-monitor' could be useful to jump to a monitor."
  (interactive)
  (let ((dma-list (display-monitor-attributes-list))
        (msg nil))
    (dotimes (i (length dma-list))
      (let ((gm (alist-get 'geometry (nth i dma-list))))
        (push (format "[%s] W:%04s H:%04s (%05s,%05s) %s"
                      i (nth 2 gm) (nth 3 gm) (nth 0 gm) (nth 1 gm)
                      (if (equal gm moom--last-monitor) "<current>" "")) msg)))
    (when moom-verbose
      (message "%s" (mapconcat 'concat (reverse msg) "\n")))))

;;;###autoload
(defun moom-jump-to-monitor (id)
  "Jump to a monitor by specifying ID."
  (interactive (list
                (read-number
                 "Target monitor: " (moom--current-monitor-id))))
  (let ((dma-list (display-monitor-attributes-list)))
    (if (< id (length dma-list))
        (let ((workarea (alist-get 'workarea (nth id dma-list))))
          (set-frame-position nil (nth 0 workarea) (nth 1 workarea))
          (when (memq window-system '(x))
            (sleep-for 0.2)) ;; FIXME
          (moom-identify-current-monitor)
          (run-hooks 'moom-after-select-monitor-hook))
      (user-error "Target index shall be less than %s" id (length dma-list)))))

;;;###autoload
(defun moom-cycle-monitors ()
  "Cycle monitors.
`moom-after-select-monitor-hook' could be useful to add some additional
actions when selecting a monitor."
  (interactive)
  (let ((dma-list (display-monitor-attributes-list)))
    (moom-jump-to-monitor
     (let ((v (1+ (moom--current-monitor-id))))
			 (if (equal v (length dma-list)) 0 v))))
  (moom-print-monitors))

;;;###autoload
(defun moom-fill-screen ()
  "Expand frame width and height to fill screen.
The font size in buffers will be increased so that the frame width could be
maintained at 80. The top left corner of the frame is moved to that of screen.
`moom-before-fill-screen-hook' and `moom-after-fill-screen-hook' can be
used to add additional actions."
  (interactive)
  (let ((last (moom--save-last-status)))
    (run-hooks 'moom-before-fill-screen-hook)
    (moom--font-resize (moom--fullscreen-font-size)
                       (+ (moom--max-frame-pixel-width)
                          (moom--frame-internal-width)))
    (set-frame-position nil
                        (moom--pos-x (nth 2 moom--screen-margin))
                        (moom--pos-y (nth 0 moom--screen-margin)))
    (set-frame-size nil
                    (moom--max-frame-pixel-width)
                    (moom--max-frame-pixel-height) t)
    (moom-print-status)
    (run-hooks 'moom-after-fill-screen-hook)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-toggle-frame-maximized ()
  "Toggle frame maximized.
No information is stored for undo."
  (interactive)
  (let ((moom--print-status nil))
    (if moom--maximized
        (progn
          (moom-restore-last-status moom--maximized)
          (setq moom--maximized nil))
      (setq moom--maximized (moom--save-last-status))
      (moom-fill-screen)))
  (moom-print-status))

;;;###autoload
(defun moom-fill-top ()
  "Fill upper half of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'top)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-bottom ()
  "Fill lower half of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'bottom)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-left ()
  "Fill left half of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'left)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-right ()
  "Fill right half of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'right)
    (when (eq window-system 'w32)
      (moom-move-frame-to-edge-right))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-top-left ()
  "Fill top left quarter of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'topl)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-top-right ()
  "Fill top right quarter of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'topr)
    (when (eq window-system 'w32)
      (moom-move-frame-to-edge-right))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-bottom-left ()
  "Fill bottom left quarter of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'botl)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-bottom-right ()
  "Fill bottom right quarter of screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom--fill-display 'botr)
    (when (eq window-system 'w32)
      (moom-move-frame-to-edge-right))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-band (&optional plist)
  "Fill screen by band region.
If PLIST is nil, `moom-fill-band-options' is applied."
  (interactive)
  (let ((last (moom--save-last-status)))
    (let* ((moom--print-status nil)
           (values (or plist moom-fill-band-options))
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
             (setq band-pixel-width (moom--max-frame-pixel-width))))
      (when (and band-pixel-width
                 band-pixel-height)
        (set-frame-size nil band-pixel-width band-pixel-height t)
        (moom-move-frame-to-center)
        (moom--make-frame-height-list)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

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
  (when (and moom-verbose
             line-spacing)
    (message "[Moom] %.1f" line-spacing)))

;;;###autoload
(defun moom-move-frame-right (&optional pixel)
  "PIXEL move the current frame to right."
  (interactive)
  (moom-identify-current-monitor)
  (let ((last (moom--save-last-status)))
    (let* ((pos-x (moom--frame-left))
           (pos-y (moom--frame-top))
           (new-pos-x (+ pos-x (or pixel
                                   (moom--shift-amount 'right)))))
      (when (>= new-pos-x (- (display-pixel-width)
                             (nth 3 moom--screen-margin)))
        (setq new-pos-x (- new-pos-x
                           (display-pixel-width)
                           (moom--frame-pixel-width)
                           (- (moom--shift-amount 'right)))))
      (set-frame-position nil
                          (moom--pos-x new-pos-x)
                          (moom--pos-y pos-y)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-left (&optional pixel)
  "PIXEL move the current frame to left."
  (interactive)
  (moom-identify-current-monitor)
  (let ((last (moom--save-last-status)))
    (let* ((pos-x (moom--frame-left))
           (pos-y (moom--frame-top))
           (new-pos-x (- pos-x (or pixel
                                   (moom--shift-amount 'left)))))
      (when (<= new-pos-x (- (nth 2 moom--screen-margin)
                             (moom--frame-pixel-width)))
        (setq new-pos-x (+ new-pos-x
                           (display-pixel-width)
                           (moom--frame-pixel-width)
                           (- (moom--shift-amount 'left)))))
      (set-frame-position nil
                          (moom--pos-x new-pos-x)
                          (moom--pos-y pos-y)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--horizontal-center-pos) '(:bound t))
                        (moom--pos-y (moom--frame-top) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--frame-left) '(:bound t))
                        (moom--pos-y (moom--vertical-center-pos) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-edge-top ()
  "Move the current frame to the top of the screen.
If you find the frame is NOT moved to the top exactly,
please configure the margins by variable `moom-user-margin'."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--frame-left) '(:bound t))
                        (moom--pos-y (nth 0 moom--screen-margin) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-edge-bottom ()
  "Move the current frame to the top of the screen.
If you find the frame is NOT moved to the bottom exactly,
please configure the margins by variable `moom-user-margin'."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--frame-left) '(:bound t))
                        (moom--pos-y
                         (- (display-pixel-height)
                            (moom--frame-pixel-height)
                            (moom--frame-internal-height)
                            (- (moom--internal-border-height))
                            (nth 1 moom--screen-margin)) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-edge-right ()
  "Move the current frame to the right edge of the screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (- (display-pixel-width)
                                        (moom--frame-pixel-width)
                                        (nth 3 moom--screen-margin))
                                     (when (eq window-system 'w32)
                                       '(:bound t))) ;; FIXME
                        (moom--pos-y (moom--frame-top) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-edge-left ()
  "Move the current frame to the left edge of the screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (nth 2 moom--screen-margin))
                        (moom--pos-y (moom--frame-top) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-centerline-from-left ()
  "Fit frame to vertical line in the middle from left side."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (- (moom--horizontal-center) (moom--frame-pixel-width))
                        (moom--pos-y (moom--frame-top)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-centerline-from-right ()
  "Fit frame to vertical line in the middle from right side."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--horizontal-center)
                        (moom--pos-y (moom--frame-top)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-centerline-from-top ()
  "Fit frame to horizontal line in the middle from above."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--frame-left))
                        (moom--pos-y (- (moom--vertical-center)
                                        (moom--frame-internal-height)
                                        (moom--frame-pixel-height))))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-centerline-from-bottom ()
  "Fit frame to horizontal line in the middle from below."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--frame-left))
                        (moom--pos-y (moom--vertical-center)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame-to-center ()
  "Move the current frame to the center of the screen."
  (interactive)
  (let ((last (moom--save-last-status)))
    (set-frame-position nil
                        (moom--pos-x (moom--horizontal-center-pos))
                        (moom--pos-y (moom--vertical-center-pos) '(:bound t)))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-move-frame (&optional arg)
  "Move the frame to somewhere (default: left top of workarea).
When ARG is a list like '(10 10), move the frame to the position.
When ARG is a single number like 10, shift the frame horizontally +10 pixel.
When ARG is nil, then move to the default position (i.e. left top of workarea)."
  (interactive)
  (let ((last (moom--save-last-status)))
    (let ((pos-x (nth 2 moom--screen-margin))
          (pos-y (nth 0 moom--screen-margin)))
      (when (eq window-system 'w32)
        (setq pos-x (+ pos-x (nth 0 moom--screen-grid)))
        (setq pos-y (+ pos-y (nth 1 moom--screen-grid))))
      (cond ((not arg) t) ;; left top of workarea by '(pos-x, pos-y)
            ((numberp arg) ;; horizontal shift
             (setq pos-x (+ pos-x arg))
             (setq pos-y (moom--frame-top)))
            ((listp arg) ;; move to '(x, y)
             (setq pos-x (+ pos-x (nth 0 arg)))
             (setq pos-y (+ pos-y (nth 1 arg)))))
      (if (eq window-system 'w32)
          (modify-frame-parameters
           nil `((left . (+ ,pos-x)) (top . (+ ,pos-y))))
        (unless (and (eq pos-x (moom--frame-left))
                     (eq pos-y (moom--frame-top)))
          (set-frame-position nil (moom--pos-x pos-x) (moom--pos-y pos-y)))))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-cycle-frame-height ()
  "Change frame height and update the internal ring.
If you find the frame is NOT changed as expected,
please configure the margins by variable `moom-user-margin'.
No information is stored for undo."
  (interactive)
  (unless moom--height-list
    (moom--make-frame-height-list))
  (let ((height (car moom--height-list)))
    ;; Skip the current height
    (when (equal height (moom--frame-height))
      (moom--cycle-frame-height-list)
      (setq height (car moom--height-list)))
    ;; Align to center or bottom line
    (cond ((equal height (moom--max-frame-height))
           (moom-change-frame-height (moom--max-frame-pixel-height) t))
          ((equal height (/ (moom--max-frame-height) 2))
           (moom-change-frame-height (moom--max-half-frame-pixel-height) t))
          (t (moom-change-frame-height height))))
  (moom--cycle-frame-height-list)
  (run-hooks 'moom-resize-frame-height-hook))

;;;###autoload
(defun moom-fill-height ()
  "Expand frame height to fill screen vertically without changing frame width."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom-move-frame-to-edge-top)
    (moom-change-frame-height (moom--max-frame-pixel-height) t)
    (moom--make-frame-height-list)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-change-frame-height (&optional frame-height pixelwise)
  "Change the hight of the current frame.
Argument FRAME-HEIGHT specifies new frame height.
If PIXELWISE is non-nil, the frame height will be changed by pixel value."
  (interactive
   (list (read-number "New Height: " (moom--frame-height))))
  (let ((last (moom--save-last-status)))
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
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-change-frame-width (&optional frame-width pixelwise)
  "Change the frame width by the FRAME-WIDTH argument.
This function does not effect font size.
If FRAME-WIDTH is nil, `moom-frame-width-single' will be used.
If PIXELWISE is non-nil, the frame width will be changed by pixel value.
In that case, variable `moom--frame-width' will keep the same value."
  (interactive
   (list (read-number "New Width: " (moom--frame-width))))
  (let ((last (moom--save-last-status)))
    (unless frame-width
      (setq frame-width moom-frame-width-single))
    (if pixelwise
        (set-frame-width nil frame-width nil pixelwise)
      (set-frame-width nil frame-width)
      (setq moom--frame-width frame-width))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last)))
  (moom-print-status))

;;;###autoload
(defun moom-change-frame-width-single ()
  "Change the frame width to single.
This function does not effect font size."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom-change-frame-width)
    (when (moom--centerize-p 'single)
      (moom-move-frame-to-horizontal-center))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-change-frame-width-double ()
  "Change the frame width to double.
This function does not effect font size."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom-change-frame-width moom-frame-width-double)
    (if (moom--centerize-p 'double)
        (moom-move-frame-to-horizontal-center)
      (moom--stay-in-region))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-change-frame-width-half-again ()
  "Change the frame width to half as large again as single width.
This function does not effect font size."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom-change-frame-width (floor (* 1.5 moom-frame-width-single)))
    (if (moom--centerize-p 'half-again)
        (moom-move-frame-to-horizontal-center)
      (moom--stay-in-region))
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))

;;;###autoload
(defun moom-fill-width ()
  "Change the frame width to fill display horizontally.
This function does not effect font size."
  (interactive)
  (let ((last (moom--save-last-status)))
    (moom-move-frame-to-edge-left)
    (moom-change-frame-width (moom--max-frame-pixel-width) t)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history last))))
(defalias 'moom-change-frame-width-max 'moom-fill-width)

(make-obsolete 'moom-change-frame-width-max 'moom-fill-width "2021-03-09")

;;;###autoload
(defun moom-delete-windows ()
  "Delete all window and make frame width single.
No information is stored for undo."
  (interactive)
  (let ((buffer (buffer-name))
        (p (point)))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (goto-char p))
  (moom-change-frame-width)
  (when (moom--centerize-p 'delete)
    (moom-move-frame-to-horizontal-center))
  (run-hooks 'moom-delete-window-hook))

;;;###autoload
(defun moom-split-window ()
  "Split window and make frame width double.
No information is stored for undo."
  (interactive)
  (moom-change-frame-width moom-frame-width-double)
  (if (moom--centerize-p 'split)
      (moom-move-frame-to-horizontal-center)
    (moom--stay-in-region))
  (split-window-right)
  (run-hooks 'moom-split-window-hook))

;;;###autoload
(defun moom-reset ()
  "Reset associated parameters."
  (interactive)
  (let ((moom--font-module-p (require 'moom-font nil t)))
    (setq moom--maximized nil)
    (setq moom--command-history nil)
    (when (or moom--non-interactive-history (called-interactively-p 'any))
      (moom--add-command-history (moom--save-last-status)))
    (moom-restore-last-status moom--init-status)
    (moom-identify-current-monitor)))

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
      (moom-toggle-frame-maximized)))
  (warn "`moom-screen-margin' is obsolated since 2020-10-01"))

(make-obsolete 'moom-screen-margin 'moom-check-user-margin "2020-10-01")

;;;###autoload
(defun moom-update-user-margin (margin)
  "Update variable `moom-user-margin' and apply it to internal margin.
MARGIN is a list with 4 integers in order of {top, down, left, right}."
  (interactive (list (moom--read-user-margin)))
  (if (not (eq 4 (length margin)))
      (user-error "[moom] Format error...%s" margin)
    (setq moom-user-margin margin)
    (moom-identify-current-monitor)
    (moom--make-frame-height-list)
    (message "[moom] `moom-user-margin' is updated to %s." moom-user-margin)))

;;;###autoload
(defun moom-check-user-margin (margin)
  "Change top, bottom, left, and right margin by provided MARGINS.
MARGIN shall be a list consists of 4 integer variables like '(10 10 20 20)."
  (interactive (list (moom--read-user-margin)))
  (let ((last (moom--save-last-status))
        (sm moom--screen-margin))
    (when (eq 4 (length margin))
      (let ((moom-user-margin margin))
        (moom-reset)
        (moom-move-frame)
        (moom-fill-screen))
      (if (y-or-n-p "[moom] Update `moom-user-margin' for this session? ")
          (moom-update-user-margin margin)
        (message "[moom] Abort.")
        (setq moom--screen-margin sm))
      (moom-restore-last-status last))))

;;;###autoload
(defun moom-restore-last-status (&optional status)
  "Restore the last frame position, size, and font-size.
STATUS is a list consists of font size, frame position, frame region, and pixel-region."
  (interactive)
  (when status
    (setq moom--last-status status))
  (let ((moom-font-before-resize-hook nil)
        (moom-font-after-resize-hook nil))
    (moom--font-resize (cdr (assoc "font-size" moom--last-status)) nil))
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
  (when moom--screen-margin
    (moom--make-frame-height-list))
  (moom-print-status))

;;;###autoload
(defun moom-toggle-font-module ()
  "Toggle `moom--font-module-p'.
When `moom--font-module-p' is nil, font size is fixed except for `moom-reset'
even if \"moom-font.el\" is loaded."
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
'all is identical to '(move fit expand fill font reset undo).
If you give only '(reset) as the argument, then \\[moom-reset] is activated.
The keybindings will be assigned only when Emacs runs in GUI."
  (when window-system
    (when (eq 'all options)
      (setq options '(move fit expand fill font reset undo)))
    (when (memq 'move options)
      (define-key moom-mode-map (kbd "M-0") 'moom-move-frame)
      (define-key moom-mode-map (kbd "M-1") 'moom-move-frame-left)
      (define-key moom-mode-map (kbd "M-2") 'moom-move-frame-to-center)
      (define-key moom-mode-map (kbd "M-3") 'moom-move-frame-right))
    (when (memq 'fit options)
      (define-key moom-mode-map (kbd "M-<f1>") 'moom-move-frame-to-edge-left)
      (define-key moom-mode-map (kbd "M-<f3>") 'moom-move-frame-to-edge-right)
      (define-key moom-mode-map (kbd "<f1>") 'moom-move-frame-to-edge-top)
      (define-key moom-mode-map (kbd "S-<f1>") 'moom-move-frame-to-edge-bottom)
      (define-key moom-mode-map (kbd "C-c f c l")
        'moom-move-frame-to-centerline-from-left)
      (define-key moom-mode-map (kbd "C-c f c r")
        'moom-move-frame-to-centerline-from-right)
      (define-key moom-mode-map (kbd "C-c f c t")
        'moom-move-frame-to-centerline-from-top)
      (define-key moom-mode-map (kbd "C-c f c b")
        'moom-move-frame-to-centerline-from-bottom))
    (when (memq 'expand options)
      (define-key moom-mode-map (kbd "<f2>") 'moom-cycle-frame-height)
      (define-key moom-mode-map (kbd "C-c f s") 'moom-change-frame-width-single)
      (define-key moom-mode-map (kbd "C-c f d") 'moom-change-frame-width-double)
      (define-key moom-mode-map (kbd "C-c f S") 'moom-delete-windows)
      (define-key moom-mode-map (kbd "C-c f D") 'moom-split-window)
      (define-key moom-mode-map (kbd "C-c f a")
        'moom-change-frame-width-half-again))
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
      (define-key moom-mode-map (kbd "C-c f f w") 'moom-fill-width)
      (define-key moom-mode-map (kbd "C-c f f h") 'moom-fill-height)
      (define-key moom-mode-map (kbd "M-<f2>") 'moom-toggle-frame-maximized))
    (when (memq 'font options)
      (define-key moom-mode-map (kbd "C--") 'moom-font-decrease)
      (define-key moom-mode-map (kbd "C-=") 'moom-font-increase)
      (define-key moom-mode-map (kbd "C-0") 'moom-font-size-reset))
    (when (memq 'reset options)
      (define-key moom-mode-map (kbd "C-c C-0") 'moom-reset))
    (when (memq 'undo options)
      (define-key moom-mode-map (kbd "C-c C-/") 'moom-undo))
    (when (and moom-verbose
               options)
      (message "[Moom] Key defined for APIs of %s." options))))

;;;###autoload
(defun moom-print-status ()
  "Print font size, frame size and origin in mini buffer."
  (interactive)
  (when (and moom-verbose
             moom--print-status)
    ;; (message "--- print!")
    (let ((message-log-max nil)
          (fc-width (frame-char-width))
          (fp-width (moom--frame-pixel-width))
          (fp-height (moom--frame-pixel-height)))
      (message
       (format
        "[Moom] Font: %spt(%dpx) | Frame: c(%d, %d) p(%d, %d) | Origin: (%s, %s)"
        (if moom--font-module-p moom-font--size "**")
        fc-width
        (/ (- fp-width (moom--frame-internal-width)) fc-width)
        (/ (- fp-height (moom--internal-border-height)) (frame-char-height))
        fp-width
        fp-height
        (- (moom--frame-left) (nth 2 moom--local-margin))
        (- (moom--frame-top) (nth 0 moom--local-margin)))))))

;;;###autoload
(defun moom-version ()
  "The release version of Moom."
  (interactive)
  (let ((moom-release "1.6.0"))
    (message "[Moom] v%s" moom-release)))

;;;###autoload
(define-minor-mode moom-mode
  "Toggle the minor mode `moom-mode'.
This mode provides a set of commands to control frame position and size.
The font size in buffers will be changed with synchronization of the updated
frame geometry so that the frame width could be maintained at 80.

No keybindings are configured as default but recommended keybindings are
implemented in `moom-mode', thus user setting is very easy.
You just use `moom-recommended-keybindings' to apply the recommended
keybindings.

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

(provide 'moom)

;;; moom.el ends here
