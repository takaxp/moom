;;; moom-transient.el --- Moom command dispatcher by transient -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 0.9.8
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/Moom
;; Package-Requires: ((emacs "25.1") (transient "0.3.7"))
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

;; This package provides an example of moom command dispatcher powered
;; by transient.el (see https://github.com/magit/transient).
;;
;; To use this package, put the following code to your init.el.
;; (with-eval-after-load "moom"
;;   (when (require 'moom-transient nil t)
;;     (moom-transient-hide-cursor) ;; if needed
;;     (define-key moom-mode-map (kbd "C-c o") #'moom-transient-dispatch)))

;;; Change Log:

;;; Code:

(require 'moom)
(require 'transient)

(defcustom moom-transient-dispatch-sticky t
  "If Non-nil, keep showing the dispatcher right after executing a command.
This option is used in `moom-transient-dispatch'."
  :type 'boolean
  :group 'moom)

(defvar moom-transient--last-command nil)
(defun moom-transient--dispatch-description ()
  "Update description about executed command."
  (let ((prompt "[moom] Last Command: ")
	(command (symbol-name this-command)))
    (if (eq 0 (string-match "^moom-" command))
	(setq moom-transient--last-command
	      (format "%s%s"
		      (propertize prompt 'face 'font-lock-type-face)
		      (propertize command 'face 'bold)))
      moom-transient--last-command)))

(defun moom-transient--insert-groups ()
  "Enable to shrink a frame horizontally."
  (setq window-size-fixed nil))
(advice-add 'transient--insert-groups :after #'moom-transient--insert-groups)

(defun moom-transient--hide-cursor (f)
  "An advice to F for hiding cursor in transient buffer."
  (let ((transient-enable-popup-navigation nil))
    (funcall f)))

(defun moom-transient--do-stay ()
  "Return a status for 'transient-suffix."
  (if moom-transient-dispatch-sticky 'transient--do-stay nil))

(defun moom-transient--font-module-status ()
  "Return whether `moom-module' is activating or not."
  (format "%s" (if moom--font-module-p
		   (propertize "on" 'face 'font-lock-type-face)
		 (propertize "off" 'face 'font-lock-warning-face))))

(defun moom-transient--font-module-status-description ()
  "Update description on whether `moom-font' is activating or not."
  (format "%s%s%s"
	  (propertize "Fill (font resizing: " 'face 'transient-heading)
	  (moom-transient--font-module-status)
	  (propertize ")" 'face 'transient-heading)))

(defun moom-transient--font-api-status-description ()
  "Update description on whether font api is available."
  (format "%s%s"
	  (propertize "Font " 'face 'transient-heading)
	  (if moom--font-module-p
	      ""
	    (concat
	     (propertize "(" 'face 'transient-heading)
	     (propertize "disabled" 'face 'font-lock-warning-face)
	     (propertize ")" 'face 'transient-heading)))))

;;;###autoload (autoload 'moom-transient-dispatch "moom-transient" nil t)
(transient-define-prefix moom-transient-dispatch ()
  "Command list of `moom'."
  :transient-suffix 'moom-transient--do-stay
  [:description
   moom-transient--dispatch-description
   ["Move"
    ("0" "top-left" moom-move-frame)
    ("1" "left" moom-move-frame-left)
    ("2" "center" moom-move-frame-to-center)
    ("3" "right" moom-move-frame-right)
    ("4" "center (hol)" moom-move-frame-to-horizontal-center)
    ("5" "center (ver)" moom-move-frame-to-vertical-center)]
   ["Expand"
    ("s" "single" moom-change-frame-width-single)
    ("d" "double" moom-change-frame-width-double)
    ("a" "3/2" moom-change-frame-width-half-again)
    ("w" "width (full)" moom-fill-width)
    ("h" "height (full)" moom-fill-height)
    ("H" "height (cycle)" moom-cycle-frame-height)]
   ["Fit (edge)"
    ("e l" "edge left" moom-move-frame-to-edge-left)
    ("e r" "edge right" moom-move-frame-to-edge-right)
    ("e t" "edge top" moom-move-frame-to-edge-top)
    ("e b" "edge bottom" moom-move-frame-to-edge-bottom)]
   ["Fit (center)"
    ("c l" "center left" moom-move-frame-to-centerline-from-left)
    ("c r" "center right" moom-move-frame-to-centerline-from-right)
    ("c t" "center top" moom-move-frame-to-centerline-from-top)
    ("c b" "center bottom" moom-move-frame-to-centerline-from-bottom)]]
  [:description
   moom-transient--font-module-status-description
   [("f 1" "top-left" moom-fill-top-left)
    ("f 2" "top-right" moom-fill-top-right)
    ("f 3" "bottom-left" moom-fill-bottom-left)
    ("f 4" "bottom-right" moom-fill-bottom-right)]
   [("f l" "left" moom-fill-left)
    ("f r" "right" moom-fill-right)
    ("f t" "top" moom-fill-top)
    ("f b" "bottom" moom-fill-bottom)]
   [("f s" "screen" moom-fill-screen)
    ("f m" "band" moom-fill-band)
    ""
    ("F" "toggle font resizing" moom-transient-toggle-font-module)]]
  [["Monitors"
    ;; ("m j" "monitor jump" moom-jump-to-monitor)
    ("m c" "monitor cycle" moom-cycle-monitors)
    ("m p" "monitor print" moom-print-monitors)]
   ["Window"
    ("S" "split" moom-split-window)
    ("D" "delete" moom-delete-windows)]
   [:description
    moom-transient--font-api-status-description
    :if (lambda () moom--font-module-p)
    ("=" "increase" moom-font-increase)
    ("-" "decrease" moom-font-decrease)
    ("R" "reset" moom-font-size-reset)]
   ["Utilities"
    ("r" "reset" moom-reset)
    ("u" "undo" moom-undo)
    ("p" "print" moom-print-status)]
   [""
    ("C" "config" moom-transient-config)
    ("v" "version" moom-transient-version)
    ("q" "quit" transient-quit-all)]])

;;;###autoload (autoload 'moom-transient-config "moom-transient" nil t)
(transient-define-prefix moom-transient-config ()
  "Command list of `moom' configuration."
  ["[moom] Configuration"
   [("t" "generate font table" moom-generate-font-table)
    ("c" "check margin" moom-check-user-margin)
    ("p" "print font name" moom-font-print-name-at-point)
    ("D" "dispatch" moom-transient-dispatch)
    ("q" "quit" transient-quit-all)]])

;;;###autoload
(defun moom-transient-toggle-font-module ()
  "Call `moom-toggle-font-module' with `moom-transient-dispatch'."
  (interactive)
  (moom-toggle-font-module)
  (call-interactively 'moom-transient-dispatch))

;;;###autoload
(defun moom-transient-hide-cursor ()
  "Hide cursor in transient buffer."
  (advice-add 'moom-transient-config :around #'moom-transient--hide-cursor)
  (advice-add 'moom-transient-dispatch :around #'moom-transient--hide-cursor))

;;;###autoload
(defun moom-transient-version ()
  "Printing version of `moom' and `moom-transient'."
  (interactive)
  (let ((alpha "0.9.7"))
    (message "%s" (concat (moom-version) "\n" "[Moom-transient] v" alpha))))

(provide 'moom-transient)

;;; moom-transient.el ends here
