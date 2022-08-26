;;; moom-transient.el --- Command dispatcher by transient -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: frames, faces, convenience
;; Version: 0.9.0
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

;; This package provides a moom command dispatcher powered by transient.el.
;; To use this package, put the following code to your init.el
;; (with-eval-after-load "moom"
;;   (when (require 'moom-transient nil t)
;;     (moom-transient-hide-cursor) ;; if needed
;;     (define-key moom-mode-map (kbd "C-c d") #'moom-transient-dispatch)))

;;; Change Log:

;;; Code:

(eval-when-compile
  (require 'moom)
  (require 'transient))

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
  "Enable to shrink a frame vertically."
  (setq window-size-fixed nil))
(advice-add 'transient--insert-groups :after #'moom-transient--insert-groups)

(defun moom-transient--hide-cursor (f)
  "An advice to F for hiding cursor in transient buffer."
  (let ((transient-enable-popup-navigation nil))
    (funcall f)))

;;;###autoload
(transient-define-prefix moom-transient-dispatch ()
  "Command list of `moom'."
  :transient-suffix 'transient--do-stay
  [:description
   moom-transient--dispatch-description
   ["Move the frame"
    ("0" "move top-left" moom-move-frame)
    ("1" "move left" moom-move-frame-left)
    ("2" "move center" moom-move-frame-to-center)
    ("3" "move right" moom-move-frame-right)]
   ["Expand"
    ("w s" "single" moom-change-frame-width-single)
    ("w d" "double" moom-change-frame-width-double)
    ("w a" "3/2" moom-change-frame-width-half-again)
    ("c h" "height" moom-cycle-frame-height)]
   ["Fit (edge)"
    ("e l" "edge left" moom-move-frame-to-edge-left)
    ("e r" "edge right" moom-move-frame-to-edge-right)
    ("e t", "edge top" moom-move-frame-to-edge-top)
    ("e b", "edge bottom" moom-move-frame-to-edge-bottom)]
   ["Fit (center)"
    ("c l" "center left" moom-move-frame-to-centerline-from-left)
    ("c r" "center right" moom-move-frame-to-centerline-from-right)
    ("c t" "center top" moom-move-frame-to-centerline-from-top)
    ("c b" "center bottom" moom-move-frame-to-centerline-from-bottom)]]
  [["Fill screen"
    ("f s" "screen" moom-fill-screen)
    ("f w" "width" moom-fill-width)
    ("f h" "height" moom-fill-height)
    ("f m" "band" moom-fill-band)]
   ["Fill screen (half)"
    ("f t" "top" moom-fill-top)
    ("f b" "bottom" moom-fill-bottom)
    ("f l" "left" moom-fill-left)
    ("f r" "right" moom-fill-right)]
   ["Fill screen (quarter)"
    ("f 1" "top left" moom-fill-top-left)
    ("f 2" "top right" moom-fill-top-right)
    ("f 3" "bottom left" moom-fill-bottom-left)
    ("f 4" "bottom right" moom-fill-bottom-right)]
   ["Utilities"
    ("r" "reset" moom-reset)
    ("u" "undo" moom-undo)
    ("v" "version" moom-version)
    ("q" "quit" transient-quit-all)]])

;;;###autoload
(transient-define-prefix moom-transient-config ()
  "Command list to configure `moom'."
  :transient-suffix 'transient--do-stay
  ["[moom] Configuration"
   [("m p" "monitor print" moom-print-monitors)
    ("m j" "monitor jump" moom-jump-to-monitor)
    ("m c" "monitor cycle" moom-cycle-monitors)
    ("m i" "monitor id" moom-identify-current-monitor)]
   [("w s" "window split" moom-split-window)
    ("w d" "window delete" moom-delete-windows)]
   [("s c" "spacing cycle" moom-cycle-line-spacing)
    ("s r" "spacing reset" moom-reset-line-spacing)]]
  [[("f t" "font table" moom-generate-font-table)
    ("t f" "toggle font" moom-toggle-font-module)
    ("t m" "toggle maximized" moom-toggle-frame-maximized)]
   [("u" "update margin" moom-update-user-margin)
    ("c" "check margin" moom-check-user-margin)
    ("p" "print" moom-print-status)
    ("r" "restore last" moom-restore-last-status)]])

;;;###autoload
(defun moom-transient-hide-cursor ()
  "Hide cursor in transient buffer."
  (advice-add 'moom-transient-config :around #'moom-transient--hide-cursor)
  (advice-add 'moom-transient-dispatch :around #'moom-transient--hide-cursor))

(provide 'moom-transient)

;;; moom-transient.el ends here
