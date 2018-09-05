;;; poly-markdown.el --- Polymode for markdown-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2018
;; Version: 0.1
;; Package-Requires: ((emacs "25") (polymode "0.1") (markdown-mode "2.3"))
;; URL: https://github.com/polymode/poly-markdown
;; Keywords: emacs
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'markdown-mode)

(defcustom pm-host/markdown
  (pm-host-chunkmode :name "Markdown"
                     :mode 'markdown-mode
                     :init-functions '(poly-markdown-remove-markdown-hooks)
                     :protect-syntax nil
                     :protect-font-lock nil)
  "Markdown host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/markdown-fenced-code
  (pm-inner-auto-chunkmode :name "markdown-fenced-code"
                           :head-matcher "^[ \t]*```[{ \t]*\\w.*\n"
                           :tail-matcher "^[ \t]*```[ \t]*$"
                           :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown fenced code block."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-inner/markdown-inline-code
  (pm-inner-auto-chunkmode :name "markdown-inline-code"
                           :head-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
                           :tail-matcher (cons "[^`]\\(`\\)[^`]" 1)
                           :mode-matcher (cons "`[ \t]*{?\\(?:lang *= *\\)?\\([[:alpha:]+-]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown inline code."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-inner/markdown-displayed-math
  (pm-inner-chunkmode :name "markdown-displayed-math"
                      :head-matcher (cons "^[ \t]*\\(\\$\\$\\)." 1)
                      :tail-matcher (cons "[^$]\\(\\$\\$\\)[^$[:alnum:]]" 1)
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Displayed math $$..$$ block.
Tail must be flowed by new line but head not (a space or comment
character would do)."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-inner/markdown-inline-math
  (pm-inner-chunkmode :name "markdown-inline-math"
                      :head-matcher (cons "[ \t\n]\\(\\$\\)[^ $\t[:digit:]]" 1)
                      :tail-matcher (cons "[^ $\\\t]\\(\\$\\)[^$[:alnum:]]" 1)
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Displayed math $$..$$ block.
Tail must be flowed by new line but head not (a space or comment
character would do)."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-poly/markdown
  (pm-polymode :name "markdown"
               :hostmode 'pm-host/markdown
               :innermodes '(pm-inner/markdown-fenced-code
                             pm-inner/markdown-inline-code
                             pm-inner/markdown-displayed-math
                             pm-inner/markdown-inline-math))
  "Markdown typical configuration"
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-markdown-mode "poly-markdown")
(define-polymode poly-markdown-mode pm-poly/markdown)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))

;;; FIXES:
(defun poly-markdown-remove-markdown-hooks (_)
  ;; get rid of aggressive hooks (VS[02-09-2018]: probably no longer necessary)
  (remove-hook 'window-configuration-change-hook 'markdown-fontify-buffer-wiki-links t)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t))

;; temporary fix #168  https://github.com/jrblevin/markdown-mode/pull/360
(defun markdown-code-block-at-pos (pos)
  "Return match data list if there is a code block at POS.
Uses text properties at the beginning of the line position.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  Return nil otherwise."
  (let ((bol (save-excursion (goto-char pos) (point-at-bol))))
    (or (get-text-property bol 'markdown-pre)
        (let* ((bounds (markdown-get-enclosing-fenced-block-construct pos))
               (second (cl-second bounds)))
          (if second
              ;; chunks are right open
              (when (< pos second)
                bounds)
            bounds)))))

(provide 'poly-markdown)
