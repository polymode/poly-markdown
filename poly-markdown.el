;;; poly-markdown.el --- Polymode for markdown-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2018
;; Version: 0.1.5
;; Package-Requires: ((emacs "25") (polymode "0.1.5") (markdown-mode "2.3"))
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
                     :init-functions '(poly-markdown-remove-markdown-hooks))
  "Markdown host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/markdown-yaml-metadata
  (pm-inner-chunkmode :name "markdown-yaml-metadata"
                      :mode 'yaml-mode
                      :head-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-begin)
                      :tail-matcher (pm-make-text-property-matcher 'markdown-yaml-metadata-end)
                      :head-mode 'host
                      :tail-mode 'host)
  "Markdown YAML metadata block."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-inner/markdown-fenced-code
  (pm-inner-auto-chunkmode :name "markdown-fenced-code"
                           :head-matcher (cons "^[ \t]*\\(```{?[[:alpha:]].*\n\\)" 1)
                           :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
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

(defun poly-markdown-displayed-math-head-matcher (count)
  (when (re-search-forward "\\\\\\[\\|^[ \t]*\\(\\$\\$\\)." nil t count)
    (if (match-beginning 1)
        (cons (match-beginning 1) (match-end 1))
      (cons (match-beginning 0) (match-end 0)))))

(defun poly-markdown-displayed-math-tail-matcher (_count)
  (if (match-beginning 1)
      ;; head matched an $$..$$ block
      (when (re-search-forward "[^$]\\(\\$\\$\\)[^$[:alnum:]]" nil t)
        (cons (match-beginning 1) (match-end 1)))
    ;; head matched an \[..\] block
    (when (re-search-forward "\\\\\\]" nil t)
      (cons (match-beginning 0) (match-end 0)))))

(defcustom pm-inner/markdown-displayed-math
  (pm-inner-chunkmode :name "markdown-displayed-math"
                      :head-matcher #'poly-markdown-displayed-math-head-matcher
                      :tail-matcher #'poly-markdown-displayed-math-tail-matcher
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Displayed math $$..$$ block.
Tail must be flowed by new line but head not (a space or comment
character would do)."
  :group 'poly-innermodes
  :type 'object)

(defun poly-markdown-inline-math-head-matcher (count)
  (when (re-search-forward "\\\\(\\|[ \t\n]\\(\\$\\)[^ $\t[:digit:]]" nil t count)
    (if (match-beginning 1)
        (cons (match-beginning 1) (match-end 1))
      (cons (match-beginning 0) (match-end 0)))))

(defun poly-markdown-inline-math-tail-matcher (_count)
  (if (match-beginning 1)
      ;; head matched an $..$ block
      (when (re-search-forward "[^ $\\\t]\\(\\$\\)[^$[:alnum:]]" nil t)
        (cons (match-beginning 1) (match-end 1)))
    ;; head matched an \(..\) block
    (when (re-search-forward "\\\\)" nil t)
      (cons (match-beginning 0) (match-end 0)))))

(defcustom pm-inner/markdown-inline-math
  (pm-inner-chunkmode :name "markdown-inline-math"
                      :head-matcher #'poly-markdown-inline-math-head-matcher
                      :tail-matcher #'poly-markdown-inline-math-tail-matcher
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Inline math $..$ block.
First $ must be preceded by a white-space character and followed
by a non-whitespace/digit character. The closing $ must be
preceded by a non-whitespace and not followed by an alphanumeric
character."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-poly/markdown
  (pm-polymode :name "markdown"
               :hostmode 'pm-host/markdown
               :innermodes '(pm-inner/markdown-fenced-code
                             pm-inner/markdown-inline-code
                             pm-inner/markdown-displayed-math
                             pm-inner/markdown-inline-math
                             pm-inner/markdown-yaml-metadata))
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

(provide 'poly-markdown)
