(require 'markdown-mode)
(require 'poly-markdown)
(require 'polymode-test-utils)
;; ada mode auto loading breaks without this
(require 'speedbar)


;; fixme: add tests when after change spans wrongly temporally cover other spans

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(defun poly-markdown-tests-set-protected (protect)
  (let ((mode (if protect 'poly-head-tail-mode 'host)))
    (oset pm-host/markdown :protect-syntax protect)
    (oset pm-host/markdown :protect-font-lock protect)
    (oset pm-inner/markdown-inline-code :head-mode mode)
    (oset pm-inner/markdown-inline-code :tail-mode mode)
    (oset pm-inner/markdown-fenced-code :head-mode mode)
    (oset pm-inner/markdown-fenced-code :tail-mode mode)))

(ert-deftest poly-markdown/spans-at-borders ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-map-over-spans
     (lambda (span)
       (let* ((sbeg (nth 1 span))
              (send (nth 2 span))
              (range1 (pm-innermost-range sbeg))
              (range2 (pm-innermost-range send)))
         (should (eq sbeg (car range1)))
         (should (eq send (cdr range1)))
         (unless (eq send (point-max))
           (should (eq send (car range2)))))))))

(ert-deftest poly-markdown/spans-at-narrowed-borders ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-map-over-spans
     (lambda (span)
       (pm-with-narrowed-to-span span
         (let* ((range1 (pm-innermost-range (point-min)))
                (range2 (pm-innermost-range (point-max))))
           (should (eq (car range1) (point-min)))
           (should (eq (cdr range1) (point-max)))
           (should (eq (car range2) (point-min)))
           (should (eq (cdr range2) (point-max)))))))))

(ert-deftest poly-markdown/narrowed-spans ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (narrow-to-region 60 200)
    (let ((span (pm-innermost-span (point-min))))
      (should (eq (car span) nil))
      (should (= (nth 1 span) 60))
      (should (= (nth 2 span) 200)))
    (widen)
    (narrow-to-region 60 500)
    (let ((span (pm-innermost-span (point-min))))
      (should (= (nth 1 span) 60))
      ;; 223 should be when inline-innermode is in
      ;; (should (= (nth 2 span) 223))
      (should (= (nth 2 span) 380)))))

(ert-deftest poly-markdown/spans-at-point-max ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (goto-char (point-max))
    (pm-switch-to-buffer)

    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) nil))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) 'tail))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) 'body))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) 'head))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) nil))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))))

;; on emacs25 in batch mode these tests fail
(when (or pm--emacs>26 (not noninteractive))
  (ert-deftest poly-markdown/headings ()
    (poly-markdown-tests-set-protected nil)
    (pm-test-poly-lock poly-markdown-mode "markdown.md"
      ((insert-1 ("^## Intro" beg))
       (insert " ")
       (pm-test-faces)
       (delete-backward-char 1))
      ((delete-2 "^2. Blockquotes")
       (pm-test-faces)
       (backward-kill-word 1))
      ((insert-new-line-3 ("^3. Two Inline" end))
       (insert "\n")
       (pm-test-faces)
       (delete-backward-char 1))))
  (ert-deftest poly-markdown/headings-protected ()
    (poly-markdown-tests-set-protected t)
    (pm-test-poly-lock poly-markdown-mode "markdown.md"
      ((insert-1 ("^## Intro" beg))
       (insert " ")
       (pm-test-faces)
       (delete-backward-char 1))
      ((delete-2 "^2. Blockquotes")
       (backward-kill-word 1))
      ((insert-new-line-3 ("^3. Two Inline" end))
       (insert "\n")
       (pm-test-faces)
       (delete-backward-char 1)))))

(ert-deftest poly-markdown/fenced-code ()
  (poly-markdown-tests-set-protected nil)
  (pm-test-poly-lock poly-markdown-mode "markdown.md"
    ((delete-fence (38))
     ;; (switch-to-buffer (current-buffer))
     (delete-forward-char 1)
     ;; (polymode-syntax-propertize (point-min) (point-max))
     (pm-test-faces)
     (insert "`"))
    ((delete-fortran-print (23))
     (forward-word)
     (delete-backward-char 1))
    ((insert-ada-hello (51))
     (insert "\"hello!\"\n")
     (indent-for-tab-command))
    ((insert-lisp-arg "&rest forms")
     (backward-sexp 2)
     (insert "first-arg "))
    ((python-kill-line (130))
     (kill-line 3))
    ((elisp-kill-sexp ("(while (setq retail" beg))
     (kill-sexp))
    ((elisp-kill-defun ("(defun delete-dups" beg))
     (kill-sexp))))

(ert-deftest poly-markdown/fenced-code-protected ()
  (poly-markdown-tests-set-protected t)
  (pm-test-poly-lock poly-markdown-mode "markdown.md"
    ((delete-fortran-print (23))
     (forward-word)
     (delete-backward-char 1))
    ((insert-ada-hello (51))
     (insert "\"hello!\"\n")
     (indent-for-tab-command))
    ((insert-lisp-arg "&rest forms")
     (backward-sexp 2)
     (insert "first-arg "))
    ((python-kill-line (130))
     (kill-line 3))
    ((elisp-kill-sexp ("(while (setq retail" beg))
     (kill-sexp))
    ((elisp-kill-defun ("(defun delete-dups" beg))
     (kill-sexp))))

(ert-deftest poly-markdown/inline-math ()
  (pm-test-run-on-string 'poly-markdown-mode
    "Some text with $\\text{inner math}$, formulas $E=mc^2$
$E=mc^2$, and more formulas $E=mc^2$;
```pascal
Some none-sense (formula $E=mc^2$)
```"
    ;; (switch-to-buffer (current-buffer))
    (goto-char 17)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 35)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 47)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 54)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 56)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 84)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 91)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 127)
    (pm-switch-to-buffer)
    (should (eq major-mode 'pascal-mode))))

(ert-deftest poly-markdown/inline-math-both ()
  (pm-test-run-on-string 'poly-markdown-mode
    "Some text with $\\text{inner math}$, formulas $E=mc^2$,
\\(E=mc^2\\), $E=mc^2$, and more formulas \\(E=mc^2\\)

```pascal
Some none-sense (formula $E=mc^2$ \\(dddd\\))
```"
    ;; (switch-to-buffer (current-buffer))
    (goto-char 17)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 35)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 48)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 54)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 57)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 58)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 64)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 69)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 97)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 98)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 155)
    (pm-switch-to-buffer)
    (should (eq major-mode 'pascal-mode))))

(ert-deftest poly-markdown/displayed-math ()
  (pm-test-run-on-string 'poly-markdown-mode
    "Some text with
$$\\text{displayed math}$$, formulas
$$E=mc^2$$
$$E=mc^2$$, and $343 more $$$ formulas $$$ and $3
 $$ E=mc^2 $$;
```pascal
Some none-sense (formula
$$E=mc^2$$ )
```"
    ;; (switch-to-buffer (current-buffer))
    (goto-char 18)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 42)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 55)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 64)
    (pm-switch-to-buffer)
    (should-not (eq major-mode 'latex-mode))
    (goto-char 66)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 84)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 102)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 118)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 169)
    (pm-switch-to-buffer)
    (should (eq major-mode 'pascal-mode))))

(ert-deftest poly-markdown/inline-code-in-host-mode ()
  (pm-test-run-on-string 'poly-markdown-mode
    "aaa `non-mode bbb` cccc"
    ;; (switch-to-buffer (current-buffer))
    (goto-char 14)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (should (eq (car (get-text-property (point) 'face))
                'markdown-inline-code-face))))

(ert-deftest poly-markdown/displayed-math-both ()
  (pm-test-run-on-string 'poly-markdown-mode
    "Some text with
$$\\text{displayed math}$$, formulas \\[E=mc^2\\]
\\[E=mc^2\\]
$$E=mc^2$$, and $343 more $$$ formulas $$$ and $3
 \\[ E=mc^2
 \\];
```pascal
Some none-sense (formula
\\[E=mc^2\\] )
```"
    ;; (switch-to-buffer (current-buffer))
    (goto-char 18)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 42)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 55)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 64)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 65)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 59)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 76)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 104)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 122)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 126)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 127)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 135)
    (pm-switch-to-buffer)
    (should (eq major-mode 'latex-mode))
    (goto-char 136)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (goto-char 178)
    (pm-switch-to-buffer)
    (should (eq major-mode 'pascal-mode))))

(ert-deftest poly-markdown/nested-$-body ()
  "Test for #6"
  (oset poly-markdown-inline-math-innermode :allow-nested t)
  (pm-test-spans 'poly-markdown-mode
    "```python
 $
python-mode-here
```
")
  (pm-test-run-on-string 'poly-markdown-mode
    "```python
 $
python-mode-here
```
"
    (switch-to-buffer (current-buffer))
    (goto-char 14)
    (pm-switch-to-buffer)
    (should (eq major-mode 'python-mode))
    (should (equal (pm-innermost-range 31 'no-cache)
                   (cons 31 34))))
  (oset poly-markdown-inline-math-innermode :allow-nested nil))

(ert-deftest poly-markdown/nested-$-head ()
  "Test for #6"
  (oset poly-markdown-inline-math-innermode :allow-nested t)
  (pm-test-spans 'poly-markdown-mode
    "```{python $ }
python-mode-here
```
")
  (pm-test-run-on-string 'poly-markdown-mode
    "```{python $ }
python-mode-here
```
"
    (switch-to-buffer (current-buffer))
    (goto-char 16)
    (pm-switch-to-buffer)
    (should (eq major-mode 'python-mode))
    (should (equal (pm-innermost-range 16 'no-cache)
                   (cons 16 33))))
  (oset poly-markdown-inline-math-innermode :allow-nested nil))

(ert-deftest poly-markdown/nested-$-general ()
  "Test for #6"
  (pm-test-spans 'poly-markdown-mode
    "```{python}
sf $sfsfdsfsfd
```

```{python test1}
python-here
a + 3 $
python-here
```
")
  (pm-test-run-on-string 'poly-markdown-mode
    "```{python}
sf $sfsfdsfsfd
```

```{python test1}
python-here
a + 3 $
python-here
```
"
    (switch-to-buffer (current-buffer))
    (pm-switch-to-buffer (goto-char 40))
    (should (or (eq major-mode 'markdown-mode)
                (eq major-mode 'poly-head-tail-mode)))
    (pm-switch-to-buffer (goto-char 71))
    (should (eq major-mode 'python-mode))))

(ert-deftest poly-markdown/yaml-block ()
  (pm-test-run-on-file poly-markdown-mode "markdown-with-yaml.md"
    (goto-char (point-min))
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (forward-line 1)
    (pm-switch-to-buffer)
    (should (eq major-mode 'yaml-mode))
    (forward-line 2)
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))))

;; useless :( #163 shows only when `kill-buffer` is called interactively
(ert-deftest poly-markdown/kill-buffer ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (let ((base-buff (buffer-base-buffer)))
      (re-search-forward "defmacro")
      (pm-switch-to-buffer)
      (let (kill-buffer-query-functions)
        (kill-buffer))
      (should-not (buffer-live-p base-buff)))))

(ert-deftest poly-markdown/indentation ()
  (pm-test-indentation poly-markdown-mode "markdown.md"))

(ert-deftest infra/poly-markdown/clone-indirect-buffer ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"

    ;; preserve ib in the host mode
    (let ((bname "cloned-buffer")
          (cbuff (current-buffer))
          (inhibit-message t))
      (goto-char 1)
      (clone-indirect-buffer bname t)
      (switch-to-buffer bname)
      (should (equal bname (buffer-name)))
      (should (eq 'markdown-mode major-mode))
      (goto-line 22) ;; fortran location
      (should (eq 'markdown-mode major-mode))
      (pm-switch-to-buffer)
      (should (eq 'markdown-mode major-mode))
      (should (equal bname (buffer-name)))
      (kill-buffer)
      (should (buffer-live-p cbuff))
      (switch-to-buffer cbuff))

    ;; also host from the innermode
    (let ((bname "cloned-buffer")
          (cbuff (current-buffer))
          (inhibit-message t))
      (goto-line 22)
      (pm-switch-to-buffer)
      (clone-indirect-buffer bname t)
      (switch-to-buffer bname)
      (should (equal bname (buffer-name)))
      (should (eq 'markdown-mode major-mode))
      (pm-switch-to-buffer)
      (should (eq 'markdown-mode major-mode))
      (should (equal bname (buffer-name)))
      (kill-buffer)
      (should (buffer-live-p cbuff)))))

(ert-deftest infra/poly-markdown/pm-map-over-modes ()
  (pm-test-map-over-modes poly-markdown-mode "markdown.md"))
