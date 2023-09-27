;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html

(setq mylisp-highlights
      '(("Sin\\|Cos\\|Sum\\|deftest\\|defcount-games" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)))

(define-derived-mode mylisp-mode lisp-mode "MyLisp"
  "major mode for editing mymath language code."
;;  (setq font-lock-defaults '((mylisp-highlights)))
  (setq font-lock-defaults '(mylisp-highlights))
  )

;(provide 'mylisp-mode)

