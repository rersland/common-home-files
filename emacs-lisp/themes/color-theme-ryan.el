

(eval-when-compile
  (require 'color-theme))

(defun color-theme-ryan-dark-1 ()
  "pale yellow and other pastels on a dark gray background"
  (interactive)
  (color-theme-install
   '(color-theme-ryan-dark-1

     ;; main palette: pale yellow text on dark gray background
     ((background-color . "gray 6")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "black")
      (foreground-color . "wheat")
      (mouse-color . "black"))
     (default ((t (:foreground "wheat" :background "gray 6"))))

     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     (underline ((t (:underline t))))

     ; plain text - wheat

     ; literals - greens
     (font-lock-string-face        ((t (:foreground "#4DB24D"))))
     (font-lock-constant-face      ((t (:foreground "#4DB24D"))))
     (font-lock-preprocessor-face  ((t (:foreground "#ff5fd7" :italic t ))))

     ; keywords - light reds
     (font-lock-keyword-face       ((t (:foreground "#ff875f"))))
     (cperl-nonoverridable-face    ((t (:foreground "#ff875f"))))
     (font-lock-type-face          ((t (:foreground "#ff875f"))))
     (font-lock-builtin-face       ((t (:foreground "#ff875f"))))

     ; names - blues
     (font-lock-variable-name-face ((t (:foreground "light blue"))))
     (cperl-array-face             ((t (:foreground "deep sky blue" :weight normal))))
     (cperl-hash-face              ((t (:foreground "deep sky blue" :weight normal))))
     (font-lock-function-name-face ((t (:foreground "#40D0FF" :bold t ))))

     ; comments - dark slate
     (font-lock-comment-face       ((t (:foreground "dark slate blue" ))))

     ;; (font-lock-comment-face       ((t (:foreground "dark slate blue" :italic t ))))
     ;; (font-lock-string-face        ((t (:foreground "#92E683"))))
     ;; (font-lock-function-name-face ((t (:foreground "#FFB774" :italic t ))))
     ;; (font-lock-type-face          ((t (:foreground "#FFB774"))))
     ;; (font-lock-warning-face       ((t (:foreground "blue" :bold t ))))

     ; whitepace highlighting
     ;; (cperl-trailing-whitespace-face ((t (:background "gray 40"))))
     ;; (trailing-whitespace-face ((t (:background "dark blue"))))
     (whitespace-space ((t (:foreground "gray 20" :background "gray 6"))))
     (whitespace-tab ((t (:foreground "gray 20" :background "gray 6"))))
     (whitespace-hspace ((t (:foreground "gray 20" :background "dark yellow"))))
     (whitespace-indentation ((t (:foreground "gray 20" :background "gray 6"))))
     (whitespace-newline ((t (:foreground "gray 20" :background "gray 6"))))
     (whitespace-trailing ((t (:foreground "#00ffff" :background "gray 6" :underline t))))

     (highlight ((t (:background "dark slate blue" :foreground "white"))))
     (isearch ((t (:background "dim gray" :foreground "aquamarine"))))
     (ispell-face ((t (:bold t :background "#FFB774" :foreground "#92E683"))))
     (menu ((t (:background "gray 20" :foreground "navajo white"))))
     (modeline ((t (:background "gray 20" :foreground "LightGray"))))
     (modeline-mousable ((t (:background "light goldenrod" :foreground "dim gray"))))
     (modeline-mousable-minor-mode ((t (:background "dim gray" :foreground "light goldenrod"))))
     (region ((t (:background "#005faf" :foreground "bright white"))))
     (secondary-selection ((t (:background "darkslateblue" :foreground "light goldenrod"))))
     (show-paren-match-face ((t (:background "turquoise" :foreground "black"))))
     (show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
     (zmacs-region ((t (:background "dark slate gray" :foreground "#93AAF2"))))

     ;; ;; blue-var family
     ;; (font-lock-variable-name-face ((t (:foreground "sky blue 1"))))
     ;; (cperl-array-face             ((t (:foreground "deep sky blue" :weight normal))))
     ;; (cperl-hash-face              ((t (:foreground "deep sky blue" :weight normal))))
     ;; (font-lock-keyword-face       ((t (:foreground "indian red"))))
     ;; (cperl-nonoverridable-face    ((t (:foreground "indian red"))))
     ;; (font-lock-type-face          ((t (:foreground "indian red"))))

     ;; ;; orange-var family
     ;; (font-lock-variable-name-face ((t (:foreground "tan"))))
     ;; (cperl-array-face             ((t (:foreground "tan" :weight normal :underline nil))))
     ;; (cperl-hash-face              ((t (:foreground "tan" :weight normal ))))
     ;; (font-lock-keyword-face       ((t (:foreground "#60a0d0"))))
     ;; (cperl-nonoverridable-face    ((t (:foreground "#60a0d0"))))
     ;; (font-lock-type-face          ((t (:foreground "#60a0d0"))))
     ;; (font-lock-comment-face       ((t (:foreground "dark slate blue" ))))
     ;; ;; (font-lock-comment-delimiter-face ((t (:foreground "dark slate blue" ))))
     ;; (font-lock-string-face        ((t (:foreground "#209060"))))
     ;; (font-lock-string-face        ((t (:foreground "indian red"))))
     ;; (font-lock-function-name-face ((t (:foreground "#40D0FF" :bold t ))))
     ;; (font-lock-constant-face      ((t (:foreground "#C17DF1"))))
     ;; (font-lock-preprocessor-face  ((t (:foreground "HotPink" :italic t ))))
     ;; (font-lock-warning-face       ((t (:foreground "blue" :bold t ))))


     ;; ;; LaTeX faces
     ;; (font-latex-math-face ((t (:foreground "#92E683"))))
     ;; (font-latex-string-face ((t (:foreground "#92E683"))))
     ;; (font-latex-italic-face ((t (:foreground "#92E683"))))
     ;; (font-latex-bold-face ((t (:foreground "#92E683"))))
     ;; (font-latex-verbatim-face ((t (:foreground "#92E683"))))

     )))


