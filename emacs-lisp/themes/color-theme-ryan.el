(eval-when-compile
  (require 'color-theme))

(defun color-theme-ryan-dark-1 ()
  "pale yellow and other pastels on a dark gray background"
  (interactive)
  (color-theme-install
   '(color-theme-ryan-dark-1

     ;; main palette: pale yellow text on dark gray background
     ((background-color . "gray7")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "wheat")
      (foreground-color . "wheat")
      (mouse-color . "black"))
     (default ((t (:foreground "wheat" :background "gray7"))))

     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (italic ((t (:italic t))))
     (underline ((t (:underline t))))

     ;; literals - greens
     (font-lock-string-face        ((t (:foreground "#4DB24D"))))
     (font-lock-constant-face      ((t (:foreground "#4DB24D"))))
     (font-lock-preprocessor-face  ((t (:foreground "#ff5fd7" :italic t ))))

     ;; keywords - light reds
     (font-lock-keyword-face       ((t (:foreground "#ff875f"))))
     (cperl-nonoverridable-face    ((t (:foreground "#ff875f"))))
     (font-lock-type-face          ((t (:foreground "#ff875f"))))
     (font-lock-builtin-face       ((t (:foreground "#ff875f"))))

     ;; names - blues
     (font-lock-variable-name-face ((t (:foreground "light blue"))))
     (cperl-array-face             ((t (:foreground "deep sky blue" :weight normal))))
     (cperl-hash-face              ((t (:foreground "deep sky blue" :weight normal))))
     (font-lock-function-name-face ((t (:foreground "#40D0FF" :bold t ))))

     ;; comments - blue-purple
     (font-lock-comment-face       ((t (:foreground "#5f5f87" ))))

     ;; highlights - turquoises    (dark to light: #046 #06a #0af)
     (highlight ((t (:background "#06a" :foreground "white"))))
     (isearch ((t (:background "#0af" :foreground "white"))))  ; primary isearch highlight
     (lazy-highlight ((t (:background "#046"))))               ; secondary isearch highlight
     (show-paren-match-face ((t (:background "#06a" :foreground "white"))))
     (show-paren-mismatch-face ((t (:background "#a0f" :foreground "white"))))
     (region ((t (:background "#06a" :foreground "white"))))

     ;; whitepace highlighting (for whitespace-mode)
     (whitespace-space ((t (:foreground "gray20" :background "gray7"))))
     (whitespace-tab ((t (:foreground "gray20" :background "gray7"))))
     (whitespace-hspace ((t (:foreground "gray20" :background "dark yellow"))))
     (whitespace-indentation ((t (:foreground "gray20" :background "gray7"))))
     (whitespace-newline ((t (:foreground "gray20" :background "gray7"))))
     (whitespace-trailing ((t (:foreground "#00ffff" :background "gray7" :underline t))))

     (ispell-face ((t (:bold t :background "#FFB774" :foreground "#92E683"))))
     (linum ((t (:foreground "gray30"))))
     (menu ((t (:background "gray20" :foreground "navajo white"))))
     (modeline ((t (:background "gray20" :foreground "LightGray"))))
     (modeline-mousable ((t (:background "light goldenrod" :foreground "dim gray"))))
     (modeline-mousable-minor-mode ((t (:background "dim gray" :foreground "light goldenrod"))))
     (secondary-selection ((t (:background "darkslateblue" :foreground "light goldenrod"))))
     (zmacs-region ((t (:background "dark slate gray" :foreground "#93AAF2"))))
     )))
