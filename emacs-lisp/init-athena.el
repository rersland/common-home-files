;; ============================================================================
;; athena-specific settings
;; ============================================================================

;; (load "~/p4/techops/coredev/emacs/wikifilesync.el")  ;; functions for editing wiki nodes
;; (load "~/p4/techops/coredev/emacs/p4.el")
;; (p4-set-p4-executable "/usr/local/bin/athena/auto_local_p4")

(setq auto-mode-alist
      (append '(("\\.pl$" . cperl-mode)
                ("\\.pm$" . cperl-mode)
                ;; ("\\.esp$" . cperl-mode)
                ("\\.esp$" . html-mode)
                ("\\.sql$" . sql-mode)
                ;; ("\\.jsx$" . rjsx-mode)
                ("\\.tsx$" . typescript-mode)
                ("\\.whiskers$" . mustache-mode))
              auto-mode-alist))

(defun setup-athena-formatting ()
  (setq c-basic-offset 4
        cperl-close-paren-offset -4
        cperl-continued-statement-offset 0
        cperl-highlight-variables-indiscriminately t
        cperl-indent-level 4
        cperl-indent-parens-as-block t
        cperl-label-offset 4
        cperl-min-label-indent 0
        cperl-use-syntax-table-text-property t
        cperl-use-syntax-table-text-property-for-tags t
        fill-column 89
        indent-tabs-mode t
        perl-indent-level 4
        tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 64 88 92 96 100 104 108 112 116 120)
        tab-width 4
        user-mail-address "rersland@athenahealth.com"))

(dolist (mode-hook '(cperl-mode-hook
                     js-mode-hook
                     css-mode-hook
                     less-css-mode-hook
                     html-mode-hook
                     mustache-mode-hook
                     typescript-mode-hook))
  (add-hook mode-hook 'setup-athena-formatting))

(defun setup-athena-jsx-formatting ()
  (setq indent-tabs-mode t
        tab-stop-list nil
        tab-width 4))
(add-hook 'rjsx-mode-hook 'setup-athena-jsx-formatting)

;; courtesy of @miblack on #emacs, 3/15/2017
(defun mannotate-region-or-buffer ()
  "Mannotates the currently selected region or the entire buffer if no region is selected. Uses --recur."
  (interactive)
  (let* ((command (if (region-active-p)
                      (let ((start-line (line-number-at-pos (region-beginning)))
                            (end-line   (if (<= 10 (char-before (region-end)) 13) ;; If it's a new line char, that pushes it to the new line, which it shouldn't >:/
                                            (- (line-number-at-pos (region-end)) 1)
                                            (line-number-at-pos (region-end)))))
                        (concat "mannotate --recur " (buffer-file-name) " " (number-to-string start-line) "-" (number-to-string end-line)))
                      (concat "mannotate --recur " (buffer-file-name))))
         (buffer-name (concat "*" command "*")))
    (with-help-window buffer-name
      (shell-command command buffer-name))))

(setq create-lockfiles nil)
(put 'narrow-to-region 'disabled nil)
