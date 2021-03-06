;; ============================================================================
;; general emacs behavior and functionality
;; ============================================================================

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use fonts
(global-font-lock-mode t)

;; turn on active region highlighting
(setq transient-mark-mode t)

;; highlight matching parens
(show-paren-mode 1)

;; assign a number to each window and use ALT-<number> to go to that window
(require 'window-numbering)
(window-numbering-mode t)

;; save file backups under ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; redefine make-auto-save-file-name so auto-save files will always be saved
;; under ~/.emacs.d/autosaves
(defun make-auto-save-file-name ()
  (let ((file-name (or buffer-file-name (buffer-name))))
    (concat "~/.emacs.d/autosaves/"
            "#"
            (replace-regexp-in-string "/" "!" file-name)
            "#")))

;; ido - improved interactivity for buffer switching and such
(ido-mode t)

;; define previous-buffer and next-buffer functions for buffer navigation
(require 'prev-next-buffer)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(electric-indent-mode -1)

(load-library "init-org-mode")

;; ============================================================================
;; style
;; ============================================================================

;; show line/column numbers in the mode line
(line-number-mode t)
(column-number-mode t)

;; show line numbers in the gutter
(global-linum-mode)

;; add a one-column space between the line numbers to the left and the text to the right
;; (defun linum-format-func (line)
;;   (let ((gutter-width (length (number-to-string (count-lines (point-min) (point-max))))))
;;     (propertize (format (format "%%%dd " gutter-width) line) 'face 'linum)))
(defun linum-format-func (line)
  ;; (propertize (format (format "%%%dd" (% (line-number-at-pos (point)) 10) ) line) 'face 'linum))
  (propertize (format "%4d " line) 'face 'linum))
(setq linum-format 'linum-format-func)

;; gui mode settings
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-splash-screen t)
(set-scroll-bar-mode 'right)

;; configure whitespace highlighting
(setq-default whitespace-style (quote (face tabs spaces trailing indentation space-mark tab-mark)))

;; This adds a hook to several programming major modes to automatically turn on
;; whitespace mode. I feel like leaving it off by default at the moment, though.
; (let ((turn-on-local-whitespace-mode (lambda () (whitespace-mode t)))
;       (list-of-hooks '(prog-mode-hook cperl-mode-hook)))
;   (while list-of-hooks
;     (add-hook (pop list-of-hooks) turn-on-local-whitespace-mode)))

;; install my personal color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-ryan-dark-1)

;; ============================================================================
;; utility functions
;; ============================================================================

;; from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  "show the name of the face displayed under the current point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "no face"))))

(defun kill-starred-buffers ()
  "reduce buffer clutter by killing buffers I probably don't need to persist"
  (and (get-buffer "*Completions*") (kill-buffer "*Completions*"))
  (and (get-buffer "*Help*") (kill-buffer "*Help*"))
  (and (get-buffer "*Compile-Log*") (kill-buffer "*Compile-Log*"))
  (and (get-buffer "*scratch*") (kill-buffer "*scratch*"))
  )

;; lets me bind a single key to start and stop recording keyboard macros
;; from http://www.emacswiki.org/emacs/KeyboardMacros
(defun toggle-kbd-macro-recording-on ()
  (interactive)
  (define-key global-map (this-command-keys) 'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))
(defun toggle-kbd-macro-recording-off ()
  (interactive)
  (define-key global-map (this-command-keys) 'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; rigidly indent or unindent an entire region by one tab
(defun indent-rigidly-one-tab ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) tab-width))
(defun unindent-rigidly-one-tab ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) (* -1 tab-width)))

(defun open-empty-buffer ()
  (interactive)
  (let ((b (generate-new-buffer "untitled")))
    (switch-to-buffer b)
    (funcall text-mode)
    (setq buffer-offer-save t)
    b))

;; ============================================================================
;; general editing settings
;; ============================================================================

;; indent with spaces only
(setq-default indent-tabs-mode nil)

;; display tab characters as 4 spaces wide
(setq-default tab-width 4)

;; wrap long lines at 89 characters
(setq-default fill-column 89)

;; default across various modes: indent in steps of 4 spaces
(setq-default c-basic-offset 4)

;; ============================================================================
;; major modes
;; ============================================================================

(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(setq tide-format-options '(
  :indentSize 4
  :tabSize 4
  :convertTabsToSpaces nil))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (local-set-key (kbd "M-n") 'tide-rename-symbol))

;; (add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ============================================================================
;; key bindings
;; ============================================================================

;; navigation
(global-set-key (kbd "M-<right>") (kbd "M-f"))
(global-set-key (kbd "M-<left>") (kbd "M-b"))
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<delete>") 'kill-word)
(global-set-key (kbd "M-<prior>") (kbd "C-u 3 <prior>"))
(global-set-key (kbd "M-<next>") (kbd "C-u 3 <next>"))
(global-set-key (kbd "C-x <left>") 'previous-buffer)
(global-set-key (kbd "C-x <right>") 'next-buffer)
(global-set-key (kbd "C-x C-l") 'goto-line)

;; split windows
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x C-p") 'other-window)
(global-set-key (kbd "C-x p") 'other-window)

;; search/replace
(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-C-s") 'query-replace-regexp)

;; org-mode
;(eval-after-load 'org-mode '(define-key ...

; org-shift[up/down/left/right]             C-c <udlr> (default)
; org-meta[up/down/left/right]              C-v <udlr>
; outline-[next/previous]-visible-heading   
; org-[forward/backward]-same-level         M-<ud>
; outline-up-heading                        C-c C-u (default)
; [forward/backward]-word                   M-<lr>
; [forward/backward]-paragraph              ---

; M-<udlr>
; C-c <udlr>
; C-v <udlr>



;; other
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "C-<f5>") 'toggle-kbd-macro-recording-on)
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f7>") 'linum-mode)

(define-prefix-command 'v-prefix)
(global-set-key (kbd "C-v") 'v-prefix)
(global-set-key (kbd "C-v C-<left>") (kbd "S-<left>"))
(global-set-key (kbd "C-v ]") 'indent-rigidly-one-tab)
(global-set-key (kbd "C-v [") 'unindent-rigidly-one-tab)
(global-set-key (kbd "C-v n") 'open-empty-buffer)

;; If we're running in PuTTY, load PuTTY-specific keymaps.
(if (getenv "SSH_TTY")
    (load-library "init-putty"))
