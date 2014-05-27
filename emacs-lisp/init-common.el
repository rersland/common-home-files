;; ============================================================================
;; general emacs behavior and functionality
;; ============================================================================

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
(require 'ido)

;; define previous-buffer and next-buffer functions for buffer navigation
(require 'prev-next-buffer)

;; ============================================================================
;; style
;; ============================================================================

;; show line/column numbers in the mode line
(line-number-mode t)
(column-number-mode t)

;; show line numbers in the gutter
;(global-linum-mode)

;; gui mode settings
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-splash-screen t)
(set-scroll-bar-mode 'right)

;; Configure whitespace highlighting and turn it on locally in various programming major
;; modes.
(setq-default whitespace-style (quote (face tabs spaces trailing indentation space-mark tab-mark)))
(let ((turn-on-local-whitespace-mode (lambda () (whitespace-mode t)))
      (list-of-hooks '(prog-mode-hook cperl-mode-hook)))
  (while list-of-hooks
    (add-hook (pop list-of-hooks) turn-on-local-whitespace-mode)))

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

;; other
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "C-<f5>") 'toggle-kbd-macro-recording-on)
(global-set-key (kbd "<f6>") 'whitespace-mode)

;; Load PuTTY keymaps if the IN_PUTTY environment variable is set. (I'll have to
;; configure PuTTY to always set this variable.)
(if (getenv "IN_PUTTY")
    (load-library "init-putty"))
