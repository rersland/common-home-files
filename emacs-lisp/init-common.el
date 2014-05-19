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

;; save file backups under ~/emacs/backups
(setq backup-directory-alist '(("." . "~/emacs/backups")))

;; redefine make-auto-save-file-name so auto-save files will always be saved
;; under ~/emacs/autosaves
(defun make-auto-save-file-name ()
  (let ((file-name (or buffer-file-name (buffer-name))))
    (concat "~/emacs/autosaves/"
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

;; turn on global whitespace highlighting
(global-whitespace-mode)
(setq whitespace-style (quote (face tabs spaces trailing indentation space-mark tab-mark)))

;; install my personal color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-ryan-dark-1)

;; ============================================================================
;; utility funcitons
;; ============================================================================

;; what face is being used at the current point?
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "no face"))))

;; reduce buffer clutter by killing buffers I probably don't need to persist
(defun kill-starred-buffers ()
  (and (get-buffer "*Completions*") (kill-buffer "*Completions*"))
  (and (get-buffer "*Help*") (kill-buffer "*Help*"))
  (and (get-buffer "*Compile-Log*") (kill-buffer "*Compile-Log*"))
  (and (get-buffer "*scratch*") (kill-buffer "*scratch*"))
  )

;; ============================================================================
;; major modes
;; ============================================================================

(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ============================================================================
;; key bindings
;; ============================================================================

(global-set-key (kbd "ESC <right>") (kbd "M-f"))
(global-set-key (kbd "ESC <left>") (kbd "M-b"))
(global-set-key (kbd "ESC <up>") 'backward-paragraph)
(global-set-key (kbd "ESC <down>") 'forward-paragraph)
(global-set-key (kbd "ESC <deletechar>") 'kill-word)    ;; ALT-DELETE
(global-set-key (kbd "ESC [ 1 ~") (kbd "C-a"))          ;; HOME
(global-set-key (kbd "<select>") (kbd "C-e"))           ;; END
(global-set-key (kbd "ESC r") 'isearch-backward-regexp)
(global-set-key (kbd "ESC s") 'isearch-forward-regexp)
(global-set-key (kbd "ESC C-s") 'query-replace-regexp)
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f6] 'start-kbd-macro)
(global-set-key [f7] 'end-kbd-macro)
(global-set-key [f8] 'recompile)
(global-set-key (kbd "ESC <prior>") (kbd "C-u 3 <prior>"))
(global-set-key (kbd "ESC <next>") (kbd "C-u 3 <next>"))
(global-set-key (kbd "C-x C-l") 'goto-line)
(global-set-key (kbd "C-x C-p") 'other-window)
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x |") 'split-window-horizontally)

;; numpad bindings
(global-set-key "\eOp" "0")
(global-set-key "\eOq" "1")
(global-set-key "\eOr" "2")
(global-set-key "\eOs" "3")
(global-set-key "\eOt" "4")
(global-set-key "\eOu" "5")
(global-set-key "\eOv" "6")
(global-set-key "\eOw" "7")
(global-set-key "\eOx" "8")
(global-set-key "\eOy" "9")
(global-set-key "\eOl" "+")
(global-set-key "\eOQ" "/")
(global-set-key "\eOR" "*")
(global-set-key "\eOS" "-")
(global-set-key "\eOn" ".")

;; buffer switching commands - defined in prev-next-buffer.el
(global-set-key (kbd "C-x <left>") 'previous-buffer)
(global-set-key (kbd "C-x <right>") 'next-buffer)




