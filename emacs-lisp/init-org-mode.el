

(global-set-key (kbd "C-c a") 'org-agenda)

(add-hook 'org-mode-hook (lambda ()
                           (local-set-key (kbd "C-c t")       'org-insert-todo-heading)
                           (local-set-key (kbd "C-v <up>")    'org-metaup)
                           (local-set-key (kbd "C-v <down>")  'org-metadown)
                           (local-set-key (kbd "C-v <left>")  'org-metaleft)
                           (local-set-key (kbd "C-v <right>") 'org-metaright)
                           (local-set-key (kbd "M-<up>")      'outline-previous-visible-heading)
                           (local-set-key (kbd "M-<down>")    'outline-next-visible-heading)
                           (local-set-key (kbd "ESC <left>")  'backward-word)
                           (local-set-key (kbd "ESC <right>") 'forward-word)
))
