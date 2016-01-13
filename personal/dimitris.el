; Disable warning about use of cursor keys for navigation
(setq prelude-guru nil)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(prelude-require-package 'solarized-theme)
(load-theme 'solarized-dark t)
(prelude-require-package 'xterm-color)
(require 'xterm-color)
(set-face-attribute 'mode-line () :height 80 :background "#0C1F1F")
(set-face-attribute 'mode-line-inactive () :height 80 :background "#0C1F1F")

(set-face-attribute 'cursor () :background "#0f0")
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq-local face-remapping-alist '((default :height 100)))))
(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist '((default :height 100))))
(with-current-buffer (get-buffer " *Echo Area 1*")
  (setq-local face-remapping-alist '((default :height 100))))


;; Look and Feel
;(global-rainbow-delimiters-mode)
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(electric-indent-mode -1)
(global-prettify-symbols-mode)
(custom-set-variables '(speedbar-show-unknown-files t))
(defun set-prelude-prog-mode-defaults ()
  (turn-off-flyspell)
  (diff-hl-mode -1)
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("function" . 402)
          ("defun" . 402))))
(add-hook 'prelude-prog-mode-hook 'set-prelude-prog-mode-defaults t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)


;; Puppet
(defun puppet-lint-fix ()
  "Run the current buffer's file through 'puppet-lint --fix'."
  (interactive)
  (puppet-run-check-command (concat "puppet-lint --fix " (buffer-file-name)) "*puppet-lint*"))


;; Ansible
(prelude-require-package 'company-ansible)
(prelude-require-package 'ansible)
(prelude-require-package 'ansible-doc)


;; Auto-complete
(prelude-require-package 'auto-complete)
(prelude-require-package 'ac-etags)
(require 'auto-complete)
(global-auto-complete-mode t)


;; Python
(prelude-require-package 'jedi)
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
  (if (equal major-mode 'python-mode) `no-indent' nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(defun set-python-mode-defaults ()
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))
  ;; (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-python-mode-defaults)
(setq flycheck-flake8-maximum-line-length 120)

;; org-mode
;; Install package to allow markdown export from org-mode
(require 'ox-gfm)


(provide 'dimitris)
;;; dimitris.el ends here
