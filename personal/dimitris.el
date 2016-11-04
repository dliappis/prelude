;; Define packages instead of modifying prelude-modules.el
(require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
;; (require 'prelude-helm) ;; Interface for narrowing and search
;; (require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;;; Programming languages support
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
(require 'prelude-haskell)
(require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-scheme)
(require 'prelude-shell)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

(prelude-require-packages '(smart-shift jinja2-mode ido-ubiquitous ido-vertical-mode))

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'flx-ido)
(flx-ido-mode 1)

; Disable warning about use of cursor keys for navigation
(setq prelude-guru nil)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(prelude-require-package 'color-theme-sanityinc-tomorrow)
;; (prelude-require-package 'solarized-theme)
;; (load-theme 'solarized-dark t)
;; (prelude-require-package 'xterm-color)
;; (require 'xterm-color)
;; (set-face-attribute 'mode-line () :height 80 :background "#0C1F1F")
;; (set-face-attribute 'mode-line-inactive () :height 80 :background "#0C1F1F")

;; (set-face-attribute 'cursor () :background "#0f0")
;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (setq-local face-remapping-alist '((default :height 80)))))
;; (with-current-buffer (get-buffer " *Echo Area 0*")
;;   (setq-local face-remapping-alist '((default :height 80))))
;; (with-current-buffer (get-buffer " *Echo Area 1*")
;;   (setq-local face-remapping-alist '((default :height 80))))
;; (set-face-attribute 'default nil :height 90)


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


;; AsciiDoc mode
(prelude-require-package 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))


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
(prelude-require-package 'company-jedi)
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
  (if (equal major-mode 'python-mode) `no-indent' nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(defun set-python-mode-defaults ()
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))
  ;; (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-python-mode-defaults)
(setq flycheck-flake8-maximum-line-length 180)

;; org-mode
;; Install package to allow markdown export from org-mode
(prelude-require-package 'ox-gfm)
(prelude-require-package 'org-present)
(prelude-require-package 'epresent)
(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

;; use grip for markdown preview
(defvar Y/markdown-process nil)
(defun Y/markdown-preview (&rest _)
  "Preview markdown file by using grip."
  (let ((gfm (if (eq major-mode 'gfm-mode) "--gfm" "")))
    (when (process-live-p Y/markdown-process)
      (kill-process Y/markdown-process))
    (setq Y/markdown-process
          (start-process-shell-command "emacs-markdown-preview"
                                       markdown-output-buffer-name
                                       (format "~/bin/viewmarkdown --browser %s" buffer-file-name)))
    (run-with-timer 10 ; sec
                    nil (lambda ()
                          (when (process-live-p Y/markdown-process)
                            (kill-process Y/markdown-process))))))

;; Use advice to use preview at both markdown-mode and gfm-mode.
(advice-add 'markdown-preview :override 'Y/markdown-preview)

;;(setq markdown-open-command "viewmarkdown --browser")

(provide 'dimitris)
;;; dimitris.el ends here
