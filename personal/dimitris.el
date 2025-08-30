;; Define packages instead of modifying prelude-modules.el
(require 'prelude-erc)
;; (require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-company)
;;; Programming languages support
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
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

;; ************************** SECTION START **************************
;; 20250827 moved away from helm to ivy/counsel/swipper
;; === Prelude: Ivy/Counsel/Swiper + Projectile (Helm-free setup) ===

;; === Prelude: Ivy/Counsel/Swiper + Projectile (Helm-free, stable) ===
(require 'prelude-ivy)

;; ---------- Projectile persistence: set path first, then load ----------
;; 1) Point to the file you already have (yours is populated):
(setq projectile-known-projects-file
      (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))

;; 2) Turn on Projectile and Counsel-Projectile
(projectile-mode +1)

;; 3) Force-load the list at startup (belt & suspenders)
(add-hook 'after-init-hook #'projectile-load-known-projects)

;; 4) Save on add/remove so it always persists
(advice-add 'projectile-add-known-project    :after (lambda (&rest _) (projectile-save-known-projects)))
(advice-add 'projectile-remove-known-project :after (lambda (&rest _) (projectile-save-known-projects)))

(use-package ivy
  :ensure t :demand t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t :after ivy
  :init (counsel-mode 1)
  :config
  (global-set-key (kbd "M-x")       #'counsel-M-x)
  (global-set-key (kbd "C-x C-f")   #'counsel-find-file)
  (global-set-key (kbd "C-x b")     #'ivy-switch-buffer)
  (global-set-key (kbd "C-h b")     #'counsel-descbinds)
  (global-set-key (kbd "C-c r")     #'counsel-recentf)
  (global-set-key (kbd "C-c k")     #'counsel-rg))   ; needs ripgrep

;; Counsel-Projectile (install + enable after counsel & projectile)
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package swiper
  :ensure t :after ivy
  :bind (("C-s" . swiper) ("C-r" . swiper)))

;; Ivy speed-ups
(when (featurep 'ivy-rich) (ivy-rich-mode -1))
(when (featurep 'all-the-icons-ivy-rich) (all-the-icons-ivy-rich-mode -1))
(setq ivy-format-functions-alist '((t . ivy-format-function-line)))
(setq ivy-height 12
      ivy-wrap nil
      ivy-re-builders-alist '((t . ivy--regex-plus))
      ivy-dynamic-exhibit-delay-ms 50)

;; Projectile (persisted, fast)
(setq projectile-globally-ignored-directories
      '(".git" ".hg" ".svn" "node_modules" "dist" "build" "out" "vendor" ".idea" ".vscode"))
(setq projectile-enable-caching t
      projectile-indexing-method 'alien)
(setq projectile-known-projects-file
      (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))

;; Optional: auto-discover on startup (edit paths, then uncomment)
;; (setq projectile-project-search-path '("~/code" "~/work"))
;; (add-hook 'emacs-startup-hook
;;           (lambda () (projectile-discover-projects-in-search-path)))

;; Use C-c p prefix everywhere
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Counsel-Projectile key habits
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p f") #'counsel-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p p") #'counsel-projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-c p s r") #'counsel-projectile-rg)) ; or s a for ag

;; AG (silver searcher) integration (OS must have `ag`)
(prelude-require-package 'ag)

;; === Treemacs (independent, stable) ===
(use-package treemacs
  :ensure t :defer t
  :config
  (setq treemacs-width 30)
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'none))  ;; later try 'simple; avoid 'deferred for now

(use-package treemacs-projectile
  :ensure t :after (treemacs projectile))

;; Treemacs toggles + usability
(with-eval-after-load 'treemacs
  (global-set-key (kbd "C-x t t") #'treemacs)
  (global-set-key (kbd "C-x t 1") #'treemacs-delete-other-windows)
  (global-set-key (kbd "C-x t C-t") #'treemacs-find-file)
  ;; PageUp / PageDown scrolling in the tree
  (define-key treemacs-mode-map (kbd "<prior>") #'scroll-down-command)
  (define-key treemacs-mode-map (kbd "<next>")  #'scroll-up-command))

(with-eval-after-load 'winum
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

;; ************************* SECTION END: Ivy/Councel/Projectile ***************


;;************  Open Treemacs on the project picked via C-c p p ****************
(defun my/open-project-in-treemacs (project)
  "Show PROJECT in Treemacs (add it if needed), then focus the Treemacs window."
  (let ((default-directory project))
    (require 'treemacs)
    ;; Ensure Treemacs is visible (don't toggle it off if it's already open)
    (when (not (memq (treemacs-current-visibility) '(visible exists)))
      (treemacs))
    ;; Try to add the project to the workspace; ignore overlaps/duplicates
    (condition-case nil
        (treemacs-add-project-to-workspace
         project (file-name-nondirectory (directory-file-name project)))
      (error nil))
    ;; Show only the chosen project's tree if available, then focus it
    (when (fboundp 'treemacs-display-current-project-exclusively)
      (treemacs-display-current-project-exclusively))
    (when (fboundp 'treemacs-select-window)
      (treemacs-select-window))))

(setq counsel-projectile-switch-project-action #'my/open-project-in-treemacs)
;; ********************** SECTION END ********************************


;; Minor mode to help with Indent Guide marks
(prelude-require-package 'indent-guide)

;; (prelude-require-packages '(smart-shift jinja2-mode ido-completing-read+ ido-vertical-mode))
(prelude-require-packages '(smart-shift jinja2-mode))

; Define ControlPath for tramp mode
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=~/.ssh/emacs-ssh-%%h-%%p-%%r "
       "-o Compression=yes "
       "-o ControlMaster=auto "
       "-o ControlPersist=yes "
       "-o ServerAliveInterval=240"))

; Disable warning about use of cursor keys for navigation
(setq prelude-guru nil)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")

(prelude-require-package 'doom-themes)
(load-theme 'doom-one t)
(set-frame-font "JetBrains Mono 10" t t)

;; Look and Feel
;(global-rainbow-delimiters-mode)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
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

;; Ansible
(prelude-require-package 'company-ansible)
(prelude-require-package 'ansible)
(prelude-require-package 'ansible-doc)

;; Auto-complete
(prelude-require-package 'auto-complete)
(prelude-require-package 'ac-etags)
(require 'auto-complete)
(global-auto-complete-mode t)

;; hcl-mode
(prelude-require-packages '(hcl-mode))
(require 'hcl-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

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
;; Autostart indent-guide-mode in Python mode
(add-hook 'python-mode-hook 'indent-guide-mode)
;; disable whitespace mode
;;(add-hook 'python-mode-hook' 'whitespace-turn-off)

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
                                       (format "~/.local/bin/viewmarkdown --browser %s" buffer-file-name)))
    (run-with-timer 10 ; sec
                    nil (lambda ()
                          (when (process-live-p Y/markdown-process)
                            (kill-process Y/markdown-process))))))

;; Use advice to use preview at both markdown-mode and gfm-mode.
(advice-add 'markdown-preview :override 'Y/markdown-preview)

;;(setq markdown-open-command "viewmarkdown --browser")

;; whitespace mode
(setq whitespace-line-column 140)

;; YAML
;; Autostart indent-guide-mode in yaml mode
(add-hook 'yaml-mode-hook 'indent-guide-mode)

;; org-capture for quick notes
(setq org-notes-dir "/home/dl/Dropbox/Notes")
(setq org-default-notes-file (concat org-notes-dir "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
             "* %?\n")
        ("k" "Linux Kernel Notes" entry (file+olp org-default-notes-file "Notes" "Linux Kernel")
             "* %?\n")
        ("j" "Journal" entry (file+olp+datetree org-default-notes-file "Journal")
             "* %?\nEntered on %U\n  %i\n  %a")))
(define-key global-map "\C-cc" 'org-capture)

;; js-mode

(setq js-indent-level 2)

;; go-mode

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)          ;; show tabs as 4 spaces
            (setq indent-tabs-mode t))) ;; still insert tabs (not spaces)

;; show line numbers everywhere
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'python-mode-hook 'display-line-numbers-mode)

;; hide annoying window during compilation
(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

(provide 'dimitris)
;;; dimitris.el ends here
