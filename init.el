(setq inhibit-startup-message t
      visible-bell t
      make-backup-files nil
      auto-save-default nil
			indent-tabs-mode nil
			undo-tree-auto-save-history nil
      )

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Line
(column-number-mode)
(display-line-numbers-mode 1)
(setq-default display-line-numbers 'relative)
(setq-default line-spacing 6)

;; Disable line numbers for some nodes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
    eshell-mode-hook
    treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; General
(setq-default tab-width 2
							truncate-lines t)
(hl-line-mode t)
(blink-cursor-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)
(toggle-truncate-lines nil)
(electric-pair-mode t)

;; KeyBindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Font
(set-face-attribute 'default nil :height 106 :font "FuraMono Nerd Font")

;; Languages shift width 
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(package-selected-packages
   '(ivy-posframe visual-fill-column org-bullets darcula-theme timu-rouge-theme ccls ghub treepy magit undo-tree dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt eterm-256color js-mode js-mode-hook evil-multiedit typescript-mode lsp-treemacs lsp-ui company-box company lsp-mode yasnippet-snippets yasnippet evil-collection general doom-themes helpful wich-key rainbow-delimiters all-the-icons-ivy smalltalk-mode all-the-icons-install-fonts all-the-icons doom-modeline ivy command-log-mode use-package atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package command-log-mode)
(command-log-mode t)

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(ivy-mode 1)

(use-package counsel
  :init (counsel-mode 1)
  :config (setq ivy-initial-inputs-alist nil))

(use-package swiper)

(use-package doom-themes
 :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config )

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")

  (rune/leader-keys
    "t" '(eshell :which-key "open terminal")
    "T"  '(:ignore T :which-key "toggles")
    "Tt" '(counsel-load-theme :which-key "choose theme")

    "d" '(:ignore d :which-key "dired")
    "do"  '(dired :which-key "open dired")

    "h"  '(:ignore h :which-key "help")
    "hd" '(:ignore hd :which-key "describe")
    "hdm" '(describe-mode :which-key "describe mode")
    "hdb" '(describe-bindings :which-key "describe bindings")
    "hdv" '(describe-variable :which-key "describe variable")
    "hdf" '(describe-function :which-key "describe function")

    "n" '(:ignore n :which-key "treemacs")
    "nt" '(treemacs :which-key "toggle treemacs")
    "nf" '(treemacs-visit-node-in-most-recently-used-window :which-key "open file")
    "ne" '(treemacs-select-window :which-key "focus treemacs")
    "nu" '(treemacs-refresh :which-key "refresh project")
    "ns" '(lsp-treemacs-symbols :which-key "toggle treemacs symbols")
    "nm" '(treemacs-move-file :which-key "move file/folder")
    "ny" '(treemacs-copy-file :which-key "copy file/folder")
    "nc" '(:ignore nc :which-key "create/add")
    "ncf" '(treemacs-create-file :which-key "create file")
    "ncd" '(treemacs-create-dir :which-key "create dir")
    "nr" '(treemacs-rename-file :which-key "rename file")
    "np" '(:ignore np :which-key "project")
    "npr" '(treemacs-rename-project :which-key "rename project")
    "npc" '(treemacs-add-project-to-workspace :which-key "add project to workspace")
    "npd" '(treemacs-remove-project-from-workspace :which-key "remove project form workspace")
    "nd" '(treemacs-delete-file :which-key "delete file/dir")

    "qq" '(save-buffers-kill-emacs :which-key "close emacs")

    "x" '(counsel-M-x :which-key "open command line")

    "s" '(save-buffer :which-key "save current buffer")

    "l"  '(:ignore l :which-key "lsp")
    "lh"  '(lsp-ui-doc-glance :which-key "toggle lsp ui doc")
    "lf"  '(:ignore lf :which-key "format")
    "lf="  '(lsp-format-buffer :which-key "format all buffer")
    "lfs"  '(lsp-format-region :which-key "format file section")
    "lg"  '(:ignore lg :which-key "peek")
    "lgr"  '(lsp-find-references :which-key "find references")
    "lgd"  '(lsp-find-definition :which-key "find definitions")
    "lgi"  '(lsp-find-implementation :which-key "find implementations")
    "lgt"  '(lsp-find-type-definition :which-key "find type definition")
    "lge"  '(lsp-treemacs-errors-list :which-key "show errors")

    "y"  '(yas-insert-snippet :which-key "insert snippet")

    "f"  '(:ignore f :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fs" '(swiper :which-key "search")

    "b"  '(:ignore c :which-key "buffer")
    "bc" '(kill-current-buffer :which-key "close current buffer")
    "bC" '(kill-buffer :which-key "close buffer")
    "bs" '(counsel-switch-buffer :which-key "switch buffer")

    "w"  '(:ignore w :which-key "window")
    "wc" '(delete-window :which-key "close window")
    "wv"  '(split-window-right :which-key "vertical split")
    "wh" '(split-window-below :which-key "horizontal split")
    "w<" '(evil-window-decrease-width :which-key "decrease width")
    "w>" '(evil-window-increase-width :which-key "increase width")
    "w-" '(evil-window-decrease-height :which-key "decrease height")
    "w=" '(evil-window-increase-height :which-key "increase height")

    "G"  '(magit-status :which-key "magit")

    "g"  '(:ignore g :which-key "goto")
    "gl" '(evil-window-right :which-key "move to window right")
    "gk" '(evil-window-up :which-key "move to window up")
    "gj" '(evil-window-down :which-key "move to window down")
    "gh" '(evil-window-left :which-key "move to window left")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-shift-width 2)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

	(define-key evil-normal-state-map (kbd "M-n") 'evil-multiedit-match-and-next)
	(define-key evil-visual-state-map (kbd "M-n") 'evil-multiedit-match-and-next)
	(define-key evil-normal-state-map (kbd "M-N") 'evil-multiedit-match-and-prev)
	(define-key evil-visual-state-map (kbd "M-N") 'evil-multiedit-match-and-prev)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-multiedit)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package yasnippet
	:config
	(setq yas-snippet-dirs '("~/.config/emacs/elpa/yasnippet-snippets-20220713.1234/snippets"))
	:init
	(yas-global-mode 1)
	:bind
	("C-c y" . yas-insert-snippet))


(use-package yasnippet-snippets)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
	(lsp-mode . efs/lsp-mode-setup)
	(html-mode . lsp-deferred)
	(css-mode . lsp-deferred)
	(js-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
	:mode "\\.ts\\'"
	:hook (typescript-mode . lsp-deferred)
	:config (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
	:init (company-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)
(treemacs-resize-icons 16)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package eterm-256color
  :hook (eshell-mode . eterm-256color-mode))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-h") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "cowsay" "lolcat")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq delete-by-moving-to-trash t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package treepy)
(use-package ghub)
(use-package magit
	:ensure t)

(setq magit-status-buffer-switch-function 'switch-to-buffer)
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ivy-posframe
	:custom-face
  (ivy-posframe-border ((t (:background "#ffffff"))))
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10)
          (internal-border-width . 40))
        ivy-posframe-height-alist '((t . 20)))
  (setq ivy-posframe-width 180)
  (ivy-posframe-mode 1))
