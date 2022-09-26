(setq inhibit-startup-message t
      visible-bell t
      make-backup-files nil
      auto-save-default nil
			indent-tabs-mode nil
      )

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode 1)

;; Disable line numbers for some nodes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
    treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; General
(setq-default tab-width 2)
(hl-line-mode t)
(blink-cursor-mode 0)
(load-theme 'doom-moonlight t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)
(toggle-truncate-lines nil)

;; KeyBindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Font
(set-face-attribute 'default nil :height 133 :font "JetBrainsMono Nerd Font")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(package-selected-packages
   '(typescript-mode lsp-treemacs lsp-ui company-box company lsp-mode yasnippet-snippets yasnippet evil-collection general doom-themes helpful wich-key rainbow-delimiters all-the-icons-ivy smalltalk-mode all-the-icons-install-fonts all-the-icons doom-modeline ivy command-log-mode use-package atom-one-dark-theme)))
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
  :init (load-theme 'doom-palenight t))

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
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "n" '(:ignore n :which-key "treemacs")
    "nt" '(treemacs :which-key "toggle treemacs")
    "ns" '(lsp-treemacs-symbols :which-key "toggle treemacs symbols")
    "qq" '(save-buffers-kill-emacs :which-key "close emacs")
    "x" '(counsel-M-x :which-key "open command line")
    "s" '(save-buffer :which-key "save current buffer")
    "f"  '(:ignore f :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fs" '(swiper :which-key "search")
    "vs" '(split-window-right :which-key "vertical split")
    "b"  '(:ignore c :which-key "buffer")
    "bc" '(kill-current-buffer :which-key "close buffer")
    "bs" '(counsel-switch-buffer :which-key "switch buffer")
    "w"  '(:ignore w :which-key "window")
    "wc" '(delete-window :which-key "close window")
    "wv"  '(split-window-below :which-key "vertical split")
    "wh" '(split-window-right :which-key "horizontal split")
    "C-l" '(evil-window-right :which-key "move to window right")
    "C-k" '(evil-window-up :which-key "move to window up")
    "C-j" '(evil-window-bottom :which-key "move to window down")
    "C-h" '(evil-window-left :which-key "move to window left")))

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

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package yasnippet
	:config
	(setq yas-snippet-dirs '("~/.config/emacs/elpa/yasnippet-snippets-20220713.1234/snippets"))
	:init
	(yas-global-mode 1))

(use-package yasnippet-snippets)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
	(lsp-mode . efs/lsp-mode-setup)
	(html-mode . lsp-deferred)
	(css-mode . lsp-deferred)
	(javascript-mode . lsp-deferred)
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
