(require 'package)
(add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Language settings
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Common settings
(require 'smartparens-config)
(smartparens-global-mode t)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)
(setq-default show-trailing-whitespace t)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook
          '(lambda ()
             (delete-trailing-whitespace)))
;;              (indent-region (point-min) (point-max)) nil))

;; Keyboard settings
(setq kill-whole-line t)
(setq-default tab-width 2 indent-tabs-mode nil)
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(setq ns-pop-up-frames nil)
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

(defun forward-word+1 ()
  (interactive)
  (forward-word)
  (forward-char))
(define-key global-map (kbd "M-f") 'forward-word+1)
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-x C-e") 'eval-buffer)
(define-key global-map (kbd "C-z") 'undo)

;; Screen settings
(load-theme 'wombat t)
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(use-package linum)
(global-linum-mode)
(setq linum-format "%d ")
(blink-cursor-mode nil)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq scroll-conservatively 1)
(setq scroll-margin 20)
(setq next-screen-context-lines 10)

(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save))
(set-face-attribute 'flycheck-error nil
                    :foreground "red"
                    :background "#671232"
                    :underline nil)

(set-face-attribute 'flycheck-warning nil
                    :foreground "yellow"
                    :background nil
                    :underline nil)

(global-set-key (kbd "C-x n") 'flycheck-next-error)
(global-set-key (kbd "C-x p") 'flycheck-previous-error)

;; Mode settings
(use-package ruby-mode)
(use-package go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode t)))
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(use-package rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(use-package flycheck-rust)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'gfm-mode-hook
          '(lambda ()
             (electric-indent-local-mode nil)))
(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(use-package docker-compose-mode)
(use-package yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode t)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(use-package undo-tree
  :config
  (global-undo-tree-mode))
(use-package dired-x)
(use-package ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(use-package company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
        (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )
    ))

(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous)
(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")

(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; git の root などを取得する関数
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

;; git
(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

;; get current path
(defun get-current-path ()
  buffer-file-name)

;; git grep
(defun git-grep (grep-dir command-args)
  (interactive
   (let ((root (concat (git-root-directory) (get-current-path))))
     (list
      (read-file-name
       "Directory for git grep: " root root t)
      (read-shell-command
       "Run git-grep (like this): "
       (format "PAGER='' git grep -I -n -i -e %s"
               "")
       'git-grep-history))))
  (let ((grep-use-null-device nil)
        (command
         (format (concat
                  "cd %s && "
                  "%s")
                 grep-dir
                 command-args)))
    (grep command)))

;; magit shortcut
(global-set-key (kbd "C-x g") 'magit-status)

;; enable racer (rust_code_complete_plugin)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; quickrun shortcut
(global-set-key (kbd "C-x C-q") 'quickrun)

;; ruby-mode settings
(setq ruby-insert-encoding-magic-comment nil)
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))
(use-package auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(autoload 'robe-ac-setup "robe-ac" "robe auto-complete" nil nil)
(add-hook 'robe-mode-hook 'robe-ac-setup)

;; python-mode settings
(use-package python-mode)
(add-hook 'python-mode-hook (lambda () (auto-complete-mode nil)))
(use-package jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128")))
 '(package-selected-packages
   (quote
    (jedi xclip wgrep web-mode use-package undo-tree tabbar sql-indent slim-mode ruby-electric rubocop robe reverse-theme racer quickrun python-mode py-autopep8 package-utils markdown-mode magit js2-refactor js2-highlight-vars hive go-mode format-all flymake-python-pyflakes flycheck-rust flycheck-pyflakes flycheck-pycheckers elpy dockerfile-mode docker-compose-mode ctags-update counsel-etags color-moccur auto-highlight-symbol all-the-icons-ivy))))
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(use-package py-autopep8)
(define-key python-mode-map (kbd "C-c i") 'py-autopep8)
(define-key python-mode-map (kbd "C-c I") 'py-autopep8-region)

;; if you use GUI
;; (defun my-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen 'fullboth)
;;
;; (defun my-non-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'width 82)
;;   (set-frame-parameter nil 'fullscreen 'fullheight)
;;
;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
;;       (my-non-fullscreen)
;;     (my-fullscreen)))
;;
;; (global-set-key (kbd "<f2>") 'toggle-fullscreen)
