;;;;;;;;;;;;;
;; ON STARTUP
;;;;;;;;;;;;;

(find-file "/home/mitchell/Documents/Org/")
(set-frame-parameter nil 'alpha-background 98)
(add-to-list 'default-frame-alist '(alpha-background . 98))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM CRAFTERS with slight tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)

(when (display-graphic-p)
  (tool-bar-mode -1)       ; Disable visible scrollbar
  (scroll-bar-mode -1)     ; Disable the toolbar
  (tooltip-mode -1)     ; Disable tooltips
  (set-fringe-mode 10))     ; Give some breathing room
(menu-bar-mode -1)      ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; soft wrap lines
(add-hook 'text-mode-hook 'visual-line-mode)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)

(load-theme 'github-modern t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package swiper :ensure t)

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

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;;;;;;;;;;;;;;;;;;;;;;
;; END SYSTEM CRAFTERS
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; MY STUFF
;;;;;;;;;;;

(keymap-global-set "C-q" 'vterm-send-next-key)
(use-package org :ensure t)
(setq org-agenda-files (quote("/home/mitchell/Org")))

;;;;;;;;;;;;
;; ORG STUFF
;;;;;;;;;;;;

(use-package htmlize :ensure t)
;(add-hook 'htmlize-buffer)

;; Keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


;; does this even work ;;;;;;;;;;;;;;;;;;;; 
;; Org minted sytax highlighting
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;
;; JULIA IDE STUFF
;;;;;;;;;;;;;;;;;;
(use-package vterm :ensure t)
(use-package ess :ensure t)
(use-package julia-mode :ensure t)
(require 'julia-mode)
(setq inferior-julia-program-name "/home/mitchell/.juliaup/bin/julia")
;(use-package julia-vterm :ensure t)
;(use-package ob-julia-vterm :ensure t)

;;;;;;;;;;;;;;;;;;
;; MATLAB IDE STUFF
;;;;;;;;;;;;;;;;;;
;(use-package matlab-mode :ensure t)


;; babel stuff
(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (julia . t)
;   (julia-vterm . t)
   (octave . t)
   (C . t)
   (shell . t)
   )
 )

;; Export helper from here: https://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("45b84ddcc65bdf01d9cc76061a9473e3291d82c9209eac0694fbbb81c57b92fd" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(github-modern-theme kaolin-themes swiper ob-julia-vterm ob-ess-julia nordic-night-theme matlab-mode htmlize doom-modeline command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
