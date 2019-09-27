;;; .emacs -- my .emacs file

;;; Commentary:
;; my init file for Emacs
;; Bryson Tanner -- bltanner105@gmail.com

;;; Code:
;; Load the package manager and necessary repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(require 'req-package)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default tab-width 4 indent-tabs-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (black-space)))
 '(custom-safe-themes
   (quote
    ("0dc9276443b11bead7db8dcdc9f18f9942e1a76f98e75f01962c568cda23f9e4" "95c10e614e5771a97865be498452c281606ab3cc948361b4d5b3c2fd85045440" "9700f321820a8214dd416082d99920ee65547dc9e105a13b8c1129509e2b03ad" "ecf0b07a6fc93e7457cc774fcb8f492ff7e1ee27fd5ec36df302c6f17640f320" "2b805a1d8891ba874b1c13d15438d8c221464dc6a53f1ced404492e88815e61f" "58a7093cb01b18092594e22cde0697c124378f6f4f7d8bd0f335daae8cca847c" "e51b64b3ee40a255d533943383d8b95e8860404dc3fee4a12d85ad6df88520e8" "21028363704c73962f891ffaaf5fbeff6e15f3ff14d2cdc91717d1f35520fd75" "1fc67b18dc7c2611168b0d934fa8ca52139260f135d00c41c15839c5ff03956e" "e0f2ab62a01d1e0ac3a6c0a88f86ab0f2e566e6c1de1690948d03fc926ffc018" "d01f39317b2c37ec69aebed69cebf7188233c0c8b637a2340526c43928ff9103" "96181346449fb761db91cdce0ef2ee0d18f7548e8f45837a34a08e788d566e4e" "6c2f5ea7513cb10481f3e7a1a1f85cbf198a16c66d54d65f214b0e7258298e88" "e251fa7d4089e7ec8011edaea7092c44157fce2721bfc39b4be0ad58d3d5bd77" "30367627abb6973bb45355639fe1d97f4b6e36598542e5258ca5f614531737f1" "03d149f1486c23cd49ee3a9035192fc391facaea70d7f42c2e6adf578996c0e8" "caf9e3e3ffa34a132bf1372ac48ae5ff3a94f9da171f87469d6899b5cb704cd0" "efeb0783a4cfb349d9f3f0ee53ead36be1d1febb642e7191a37511a2bbe51a37" "5b8a79dffd95c493aa0e67408e60fb6dc6dabc18d2735c59238202835a78129d" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "7803ff416cf090613afd3b4c3de362e64063603522d4974bcae8cfa53cf1fd1b" default)))
 '(ecb-options-version "2.50")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (flycheck-rtags company-rtags rtags cmake-font-lock cmake-mode fic-ext-mode restart-emacs powerline company-jedi jedi spacemacs-theme helm helm-rtags irony-eldoc flycheck-irony company-irony irony flycheck company cyberpunk-2019-theme cyberpunk-theme ansi package-build epl git commander f dash s)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active0 ((t (:inherit mode-line :background "blue1" :foreground "SystemWindow"))))
 '(powerline-active1 ((t (:inherit mode-line :background "medium violet red" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "black" :foreground "white")))))

;; disable backups
(setq backup-inhibited t)

;; customizations
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; show startup screen only if not opening another file
(defun splash-screen-conditional()
  "Open startup screen instead of *scratch* buffer."
  (when (and (string-equal "*scratch*" (buffer-name)) (not (buffer-file-name)))
	(display-splash-screen))
  )
(add-hook 'after-init-hook 'splash-screen-conditional)

;; enable ido
(require 'ido)
(ido-mode t)

;; enable powerline
(require 'powerline)
(powerline-default-theme)

;;; Config for all Programming
(add-hook 'prog-mode-hook 'show-paren-mode)
;; FIC mode to find TODO, FIXME, BUG, and KLUDGE
(req-package fic-mode :require fic-mode :config
  (progn
	(add-hook 'prog-mode-hook 'fic-ext-mode)))
;; setup helm
(req-package helm :config
  (progn
	(require 'helm-config)
	
	(global-set-key (kbd "C-c-h") 'helm-command-prefix)
	(global-unset-key (kbd "C-x c"))
	
	(setq helm-move-to-line-cycle-in-source t
		  helm-ff-search-library-in-sexp t
		  helm-scroll-amount 8
		  helm-ff-file-name-history-use-recentf t
		  helm-echo-input-in-header-line t)
	
	(global-set-key (kbd "M-x") 'helm-M-x)
	(setq helm-M-x-fuzzy-match t)
	(global-set-key (kbd "C-x C-f") 'helm-find-files)
	(global-set-key (kbd "M-y") 'helm-show-kill-ring)
	(global-set-key (kbd "C-x b") 'helm-mini)
	(setq helm-buffers-fuzzy-matching t
		  helm-recentf-fuzzy-match t
		  helm-semantic-fuzzy-match t
		  helm-imenu-fuzzy-match t)
	(global-set-key (kbd "C-c C-o") 'helm-occur)
	(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
	
	(setq helm-split-window-in-side-p t)
	(setq helm-autoresize-max-height 50)
	(setq helm-autoresize-min-height 30)
	(helm-autoresize-mode 1)
	(helm-mode 1)))

;;; C++ config : company, flycheck, rtags, (irony)
(req-package company :config
  (progn
	(add-hook 'after-init-hook 'global-company-mode)
	(global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
	(setq company-idle-delay 0)))
(req-package flycheck :config
  (progn
	(global-flycheck-mode)))
(req-package rtags :config
  (progn
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

	
    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)

    (setq rtags-use-helm t)

    ;; Shutdown rdm when leaving emacs.
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)))
(req-package helm-rtags :require helm rtags :config
  (progn
    (setq rtags-display-result-backend 'helm)))
(req-package company-rtags :require company rtags :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)))
(req-package flycheck-rtags :require flycheck rtags :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.)
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags))))

;;(req-package irony :config
;;  (progn
;;	(unless (irony--find-server-executable) (call-interactively #'irony-install-server))
;;
;;	(add-hook 'c-mode-hook 'irony-mode)
;;	(add-hook 'c++-mode-hook 'irony-mode)
;;
;;	(setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
;;													irony-cdb-clang-complete))
;;	(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))
;; (req-package company-irony :require company-irony :config
;;   (progn
;; 	(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))
;; (req-package flycheck-irony :require flycheck-irony :config
;;   (progn
;; 	(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))
;; (req-package irony-eldoc :require irony-eldoc :config
;;   (progn
;; 	(add-hook 'irony-mode-hook #'irony-eldoc)))
(add-hook 'c++-mode-hook (lambda() (set-fill-column 100)))
(add-hook 'c-mode-hook (lambda() (set-fill-column 100)))
(add-hook 'c++-mode-hook (lambda() (setq-default tab-width 2 indent-tabs-mode nil)))
(add-hook 'c-mode-hook (lambda() (setq-default tab-width 2 indent-tabs-mode nil)))

;; Cmake config : cmake-mode
(req-package cmake-mode :require cmake-mode)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;;; Python config : company, jedi
(req-package company-jedi :require company-jedi :config
  (progn
	(defun my-python-mode-hook()
	  (add-to-list 'company-backends 'company-jedi))
	(add-hook 'python-mode-hook 'my-python-mode-hook)))
(add-hook 'python-mode-hook (lambda() (set-fill-column 80)))
(add-hook 'python-mode-hook (lambda() (setq-default tab-width 4 indent-tabs-mode nil)))

;; Automatic major modes for certain files
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; custom packages for testing purposes
(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'cladder)

(req-package-finish)
(provide '.emacs)
;;; .emacs ends here
