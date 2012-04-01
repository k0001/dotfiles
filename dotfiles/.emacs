;; Utils
(defun range (start stop step)
  (if (> start stop)
      nil
      (cons start (range (+ step start) stop step))))

;; Color Themes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-billw)))


;; Emacs Whitespace
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (range tab-width 160 tab-width))


;; Helm
(add-to-list 'load-path "~/.emacs.d/helm.git-8d65d424")
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)


;; WhiteSpace Mode
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style
      '(face tabs spaces trailing lines lines-tail newline empty tab-mark))


;; evil mode
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433"
                        "~/.emacs.d/evil.git-5b6e5433/libs") 
(require 'evil)
(evil-mode 1)


;; TwitteringMode
(add-to-list 'load-path "~/.emacs.d/twittering-mode.git-535741f1")
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; CC mode
(add-hook 'c-mode-common-hook
          '(lambda () (setq-default indent-tabs-mode t)))
(setq-default c-default-style "linux"
              c-basic-offset tab-width
              c-indent-tabs-mode t
              ;c-indent-level tab-width
              c-argdecl-indent 0
              c-tab-always-indent t
              backward-delete-function nil)

; Haskell mode
(load "~/.emacs.d/haskell-mode.git-7db6fabc/haskell-site-file")
; Only one of the following indentation methods may be used
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
