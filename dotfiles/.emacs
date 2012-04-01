;; Color Themes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-billw)))

;; evil
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433"
                        "~/.emacs.d/evil.git-5b6e5433/libs")
(require 'evil)
(evil-mode 1)

;; TwitteringMode
(add-to-list 'load-path "~/.emacs.d/twittering-mode.git-535741f1")
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; Haskell mode
(load "~/.emacs.d/haskell-mode.git-7db6fabc/haskell-site-file")
; Only one of the following indentation methods may be used
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
