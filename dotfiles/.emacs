;; Utils
(defun range (start stop step)
  (if (> start stop)
      nil
      (cons start (range (+ step start) stop step))))

;; Disable menu bar
(menu-bar-mode)


;; Line numbers
(global-linum-mode)
(setq column-number-mode t)

;(defun etags-gen-c (path)
;  "Create etags for .c/.h files"
;  (interact "sSource path:"))

;(defadvice find-tag (before c-tag-file activate)
;  "Automatically create tags file."
;  (let ((tag-file (concat default-directory "TAGS")))
;    (unless (file-exists-p tag-file)
;      (shell-command "find ./ -name *.[ch] -print | xargs etags -a"))
;    (visit-tags-table tag-file)))


;; DuckDuckGO
(defun duckduckgo-search-text (text)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (replace-regexp-in-string " " "+" text))))

(defun duckduckgo-search-input (text)
  (interactive "sDuckDuckGo Search: ")
  (duckduckgo-search-text text))

;(defun apply-on-selection (func)
;  (let (pos1 pos2 bds)
;    (if (region-active-p)
;        (setq pos1 (region-beginning) pos2 (region-end))
;      (progn
;        (setq bds (bounds-of-thing-at-point 'symbol))
;        (setq pos1 (car bds) pos2 (cdr bds))))
;    (func pos1 pos2)))

(global-set-key (kbd "C-x g") 'duckduckgo-search-input)


;; Alignment
(global-set-key (kbd "C-x a r") 'align-regexp)


;; Clipboard
(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;(setq interprogram-cut-function 'x-select-text)

;; Color Themes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-billw)))


;; Emacs Whitespace
(auto-fill-mode t)
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (range tab-width 160 tab-width))

;; WhiteSpace Mode
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style
      '(face tabs trailing lines-tail newline tab-mark))
(setq whitespace-line-column nil) ; if nil, fill-column is used instead

;; Ido mode
(require 'ido)
;; Helm
;(add-to-list 'load-path "~/.emacs.d/helm.git-8d65d424")
;(require 'helm-config)
;(helm-mode 1)
;(global-set-key (kbd "C-x b") 'helm-mini)
;(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;(global-set-key (kbd "C-c C-s") (lambda () ((interact) (eshell default-directory))))




;; evil mode
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433")
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433/lib")
(require 'evil)
; Tune ESC key behaviour so that it plays well with evil mode
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(evil-esc-delay 0)
 '(ido-mode (quote both) nil (ido)))
(evil-mode t)


;; TwitteringMode
(add-to-list 'load-path "~/.emacs.d/twittering-mode.git-535741f1")
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; Doxymac
(add-to-list 'load-path "~/.emacs.d/doxymacs-1.8.0/lisp")
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'doxymacs)
            (doxymacs-mode t)
            (doxymacs-font-lock)))

;; CC mode
;(add-hook 'c-mode-hook
;          (lambda () (setq indent-tabs-mode t)))
(setq-default c-default-style "linux"
              c-basic-offset tab-width
              ;??? exists?? c-indent-tabs-mode t
              ;??? exists?? c-indent-level tab-width
              c-argdecl-indent 0
              c-tab-always-indent t
              backward-delete-function nil)

;; Haskell mode
(load "~/.emacs.d/haskell-mode.git-6d7a3c48/haskell-site-file")

; Indentation: Only one of the following methods may be used
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Haskel Unicode Input Method
;(add-to-list 'load-path "~/.emacs.d/haskell-unicode-input-method.git-d8d16814")
;(require 'haskell-unicode-input-method)
;(add-hook 'haskell-mode-hook
;          (lambda () (set-input-method "haskell-unicode")))



;; Haskell Scion
;; Substitute the desired version for <version>
;(add-to-list 'load-path "~/.emacs.d/scion-0.3/")
;(require 'scion)
;
;;; if ./cabal/bin is not in your $PATH
;(setq scion-program "~/.cabal/bin/scion-server")
;
;(defun my-haskell-hook ()
;    ;; Whenever we open a file in Haskell mode, also activate Scion
;    (scion-mode 1)
;      ;; Whenever a file is saved, immediately type check it and
;      ;; highlight errors/warnings in the source.
;      (scion-flycheck-on-save 1))
;
;(add-hook 'haskell-mode-hook (lambda ()
;    ;; Whenever we open a file in Haskell mode, also activate Scion
;    (scion-mode 1)
;      ;; Whenever a file is saved, immediately type check it and
;      ;; highlight errors/warnings in the source.
;      (scion-flycheck-on-save 1)))
;
;;; WARNING: This causes some versions of Emacs to fail so badly
;;; that Emacs needs to be restarted.
;(setq scion-completing-read-function 'ido-completing-read)


;; Quack (scheme mode)
(load "~/.emacs.d/quack-0.44/quack")
(require 'quack)
