;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (start stop step)
  (if (> start stop)
      nil
      (cons start (range (+ step start) stop step))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Set Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(evil-want-C-i-jump nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Networking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq socks-override-functions 1)
;(setq socks-noproxy '("no_proxy"))
;(setq socks-server '("local socks" "127.0.0.1" 9999 5))
;(setq url-proxy-services '(("no_proxy" . "local\\.net")
;                           ("http"     . "127.0.0.1:3128"))
;(require 'socks)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs interface details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(auto-fill-mode t)
(global-linum-mode)
(global-hl-line-mode)
(setq column-number-mode t)
(show-paren-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags files support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun etags-gen-c (path)
;  "Create etags for .c/.h files"
;  (interact "sSource path:"))

;(defadvice find-tag (before c-tag-file activate)
;  "Automatically create tags file."
;  (let ((tag-file (concat default-directory "TAGS")))
;    (unless (file-exists-p tag-file)
;      (shell-command "find ./ -name *.[ch] -print | xargs etags -a"))
;    (visit-tags-table tag-file)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DuckDuckGO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duckduckgo-search-text (text)
  (browse-url
   (concat "https://duckduckgo.com/?q="
           (replace-regexp-in-string " " "+" text))))

(defun duckduckgo-search-input (text)
  (interactive "sDuckDuckGo Search: ")
  (duckduckgo-search-text text))

(global-set-key (kbd "C-x g") 'duckduckgo-search-input)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x a r") 'align-regexp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard integration with X and such.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;(setq interprogram-cut-function 'x-select-text)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Color Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized.git-1aba0ed6")
(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rainbow delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters-1.3.4")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard emacs stuff
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (range tab-width 160 tab-width))
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; WhiteSpace Mode stuff
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style
      '(face tabs trailing lines-tail newline tab-mark))
(setq whitespace-line-column nil) ; if nil, fill-column is used instead



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ido mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
; The standard "C-x C-b" don't play well when inside tmux.
(ido-mode 'both)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map "\C-x\C-f"
              'ibuffer-ido-find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433")
(add-to-list 'load-path "~/.emacs.d/evil.git-5b6e5433/lib")
(require 'evil)

(defun evil-undefine ()
   (interactive)
    (let (evil-mode-map-alist)
         (call-interactively (key-binding (this-command-keys)))))
; never map some keys
(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
(define-key evil-normal-state-map (kbd "RET") 'evil-undefine)
; Tune ESC key behaviour so that it responds faster
(setq evil-esc-delay 0)

(evil-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doxymac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/doxymacs-1.8.0/lisp")
(add-hook 'c-mode-common-hook
          (lambda ()
            (require 'doxymacs)
            (doxymacs-mode t)
            (doxymacs-font-lock)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CC mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default c-default-style "linux"
              c-basic-offset tab-width
              c-argdecl-indent 0
              c-tab-always-indent t
              backward-delete-function nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Haskell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/haskell-mode.git-ea8eec6d/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Scion
;(add-to-list 'load-path "~/tmp/scion.git/emacs")
;(require 'scion)
;(setq scion-program "~/tmp/scion.git/dist/build/scion-server/scion-server")
;
;(defun my-haskell-hook ()
;  (scion-mode 1)
;  (scion-flycheck-on-save 1))
;(add-hook 'haskell-mode-hook 'my-haskell-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParEdit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'paredit-mode "paredit"
          "Minor mode for pseudo-structurally editing Lisp code." t)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun my-paredit-mode ()
  (paredit-mode +1)
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (eldoc-add-command 'electrify-return-if-match))

(add-hook 'emacs-lisp-mode-hook       'my-paredit-mode)
(add-hook 'lisp-mode-hook             'my-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'my-paredit-mode)
(add-hook 'scheme-mode-hook           'my-paredit-mode)
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (my-paredit-mode)
            (define-key slime-repl-mode-map
              (read-kbd-macro paredit-backward-delete-key) nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quack (Scheme mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/quack-0.44/quack")
(require 'quack)
(setf quack-fontify-style nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLIME (Common Lisp mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "~/quicklisp/slime-helper.el")
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime-2012-04-14")
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(require 'slime)
(slime-setup)
(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-when-complete-filename-expand t
           slime-truncate-lines nil
           slime-autodoc-use-multiline-p t)
     (slime-setup '(slime-fancy slime-asdf))
     (define-key slime-repl-mode-map (kbd "C-c ;")
       'slime-insert-balanced-comments)
     (define-key slime-repl-mode-map (kbd "C-c M-;")
       'slime-remove-balanced-comments)
     (define-key slime-mode-map (kbd "C-c ;")
       'slime-insert-balanced-comments)
     (define-key slime-mode-map (kbd "C-c M-;")
       'slime-remove-balanced-comments)
     (define-key slime-mode-map (kbd "RET") 'newline-and-indent)
     (define-key slime-mode-map (kbd "C-j") 'newline)
     (define-key slime-mode-map (kbd "M-t") 'transpose-sexps)
     (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
     (define-key slime-mode-map (kbd "M-b") 'backward-sexp)
     (define-key slime-mode-map (kbd "C-M-b") 'backward-char)
     (define-key slime-mode-map (kbd "M-f") 'forward-sexp)
     (define-key slime-mode-map (kbd "C-M-f") 'forward-char)))

(add-hook 'lisp-mode-hook (lambda ()
                            (cond ((not (featurep 'slime))
                                   (require 'slime)
                                   (normal-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TwitteringMode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/twittering-mode.git-535741f1")
(require 'twittering-mode)
(setq twittering-use-master-password t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eproject
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/eproject.git-fead080e")
(require 'eproject)

(define-project-type canopus (generic)
  (look-for ".canopus.keep"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OfflineIMAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/offlineimap-el.git-c0d6df70")
;(require 'offlineimap)
;(add-hook 'gnus-before-startup-hook 'offlineimap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GForth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-gforth-el "~/.emacs.d/gforth-0.7.0/gforth.el")
(cond ((file-readable-p my-gforth-el)
       (load-library my-gforth-el)
       (autoload 'forth-mode my-gforth-el)
       (setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode) auto-mode-alist))
       (autoload 'forth-block-mode my-gforth-el)
       (setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode) auto-mode-alist))
       (add-hook 'forth-mode-hook (function (lambda ()
                                              (setq forth-program-name "/usr/bin/gforth")
                                              (setq forth-indent-level 4)
                                              (setq forth-minor-indent-level 2)
                                              (setq forth-hilight-level 3))))))
