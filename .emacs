(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq inhibit-startup-screen t)

; disable toolbar
(tool-bar-mode -1)

; default window split is horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Make sure a package is installed
(defun package-require (package)
  "Install a PACKAGE unless it is already installed 
or a feature with the same name is already active.

Usage: (package-require 'package)"
  ; try to activate the package with at least version 0.
  (package-activate package '(0))
  ; try to just require the package. Maybe the user has it in his local config

  (condition-case nil
      (require package)
    ; if we cannot require it, it does not exist, yet. So install it.
    (error (progn
             (package-install package)
             (require package)))))

;; Flycheck: On the fly syntax checking
(package-require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
; stronger error display
(defface flycheck-error
  '((t (:foreground "red" :underline (:color "Red1" :style wave) :weight bold)))
  "Flycheck face for errors"
  :group "flycheck")

;; visual line mode globally
(require 'adaptive-wrap)
(with-eval-after-load 'adaptive-wrap
  (setq-default adaptive-wrap-extra-indent 2))
(add-hook 'visual-line-mode-hook
          (lambda () (adaptive-wrap-prefix-mode t)))
(global-visual-line-mode t)

;; ESS auto-complete
(ac-config-default)
(setq ess-use-auto-complete 1)
(setq ac-sources '(ac-source-R ac-source-R-args ac-source-R-objects))
(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)

(allout-mode)

; go to the last change
(package-require 'goto-chg)
(global-set-key [(control .)] 'goto-last-change)
; M-. can conflict with etags tag search. But C-. can get overwritten
; by flyspell-auto-correct-word. And goto-last-change needs a really
; fast key.
(global-set-key [(meta .)] 'goto-last-change)
; ensure that even in worst case some goto-last-change is available
(global-set-key [(control meta .)] 'goto-last-change)

; use key chords invoke commands
(package-require 'key-chord)
(key-chord-mode 1)

; buffer actions
(key-chord-define-global "vb"     'eval-buffer)
(key-chord-define-global "cy"     'yank-pop)

; frame actions
(key-chord-define-global "xo"     'other-window);


(defun kill-this-buffer-if-not-modified ()
  (interactive)
  ; taken from menu-bar.el
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))
(key-chord-define-global "xk"     'kill-this-buffer-if-not-modified)

; save backups in other directory
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

; save the place in files
(require 'saveplace)
(setq-default save-place t)

; save minibuffer history
(require 'savehist)
;; increase the default history cutoff
(setq history-length 500)
(savehist-mode t)
(setq savehist-additional-variables
      '(regexp-search-ring
        register-alist))

; ESS "starting evaluation" bug fix
(setq ess-eval-visibly-p 'nowait)

(defun ots-ess-mode-set-faces ()
  "Set faces for editing R files."
  (font-lock-add-keywords
   nil '(("\\<\\(args\\|browser\\|function\\|gc\\|invisible\\|library\\|messagef?0?\\|options\\|print\\|require\\|rm\\|stop\\|stopif\\|stopifnot\\|system\\|try\\|tryCatch\\|UseMethod\\|warning\\|with\\)("
          1 font-lock-keyword-face)
         ("\\<\\(Recall\\|return\\)("
          1 font-lock-function-name-face)
         ("^ *\\([a-zA-Z0-9._]+\\) *\\(=\\|<<?-\\) *\\(function\\|local\\)\\>"
          1 font-lock-function-name-face)
         ("\\(\\.\\.\\.\\)"
          1 font-lock-keyword-face)
         ("[^= ]\\(=\\)[^= ]"
          1 font-lock-keyword-face))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
