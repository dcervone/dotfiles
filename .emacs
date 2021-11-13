(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ))

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown+r-mode))
(setq polymode-exporter-output-file-format "%s")

(package-initialize)
(setq inhibit-startup-screen t)
(elpy-enable)

;; (elpy-use-ipython)
(setq py-shell-name "python3")
(setq python-shell-interpreter "/usr/local/bin/python3.6")
(setq elpy-rpc-python-command "/usr/local/bin/python3.6")
      ;; python-shell-interpreter-args "--simple-prompt")

;; reveal.js location
(setq org-reveal-root "file:///Users/dcervone/utils/reveal.js")

; disable toolbar
(tool-bar-mode -1)

;; override "_" to "<-" in ESS
(require 'ess-site)
(ess-toggle-underscore nil)
(setq ess-default-style 'RStudio)

;; gcloud ssh
(setq tramp-default-method "ssh")

(require 'tramp)

(add-to-list 'tramp-methods
  '("gssh"
    (tramp-login-program        "gcloud compute ssh --ssh-flag='-X' --ssh-flag='-ServerAliveInterval=100' --zone 'us-east1-b'")
    (tramp-login-args           (("%h")))
    (tramp-async-args           (("-q")))
    (tramp-remote-shell         "/bin/bash")
    (tramp-remote-shell-args    ("-c"))
    (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                 ("-o" "UserKnownHostsFile=/dev/null")
                                 ("-o" "StrictHostKeyChecking=no")))
    (tramp-default-port         22)))

(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))


;; dockerfile mode
(add-to-list 'load-path "/Users/dcervone/utils/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
(setq dockerfile-mode-command "docker")

;; default meta key on mac is COMMAND
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

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

;; window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

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

;; comment keyword highligting
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(defun font-lock-comment-annotations-ess ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'ess-mode-hook 'font-lock-comment-annotations-ess)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" default)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t))))
 '(flycheck-lintr-linters
   "with_defaults(line_length_linter(120), object_name_linter = NULL)")
 '(package-selected-packages
   (quote
    (sql-indent multiple-cursors poly-R yaml-mode doom-themes use-package stan-snippets flycheck-stan eldoc-stan company-stan stan-mode flymd pyenv-mode markdown-preview-eww markdown-preview-mode ox-reveal org magit markdown-mode frame-cmds ein request websocket elpy anaconda-mode ess key-chord goto-chg flycheck auto-complete adaptive-wrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

