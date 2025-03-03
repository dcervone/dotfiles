(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ))

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown+r-mode))
(setq polymode-exporter-output-file-format "%s")

(add-to-list 'load-path "~/.emacs.d/lisp/")

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
    (tramp-login-program        "gcloud compute ssh --ssh-flag='-X' --ssh-flag='-ServerAliveInterval=100' --zone 'us-central1-a'")
    (tramp-login-args           (("%h")))
    (tramp-async-args           (("-q")))
    (tramp-remote-shell         ("/bin/bash"))
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

(eval-when-compile (require 'cl-lib))

(require 'tramp)
(require 'tramp-cache)

(defgroup kubernetes-tramp nil
  "TRAMP integration for Docker containers deployed in a kubernetes cluster."
  :prefix "kubernetes-tramp-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/gruggiero/kubernetes-tramp")
  :link '(emacs-commentary-link :tag "Commentary" "kubernetes-tramp"))

(defcustom kubernetes-tramp-kubectl-executable "kubectl"
  "Path to kubectl executable."
  :type 'string
  :group 'kubernetes-tramp)

;;;###autoload
(defcustom kubernetes-tramp-kubectl-options nil
  "List of kubectl options."
  :type '(repeat string)
  :group 'kubernetes-tramp)

(defcustom kubernetes-tramp-use-names nil
  "Whether use names instead of id."
  :type 'boolean
  :group 'kubernetes-tramp)

(defcustom tramp-remote-shell-executable "bash"
  "Default shell executable"
  :type 'string
  :group 'kubernetes-tramp)

;;;###autoload
(defconst kubernetes-tramp-completion-function-alist
  '((kubernetes-tramp--parse-running-containers  ""))
  "Default list of (FUNCTION FILE) pairs to be examined for kubectl method.")

;;;###autoload
(defconst kubernetes-tramp-method "kubectl"
  "Method to connect docker containers.")

(defun kubernetes-tramp--running-containers ()
  "Collect kubernetes running containers.

Return a list of containers names"
  (cl-loop for line in (cdr (apply #'process-lines kubernetes-tramp-kubectl-executable (list "get" "po" )))
           for info = (split-string line "[[:space:]]+" t)
           collect (car info)))

(defun kubernetes-tramp--parse-running-containers (&optional ignored)
  "Return a list of (user host) tuples.

TRAMP calls this function with a filename which is IGNORED.  The
user is an empty string because the kubectl TRAMP method uses bash
to connect to the default user containers."
  (cl-loop for name in (kubernetes-tramp--running-containers)
           collect (list ""  name)))

;;;###autoload
(defun kubernetes-tramp-cleanup ()
  "Cleanup TRAMP cache for kubernetes method."
  (interactive)
  (let ((containers (apply 'append (kubernetes-tramp--running-containers))))
    (maphash (lambda (key _)
               (and (vectorp key)
                    (string-equal kubernetes-tramp-method (tramp-file-name-method key))
                    (not (member (tramp-file-name-host key) containers))
                    (remhash key tramp-cache-data)))
             tramp-cache-data))
  (setq tramp-cache-data-changed t)
  (if (zerop (hash-table-count tramp-cache-data))
      (ignore-errors (delete-file tramp-persistency-file-name))
    (tramp-dump-connection-properties)))

;;;###autoload
(defun kubernetes-tramp-add-method ()
  "Add kubectl tramp method."
  (add-to-list 'tramp-methods
               `(,kubernetes-tramp-method
                 (tramp-login-program      ,kubernetes-tramp-kubectl-executable)
                 (tramp-login-args         (,kubernetes-tramp-kubectl-options ("exec" "-it") ("-u" "%u") ("%h") ("sh")))
                 (tramp-remote-shell       ,tramp-remote-shell-executable)
                 (tramp-remote-shell-args  ("-i" "-c")))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (kubernetes-tramp-add-method)
     (tramp-set-completion-function kubernetes-tramp-method kubernetes-tramp-completion-function-alist)))

(provide 'kubernetes-tramp)

(push
 (cons
  "kubectl"
  '((tramp-login-program "kubectl")
    (tramp-login-args (("exec" "-it") ("%h") ("-c" "model") ("--") ("/bin/bash")))
    (tramp-remote-shell "/bin/bash")
    (tramp-remote-shell-args ("-i" "-c"))))
 tramp-methods)

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
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("ac2245d2bab0100a4dc0ff79ac03b88afc0960eedf7585e151f829ab5d36a411" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "45e76a1b1e3bd74adb03192bf8d6eea2e469a1cf6f60088b99d57f1374d77a04" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "e964832f274625fa45810c688bdbe18caa75a5e1c36b0ca5ab88924756e5667f" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" default))
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
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
     (ess-R-fl-keyword:%op% . t)))
 '(flycheck-lintr-linters
   "with_defaults(line_length_linter(120), object_name_linter = NULL)")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2B34" "#FAC863"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2B34" "#99C794"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2B34" "#A7ADBA"))
 '(objed-cursor-color "#EC5f67")
 '(package-selected-packages
   '(kubernetes ir-black-theme color-theme-sanityinc-tomorrow sql-indent multiple-cursors poly-R yaml-mode doom-themes use-package stan-snippets flycheck-stan eldoc-stan company-stan stan-mode flymd pyenv-mode markdown-preview-eww markdown-preview-mode ox-reveal org magit markdown-mode frame-cmds ein request websocket elpy anaconda-mode ess key-chord goto-chg flycheck auto-complete adaptive-wrap))
 '(pdf-view-midnight-colors (cons "#D8DEE9" "#1B2B34"))
 '(rustic-ansi-faces
   ["#1B2B34" "#EC5f67" "#99C794" "#FAC863" "#6699CC" "#E27E8D" "#5FB3B3" "#D8DEE9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

