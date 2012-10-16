;; My init.el
;; Most of the code here was scavenged from the Emacs Starter Kit (thanks Phil!), and modified to suit me.

;; The single most important thing in any .emacs file:
(if (fboundp 'set-message-beep)
  (set-message-beep 'silent))

;; GUI stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style 0)
(global-hl-line-mode)

;; The default frame title in emacs is not very useful
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))

;; Change where custom settings are stored (I don't want emacs touching my init.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(let ((is-mac (eq system-type 'darwin)))
  (custom-set-faces
   (if is-mac
       '(default ((t (:inherit nil :stipple nil :background "#0C1021" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 139 :width normal :foundry "unknown" :family "Monaco"))))
       '(default ((t (:inherit nil :stipple nil :background "#0C1021" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
       )))

;; I finally figured out why opening and saving files was sooooooo
;; slow!!!
(setq vc-handled-backends nil)

;; Use the rectangle selection from cua mode, but not the rest of cua.
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; http://stackoverflow.com/questions/9435019/how-do-i-source-my-zshrc-within-emacs
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))


;; Set up package archives
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

;; Function to install a list of packages
(defun ensure-packages-are-installed (packages)
  "Install all the given packages if they aren't installed already."
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))


;; List of packages that like to have
(defvar my-packages
 '(
   idle-highlight-mode
   windsize

   ruby-mode
   css-mode
   yaml-mode
   sass-mode
   scss-mode
   haml-mode
   inf-ruby
   less-css-mode
   ;rinari
   coffee-mode

   clojure-mode
   paredit
   nrepl

   auto-complete
   ac-nrepl

   scala-mode
   
   find-file-in-project
   smex

   color-theme
   color-theme-solarized
   color-theme-twilight
   zenburn

   magit
   ))

;; Install packages
(ensure-packages-are-installed my-packages)


;; Directories for stuff that's not in elpa or marmalade (eg: blackboard):
(add-to-list 'load-path (concat user-emacs-directory "extras"))

;; Autoloads don't seem to work for some stuff
(autoload 'color-theme-twilight "color-theme-twilight" nil t)
(autoload 'color-theme-blackboard "blackboard" nil t)

;; Default to my favorite color theme
(color-theme-zenburn)

;; uniquify - makes sure buffer names are unique in a sensible way.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; recentf - Keeps track of recently closed files
(require 'recentf)
(recentf-mode 1)

;; Find-File-At-Point - very useful - makes C-x C-f start with a good guess at what you want.
(require 'ffap)

;; Always show the column number along with the line number.
(column-number-mode t)

;; saveplace - Remember place in files
(require 'saveplace)
(setq save-place t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Never insert tabs. Tabs are the tool of the devil.
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)

(set-default 'truncate-lines t)

;; Configure various thingies.
(setq inhibit-startup-message t
      shift-select-mode nil
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      )


;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


;; DOS mode for batch files
(autoload 'dos-mode "dos" nil t)
(add-to-list 'auto-mode-alist '(".bat" . dos-mode))

;; Groovy mode
(autoload 'groovy-mode "groovy-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))
; groovy mode does not inherit from prog-mode
(add-hook 'groovy-mode-hook 'turn-on-idle-hilight)

;; Log4J mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
;(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))


;; Text mode
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Ruby
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Emacs is awsome, except when it comes to indenting code. Sigh.
(defun tabs-mode ()
  (interactive)
  (setq indent-tabs-mode t
        tab-with 4
        js-indent-level 4
        c-indent-level 4))


;; Hippie expand: I don't really know how hippie-expand works, but I have always
;; been pretty happy with the completion when using ESK, so I have just copied
;; from there for now.
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)


;; Grep does not ignore class files by default
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; ibuffer mode sets C-x C-f to something less cool than ido
(eval-after-load 'ibuffer
  '(define-key ibuffer-mode-map (kbd "C-x C-f") nil))

;; Cosmetics (copied from ESK)
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))


;; Change paredit bindings - I can't get used to some of the built-in ones.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
     ))


(defun dired-insert-subdir-recursive ()
  (interactive)
  (dired-insert-subdir (dired-get-filename) (concat dired-listing-switches "R")))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c r") 'dired-insert-subdir-recursive)
     ))


;; Stuff from ESK, for cleaning up buffers:
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


;; This looks really useful! I didn't know about it until today.
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Open a recent file
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; For when I open a read-only file, then want to edit it.
(defun make-writable ()
  (interactive)
  (toggle-read-only -1)               ; make it writable
  (chmod (buffer-file-name) #o666))   ; I really just want "+w", but I don't know how to do that


;; Sometimes I want the left buffer on the right and v/v.
(defun swap-buffers ()
  (interactive)
  (let* ((this-buffer (window-buffer))
         (other-window (next-window))
         (other-buffer (window-buffer other-window)))
    (show-buffer other-window this-buffer)
    (show-buffer (other-window 0) other-buffer)))


;; For when I visit a file, and it opens in the wrong window.
(defun move-buffer-to-other-window ()
  (interactive)
  (let ((m (point-marker)))
    (bury-buffer)
    (other-window 1)
    (switch-to-buffer (marker-buffer m))))

(global-set-key (kbd "C-S-o") 'move-buffer-to-other-window)


(defun dired-r ()
  (interactive)
  (let ((old-value dired-listing-switches))
    (set-variable 'dired-listing-switches "-lR")
    (ido-dired)
    (set-variable 'dired-listing-switches old-value)))


;; Maxmize on startup. TODO - make this work on linux too
(defun maximize ()
  (interactive)
  (w32-send-sys-command 61488))         ; no idea why this works, but
                                        ; it does.

;; Shenanigans to make "maximize-on-startup" actually work:
(add-hook 'emacs-startup-hook
          '(lambda ()
             (run-at-time "1 sec" nil 'maximize)))

;; Server-start. Not sure if this need to be in a hook, but it works well enough.
(add-hook 'emacs-startup-hook
          'server-start)


(defun turn-on-idle-hilight ()
  (idle-highlight-mode t))

;; Set up buffers for prog-mode. This hook should run when opening any
;; file that contains code (i.e. not plain text)
(defun my-prog-mode-hook ()
  ;; Idle highlight mode - can't live without it.
  (turn-on-idle-hilight)

  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)


(defun turn-on-paredit ()
  (paredit-mode t))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'clojure-mode-hook    'turn-on-paredit)

(add-hook 'ruby-mode-hook       'esk-paredit-nonlisp)
(add-hook 'espresso-mode-hook   'esk-paredit-nonlisp)
(add-hook 'js-mode-hook         'esk-paredit-nonlisp)


;; Rails
(when (file-exists-p "~/src/rinari")
  (add-to-list 'load-path "~/src/rinari")
  (require 'rinari))


(defun hide-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;; Key bindings

;; Misc:
(global-set-key (kbd "C-S-w")   'make-writable)
(global-set-key (kbd "C-S-k")   'kill-this-buffer)
(global-set-key (kbd "C-S-f")   'find-grep)
(global-set-key (kbd "C-x M-d") 'dired-r)
(global-set-key (kbd "<f8>")    'toggle-truncate-lines)

;; Window management - windmove to switch, windsize to resize
(require 'windsize)
(windmove-default-keybindings) ;; Shift+direction
(windsize-default-keybindings) ;; C-S+direction

;; Other keybindings, mostly taken from ESK (becuase that's what I got used to in my first year of emacs):

;; smex makes M-x better
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Turn paredit on and off easily
(global-set-key (kbd "<f9>") 'paredit-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "C-c q") 'join-line)


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
