;; My init.el
;; Most of the code here was scavenged from the Emacs Starter Kit (thanks Phil!), and modified to suit me.


;; Turn off toolbar (I don't use it)
(tool-bar-mode -1)

(setq custom-file "~/.emacs.d/custom.el")


;; Add Marmalade repo.
;; This will cause "package-install" to have access to many, many more packages.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Install all the packages that I use:
(defun ensure-packages-are-installed (packages)
  "Install all the given packages if they aren't installed already."
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(ensure-packages-are-installed
 '(color-theme
   ;color-theme-blackboard ; No package for this!!
   color-theme-railscasts
   color-theme-twilight
   color-theme-zenburn

   idle-highlight-mode
   windsize

   ruby-mode
   ;inf-ruby
   css-mode
   yaml-mode
   sass-mode
   haml-mode

   clojure-mode
   clojurescript-mode
   paredit
   slime

   find-file-in-project
   smex
   ))


(require 'color-theme)
(setq color-theme-is-global t)

(autoload 'color-theme-railscasts "color-theme-railscasts" nil t)
(autoload 'color-theme-twilight "color-theme-twilight" nil t)
(autoload 'color-theme-zenburn "color-theme-zenburn" nil t)


;; Directory for stuff that's not in elpa or marmalade (eg: blackboard):
(add-to-list 'load-path (concat user-emacs-directory "extras"))


;; Default to my favorite color theme
(require 'blackboard)
(color-theme-blackboard)


;; Require a bunch of stuff
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)  ; Keeps track of recently closed files
(require 'ffap)     ; Find-File-At-Point - very useful

(recentf-mode 1)


;; Always show the column number along with the line number.
(column-number-mode t)


;; Remember place in files
(require 'saveplace)
(setq save-place t)


;; Idle highlight mode - can't live without it. TODO - this doesn't work - put it in a hook?
(idle-highlight-mode t)


;; This might need to be in a hook (not sure yet):
(set (make-local-variable 'comment-auto-fill-only-comments) t)
(auto-fill-mode t)


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


;; The default frame title in emacs is not very useful
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))


;; Configure various thingies.
(setq inhibit-startup-message t
      color-theme-is-global t
      shift-select-mode nil
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 120
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      )


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)


;; ido-mode - TODO: find out what the hell "ido" means... and what this mode does.
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


;; Never insert tabs. Tabs are the tool of the devil.
(set-default 'indent-tabs-mode nil)


;; Text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)


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


;; Cosmetics (copied from ESK)
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))


;; Clojure stuff

;; Change paredit bindings - I can't get used to some of the built-in ones.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
     ))


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






;; Key bindings

;; Mine:
(global-set-key (kbd "C-S-w") 'make-writable)
(global-set-key (kbd "C-S-k") 'kill-this-buffer)
(global-set-key (kbd "C-S-f") 'find-grep)

(require 'windsize)
(windsize-default-keybindings)

;; Other keybindings, mostly taken from ESK (becuase that's what I got used to in my first year of emacs):

;; smex makes M-x better
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; 
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

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

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching.
(windmove-default-keybindings) ;; Shift+direction

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "C-c q") 'join-line)


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


