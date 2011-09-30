;; My init.el
;; Most of the code here was scavenged from the Emacs Starter Kit (thanks Phil!), and modified to suit me.


;; Turn off toolbar (I don't use it)
(tool-bar-mode -1)

;; The default frame title in emacs is not very useful
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))

;; Change where custom settings are stored (I don't want emacs touching my init.el)
(setq custom-file "~/.emacs.d/custom.el")


;; Add Marmalade repo.
;; This will cause "package-install" to have access to many, many more packages.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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

;; These are the packages that like to have
;; They get installed at the very end of this file
(defvar my-packages
 '(color-theme
   ;color-theme-blackboard ; No package for this!!
   color-theme-railscasts
   color-theme-twilight
   color-theme-zenburn

   idle-highlight-mode
   windsize

   ruby-mode
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

;; These do not define their own autoloads:
(autoload 'color-theme-railscasts "color-theme-railscasts" nil t)
(autoload 'color-theme-twilight "color-theme-twilight" nil t)
(autoload 'color-theme-zenburn "color-theme-zenburn" nil t)


;; Directories for stuff that's not in elpa or marmalade (eg: blackboard):
(add-to-list 'load-path (concat user-emacs-directory "extras"))

;; Default to my favorite color theme (currently in "extras" - there is no package)
(require 'blackboard)
(color-theme-blackboard)


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


;; Configure various thingies.
(setq inhibit-startup-message t
      shift-select-mode nil
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      )


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


;; Change paredit bindings - I can't get used to some of the built-in ones.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-slurp-sexp)
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


;; Set up buffers for prog-mode. This hook should run when opening any
;; file that contains code (i.e. not plain text)
(defun my-prog-mode-hook ()
  ;; Idle highlight mode - can't live without it.
  (idle-highlight-mode t)

  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)


;; Rails
(when (file-exists-p "~/src/rinari")
  (add-to-list 'load-path "~/src/rinari")
  (require 'rinari))


;; Key bindings

;; Misc:
(global-set-key (kbd "C-S-w") 'make-writable)
(global-set-key (kbd "C-S-k") 'kill-this-buffer)
(global-set-key (kbd "C-S-f") 'find-grep)

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



;; Finally, install all the packages I like to have. I do this at the
;; end so that if anything goes wrong (eg: I add a new package, and
;; it's not found in the archive), then all my other setup has
;; completed.
(ensure-packages-are-installed my-packages)
