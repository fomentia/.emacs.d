(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(require 'use-package)

(add-to-list 'load-path "/Users/isaac/.emacs.d/packages/")
(require 'go-autocomplete)
(require 'zen-mode)

(global-git-gutter+-mode +1)
(global-auto-revert-mode +1)

(setq tide-format-options '(:indentSize 2 :tabSize 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(require 'flycheck)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'js2-mode-hook #'setup-tide-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key (kbd "C-c g s") 'magit-status)

(global-set-key (kbd "C-c t r r") 'tide-refactor)
(global-set-key (kbd "C-c t r n s") 'tide-rename-symbol)
(global-set-key (kbd "C-c t r n f") 'tide-rename-file)
(global-set-key (kbd "C-c t ?") 'tide-references)
(global-set-key (kbd "C-c t f") 'tide-fix)

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-d" . delete-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)
 ("C-x C-r" . sp-rewrap-sexp))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; TODO
(global-set-key (kbd "ESC <up>") 'move-line-up)

(defun emacs-config()
  (interactive)
  (find-file user-init-file))

(setq kill-whole-line t)

(defun newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'newline-below)

(defun newline-above()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-M-o") 'newline-above)

;; TODO
(defun find-and-replace()
  (interactive)
  (isearch-forward)
  (kill-region)
  (query-replace))

(defun kill-line-backwards()
  (interactive)
  (kill-line 0))

(defun elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

(require 'smartparens)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add elixir-do-end-close-action)
                 :actions '(insert)))

(sp-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
(sp-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))

(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

(require 'sticky-windows)
(global-set-key [(control x) (?0)] 'sticky-window-delete-window)
(global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)

(require 'auto-complete-config)
(ac-config-default)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(define-key ac-completing-map [return] nil)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))
(global-set-key (kbd "<f8>") 'neotree-project-dir)

(defun quiver ()
  "Open Quiver directory."
  (interactive)
  (find-file "/Users/isaac/Dropbox/Documents/Quiver/"))

(defun quiver-quick-note ()
  "Create an untitled file in the Quiver root directory."
  (interactive)
  (find-file (concat "/Users/isaac/Dropbox/Documents/Quiver/untitled" (format-time-string "%Y%m%d%H%M%S") ".md")))

(delete-selection-mode 1)

(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(setq helm-mode-fuzzy-match t)

(define-globalized-minor-mode global-linum-mode linum-mode
  (lambda () (linum-mode 1)))
(global-linum-mode 1)

(setq linum-format "%3s \u2502 ")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (toggle-menu-bar-mode-from-frame -1))

(when (eq system-type 'windows-nt)
  (add-to-list 'load-path "c:/Users/Isaac/.emacs.d/packages/wc-mode")
  (require 'wc-mode)
  (global-set-key "\C-cw" 'wc-mode))

(windmove-default-keybindings)

(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c u") 'windmove-up)

(global-set-key (kbd "C-c d w") 'define-word-at-point)

(global-set-key (kbd "C-c a g p") 'ag-project)
(global-set-key (kbd "C-c a g g") 'helm-ag)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c C-j") 'robe-jump))

(eval-after-load 'go-mode
  (lambda ()
    (define-key go-mode-map (kbd "C-t .") #'go-test-current-test)
    (define-key go-mode-map (kbd "C-t f") #'go-test-current-file)
    (define-key go-mode-map (kbd "C-c d .") #'godoc-at-point)))

(setq org-hide-emphasis-markers t)
(setq org-todo-keywords
      '((sequence "TODO(!)" "BLOCK(!)" "DONE(d!)")))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;; scroll window up/down by one line
;;(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
;;(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-n") (kbd "C-n C-l"))
(global-set-key (kbd "M-p") (kbd "C-p C-l"))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" default)))
 '(fill-column 80)
 '(js-indent-level 2)
 '(js2-bounce-indent-p nil)
 '(markdown-command "/usr/local/bin/markdown")
 '(neo-vc-integration (quote (face)))
 '(neo-window-fixed-size nil)
 '(neo-window-width 40)
 '(package-selected-packages
   (quote
    (terraform-mode use-package git-gutter-fringe+ git-gutter+ js2-mode company tide graphql-mode org-bullets coffee-mode go-rename yaml-mode smartparens dash-at-point slim-mode git-gutter haml-mode auto-complete flycheck gotest go-mode swift3-mode robe csharp-mode swift-mode helm-projectile ag alchemist helm-ag linum-relative helm markdown-preview-mode kotlin-mode fzf neotree projectile slime focus elixir-mode web-mode exec-path-from-shell magit slack websocket markdown-mode impatient-mode hamburg-theme define-word)))
 '(projectile-mode t nil (projectile))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(smartparens-global-mode t)
 '(typescript-indent-level 2)
 '(xterm-mouse-mode t))

;; See this wiki page for instructions: https://www.emacswiki.org/emacs/AspellWindows
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell"))
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/ispell"))

(setq focus-dimness 16)

;; For some reason, Flyspell mode causes Emacs to lag on Windows
(unless (eq system-type 'windows-nt)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))

;; (when (eq system-type 'darwin)
;;   (load-file ".emacs.d/macros/insert-html-template.el"))
;; (when (eq system-type 'windows-nt)
;;   (load-file "C:/Users/Isaac/.emacs.d/macros/insert-html-template.el"))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-script-padding 2)
(setq web-mode-enable-auto-closing t)

(setq-default indent-tabs-mode nil)

;; TODO: Sign in to more than one team
(setq slack-request-timeout 15)
(setq request-backend 'curl)
(slack-register-team (quote :name) "Triangle Devs" (quote :client-id) "" (quote :client-secret) "" (quote :token) "")

(set-face-attribute 'default nil :height 140)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mobi.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; ####################
;; ####            ####
;; #### MU4E SETUP ####
;; ####            ####
;; ####################

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (require 'mu4e)
;; (setq mu4e-mu-binary "/usr/local/bin/mu/")
;; (setq mu4e-maildir "~/.Mail")
;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)
;; ;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")

;; ;; shortcuts
;; (setq mu4e-maildir-shortcuts
;;     '( ("/INBOX"               . ?i)
;;        ("/[Gmail].Sent Mail"   . ?s)))

;; ;; something about ourselves
;; (setq
;;    user-mail-address "isaac@thewilliams.ws"
;;    user-full-name  "Isaac Williams"
;;    mu4e-compose-signature
;;     (concat
;;       "Cheers,\n"
;;       "Isaac Williams\n"))

;; ;; show images
;; (setq mu4e-show-images t)

;; ;; use imagemagick, if available
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))

;; ;; convert html emails properly
;; ;; Possible options:
;; ;;   - html2text -utf8 -width 72
;; ;;   - textutil -stdin -format html -convert txt -stdout
;; ;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;; ;;   - w3m -dump -cols 80 -T text/html
;; ;;   - view in browser (provided below)
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; ;; add option to view html message in a browser
;; ;; `aV` in view to activate
;; (add-to-list 'mu4e-view-actions
;;   '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; (setq mu4e-update-interval 300)

;; ;; configuration for sending mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)

(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-;") 'comment-line)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'tango-dark)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
