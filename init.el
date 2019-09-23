; list the packages you want
(setq package-list '(add-node-modules-path prettier-js monokai-theme terraform-mode use-package git-gutter-fringe+ git-gutter+ js2-mode company tide graphql-mode org-bullets coffee-mode go-rename yaml-mode smartparens dash-at-point slim-mode git-gutter haml-mode auto-complete flycheck gotest go-mode swift3-mode robe csharp-mode swift-mode helm-projectile ag alchemist helm-ag linum-relative helm markdown-preview-mode kotlin-mode fzf neotree projectile slime focus elixir-mode web-mode exec-path-from-shell magit slack websocket markdown-mode impatient-mode hamburg-theme define-word))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(add-to-list 'load-path "/home/isaac/.emacs.d/packages/")
(require 'go-autocomplete)
(require 'zen-mode)
(require 'goto-chg)

(require 'bgex)

(when (boundp 'bgex-exist-p)
  (bgex-set-image-default "~/.emacs.d/images/japanese-complex-deshine-dark.jpg"))

(global-undo-tree-mode)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'evil-magit)

(require 'evil-surround)
(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition)

(evil-ex-define-cmd "ph" 'helm-projectile)
(evil-ex-define-cmd "ps" 'projectile-ag)
(evil-ex-define-cmd "pp" 'helm-projectile-switch-project)

(evil-ex-define-cmd "gs" 'magit-status)

(global-set-key (kbd "M-o") 'ace-window)

;; (require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-mode-all)
;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil)

;; (define-key god-local-mode-map (kbd "z") 'repeat)

;; (global-set-key (kbd "C-x C-1") 'delete-other-windows)
;; (global-set-key (kbd "C-x C-2") 'split-window-below)
;; (global-set-key (kbd "C-x C-3") 'split-window-right)
;; (global-set-key (kbd "C-x C-0") 'delete-window)

;; (defun c/god-mode-update-cursor ()
;;   (let ((limited-colors-p (> 257 (length (defined-colors)))))
;;     (cond (god-local-mode (progn
;;                             (set-face-background 'mode-line (if limited-colors-p "white" "#ea00d9"))
;;                             (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#99008f"))))
;;           (t (progn
;;                (set-face-background 'mode-line (if limited-colors-p "black" "#133e7c"))
;;                (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2242")))))))

;; (add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor)

;; (require 'god-mode-isearch)
;; (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
;; (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(winner-mode 1)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

(global-git-gutter+-mode +1)
(global-auto-revert-mode +1)

(setq-default mode-line-format
              (list
               " " mode-line-modified
               " %[" mode-line-buffer-identification "%] %l %6 "
               mode-line-misc-info
               mode-line-end-spaces))
(setq global-mode-string '((t jabber-activity-mode-string)
                           "" display-time-string appt-mode-string))

(setq tide-format-options '(:indentSize 2 :tabSize 2))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (add-node-modules-path)
  (prettier-js-mode))

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
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'js2-mode-hook #'prettier-js-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key (kbd "C-c g s") 'magit-status)

(global-set-key (kbd "C-c t r r") 'tide-refactor)
(global-set-key (kbd "C-c t r n s") 'tide-rename-symbol)
(global-set-key (kbd "C-c t r n f") 'tide-rename-file)
(global-set-key (kbd "C-c t ?") 'tide-references)
(global-set-key (kbd "C-c t f") 'tide-fix)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 18)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t M-t" . treemacs-find-tag)
        ("C-x t p"   . treemacs-projectile)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

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
(setq linum-relative-format "%3s \u2502 ")

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
(global-set-key (kbd "C-c p") 'windmove-up)

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
 '(ansi-color-names-vector
   ["#091833" "#8b0000" "#54CC00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#0abdc6"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("ace21d57cd9a22c96c021acfd2938411e3374626fe8d91afb9bb969b5269ea75" "83fb682c2c080d044a23f5c9b0e8d3c3ef752ce41bc4beafe3ac06cbae9e4b01" "f5901c2abb75780141384a95fe8416d6d94ef802c0dc4ba7dfabc785153dd7a6" "041a333ed0880fe10608829ebaf7d2070f3f83a5df1b7b3345b619bb46734356" "f2df7cfec61a1d685eb939dc5096e60ee3e62f17f76af51baef1e621bc4c17bb" "084325b20981d7c3a8e454e25ff8ef410135f6c03dc2f458deba86eecb57b368" "1467b97c70dd0974afd8bd8ed26d0b82f257f4d8079fbfbec13d9db456c0523a" "56fc07d87e173f37f85c38b98b80944f98d3eb0e50d817cdca35aaa72028db4e" "30212d0c197f5bc8555b0075b77240a4e5d510e4462b88faa10cc6e0b1dddac8" "e6a92c75403845f3613c7ce7ab060f0f441bcebf43765598c6b5f7f6fc730851" "7f27c0e6edb39598a72408be38cc49dce802e20779677270cf79c9932fc0d556" "5d7454f5c24c2f096662972cc7645f492d90e67cbc5e3652515eac4b5ad69da2" "87f7e90479cca43f4c265b0687601a19551d4ee0e2642365380276064c7b7903" "ebd228a944cd0091a3c8c93772bc4820814407aa9a6260a80e7240f3b8974974" "76f48635ead5b77140d3b59fcd911d77d9a541c1acf812adeaec3395f3cdbc06" "a4d4f55e18786882b3c43cd895b75ee0a54e52061ab9b3dc5362c50e7bae57f4" "fe693fc4aa48e346b718e41213ce3fb96355a2d1af3e19155a960b22085e9df7" "cc36f3be034398efe9eb0efc96bd15a9400a5aea6fd00525552074883b4c0928" "36784e2a5b7bca5990a7ce95387bd6b0b9c86194e2e79e7fe5d10b40aa0109c8" "660bb44db68f8134d748c3c665a8899d83fa6d70e9beb2b85adad95328ab935b" "5cc94098d0f872615b56c8b01f20832a48860e7aeebdc98864337edb2d54655d" "7655a356f577b4e00433de2905bd9f7f40c8757b438516f28ebe5e238d9b5f42" "eddae2b99b0ab9d2d4c6f5b9dc00fa27abd2468ed9907a83cf444ce5b3d44bd2" "281e5d7e6f6d52e40f49fcba0d34d778e35596344899ce6a259e83cb186bf105" "01ed139ce169284f85d2505e938e9b811f4c6e192c25ce6b61bf9a94b4c8d5ae" "932048d4fc1b43aff00c8e033b864e2308e2b159ed88ae2c940b8bdfc8590568" "f102b63a814b62f3402ef92e297dbdb067ebdcaea3f54bb7337c3e1cd639574c" "819210ebc677a62eac936001b78929fc3b3cbb4a9362f1f0d993fa036d7d48e9" "60764b9e2ae417b08315580d2e0e4e92f3cea52f52b3d72a007e387c70cef3cc" "1566fe35f57aa85e2d642c06a7954313cf17be72ae51549597b68cf0cd33bec6" "d321d34fac5e177603b2dc851162b07ae401573fd53cc1634e1fe273952f5d5b" "5dcf514305d8e1b03e10d9f6be9e874625131637f30b5932cbe0b1877a956dc4" "c864950582c7625684e3285e7ed2de5680c2cc2f60a14049450714f3db7cbf55" "637f5effb1c56c2cf11709a42276d224c8b22a4844da917848191d191a993055" "a8b1aa7353f7173da101316b74c2aefe21213891f583ff22488631a11295f79e" "658914a86cd878894662928627d06d8389b07f862d1227e7267ef3599f747ac0" "a783ea6e244fadcd2fd3b78e35d3e7958a811e44e22a9a813a443f12937b691b" "9f49e9b5a6f05f673bb457c01f6bbd459558357b8c43d0129eb279fcf98945c4" "8d0bd997b6983d67124ece9365b39d5df387093e502c1c04416f648669d5caf8" "d097cdfbb69938cb3b77cc0f2ab016bae5d5ab66db0ce18f00f823db365f7985" "4ef5eb5cfa9d96285d371c6db8e3298511d40757a75b22612a4d0ed9eb82949e" "a08afb44d45ed703261fbe3077e1af18bd02de4871ddf755ecffd3e3177191f1" "1afc3e9ec1477f1c09e6bcf4e24cb09a5dfa31da9ed38555fdc41b9e350785dc" "4ea623611408579032a57d69dcb9e5fad52a2fe589d94573a2584a775a6554f9" "b3f5debfab3352c494cf5fc57ac15b8e55f84d145b1ece1c755aade25ef2ce19" "577231e8f5324af001988999b58920c45f7b39c68484e96271e073a863fce56b" "817d0e812b47380b0d503c7314e367ab8cdfe7ba37f15666065ef57d75e924b6" "a0047f65abd446cd1ccba681babf325a6d2989e874cf5e198c3785b5fd8fecc0" "bc90c42a25542fd7a492b6ed1c8431d130c8073db3edf579b945d9f0e866637e" "5d89ca5f4703e245ead42a18c99bb874b0c804ba6b77cc527af0cd74ffec8a73" "6902b926fbe37e93408c8474f397b7995a7ebbe56f24f350aad8493a5c00f591" "e5146d606108b3fcdedfd4fb41cb062866f5887b57bb69cbc4565168dad99015" "cc75aeb28125a4b133bc80e40815cbdc1500c132bfda81b69284fc9d5260692a" "fa16dc132f8c243a351fdbd8e496cdfa0153754b72d356d2076cf9c94de7a7d2" "ca5f27ac2f9b64c9005b02efb1e28bef9a69bbed9adef42083e419e7f4ef3e5e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "7803ff416cf090613afd3b4c3de362e64063603522d4974bcae8cfa53cf1fd1b" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" default)))
 '(fci-rule-color "#173c82")
 '(fill-column 80)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(js-indent-level 2)
 '(js2-bounce-indent-p nil)
 '(magit-diff-use-overlays nil)
 '(markdown-command "/home/isaac/.local/bin/markdown_py")
 '(neo-vc-integration (quote (face)))
 '(neo-window-fixed-size nil)
 '(neo-window-width 40)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (evil-surround evil-magit undo-tree god-mode highlight cyberpunk-2019-theme smooth-scrolling treemacs-icons-dired treemacs treemacs-magit treemacs-projectile solarized-theme add-node-modules-path prettier-js monokai-theme terraform-mode use-package git-gutter-fringe+ git-gutter+ js2-mode company tide graphql-mode org-bullets coffee-mode go-rename yaml-mode smartparens dash-at-point slim-mode git-gutter haml-mode auto-complete flycheck gotest go-mode swift3-mode robe csharp-mode swift-mode helm-projectile ag alchemist helm-ag linum-relative helm markdown-preview-mode kotlin-mode fzf neotree projectile slime focus elixir-mode web-mode exec-path-from-shell magit slack websocket markdown-mode impatient-mode hamburg-theme define-word)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(projectile-mode t nil (projectile))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(smartparens-global-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
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
;; (setq slack-request-timeout 15)
;; (setq request-backend 'curl)
;; (slack-register-team (quote :name) "Triangle Devs" (quote :client-id) "" (quote :client-secret) "" (quote :token) "")

(set-face-attribute 'default nil :height 120)

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

(load-theme 'cyberpunk)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
