;;; cyberpunk-theme.el --- Cyberpunk Color Theme

;; Copyright 2012-2018, Nicholas M. Van Horn

;; Author: Nicholas M. Van Horn <nvanhorn@protonmail.com>
;; Homepage: https://github.com/n3mo/cyberpunk-theme.el
;; Keywords: color theme cyberpunk
;; Package-Version: 20190717.1509
;; Version: 1.21

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;; "and he'd still see the matrix in his sleep, bright lattices of logic
;; unfolding across that colorless void..."
;; William Gibson, Neuromancer.

;;; Commentary:

;; This theme is a port of Sam Aaron's overtone/emacs-live theme of the
;; same name (https://github.com/overtone/emacs-live). The original theme
;; was designed for use with the color-theme package. This theme adopts
;; the new built-in theme support deftheme. Additionally, this
;; theme strives to offer as many mode-specific customizations as
;; possible, with further tweaks that suit my fancy.

(deftheme cyberpunk "The Cyberpunk color theme")

(defcustom cyberpunk-transparent-background nil
  "Make transparent background in terminal. (Workaround)")

(let ((class '((class color) (min-colors 89)))
      ;; Cyberpunk palette
      (cyberpunk-fg "#0abdc6")
      (cyberpunk-bg-1 "#1b4798")
      (cyberpunk-bg-05 "#173c82")
      (cyberpunk-bg "#091833")
      (cyberpunk-bg+1 "#0b1e41")
      (cyberpunk-bg+2 "#0f2857")
      (cyberpunk-bg+3 "#13326c")
      (cyberpunk-error+1 "#dca3a3")
      (cyberpunk-error "#FF110B")
      (cyberpunk-error-1 "#8b0000")
      (cyberpunk-error-2 "#8b0000")
      (cyberpunk-error-3 "#9c6363")
      (cyberpunk-error-4 "#8c5353")
      (cyberpunk-error-5 "#711c91")
      (cyberpunk-accent "#ff69b4")
      (cyberpunk-accent-1 "#ff1493")
      (cyberpunk-accent-2 "#cd1076")
      (cyberpunk-secondary-accent-2 "#FF6400")
      (cyberpunk-secondary-accent-1 "#ff8c00") ;; DarkOrange
      (cyberpunk-secondary-accent "#ffa500")
      (cyberpunk-highlight "#ffff00")
      (cyberpunk-highlight-1 "#FFAF0B")
      (cyberpunk-highlight-2 "#d0bf8f")
      (cyberpunk-highlight-3 "#D8FA3C")
      (cyberpunk-highlight-4 "#E9C062")
      (cyberpunk-highlight-5 "#ffd700")
      (cyberpunk-green-2 "#2b6600")
      (cyberpunk-green-1 "#409900")
      (cyberpunk-green "#54CC00")
      (cyberpunk-green+1 "#6aff00")
      (cyberpunk-green+2 "#a6ff66")
      (cyberpunk-green+3 "#c4ff99")
      (cyberpunk-green+4 "#c4ff99")
      (cyberpunk-cyan "#93e0e3")
      (cyberpunk-blue+1 "#94bff3")
      (cyberpunk-blue "#0000ff")    ;; blue
      (cyberpunk-blue-1 "#7b68ee")  ;; medium slate blue
      (cyberpunk-blue-2 "#6a5acd")  ;; slate blue
      (cyberpunk-blue-3 "#add8e6")  ;; light blue
      (cyberpunk-blue-4 "#b2dfee")  ;; LightBlue2
      (cyberpunk-blue-5 "#2665d9")
      (cyberpunk-blue-6 "#96CBFE")
      (cyberpunk-blue-7 "#00ffff")
      (cyberpunk-blue-8 "#4F94CD")
      (cyberpunk-magenta "#dc8cc3")
      (cyberpunk-base (if (and cyberpunk-transparent-background
                                (not (display-graphic-p))
                                (eq system-type 'darwin))
                           "ARGBBB000000"
                         "#091833"))
      (cyberpunk-base-2 "#0C1021")
      (cyberpunk-base-3 "#0A0A0A")
      (cyberpunk-text "#0abdc6")
      (cyberpunk-text-2 "#0ab9c2")
      (cyberpunk-text-3 "#09a2aa")
      (cyberpunk-text-4 "#088a91")
      (cyberpunk-text-5 "#067379")
      (cyberpunk-text-6 "#055c61")
      (cyberpunk-text-7 "#044549")
      (cyberpunk-text-8 "#032e30")
      (cyberpunk-white "#ffffff")
      (cyberpunk-white-2 "#F8F8F8")
      (cyberpunk-white-3 "#fffafa"))

 (custom-theme-set-faces
   'cyberpunk
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,cyberpunk-highlight :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,cyberpunk-highlight-2 :underline t :weight normal))))
   `(blue ((,class (:foreground ,cyberpunk-blue))))
   `(bold ((,class (:bold t))))
   `(border-glyph ((,class (nil))))
   `(buffers-tab ((,class (:background ,cyberpunk-base-2 :foreground ,cyberpunk-white-2))))

   ;;; basic coloring
   `(default ((,class (:foreground ,cyberpunk-text :background ,cyberpunk-base))))
   `(cursor ((,class (:background ,cyberpunk-fg))))
   `(escape-glyph-face ((,class (:foreground ,cyberpunk-error))))
   `(fringe ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg))))
   `(header-line ((,class (:foreground ,cyberpunk-highlight
                                       :background ,cyberpunk-bg-1
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,cyberpunk-text-5))))

   ;; mode line
   `(mode-line ((,class (:background ,cyberpunk-bg-1 :foreground ,cyberpunk-text))))
   `(mode-line-inactive ((,class (:background ,cyberpunk-bg+3 :foreground ,cyberpunk-text-3))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,cyberpunk-text))))

   ;; tide
   `(tide-hl-identifier-face ((,class (:background ,cyberpunk-accent-2))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,cyberpunk-highlight))))
   `(compilation-enter-directory-face ((,class (:foreground ,cyberpunk-green))))
   `(compilation-error-face ((,class (:foreground ,cyberpunk-error-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,cyberpunk-fg))))
   `(compilation-info-face ((,class (:foreground ,cyberpunk-blue))))
   `(compilation-info ((,class (:foreground ,cyberpunk-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,cyberpunk-green))))
   `(compilation-line-face ((,class (:foreground ,cyberpunk-highlight))))
   `(compilation-line-number ((,class (:foreground ,cyberpunk-highlight))))
   `(compilation-message-face ((,class (:foreground ,cyberpunk-blue))))
   `(compilation-warning-face ((,class (:foreground ,cyberpunk-highlight-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))
   `(grep-error-face ((,class (:foreground ,cyberpunk-error :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-error))))
   `(grep-match-face ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))
   `(match ((,class (:background ,cyberpunk-base :foreground ,cyberpunk-accent-1))))

   ;;; multiple-cursors
   `(mc/cursor-face ((,class (:inverse-video nil, :background ,cyberpunk-accent :foreground ,cyberpunk-base))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))
   `(isearch-fail ((,class (:background ,cyberpunk-error-1))))

   `(lazy-highlight ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-highlight))))
   `(query-replace ((,class (:background ,cyberpunk-text-5))))
   `(Highline-face ((,class (:background ,cyberpunk-green-1))))
   `(left-margin ((,class (nil))))
   `(toolbar ((,class (nil))))
   `(text-cursor ((,class (:background ,cyberpunk-highlight :foreground ,cyberpunk-base))))

   `(menu ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg))))
   `(minibuffer-prompt ((,class (:foreground ,cyberpunk-green+1 :background ,cyberpunk-base))))
   `(mode-line
     ((,class (:foreground ,cyberpunk-blue-5
                           :background ,cyberpunk-text-5
                           :box (:line-width -1 :color ,cyberpunk-blue-5)))))
   ;; `(mode-line-buffer-id ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,cyberpunk-text-7
                           :background ,cyberpunk-text-6
                           :box (:line-width -1 :color ,cyberpunk-blue-5)))))
   `(region ((,class (:background ,cyberpunk-error-5))))
   `(secondary-selection ((,class (:background ,cyberpunk-bg+2))))
   `(trailing-whitespace ((,class (:background ,cyberpunk-error))))
   `(vertical-border ((,class (:foreground ,cyberpunk-text-5 :background ,cyberpunk-base))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,cyberpunk-blue-5))))
   `(font-lock-comment-face ((,class (:foreground ,cyberpunk-text-2 :italic t))))
   ;; `(font-lock-comment-delimiter-face ((,class (:foreground ,cyberpunk-green))))
   `(font-lock-constant-face ((,class (:foreground ,cyberpunk-blue-6))))
   ;; `(font-lock-doc-face ((,class (:foreground ,cyberpunk-green+1))))
   `(font-lock-doc-face ((,class (:foreground ,cyberpunk-highlight-1))))
   `(font-lock-function-name-face ((,class (:foreground ,cyberpunk-accent-1))))
   `(font-lock-keyword-face ((,class (:foreground ,cyberpunk-blue-5))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,cyberpunk-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,cyberpunk-text-3))))
   `(font-lock-string-face ((,class (:foreground ,cyberpunk-green+1))))
   `(font-lock-type-face ((,class (:foreground ,cyberpunk-green+3))))
   `(font-lock-variable-name-face ((,class (:foreground ,cyberpunk-accent))))
   `(font-lock-warning-face ((,class (:foreground ,cyberpunk-accent))))
   `(font-lock-reference-face ((,class (:foreground ,cyberpunk-text))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyberpunk-highlight-4))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyberpunk-error))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(gui-element ((,class (:background ,cyberpunk-text-5 :foreground ,cyberpunk-blue-6))))

   ;;; newsticker
   ;; These are currently placeholders that probably look terrible.
   ;; Someone who uses newsticker is welcome to change these
   `(newsticker-date-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-default-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,cyberpunk-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,cyberpunk-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,cyberpunk-green))))
   `(newsticker-new-item-face ((,class (:foreground ,cyberpunk-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,cyberpunk-error))))
   `(newsticker-old-item-face ((,class (:foreground ,cyberpunk-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,cyberpunk-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,cyberpunk-error))))
   `(newsticker-treeview-old-face ((,class (:foreground ,cyberpunk-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,cyberpunk-highlight))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,cyberpunk-fg))))
   `(ack-file ((,class (:foreground ,cyberpunk-blue))))
   `(ack-line ((,class (:foreground ,cyberpunk-highlight))))
   `(ack-match ((,class (:foreground ,cyberpunk-secondary-accent :background ,cyberpunk-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(font-latex-string ((,class (:foreground ,cyberpunk-green))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))
   `(font-latex-sectioning-0 ((,class (:foreground ,cyberpunk-blue :background ,cyberpunk-base :scale 1.5))))
   `(font-latex-sectioning-1 ((,class (:foreground ,cyberpunk-blue :background ,cyberpunk-base :scale 1.5))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,cyberpunk-bg-05 :underline t))))
   `(ac-candidate-face ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-text))))
   `(ac-selection-face ((,class (:background ,cyberpunk-accent-1 :foreground ,cyberpunk-base))))
   `(popup-tip-face ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-text))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,cyberpunk-base-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,cyberpunk-text-5))))
   `(popup-isearch-match ((,class (:background ,cyberpunk-base :foreground ,cyberpunk-accent-1))))

   `(window-number-face ((,class (:background ,cyberpunk-text-6 :foreground ,cyberpunk-blue-5))))

   ;; company-mode
   `(company-tooltip ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-text))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,cyberpunk-text))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,cyberpunk-blue))))
   `(company-tooltip-selection ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))
   `(company-tooltip-annotation ((,class (:inherit company-tooltip :foreground ,cyberpunk-base-3))))
   `(company-scrollbar-fg ((,class (:background ,cyberpunk-text))))
   `(company-scrollbar-bg ((,class (:background ,cyberpunk-bg-05))))
   `(company-preview ((,class (:foreground ,cyberpunk-text :background ,cyberpunk-accent-1))))
   `(company-preview-common ((,class (:foreground ,cyberpunk-text :background ,cyberpunk-accent-1))))

   ;; diff
   `(diff-added ((,class (:foreground ,cyberpunk-green))))
   `(diff-changed ((,class (:foreground ,cyberpunk-highlight))))
   `(diff-removed ((,class (:foreground ,cyberpunk-error))))
   `(diff-header ((,class (:background ,cyberpunk-bg+2))))
   `(diff-file-header ((,class (:background ,cyberpunk-bg+2 :foreground ,cyberpunk-fg :bold t))))

   ;; ediff
   `(ediff-current-diff-Ancestor ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-accent))))
   `(ediff-current-diff-A ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg-05))))
   `(ediff-current-diff-B ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg+1))))
   `(ediff-current-diff-C ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg+2))))
   `(ediff-even-diff-Ancestor ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg-05))))
   `(ediff-even-diff-A ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+1))))
   `(ediff-even-diff-B ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+2))))
   `(ediff-even-diff-C ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+3))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent))))
   `(ediff-fine-diff-A ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-blue-5))))
   `(ediff-fine-diff-B ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-blue-5))))
   `(ediff-fine-diff-C ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-blue-5))))
   `(ediff-odd-diff-Ancestor ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-text-2))))
   `(ediff-odd-diff-A ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-text-3))))
   `(ediff-odd-diff-B ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-text-4))))
   `(ediff-odd-diff-C ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-text))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,cyberpunk-green+4 :background ,cyberpunk-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,cyberpunk-error :background ,cyberpunk-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,cyberpunk-blue-5 :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,cyberpunk-magenta :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,cyberpunk-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,cyberpunk-error+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,cyberpunk-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyberpunk-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,cyberpunk-error-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,cyberpunk-highlight-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,cyberpunk-highlight-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,cyberpunk-secondary-accent-2 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,cyberpunk-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,cyberpunk-highlight))))
   `(erc-keyword-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,cyberpunk-error :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,cyberpunk-green))))
   `(erc-pal-face ((,class (:foreground ,cyberpunk-secondary-accent :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,cyberpunk-secondary-accent :background ,cyberpunk-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,cyberpunk-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(gnus-summary-high-ancient ((,class (:foreground ,cyberpunk-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,cyberpunk-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,cyberpunk-secondary-accent :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,cyberpunk-blue))))
   `(gnus-summary-low-read ((t (:foreground ,cyberpunk-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,cyberpunk-secondary-accent :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,cyberpunk-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,cyberpunk-blue+1))))
   `(gnus-summary-normal-read ((,class (:foreground ,cyberpunk-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,cyberpunk-secondary-accent :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,cyberpunk-fg))))
   `(gnus-summary-selected ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,cyberpunk-highlight-2))))
   `(gnus-cite-10 ((,class (:foreground ,cyberpunk-highlight-1))))
   `(gnus-cite-11 ((,class (:foreground ,cyberpunk-highlight))))
   `(gnus-cite-2 ((,class (:foreground ,cyberpunk-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,cyberpunk-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,cyberpunk-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,cyberpunk-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,cyberpunk-green))))
   `(gnus-cite-7 ((,class (:foreground ,cyberpunk-error))))
   `(gnus-cite-8 ((,class (:foreground ,cyberpunk-error-1))))
   `(gnus-cite-9 ((,class (:foreground ,cyberpunk-error-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,cyberpunk-highlight))))
   `(gnus-group-news-2-empty ((,class (:foreground ,cyberpunk-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,cyberpunk-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,cyberpunk-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,cyberpunk-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,cyberpunk-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,cyberpunk-bg+2))))
   `(gnus-signature ((,class (:foreground ,cyberpunk-highlight))))
   `(gnus-x ((,class (:background ,cyberpunk-fg :foreground ,cyberpunk-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,cyberpunk-green
                           :background ,cyberpunk-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,cyberpunk-highlight
                           :background ,cyberpunk-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,cyberpunk-bg-1 :underline nil))))
   `(helm-selection-line ((,class (:background ,cyberpunk-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-highlight-2))))
   `(helm-candidate-number ((,class (:foreground ,cyberpunk-green+4 :background ,cyberpunk-bg-1))))
   `(helm-ff-directory ((,class (:foreground ,cyberpunk-accent :background ,cyberpunk-bg))))
   `(helm-ff-dotted-directory ((,class (:foreground ,cyberpunk-accent :background ,cyberpunk-bg))))

   ;; hl-line-mode
   `(hl-sexp-face ((,class (:background ,cyberpunk-text-5))))
   `(hl-line-face ((,class (:background ,cyberpunk-text-5))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,cyberpunk-accent-1 :background ,cyberpunk-base))))
   `(ido-only-match ((,class (:foreground ,cyberpunk-accent-1 :background ,cyberpunk-base))))
   `(ido-subdir ((,class (:foreground ,cyberpunk-text-4 :backgroun ,cyberpunk-base))))
   `(ido-indicator ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,cyberpunk-secondary-accent))))
   `(js2-error-face ((,class (:foreground ,cyberpunk-error :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,cyberpunk-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,cyberpunk-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,cyberpunk-green+3))))
   `(js2-function-param-face ((,class (:foreground ,cyberpunk-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,cyberpunk-secondary-accent))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,cyberpunk-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,cyberpunk-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,cyberpunk-error+1))))
   `(jabber-rare-time-face ((,class (:foreground ,cyberpunk-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,cyberpunk-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,cyberpunk-error+1))))
   `(jabber-activity-face((,class (:foreground ,cyberpunk-error+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,cyberpunk-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,cyberpunk-green+2 :background ,cyberpunk-bg))))

   ;;linum-relative
   `(linum-relative-current-face ((,class (:inherit linum :foreground ,cyberpunk-white :weight bold))))

   ;; magit
   ;; magit headings and diffs
   `(magit-section-highlight ((t (:background ,cyberpunk-bg+1))))
   `(magit-section-heading ((t (:foreground ,cyberpunk-blue+1 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,cyberpunk-error+1 :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,cyberpunk-bg+2  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,cyberpunk-bg+2
                                            :foreground ,cyberpunk-blue-6 :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,cyberpunk-bg))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,cyberpunk-bg+1))))
   `(magit-diff-hunk-heading-selection ((t (:background ,cyberpunk-bg+1
                                            :foreground ,cyberpunk-blue-6))))
   `(magit-diff-lines-heading          ((t (:background ,cyberpunk-blue-6
                                            :foreground ,cyberpunk-bg+1))))
   `(magit-diff-added                  ((t (:foreground ,cyberpunk-blue-5))))
   `(magit-diff-added-highlight        ((t (:inherit magit-diff-added :weight bold))))
   `(magit-diff-removed                ((t (:foreground ,cyberpunk-magenta))))
   `(magit-diff-removed-highlight      ((t (:inherit magit-diff-removed :weight bold))))
   `(magit-diff-context                ((t (:foreground ,cyberpunk-text))))
   `(magit-diff-context-highlight      ((t (:inherit magit-diff-context :weight bold))))
   `(magit-diffstat-added   ((t (:inherit magit-diff-added))))
   `(magit-diffstat-removed ((t (:inherit magit-diff-removed))))
   ;; magit popup
   `(magit-popup-heading             ((t (:foreground ,cyberpunk-accent-1  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,cyberpunk-blue+1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,cyberpunk-blue-4   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,cyberpunk-fg    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,cyberpunk-blue-2  :weight bold))))
   ;; ;; magit process
   `(magit-process-ok    ((t (:foreground ,cyberpunk-green+1  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,cyberpunk-accent-2    :weight bold))))
   ;; ;; magit log
   `(magit-log-author    ((t (:foreground ,cyberpunk-accent))))
   `(magit-log-date      ((t (:foreground ,cyberpunk-text))))
   `(magit-log-graph     ((t (:foreground ,cyberpunk-white-2))))
   ;; ;; magit sequence
   `(magit-sequence-pick ((t (:foreground ,cyberpunk-magenta))))
   `(magit-sequence-stop ((t (:foreground ,cyberpunk-green+1))))
   `(magit-sequence-part ((t (:foreground ,cyberpunk-accent-1))))
   `(magit-sequence-head ((t (:foreground ,cyberpunk-blue+1))))
   `(magit-sequence-drop ((t (:foreground ,cyberpunk-secondary-accent))))
   `(magit-sequence-done ((t (:foreground ,cyberpunk-text-2))))
   `(magit-sequence-onto ((t (:foreground ,cyberpunk-text-2))))
   ;; ;; magit bisect
   `(magit-bisect-good ((t (:foreground ,cyberpunk-green+1))))
   `(magit-bisect-skip ((t (:foreground ,cyberpunk-accent-1))))
   `(magit-bisect-bad  ((t (:foreground ,cyberpunk-secondary-accent))))
   ;; ;; magit blame
   `(magit-blame-heading ((t (:background ,cyberpunk-bg+1 :foreground ,cyberpunk-green))))
   `(magit-blame-hash    ((t (:background ,cyberpunk-bg+1 :foreground ,cyberpunk-green))))
   `(magit-blame-name    ((t (:background ,cyberpunk-bg+1 :foreground ,cyberpunk-accent-1))))
   `(magit-blame-date    ((t (:background ,cyberpunk-bg+1 :foreground ,cyberpunk-highlight-1))))
   `(magit-blame-summary ((t (:background ,cyberpunk-bg+1 :foreground ,cyberpunk-blue-4
                                          :weight bold))))
   ;; ;; magit references etc
   `(magit-dimmed         ((t (:foreground ,cyberpunk-bg+3))))
   `(magit-hash           ((t (:foreground ,cyberpunk-green-1))))
   `(magit-tag            ((t (:foreground ,cyberpunk-accent-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,cyberpunk-green+2  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,cyberpunk-blue+1   :weight bold))))
   `(magit-branch-current ((t (:foreground ,cyberpunk-green   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,cyberpunk-blue   :weight bold))))
   `(magit-refname        ((t (:background ,cyberpunk-bg+2 :foreground ,cyberpunk-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,cyberpunk-bg+2 :foreground ,cyberpunk-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,cyberpunk-bg+2 :foreground ,cyberpunk-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,cyberpunk-green))))
   `(magit-signature-bad       ((t (:foreground ,cyberpunk-error))))
   `(magit-signature-untrusted ((t (:foreground ,cyberpunk-highlight))))
   `(magit-cherry-unmatched    ((t (:foreground ,cyberpunk-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,cyberpunk-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,cyberpunk-green))))
   `(magit-reflog-amend        ((t (:foreground ,cyberpunk-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,cyberpunk-green))))
   `(magit-reflog-checkout     ((t (:foreground ,cyberpunk-blue))))
   `(magit-reflog-reset        ((t (:foreground ,cyberpunk-error))))
   `(magit-reflog-rebase       ((t (:foreground ,cyberpunk-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,cyberpunk-green))))
   `(magit-reflog-remote       ((t (:foreground ,cyberpunk-cyan))))
   `(magit-reflog-other        ((t (:foreground ,cyberpunk-cyan))))

   `(eval-sexp-fu-flash ((,class (:background ,cyberpunk-text-8 :foreground ,cyberpunk-accent-2))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,cyberpunk-blue-5))))
   `(message-header-other ((,class (:foreground ,cyberpunk-green))))
   `(message-header-to ((,class (:foreground ,cyberpunk-accent-1 :weight bold))))
   `(message-header-from ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(message-header-cc ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(message-header-subject ((,class (:foreground ,cyberpunk-secondary-accent :weight bold))))
   `(message-header-xheader ((,class (:foreground ,cyberpunk-green))))
   `(message-mml ((,class (:foreground ,cyberpunk-highlight :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(mew-face-header-from ((,class (:foreground ,cyberpunk-highlight))))
   `(mew-face-header-date ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-to ((,class (:foreground ,cyberpunk-error))))
   `(mew-face-header-key ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-private ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-important ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,cyberpunk-error))))
   `(mew-face-header-xmew ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,cyberpunk-error))))
   `(mew-face-body-url ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(mew-face-body-comment ((,class (:foreground ,cyberpunk-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(mew-face-body-cite4 ((,class (:foreground ,cyberpunk-highlight))))
   `(mew-face-body-cite5 ((,class (:foreground ,cyberpunk-error))))
   `(mew-face-mark-review ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-mark-delete ((,class (:foreground ,cyberpunk-error))))
   `(mew-face-mark-unlink ((,class (:foreground ,cyberpunk-highlight))))
   `(mew-face-mark-refile ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-mark-unread ((,class (:foreground ,cyberpunk-error-2))))
   `(mew-face-eof-message ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-eof-part ((,class (:foreground ,cyberpunk-highlight))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,cyberpunk-cyan :background ,cyberpunk-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-error :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,cyberpunk-highlight))))
   `(nav-face-button-num ((,class (:foreground ,cyberpunk-cyan))))
   `(nav-face-dir ((,class (:foreground ,cyberpunk-green))))
   `(nav-face-hdir ((,class (:foreground ,cyberpunk-error))))
   `(nav-face-file ((,class (:foreground ,cyberpunk-fg))))
   `(nav-face-hfile ((,class (:foreground ,cyberpunk-error-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background ,cyberpunk-base))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,cyberpunk-base))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,cyberpunk-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,cyberpunk-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,cyberpunk-bg+1))))

   ;; org-mode
   `(org-document-title ((,class (:foreground ,cyberpunk-blue-3 :background ,cyberpunk-base :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,cyberpunk-blue-3 :background ,cyberpunk-base :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,cyberpunk-text-2 :background ,cyberpunk-base))))
   `(org-agenda-date-today
     ((,class (:foreground ,cyberpunk-secondary-accent-2 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,cyberpunk-text-2 :foreground ,cyberpunk-base
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,cyberpunk-blue-7 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,cyberpunk-green
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,cyberpunk-secondary-accent :weight bold
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,cyberpunk-accent-1 :height 1.3))))
   `(org-level-2 ((,class (:foreground ,cyberpunk-highlight :height 1.2))))
   `(org-level-3 ((,class (:foreground ,cyberpunk-blue-5 :height 1.1))))
   `(org-level-4 ((,class (:foreground ,cyberpunk-green))))
   `(org-level-5 ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(org-level-6 ((,class (:foreground ,cyberpunk-accent))))
   `(org-level-7 ((,class (:foreground ,cyberpunk-green+3))))
   `(org-level-8 ((,class (:foreground ,cyberpunk-blue-1))))
   `(org-link ((,class (:foreground ,cyberpunk-blue-6 :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,cyberpunk-text-7 :foreground ,cyberpunk-base))))
   `(org-column-title ((,class (:background ,cyberpunk-text-7 :underline t :weight bold))))
   `(org-block ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg-05))))
   `(org-block-begin-line
     ((,class (:foreground "#008ED1" :background ,cyberpunk-bg-1))))
   `(org-block-background ((,class (:background ,cyberpunk-bg-05))))
   `(org-block-end-line
     ((,class (:foreground "#008ED1" :background ,cyberpunk-bg-1))))

   ;; `(org-deadline-announce ((,class (:foreground ,cyberpunk-error-1))))
   ;; `(org-scheduled ((,class (:foreground ,cyberpunk-green+4))))
   ;; `(org-scheduled-previously ((,class (:foreground ,cyberpunk-error-4))))
   ;; `(org-scheduled-today ((,class (:foreground ,cyberpunk-blue+1))))
   ;; `(org-special-keyword ((,class (:foreground ,cyberpunk-highlight-1))))
   ;; `(org-table ((,class (:foreground ,cyberpunk-green+2))))
   ;; `(org-time-grid ((,class (:foreground ,cyberpunk-secondary-accent))))
   ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((,class (:bold t :foreground ,cyberpunk-error :weight bold :underline nil))))
   ;; `(org-formula ((,class (:foreground ,cyberpunk-highlight-2))))
   ;; `(org-headline-done ((,class (:foreground ,cyberpunk-green+3))))
   ;; `(org-hide ((,class (:foreground ,cyberpunk-bg-1))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; emms
   `(emms-browser-year/genre-face ((,class (:foreground ,cyberpunk-blue-3 :height 1.0))))
   `(emms-browser-artist-face ((,class (:foreground ,cyberpunk-accent-1 :height 1.0))))
   `(emms-browser-composer-face ((,class (:foreground ,cyberpunk-blue-3 :height 1.0))))
   `(emms-browser-performer-face ((,class (:foreground ,cyberpunk-blue-3 :height 1.0))))
   `(emms-browser-album-face ((,class (:foreground ,cyberpunk-highlight :height 1.0))))
   `(emms-browser-track-face ((,class (:foreground ,cyberpunk-blue-5 :height 1.0))))

   ;; Calfw
   `(cfw:face-title ((,class (:foreground ,cyberpunk-accent-1 :weight bold :height 1.8))))
   `(cfw:face-header ((,class (:foreground ,cyberpunk-highlight-5 :weight bold))))
   `(cfw:face-sunday ((,class (:foreground ,cyberpunk-error :weight bold))))
   `(cfw:face-saturday ((,class (:foreground ,cyberpunk-green :weight bold))))
   `(cfw:face-holiday ((,class (:foreground ,cyberpunk-accent-2 :weight bold))))
   `(cfw:face-grid ((,class (:foreground ,cyberpunk-text-3))))
   `(cfw:face-default-content ((,class (:foreground ,cyberpunk-cyan))))
   `(cfw:face-periods ((,class (:foreground ,cyberpunk-cyan :weight bold))))
   `(cfw:face-day-title ((,class (:foreground ,cyberpunk-fg))))
   `(cfw:face-default-day ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(cfw:face-annotation ((,class (:foreground ,cyberpunk-text))))
   `(cfw:face-disable ((,class (:foreground ,cyberpunk-text-2 :weight bold))))
   `(cfw:face-today-title ((,class (:foreground ,cyberpunk-blue :background ,cyberpunk-magenta))))
   `(cfw:face-today ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(cfw:face-select ((,class (:background ,cyberpunk-bg+2))))
   `(cfw:face-toolbar ((,class (:background ,cyberpunk-blue-8))))
   `(cfw:face-toolbar-button-off ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-blue-8 :weight bold))))
   `(cfw:face-toolbar-button-on ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-secondary-accent-1 :weight bold))))

   ;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyberpunk-error-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyberpunk-green-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyberpunk-accent-1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,cyberpunk-highlight))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,cyberpunk-green))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,cyberpunk-blue-3))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cyberpunk-blue-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,cyberpunk-text))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,cyberpunk-white))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,cyberpunk-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,cyberpunk-error-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,cyberpunk-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,cyberpunk-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,cyberpunk-error))))
   `(rpm-spec-macro-face ((,class (:foreground ,cyberpunk-highlight))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,cyberpunk-error))))
   `(rpm-spec-package-face ((,class (:foreground ,cyberpunk-error))))
   `(rpm-spec-section-face ((,class (:foreground ,cyberpunk-highlight))))
   `(rpm-spec-tag-face ((,class (:foreground ,cyberpunk-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,cyberpunk-error))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(rst-level-2-face ((,class (:foreground ,cyberpunk-green+1))))
   `(rst-level-3-face ((,class (:foreground ,cyberpunk-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,cyberpunk-highlight-2))))
   `(rst-level-5-face ((,class (:foreground ,cyberpunk-cyan))))
   `(rst-level-6-face ((,class (:foreground ,cyberpunk-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,cyberpunk-error-3 :background ,cyberpunk-base))))
   `(show-paren-match ((,class (:foreground ,cyberpunk-base :background ,cyberpunk-accent-1))))

   `(naeu-green-face ((,class (:foreground ,cyberpunk-green :background ,cyberpunk-base))))
   `(naeu-pink-face ((,class (:foreground ,cyberpunk-accent-1 :background ,cyberpunk-base))))
   `(naeu-blue-face ((,class (:foreground ,cyberpunk-blue-1 :background ,cyberpunk-base))))
   `(naeu-orange-face ((,class (:foreground ,cyberpunk-highlight-1 :background ,cyberpunk-base))))
   `(naeu-red-face ((,class (:foreground ,cyberpunk-secondary-accent :background ,cyberpunk-base))))
   `(naeu-grey-face ((,class (:foreground ,cyberpunk-text-7 :background ,cyberpunk-base))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,cyberpunk-error))))

  ;;; ansi-term
   `(term-color-black ((,class (:foreground ,cyberpunk-bg
                                            :background ,cyberpunk-bg-1))))
   `(term-color-red ((,class (:foreground ,cyberpunk-error-2
                                          :background ,cyberpunk-error-4))))
   `(term-color-green ((,class (:foreground ,cyberpunk-green
                                            :background ,cyberpunk-green+2))))
   `(term-color-yellow ((,class (:foreground ,cyberpunk-secondary-accent
                                             :background ,cyberpunk-highlight))))
   `(term-color-blue ((,class (:foreground ,cyberpunk-blue-1
                                           :background ,cyberpunk-blue-4))))
   `(term-color-magenta ((,class (:foreground ,cyberpunk-magenta
                                              :background ,cyberpunk-error))))
   `(term-color-cyan ((,class (:foreground ,cyberpunk-cyan
                                           :background ,cyberpunk-blue))))
   `(term-color-white ((,class (:foreground ,cyberpunk-fg
                                            :background ,cyberpunk-bg-1))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,cyberpunk-text-5))))

   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,cyberpunk-accent-1 :background ,cyberpunk-base))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-bg+1))))
   `(whitespace-hspace ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-bg+1))))
   `(whitespace-tab ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-error))))
   `(whitespace-newline ((,class (:foreground ,cyberpunk-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,cyberpunk-error :background ,cyberpunk-bg))))
   `(whitespace-line ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,cyberpunk-secondary-accent :foreground ,cyberpunk-secondary-accent))))
   `(whitespace-indentation ((,class (:background ,cyberpunk-highlight :foreground ,cyberpunk-error))))
   `(whitespace-empty ((,class (:background ,cyberpunk-highlight :foreground ,cyberpunk-error))))
   `(whitespace-space-after-tab ((,class (:background ,cyberpunk-highlight :foreground ,cyberpunk-error))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,cyberpunk-error-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,cyberpunk-error-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,cyberpunk-secondary-accent))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,cyberpunk-error-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,cyberpunk-error))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,cyberpunk-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,cyberpunk-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,cyberpunk-error+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,cyberpunk-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,cyberpunk-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,cyberpunk-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,cyberpunk-highlight))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,cyberpunk-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,cyberpunk-green+4))))

   ;; yasnippet
   `(yas/field-highlight-face ((,class (:background ,cyberpunk-accent-1 :foreground ,cyberpunk-base))))

   ;; enh-ruby-mode enh-ruby-op-face
   `(enh-ruby-op-face ((,class (:foreground ,cyberpunk-blue-7))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,cyberpunk-green+2))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,cyberpunk-green+2))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,cyberpunk-blue-1))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,cyberpunk-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,cyberpunk-bg-1 :foreground ,cyberpunk-bg-1))))

   ;; customize
   `(custom-button ((,class (:box (:line-width 2 :style released-button)
                                  :background ,cyberpunk-bg-05 :foreground ,cyberpunk-fg))))
   `(custom-button-unraised ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-fg))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'cyberpunk
   `(ansi-color-names-vector [,cyberpunk-bg ,cyberpunk-error-2 ,cyberpunk-green ,cyberpunk-secondary-accent
                                          ,cyberpunk-blue-1 ,cyberpunk-magenta ,cyberpunk-cyan ,cyberpunk-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,cyberpunk-bg-05)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cyberpunk)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; cyberpunk-theme.el ends here.
