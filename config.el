;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
      ;; user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'doom-zenburn)
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;=======================================================
;;#######################################################
;;base config
;;#######################################################
;;=======================================================


;; load environment
(load! "~/.doom.d/env.el")

;; column line long 79 char
(setq! display-fill-column-indicator-column 79)

;; font
 (setq! doom-font (font-spec :family "Hack" :size 15 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Hack" :size 13)
      doom-unicode-font (font-spec :family "Hack")
      doom-big-font (font-spec :family "Hack" :size 24))

;; dont add end allow line
 (setq! mode-require-final-newline nil)

;; makes the column number show up
(column-number-mode 1)
(setq! python-shell-completion-native-disabled-interpreters '("python3"))

;;replace name buffer
(setq! doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq! read-process-output-max (* 2048 2048))
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; delay buffer hint
(setq! which-key-idle-delay 0.5)

;;;; Disable backup
(setq! make-backup-files nil)

;;disable dialog window
(setq! use-dialog-box nil)

;; auto read on disc file
(global-auto-revert-mode 1)
(setq! global-auto-revert-non-file-buffers t)

;;consult
(setq consult-locate-args "mdfind")

;;limit
(setq! undo-limit 80000000)
(setq! scroll-margin 2)

(setq! lsp-use-plists "true")

;; enable on-the-fly spell checking
(setq! flyspell-use-meta-tab nil)

;;cache projectile enable
(setq! projectile-enable-caching t)

;; vterm height window
(setq! multi-vterm-dedicated-window-height-percent 40)

;; set default dictionary
(setq! ispell-dictionary "american")

;;fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(winner-mode 1)
;;;; Delete selection
(delete-selection-mode t)
(global-superword-mode t)
(whole-line-or-region-global-mode)

;;;;=======================================================
;;#######################################################
;;base config end
;;#######################################################
;;=======================================================


;;=======================================================
;;#######################################################
;;my custom function
;;#######################################################
;;=======================================================

;; Turn on warn highlighting for characters outside of the 'width' char limit
(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
   that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

;;-------------------------------------------------------
;; Shift the selected region right if distance is positive, left if
;; negative
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 4))

(defun shift-left ()
  (interactive)
  (shift-region -4))

(defun delete-word (arg)
;; "Delete characters forward until encountering the end of a word.
;; With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
;; "Delete characters backward until encountering the end of a word.
;; With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun last-half-delete-line ()
  (interactive)
  (delete-region (point) (line-end-position)))

(defun first-half-delete-line ()
  (interactive)
  (delete-region (point) (line-beginning-position)))

;;move text and return region
(defun move-line-or-region-right ()
  (interactive)
(whole-line-or-region-indent-rigidly-right-to-tab-stop 1)
(setq deactivate-mark nil))

(defun move-line-or-region-left ()
  (interactive)
(whole-line-or-region-indent-rigidly-left-to-tab-stop 1)
(setq deactivate-mark nil))

;;delete line and dont add kill ring
(defun my-delete-line-this-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

;;replace yank
(defun my-replace-yank ()
  (interactive)
  (my-delete-line-this-line)
  (yank)
)

;; yank and dont move cursor to new line
(defun custom-yank ()
  (interactive)
  (yank)
  (beginning-of-line)
  (backward-delete-char 1)
  )

;; replace region
(defun repl-yank(beg end)
  (interactive "r")
  (delete-region beg end)
  (yank 1))

;; if region replace, or yank and dont add new line
(defun crazy-yank ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'repl-yank))
        (call-interactively #'custom-yank)
  )

;; set breack point hith majore mode
(defun my/set-breackpoint ()
  (interactive)
  (when (equal major-mode 'python-mode)
    (add-py-debug))
  (when (equal major-mode 'js-mode)
    (add-js-debug))
  (when (equal major-mode 'rjsx-mode)
    (add-js-debug))
  )

(defun my/jump-breackpoint ()
  (interactive)
    (when (equal major-mode 'python-mode)
    (my/jump-python-breackpoint))
  (when (equal major-mode 'js-mode)
    (my/jump-js-breackpoint))
  (when (equal major-mode 'rjsx-mode)
    (my/jump-js-breackpoint))

;; jump python breckpoint
(defun my/jump-python-breackpoint ()
    (interactive)
    (search-forward-regexp "^[ ]*import ipdb; ipdb.set_trace();")
    (move-beginning-of-line 1))
  )

;; jump js breckpoint
(defun my/jump-js-breackpoint ()
    (interactive)
    (search-forward-regexp "^[ ]*debugger;")
    (move-beginning-of-line 1))

;;run ipdb debugg and realgud track
(defun my/django-runserver ()
  (cd "/Users/dmitrijmartys/SRC/zakupki/zakupki/src/zakupki")
  (interactive)
  ;; (if (boundp 'buf)
      ;; (pop-to-buffer buf)
    ;; (setq buf (multi-vterm-dedicated-toggle)))
  (multi-vterm-dedicated-toggle)
  (vterm-send-string "python manage.py runserver --noreload")
  (vterm-send-return))

(defun my/ls-activete-region()
  (interactive)
  (activate-mark)
  )

(defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point))

    (defun copy-thing (begin-of-thing end-of-thing &optional arg)
      "Copy thing between beg & end into kill ring."
      (save-excursion
        (let ((beg (get-point begin-of-thing 1))
              (end (get-point end-of-thing arg)))
          (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'forward-word 'backward-word arg)
     )

(defun my-mark-word (N)
  (interactive "p")
  (if (and
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word-backward)))
      (set-mark (point)))
  (forward-word N))

(defun my-mark-line (N)
  (interactive "p")
  (set-mark (point))
  (end-of-line))

(defun my-mark-line-backward (N)
  (interactive "p")
  (set-mark (point))
  (back-to-indentation))

(defun my-mark-word-backward (N)
  (interactive "p")
  (if (and
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word)))
      (set-mark (point)))
  (backward-word N))

(defun add-py-debug ()
      "add debug code and move line down"
    (interactive)
    (highlight-regexp "import ipdb; ipdb.set_trace();" 'company-echo-common)
    (save-excursion (insert "import ipdb; ipdb.set_trace();")))

(defun add-js-debug ()
      "add debug code and move line down"
    (interactive)
    (highlight-regexp "debugger;" 'company-echo-common)
    (save-excursion (insert "debugger;")))

(defun remove-py-debug ()
  "remove py debug code, if found"
  (interactive)
  (let ((x (line-number-at-pos))
    (cur (point)))
    (search-forward-regexp "^[ ]*import ipdb; ipdb.set_trace();")
    (if (= x (line-number-at-pos))
    (let ()
      (move-beginning-of-line 1)
      (kill-line 1)
      (move-beginning-of-line 1))
      (goto-char cur))))

(defun my/select-current-line-and-forward-line (arg)
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun my/select-current-line-and-previous-line (arg)
  (interactive "p")
  (when (not (use-region-p))
    (end-of-line)
    (set-mark-command nil))
  (previous-line arg)
  (beginning-of-line))


(defun zz/goto-match-paren (arg)
    "Go to the matching paren/bracket, otherwise (or if ARG is not
    nil) insert %.  vi style of % jumping to matching brace."
    (interactive "p")
    (if (not (memq last-command '(set-mark
                                  cua-set-mark
                                  zz/goto-match-paren
                                  down-list
                                  up-list
                                  end-of-defun
                                  beginning-of-defun
                                  backward-sexp
                                  forward-sexp
                                  backward-up-list
                                  forward-paragraph
                                  backward-paragraph
                                  end-of-buffer
                                  beginning-of-buffer
                                  backward-word
                                  forward-word
                                  mwheel-scroll
                                  backward-word
                                  forward-word
                                  mouse-start-secondary
                                  mouse-yank-secondary
                                  mouse-secondary-save-then-kill
                                  move-end-of-line
                                  move-beginning-of-line
                                  backward-char
                                  forward-char
                                  scroll-up
                                  scroll-down
                                  scroll-left
                                  scroll-right
                                  mouse-set-point
                                  next-buffer
                                  previous-buffer
                                  previous-line
                                  next-line
                                  back-to-indentation
                                  doom/backward-to-bol-or-indent
                                  doom/forward-to-last-non-comment-or-eol
                                  )))
        (self-insert-command (or arg 1))
      (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
            (t (self-insert-command (or arg 1))))))


(defun my/kill-ring-save (beg end)
  (interactive "r")
  (if mark-active
      (kill-ring-save beg end)
    (kill-ring-save (line-beginning-position) (line-end-position))))


(defun my/kill-region (beg end)
  (interactive "r")
  (if mark-active
      (kill-region beg end)
    (kill-region (line-beginning-position) (line-end-position))
    (backward-delete-char 1)))


(defun my/mark-text-in-qutes ()
     (interactive)
     (er/expand-region 1)
     (er/expand-region 1))

;;=======================================================
;;#######################################################
;;my custom function end
;;#######################################################
;;=======================================================
;;=======================================================


;;=======================================================
;;#######################################################
;;keymap
;;#######################################################
;;=======================================================

(map! :after smartparens
      :map smartparens-mode-map
      "C-d" nil
      "C-a" nil
      "M-C-d" nil
      "M-C-a" nil
      "C-M-q" nil
      "C-M-e" nil
      )

(map! :after vertico
      :map vertico-map
      "M-s" nil
      )

(map! :after dired
      :map dired-mode-map
      "M-s" nil
      )

(map! :after ibuffer-vc
      :map ibuffer-mode-map
      "M-s" nil
      )

(map! :after magit
      :map magit-mode-map
      "M-w" nil
      )

(map! :after magit-blame
      :map magit-blame-read-only-mode-map
      "M-w" nil
      )

(map! :after term
      :map term-mode-map
      "M-o" nil
      )

(map! :after vterm
      :map vterm-copy-mode-map
      "C-a" nil
      )


(map! :after prog-mode
      :map prog-mode-map
      "M-q" nil
      "C-M-q" nil
      )

(with-eval-after-load 'centered-cursor-mode
  (define-key ccm-map (kbd "M-v") nil)
  )

(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map (kbd "C-d") nil)
  )

(with-eval-after-load 'lsp-mode
  (define-key lsp-signature-mode-map (kbd "M-a") nil)
  )

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-M-q") nil)
  )


(defvar my-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (global-unset-key (kbd "s-="))
    (global-unset-key (kbd "s--"))
    (global-unset-key (kbd "M-s"))
    (global-unset-key (kbd "M-a"))
    (global-unset-key (kbd "M-d"))
    (global-unset-key (kbd "M-q"))
    (global-unset-key (kbd "M-e"))
    (global-unset-key (kbd "M-s-w"))
    (global-unset-key (kbd "M-s-s"))
    (global-unset-key (kbd "M-C-s"))
    (global-unset-key (kbd "M-C-w"))
    (global-unset-key (kbd "M-s-d"))
    (global-unset-key (kbd "M-s-a"))
    (global-unset-key (kbd "C-d"))
    (global-unset-key (kbd "C-a"))
    (global-unset-key (kbd "C-s"))
    (global-unset-key (kbd "s-["))
    (global-unset-key (kbd "C-M-q"))
    (global-unset-key (kbd "C-M-e"))


    (global-set-key (kbd "M-<up>") (lambda () (interactive) (forward-line -10)))
    (global-set-key (kbd "M-<down>") (lambda () (interactive) (forward-line  10)))
    (global-set-key (kbd "M-s-w") (lambda () (interactive) (forward-line -10)))
    (global-set-key (kbd "M-s-s") (lambda () (interactive) (forward-line  10)))
    (global-set-key (kbd "M-<right>") 'forward-word)
    (global-set-key (kbd "M-<left>") 'backward-word)
    (global-set-key (kbd "M-w") 'previous-line)
    (global-set-key (kbd "M-s") 'next-line)
    (global-set-key (kbd "M-a") 'backward-char)
    (global-set-key (kbd "M-d") 'forward-char)
    (global-set-key (kbd "M-q") 'backward-word)
    (global-set-key (kbd "M-e") 'forward-word)
    (global-set-key (kbd "M-C-s") 'end-of-buffer)
    (global-set-key (kbd "M-C-w") 'beginning-of-buffer)
    (global-set-key (kbd "M-s-d") 'end-of-line)
    (global-set-key (kbd "M-s-a") 'beginning-of-line)
    (global-set-key (kbd "C-d") 'delete-word)
    (global-set-key (kbd "C-a") 'backward-delete-word)
    (global-set-key (kbd "C-s") 'kill-whole-line)
    (global-set-key (kbd "C-x M-w") 'windmove-up)
    (global-set-key (kbd "C-x M-s") 'windmove-down)
    (global-set-key (kbd "C-x M-a") 'windmove-left)
    (global-set-key (kbd "C-x M-d") 'windmove-right)
    (global-set-key (kbd "s-<down>") 'shrink-window)
    (global-set-key (kbd "s-<up>") 'enlarge-window)
    (global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
    (global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
    (global-set-key (kbd "s-4") 'bookmark-bmenu-list)
    (global-set-key (kbd "C-b") 'bookmark-set)
    (global-set-key (kbd "M-b") 'bookmark-jump)
    (global-set-key (kbd "s-t") 'multi-vterm-dedicated-toggle)
    (global-set-key (kbd "s-M-t") 'vterm-copy-mode)
    (global-set-key (kbd "s-T") 'multi-vterm)
    (global-set-key (kbd "s-d") 'duplicate-line)
    (global-set-key (kbd "s-1") 'previous-buffer)
    (global-set-key (kbd "s-2") 'next-buffer)
    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") ' my/kill-ring-save)
    (global-set-key (kbd "s-x") 'my/kill-region)
    (global-set-key (kbd "s-/") 'comment-line)
    (global-set-key (kbd "M-SPC") 'newline-and-indent)
    (global-set-key (kbd "M--") 'set-mark-command)
    (global-set-key (kbd "M-=") 'rectangle-mark-mode)
    (global-set-key (kbd "s-3") 'ibuffer)
    (global-set-key (kbd "s-]") 'goto-last-change-reverse)
    (global-set-key (kbd "s-[") 'goto-last-change)
    (global-set-key (kbd "s-\\") 'consult-mark)
    (global-set-key (kbd "M-s-e") 'forward-paragraph)
    (global-set-key (kbd "M-s-q") 'backward-paragraph)
    (global-set-key (kbd "M-<tab>") 'move-line-or-region-right)
    (global-set-key (kbd "M-s-<tab>") 'move-line-or-region-left)
    (global-set-key (kbd "C-w") 'move-text-up)
    (global-set-key (kbd "C-s") 'move-text-down)
    (global-set-key (kbd "s-Z") 'undo-fu-only-redo)
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)
    (global-set-key (kbd "M-j") 'avy-goto-char-timer)
    (global-set-key (kbd "s-V") 'browse-kill-ring)
    (global-set-key (kbd "M-o") 'ace-window)
    (global-set-key (kbd "<f5>") 'revert-buffer)
    (global-set-key (kbd "C-k") 'kill-this-buffer)
    (global-set-key (kbd "C-M-d") 'last-half-delete-line)
    (global-set-key (kbd "C-M-a") 'first-half-delete-line)
    (global-set-key (kbd "M-i") 'consult-imenu)
    (global-set-key (kbd "<up>") 'comint-previous-input)
    (global-set-key (kbd "<down>") 'comint-next-input)
    (global-set-key (kbd "s-w") 'my/select-current-line-and-previous-line)
    (global-set-key (kbd "s-s") 'my/select-current-line-and-forward-line)
    (global-set-key (kbd "M-s-z") 'my/ls-activete-region)
    (global-set-key (kbd "M-p") 'back-to-indentation)
    (global-set-key (kbd "M-s-p") 'delete-horizontal-space)
    (global-set-key (kbd "s-g") 'consult-ripgrep)
    (global-set-key (kbd "s-M-g") 'consult-git-grep)
    (global-set-key (kbd "M-v") 'pyvenv-activate)
    (global-set-key (kbd "s-n") '+vc-gutter/next-hunk)
    (global-set-key (kbd "s-p") '+vc-gutter/previous-hunk)
    (global-set-key (kbd "s-b") 'magit-blame-addition)
    (global-set-key (kbd "s-M-q") 'beginning-of-defun)
    (global-set-key (kbd "s-M-e") 'end-of-defun)
    (global-set-key (kbd "s-e") 'my-mark-word)
    (global-set-key (kbd "C-e") 'my-mark-line)
    (global-set-key (kbd "s-q") 'my-mark-word-backward)
    (global-set-key (kbd "C-q") 'my-mark-line-backward)
    (global-set-key (kbd "s-A") 'mark-paragraph)
    (global-set-key (kbd "M-h") 'lsp-ui-doc-show)
    (global-set-key (kbd "s-C-v") 'my-replace-yank)
    (global-set-key (kbd "C-s-c") 'copy-word)
    (global-set-key (kbd "s-C-x") 'my-delete-line-this-line)
    (global-set-key (kbd "s-C-a") 'mark-defun)
    (global-set-key (kbd "C-<tab>") 'company-ispell)
    (global-set-key (kbd "M-0") 'zz/goto-match-paren)
    (global-set-key (kbd "C-t") 'google-translate-at-point)
    (global-set-key (kbd "M-m") 'kmacro-end-and-call-macro)
    (global-set-key (kbd "M-s--") 'my/mark-text-in-qutes)

    map))

(map! :leader
        (:prefix "b"
                :desc "black/format region" "r" #'+format/region
                :desc "black/format buffer" "b" #'+format/buffer
                ))

(map! :leader
        (:prefix "f"
                :desc "find file" "f" #'consult-find
                ))
(map! :leader
        (:prefix "s"
         :desc "sort lines" "s" #'sort-lines
                ))

(map! :leader
        (:prefix "d"
         :desc "set debug breakpoint" "s" #'my/set-breackpoint
         :desc "jump to breakpoint" "j" #'my/jump-breackpoint
         :desc "run django server" "d" #'my/django-runserver
         ))

(map! :leader
        (:prefix "f"
         :desc "history" "h" #'recentf-open-files
                ))

(map! :leader
        (:prefix "v"
         :desc "replace line and yank" "v" #'(lambda ()
                                              (interactive)
                                              (my-delete-line-this-line)
                                              (yank)
                                              ))
         )


(define-minor-mode my-keys-mode
 "Minor mode with the keys I use."
  :global t
  :init-value t
  :keymap my-keys-mode-map)

;;=======================================================
;;#######################################################
;;keymap end
;;#######################################################
;;=======================================================


;;=======================================================
;;#######################################################
;;use package
;;#######################################################
;;=======================================================

;; russian key to eng binding
(use-package! reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(use-package! tree-sitter
   :after python-mode
   :defer t
   :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (global-tree-sitter-mode)
  (add-hook!  'python-mode-hook  #'tree-sitter-hl-mode)
  (add-hook! 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

;;localize
(use-package! mule
  :defer 0.1
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))

;;set interpritatior
(use-package! python
  :config
  (setq python-shell-interpreter "python3"))

(use-package! tooltip
  :defer t
  :custom
  (tooltip-mode -1))

;;time
(use-package! time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-mode t))

(use-package! lsp-pyright
  :ensure t
  :init
    (setq lsp-pyright-multi-root nil)
    (setq lsp-enable-file-watchers nil)
    (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package! lsp-mode
  :commands (lsp)
  :diminish (lsp-mode . "lsp")
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-signature-doc-lines 5)
  (setq gc-cons-threshold 100000000)
  (setq lsp-restart 'ignore)
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root nil)
  (lsp-eldoc-enable-hover nil)
  ;; (lsp-signature-auto-activate nil)
  (lsp-completion-enable t)
)

(use-package! lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
   (set-lsp-priority!  'pyright 1)
  )

(use-package! flycheck
  :ensure t
  :config
  (setq flycheck-enabled-checkers '(python-flake8 python-mypy))
  (setq flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-select-checker 'python-flake8)
  (setq lsp-diagnostics-provider :auto)
  (setq flycheck-checkers (remove 'python-pylint flycheck-checkers))
  (setq flycheck-checkers (remove 'python-pycompile flycheck-checkers))
  (setq flycheck-checkers (remove 'python-pyright flycheck-checkers))
  :init (global-flycheck-mode))

;;company
(use-package! company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))

;;ispell
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '( :seperate
    company-ispell
    company-files
    company-yasnippet))

;;Toggle centered cursor
(use-package! centered-cursor-mode
  :demand
  :config
  (global-centered-cursor-mode))

;;ibuffer
(use-package! ibuffer-vc
  :defer t
  :ensure t
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-ivy--icon-for-mode major-mode))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

;; autocmplite
(use-package hippie-expand
  :bind
  ([remap dabbrev-expand] . hippie-expand))

;;go to last change buffer
(use-package! goto-chg)

;; replace text
(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("s-r" . iedit-mode))

;;kill-ring history
(use-package! browse-kill-ring)

;;vterm add active link
(use-package! vterm
  :config
  (setq! vterm-timer-delay 0.01)
  :hook
  (vterm-mode . goto-address-mode)
  (vterm-mode . (lambda () (centered-cursor-mode -1))))

;; translate
(use-package! google-translate
  :config
  (setq! google-translate-default-source-language "en")
  (setq! google-translate-default-target-language "ru")
  )

;;=======================================================
;;=======================================================
;;#######################################################
;;use package end
;;#######################################################
;;=======================================================
;;=======================================================


;;=======================================================
;;=======================================================
;;#######################################################
;; hooks
;;#######################################################
;;=======================================================
;;=======================================================

;; column line 79 symbol
(add-hook! 'prog-mode-hook 'display-fill-column-indicator-mode)

;; spell in text mode
(add-hook! 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))

;;=======================================================
;;=======================================================
;;#######################################################
;; hooks end
;;#######################################################
;;=======================================================
;;=======================================================
