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
(setq doom-theme 'doom-spacegrey)
;; (setq doom-theme 'doom-zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
;;localize
(use-package mule
  :defer 0.1
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))
;; =======================================================
;; =======================================================
;; load environment
(load! "~/.doom.d/env.el")
;; ======================================================
;; ======================================================
;; dont add end allow line
 (setq mode-require-final-newline nil)
;;=======================================================
;;cursor
;; (setq-default cursor-type 'bar)
;;=======================================================
;;  relative number line
(setq display-line-numbers-type 'relative)
;;=======================================================
;; makes the column number show up
(column-number-mode 1)
;;=======================================================
;;=======================================================
;;replace name buffer
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
;;=======================================================
;; Turn on warn highlighting for characters outside of the 'width' char limit
(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
   that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
;; ;;=======================================================
;; delay buffer hint
(setq which-key-idle-delay 0.5)
;;=======================================================
;;;; Delete selection
(delete-selection-mode t)
;;=======================================================
;;;; Disable backup
(setq make-backup-files nil)
;;=======================================================
;;column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;;=======================================================
(winner-mode 1)
;;=======================================================
;;disable dialog window
(setq use-dialog-box nil)
;;=======================================================
;; auto read on disc file
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;;=======================================================
(use-package! tooltip
  :defer t
  :custom
  (tooltip-mode -1))

;;time
(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-mode t))
;;=======================================================
;; font
(setq doom-font (font-spec :family "Monaco" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 13)
      doom-unicode-font (font-spec :family "Monaco")
      doom-big-font (font-spec :family "Monaco" :size 24))
;;=======================================================
(whole-line-or-region-global-mode)
;;=======================================================
;;key map
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
;;=======================================================
;;light long line
;; (add-hook 'python-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{80\\}" 'hi-salmon)))
;;=======================================================
;;light import pdb
(add-hook 'python-mode-hook '(lambda () (highlight-lines-matching-regexp "import ipdb; ipdb.set_trace();" 'hi-aquamarine)))
;;=======================================================
;;limit
(setq undo-limit 80000000)
(setq scroll-margin 2)
;;=======================================================
;;keymap
;;
(map! :after smartparens
      :map smartparens-mode-map
      "C-d" nil
      "C-a" nil
      "M-C-d" nil
      "M-C-a" nil
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


(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map (kbd "C-d") nil)
  )


(defvar my-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (global-set-key (kbd "M-<up>") (lambda () (interactive) (forward-line -10)))
    (global-set-key (kbd "M-<down>") (lambda () (interactive) (forward-line  10)))
    (global-set-key (kbd "M-<right>") 'forward-word)
    (global-set-key (kbd "M-<left>") 'backward-word)

    (global-unset-key (kbd "M-w"))
    (global-set-key (kbd "M-w") 'previous-line)

    (global-unset-key (kbd "M-s"))
    (global-set-key (kbd "M-s") 'next-line)

    (global-unset-key (kbd "M-a"))
    (global-set-key (kbd "M-a") 'backward-char)

    (global-unset-key (kbd "M-d"))
    (global-set-key (kbd "M-d") 'forward-char)

    (global-unset-key (kbd "M-q"))
    (global-set-key (kbd "M-q") 'backward-word)

    (global-unset-key (kbd "M-e"))
    (global-set-key (kbd "M-e") 'forward-word)

    (global-unset-key (kbd "M-s-w"))
    (global-set-key (kbd "M-s-w") (lambda () (interactive) (forward-line -10)))

    (global-unset-key (kbd "M-s-s"))
    (global-set-key (kbd "M-s-s") (lambda () (interactive) (forward-line  10)))

    (global-unset-key (kbd "M-C-s"))
    (global-set-key (kbd "M-C-s") 'end-of-buffer)

    (global-unset-key (kbd "M-C-w"))
    (global-set-key (kbd "M-C-w") 'beginning-of-buffer)

    (global-unset-key (kbd "M-s-d"))
    (global-set-key (kbd "M-s-d") 'end-of-line)

    (global-unset-key (kbd "M-s-a"))
    (global-set-key (kbd "M-s-a") 'beginning-of-line)

    (global-unset-key (kbd "C-d"))
    (global-set-key (kbd "C-d") 'sp-delete-word)

    (global-unset-key (kbd "C-a"))
    (global-set-key (kbd "C-a") 'sp-backward-delete-word)

    (global-unset-key (kbd "C-s"))
    (global-set-key (kbd "C-s") 'kill-whole-line)

    (global-unset-key (kbd "s-["))
    (global-set-key (kbd "s-[") 'pop-global-mark)

    ;; switch window
    (global-set-key (kbd "C-x M-w") 'windmove-up)
    (global-set-key (kbd "C-x M-s") 'windmove-down)
    (global-set-key (kbd "C-x M-a") 'windmove-left)
    (global-set-key (kbd "C-x M-d") 'windmove-right)

    (global-set-key (kbd "s-<down>") 'end-of-buffer)
    (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
    ;;bookmarks
    (global-set-key (kbd "s-4") 'bookmark-bmenu-list)
    (global-set-key (kbd "C-b") 'bookmark-set)
    (global-set-key (kbd "M-b") 'bookmark-jump)

    (global-set-key (kbd "s-t") '+vterm/toggle)
    (global-set-key (kbd "s-T") 'vterm-other-window)
    (global-set-key (kbd "s-d") 'duplicate-line)

    (global-set-key (kbd "s-1") 'previous-buffer)
    (global-set-key (kbd "s-2") 'next-buffer)

    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-x") 'kill-region)

    (global-set-key (kbd "s-/") 'comment-line)

    (global-set-key (kbd "M-SPC") 'newline-and-indent)

    (global-set-key (kbd "M--") 'set-mark-command)

    (global-set-key (kbd "s-3") 'ibuffer)

    (global-set-key (kbd "M-]") 'goto-last-change-reverse)
    (global-set-key (kbd "M-[") 'goto-last-change)

    (global-set-key (kbd "M-s-e") 'forward-paragraph)
    (global-set-key (kbd "M-s-q") 'backward-paragraph)

    (global-set-key (kbd "M-<tab>") 'whole-line-or-region-indent-rigidly-right-to-tab-stop)
    (global-set-key (kbd "M-s-<tab>") 'whole-line-or-region-indent-rigidly-left-to-tab-stop)

    (global-set-key (kbd "C-w") 'move-text-up)
    (global-set-key (kbd "C-s") 'move-text-down)

    (global-set-key (kbd "s-Z") 'undo-fu-only-redo)

    (global-unset-key (kbd "s-="))
    (global-unset-key (kbd "s--"))
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)

    (global-set-key (kbd "M-j") 'avy-goto-char-timer)

    map))


(map! :leader
        (:prefix "b"
                :desc "find definition" "f" #'lsp-find-definition
                :desc "find declaration" "F" #'lsp-find-declaration
                :desc "dap breakpoint toggle" "b" #'dap-breakpoint-toggle
                :desc "dap breakpoint delete" "B" #'dap-breakpoint-delete
                :desc "dap repl" "r" #'dap-ui-repl
                :desc "dap debug hydra" "u" #'dap-hydra))

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
         :desc "set breakpoint" "s" #'add-py-debug
         :desc "remove breakpoint" "r" #'remove-py-debug
         :desc "jump to breakpoint" "j" #'(lambda ()
                                 (interactive)
                                 (search-forward-regexp "^[ ]*import ipdb; ipdb.set_trace();")
                                 (move-beginning-of-line 1))
                ))

(map! :leader
        (:prefix "f"
         :desc "history" "h" #'recentf-open-files
                ))

;; jump to next bracket (->  )
(after! smartparens
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
  (map! "M-0" 'zz/goto-match-paren))

;;=======================================================
;; russian key to eng
(loop
  for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
  for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
  do
  (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
  (eval `(define-key key-translation-map (kbd ,(concat "s-" (string from))) (kbd ,(concat "s-" (string to)))))
  (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to))))))
;;=======================================================
(define-minor-mode my-keys-mode
 "Minor mode with the keys I use."
  :global t
  :init-value t
  :keymap my-keys-mode-map)
;; grep
(global-set-key (kbd "s-g") 'consult-ripgrep)
;;=======================================================
;;fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;=======================================================
;;autoforma js
;; ( add-hook 'js2-mode-hook #'format-all-mode)
;; =======================================================
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defun my/tree-sitter-query-interface ()
  (interactive)
  (remove-overlays)
  (when-let*
      ((patterns "((struct_specifier (field_declaration_list (field_declaration (type_identifier) @funcRetType (function_declarator (parenthesized_declarator (pointer_declarator (field_identifier) @funcName)) (parameter_list) @args)))) (type_identifier) @ifType)")
       (query
        (condition-case err
            (tsc-make-query tree-sitter-language patterns)

          ((tsc-query-invalid-node-type
            tsc-query-invalid-field
            tsc-query-invalid-capture)
           (message "%s: %s" (get (car err) 'error-message) (cadr err))
           nil)
          (tsc-query-invalid
           (message "%s" (get (car err) 'error-message))
           nil)))
       (root-node (tsc-root-node tree-sitter-tree))
       (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties))
       )
    (if (= (length captures) 0)
        (message "No captures found")
      (let* ((interface '()) (func '()))
        (mapc (lambda (element)
                (cond ((assoc (car element) func)
                       (setq interface (cons func interface))
                       (setq func '()))
                      (t (setq func (cons element func))))
                )
                captures)
        interface)
      )))

(defun my/tree-sitter-gen-if-impl ()
  (interactive)
  (remove-overlays)
  (let* ((interface (my/tree-sitter-query-interface))
        (ifstr "ifType")
        (methodFStr "\nstatic %s %s%s {\n\tCHECK(inst, return ERROR);\n\timpl_t *const me = CONTAINER_OF(inst, impl_t, itf);\n\treturn OK;\n}\n")
        (implFStr "typedef struct {\n\t%s itf;\n} impl_t;\n")
        (ifFStr "static %s itf = {\n%s\n};\n")
        (ifName "")
        (funcNamesStr "")
        (fullStr "")
        (currentImpl "")
        (currentFunc ""))
        (mapc (lambda (func)
                ;; (print (tsc-node-text func))
                (cond ((and (assoc-string "funcRetType" func)
                            (assoc-string "funcName" func)
                            (assoc-string "args" func))
                      (setq funcNamesStr (concat funcNamesStr (format "\n\t.%s = %s,\n"
                                                                      (tsc-node-text
                                                                       (cdr (assoc-string "funcName" func)))
                                                                      (tsc-node-text
                                                                       (cdr (assoc-string "funcName" func))))))
                      (let* ((ifSym_funcRetType (tsc-node-text (cdr (assoc-string "funcRetType" func))))
                             (ifSym_funcName (tsc-node-text (cdr (assoc-string "funcName" func))))
                             (ifSym_args (tsc-node-text (cdr (assoc-string "args" func)))))
                        (setq currentFunc (format methodFStr ifSym_funcRetType ifSym_funcName ifSym_args))
                        (setq fullStr (concat fullStr currentFunc))
                        ))
                        ;; (yas-expand-snippet ifSnip nil nil '((ifSym_funcRetType)
                        ;;                                      (ifSym_funcName)
                        ;;                                      (ifSym_args))))
                        ((assoc-string "ifType" func)
                        ;; TODO make this more robust
                        (setq ifName (tsc-node-text (cdr (assoc-string "ifType" func))))
                        (setq currentImpl (format implFStr ifName))
                        )))
                interface)
                ;; Puts the result in the kill ring
                (kill-new (concat currentImpl fullStr (format ifFStr ifName funcNamesStr)))
                )
  )


(defun my/tree-sitter-get-function-node ()
  (interactive)
  (when-let*
      ((cursor (tsc-make-cursor tree-sitter-tree))
       (root (tsc-root-node tree-sitter-tree)))
    ;; (tsc-reset-cursor cursor (tree-sitter-node-at-pos))
    (tsc-goto-first-child-for-position cursor (point))
    (if (eq 'function_definition (tsc-node-type (tsc-current-node cursor))) (tsc-current-node cursor) nil)))

( use-package tree-sitter
   :after python-mode
   :defer t
   :config
  ( require  ' tree-sitter )
  ( require  ' tree-sitter-langs )
  ( require  ' tree-sitter-hl )
  ( add-hook  ' python-mode -hook  #' Tree-sitter-hl-mode )
  )
;;=======================================================
;;set interpritatior
(setq python-shell-completion-native-disabled-interpreters '("python3"))
;;=======================================================
;; =======================================================

(after! lsp-python-ms
   (set-lsp-priority!  'pyright 1))

(setq read-process-output-max (* 1024 1024))

( after! lsp-mode
   ( setq lsp-restart 'ignore))

(after! ess
  (setq ess-eval-visibly 'nowait))

(after! ess
  (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:%op% . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t))))

(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


(use-package! lsp-mode
  :diminish (lsp-mode . "lsp")
  :config
  (lsp-treemacs-sync-mode 1)
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.25)
  (setq gc-cons-threshold 100000000)

  ;; (setq lsp-ui-doc-enable t)
  ;; (setq lsp-completion-show-label-description t)
  ;; (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-completion-show-label-description t)
)


(setq lsp-enable-file-watchers nil)
;;=======================================================
;;company
(setq company-idle-delay 0)
(setq company-show-numbers t)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
;;=======================================================
;;ispell
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '( :seperate
    company-ispell
    company-files
    company-yasnippet))

;;=======================================================
;;aas
( use-package! aas
   :commands aas-mode)
;;=======================================================
(setq yas-triggers-in-field t)
;;=======================================================
;;=======================================================
(defun add-py-debug ()
      "add debug code and move line down"
    (interactive)
    (move-beginning-of-line 1)
    (insert "import ipdb; ipdb.set_trace();\n"))

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
;;=======================================================
;; debuger
(defun debugging-mode ()
  (interactive)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode)
  (dap-ui-controls-mode 1)
  ;; (dap-ui-sessions)
  (dap-ui-locals)
  ;; (dap-ui-breakpoints)
  (dap-ui-repl)
  )


(defun stop-debugging-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode 0)
  (dap-ui-mode 0)
  (dap-ui-controls-mode 0)
  (delete-other-windows) ;; hide all the dap UI. I might want to delete the buffers as well.
  (kill-buffer "*dap-ui-repl*")
  )


(use-package! dap-mode
  :diminish dap-mode
  :after(lsp-mode)

  :ensure t
  :config
  (require 'dap-python)
  (require 'dap-ui)
  (setq dap-python-debugger 'debugpy)
  (setq dap-auto-configure-features '(locals controls tooltip))

  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
    (dap-session-created . (lambda (&_rest) (debugging-mode)))
    (dap-terminated . (lambda (&_rest) (stop-debugging-mode)))))


(use-package! lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package! treemacs
  :config (treemacs-git-mode 'deferred)
  :commands (treemacs)
  :after (lsp-mode))

;;=======================================================
(use-package! semantic
  :init
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
    (semantic-mode 1)
    (require 'stickyfunc-enhance))
;; =======================================================
;; load debug templates
(load! "~/.doom.d/debug-templates.el")
;;=======================================================
;; Extra ligatures
(setq +ligatures-extras-in-modes
      '(not special-mode comint-mode eshell-mode term-mode vterm-mode python-mode))
;;=======================================================
;;Toggle centered cursor
(use-package! centered-cursor-mode
  :demand
  :config
  (global-centered-cursor-mode))
;;=======================================================
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
;;=======================================================
;; autocmplite
(use-package hippie-expand
  :bind
  ([remap dabbrev-expand] . hippie-expand))
;;=======================================================
(use-package! goto-chg)
;;=======================================================
;;marginalia
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))
;;=======================================================
;; enable on-the-fly spell checking
(setq flyspell-use-meta-tab nil)
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))
;; prevent flyspell from finding misspellings in code
(add-hook 'prog-mode-hook
          (lambda ()
            ;; `ispell-comments-and-strings'
            (flyspell-prog-mode)))
;;=======================================================
