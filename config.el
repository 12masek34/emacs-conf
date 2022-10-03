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
;;
;; =======================================================
;; =======================================================
;; load environment
(load! "~/.doom.d/env.el")
;; =======================================================
;;=======================================================
;;cursor
;; (setq-default cursor-type 'bar)
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
;; font
(setq doom-font (font-spec :family "Monaco" :size 13 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 12)
      doom-unicode-font (font-spec :family "Monaco")
      doom-big-font (font-spec :family "Monaco" :size 24))
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

;;=======================================================
(defvar my-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (global-set-key (kbd "M-<up>") (lambda () (interactive) (forward-line -10)))
    (global-set-key (kbd "M-<down>") (lambda () (interactive) (forward-line  10)))
    (global-set-key (kbd "M-<right>") 'forward-word)
    (global-set-key (kbd "M-<right>") 'forward-word)
    ;; switch window
    (global-set-key (kbd "C-x <up>") 'windmove-up)
    (global-set-key (kbd "C-x <down>") 'windmove-down)
    (global-set-key (kbd "C-x <left>") 'windmove-left)
    (global-set-key (kbd "C-x <right>") 'windmove-right)
    (global-set-key (kbd "s-<down>") 'end-of-buffer)
    (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
    ;;bookmarks
    (global-set-key (kbd "<f1>") 'bookmark-bmenu-list)
    (global-set-key (kbd "C-b") 'bookmark-set)
    (global-set-key (kbd "M-b") 'bookmark-jump)
    (global-set-key (kbd "C-d") 'bookmark-delete)

    (global-set-key (kbd "s-t") '+vterm/toggle)
    (global-set-key (kbd "s-T") 'vterm-other-window)
    (global-set-key (kbd "s-d") 'duplicate-line)

    ;; Map previous and next buffer to C-s-
    (global-set-key (kbd "C-s-<left>") 'previous-buffer)
    (global-set-key (kbd "C-s-<right>") 'next-buffer)

    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-x") 'kill-region)

    (global-set-key (kbd "s-/") 'comment-line)
    map))
;;=======================================================
(define-minor-mode my-keys-mode
 "Minor mode with the keys I use."
  :global t
  :init-value t
  :keymap my-keys-mode-map)
;; grep
(global-set-key (kbd "s-g") 'projectile-grep)
;;=======================================================
;;fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;=======================================================
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
  (setq gc-cons-threshold 100000000))

;;=======================================================
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

(map! :leader
        (:prefix "b"
                :desc "find definition" "f" #'lsp-find-definition
                :desc "find declaration" "F" #'lsp-find-declaration
                :desc "dap breakpoint toggle" "b" #'dap-breakpoint-toggle
                :desc "dap breakpoint delete" "B" #'dap-breakpoint-delete
                :desc "dap repl" "r" #'dap-ui-repl
                :desc "dap debug hydra" "u" #'dap-hydra))



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
