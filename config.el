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
;;#######################################################
;;base config
;;#######################################################
;;=======================================================


;; load environment
(setenv "LSP_USE_PLISTS" "1")
(setenv "LC_ALL" "en_US.UTF-8")

;; column line long 120 char
(setq! display-fill-column-indicator-column 120)

;; font
 (setq! doom-font (font-spec :family "Hack" :size 15 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Hack" :size 13)
      doom-unicode-font (font-spec :family "Hack")
      doom-big-font (font-spec :family "Hack" :size 24))

;; dont add end allow line
 (setq! mode-require-final-newline nil)

(setq! python-shell-completion-native-disabled-interpreters '("python3"))

;;replace name buffer
(setq! doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(setq! read-process-output-max (* 2048 2048))
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; delay buffer hint
(setq! which-key-idle-delay 0.5)

;; Disable backup
(setq! make-backup-files nil)

;; disable dialog window
(setq! use-dialog-box nil)

;; auto read on disc file
(global-auto-revert-mode 1)
(setq! global-auto-revert-non-file-buffers t)

;; limit
(setq! undo-limit 80000000)
(setq! scroll-margin 2)

;; minimal size buffer
(setq! window-safe-min-height 10)

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

(setq! scroll-margin 7)

(winner-mode 1)

;; Delete selection
(delete-selection-mode t)
(global-superword-mode t)

;; lsp sql sever
(setq! lsp-sqls-server "~/go/bin/sqls")
(setq! lsp-sqls-workspace-config-path nil)
(setq! lsp-sqls-connections nil)
(setq! sql-connection-alist nil)

(with-eval-after-load "magit"
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches))

;; dired
(setq! dired-omit-files nil)

;; remove line wrapping
(setq! global-visual-line-mode nil)
;;=======================================================
;;#######################################################
;;base config end
;;#######################################################
;;=======================================================


;;=======================================================
;;#######################################################
;;my custom function
;;#######################################################
;;=======================================================

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

(defun my/toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;;;###autoload
(defmacro any-nil? (&rest args)
  `(not (and ,@args)))

;;;###autoload
(defmacro throw-if (condition &optional error-description)
  "if condition is true, thrown an error"
  `(if ,condition (error (or ,error-description ""))))

;;;###autoload
(defun format-postgres-sqls (host port user password db)
  (format "host=%s port=%s user=%s password=%s dbname=%s"
          host port user password db))

;;;###autoload
(defun format-mysql-sqls (host port user password db)
  (format "%s:%s@tcp(%s:%s)/%s" user password host port db))

;;;###autoload
(defun format-postgres-uri (host port user password db)
  (format "postgresql://%s:%s@%s:%s/%s" user password host port db))


;;;###autoload
(defun add-to-sqls-connections (db-type data-src-name)
  (add-to-list 'lsp-sqls-connections
               (list (cons 'driver db-type)
                     (cons 'dataSourceName data-src-name))))

;;;###autoload
(defmacro add-to-sql-conection-alist (db-type name host port user password db)
  `(add-to-list 'sql-connection-alist
                (list (quote ,name)
                     (list 'sql-product (quote ,db-type))
                     (list 'sql-user ,user)
                     (list 'sql-server ,host)
                     (list 'sql-port ,port)
                     (list 'sql-password ,password)
                     (list 'sql-database ,db))))

;;;###autoload
(defmacro sql-add-postgres-db (name &rest db-info)
  "Adds a mysql database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 5432
   e.g.:
   (sql-add-postgres-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 5432))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (let ((full-uri (format-postgres-uri host port user password db))
           (data-src-name (format-postgres-sqls host port user password db)))
       (add-to-sqls-connections "postgresql" data-src-name)
       (add-to-sql-conection-alist 'postgres ,name host port user password full-uri))))

;;;###autoload
(defmacro sql-add-mysql-db (name &rest db-info)
  "Adds a mysql database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 3306
   e.g.:
   (sql-add-mysql-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 3306))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (add-to-sqls-connections "mysql" (format-mysql-sqls host port user password db))
     (add-to-sql-conection-alist 'mysql ,name host port user password db)))

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

(defvar my-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (global-set-key (kbd "s-k") 'shrink-window)
    (global-set-key (kbd "s-j") 'enlarge-window)
    (global-set-key (kbd "s-h") 'shrink-window-horizontally)
    (global-set-key (kbd "s-l") 'enlarge-window-horizontally)
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)
    (global-set-key (kbd "s-V") 'consult-yank-from-kill-ring)
    (global-set-key (kbd "<f5>") 'revert-buffer)
    (global-set-key (kbd "C-k") 'kill-this-buffer)
    (global-set-key (kbd "s-n") '+vc-gutter/next-hunk)
    (global-set-key (kbd "s-p") '+vc-gutter/previous-hunk)
    (global-set-key (kbd "s-b") 'magit-blame-addition)
    (global-set-key (kbd "C-t") 'google-translate-at-point)
    (global-set-key (kbd "s-;") 'my/toggle-camelcase-underscores)
    (global-set-key (kbd "s-r") 'iedit-mode)

    (evil-define-key 'normal 'global (kbd "C-t") 'google-translate-at-point)
    (evil-define-key 'normal 'global (kbd "C-d") '(lambda () (interactive) (forward-line  10)))
    (evil-define-key 'normal 'global (kbd "C-u") '(lambda () (interactive) (forward-line  -10)))

    map))

(define-minor-mode my-keys-mode
 "Minor mode with the keys I use."
  :global t
  :init-value t
  :keymap my-keys-mode-map)

(map! :leader
        (:prefix "b"
                :desc "black/format region" "r" #'+format/region
                ))

(map! :leader
        (:prefix "i"
                :desc "isort format python import" "i" #'py-isort-region
                ))

(map! :leader
        (:prefix "d"
         ;; debug
         :desc "set debug breakpoint" "s" #'my/set-breackpoint
         ;; ddatabase
         :desc "switch connection to db" "c" #'lsp-sql-switch-connection
         :desc "switch database" "d" #'lsp-sql-switch-database
         :desc "execute sql query" "e" #'lsp-sql-execute-query
         ))

(map! :leader
        (:prefix "m"
                :desc "mc/edit-lines" "m" #'mc/edit-lines
                ))


(map! :leader
        (:prefix "r"
                :desc "restclient-http-send-current" "e" #'restclient-http-send-current
                ))

(map! :leader
        (:prefix "j"
                :desc "json-pretty-print" "p" #'json-pretty-print
                ))

(map! :leader
        (:prefix "i"
                :desc "ispell-change-dictionary" "d" #'ispell-change-dictionary
                :desc "ispell-region" "r" #'ispell-region
                ))
(map! :leader
        (:prefix "v"
                :desc "consult-yank-from-kill-ring" "v" #'consult-yank-from-kill-ring
                ))

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

;;set interpritatior
(use-package! python
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-def-block-scale 1)
  (setq python-shell-interpreter "python3"))

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
  (setq lsp-use-plists "true")
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-signature-doc-lines 5)
  (setq gc-cons-threshold 100000000)
  (setq lsp-restart 'ignore)
  :custom
  (lsp-keep-workspace-alive nil)
  ;; lsp-workspace-folders-remove - remove source project
  ;; lsp-workspace-folders-add - add new source project
  (lsp-auto-guess-root nil)
  (lsp-eldoc-enable-hover nil)
  ;; (lsp-signature-auto-activate nil)
  (lsp-completion-enable t)
)

(use-package! flycheck
  :ensure t
  :config
  (setq flycheck-select-checker 'python-pyright)
  (setq lsp-diagnostics-provider :auto)
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

;; replace text
(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magenta"))

;;vterm add active link
(use-package! vterm
  :config
  (setq! vterm-timer-delay nil)
  :hook
  (vterm-mode . goto-address-mode))

;; translate
(use-package! google-translate
  :config
  (setq! google-translate-default-source-language "en")
  (setq! google-translate-default-target-language "ru")
  )

;;consult
(use-package! consult
  :config
  (setq! consult-locate-args "mdfind"))

;; consult
(consult-customize +default/search-project :preview-key 'any)

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

;; spell in text mode
(add-hook! 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))

;; indent to js mode
(add-hook! 'js2-mode-hook
  (setq js-indent-level 4)
  (setq js2-basic-offset 4))

;; lsp sql
(add-hook! 'sql-mode-hook 'lsp)

;; column indicator
(add-hook!'prog-mode-hook #'display-fill-column-indicator-mode)

(remove-hook! 'doom-first-input-hook #'evil-snipe-mode)

;;=======================================================
;;=======================================================
;;#######################################################
;; hooks end
;;#######################################################
;;=======================================================
;;=======================================================

;;=======================================================
;;=======================================================
;;#######################################################
;; load
;;#######################################################
;;=======================================================
;;=======================================================

;; config db
(load! "~/.doom.d/database.el")

;;private
(load! "~/.doom.d/env.el")

;;=======================================================
;;=======================================================
;;#######################################################
;; load end
;;#######################################################
;;=======================================================
;;=======================================================