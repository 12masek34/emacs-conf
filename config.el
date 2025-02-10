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
(setq! which-key-idle-delay 0.05)

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
(setq! lsp-sqls-timeout 300)
(setq! lsp-response-timeout 300)

;; magit
(with-eval-after-load "magit"
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-local-branches))

;; dired
(setq! dired-omit-files nil)

;; remove line wrapping
(setq! global-visual-line-mode nil)
(setq! truncate-lines t)

;; search engine eww
(setq! eww-search-prefix "https://www.google.com/search?q=")

;; ispell multiple config
(with-eval-after-load "ispell"
(setq! ispell-program-name "hunspell")
(setq! ispell-local-dictionary "ru_RU,en_US")
(setq! ispell-dictionary "ru_RU,en_US")
(ispell-set-spellchecker-params)
(ispell-hunspell-add-multi-dic "ru_RU,en_US"))

;; sql formatter
(setq sqlformat-command 'pgformatter)

;; ChatGPT config
(setq! chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
(setq! chatgpt-shell-insert-dividers t)

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

(defun my/set-logging ()
  (interactive)
  (when (equal major-mode 'python-mode)
    (add-py-logging))
  )

(defun add-py-logging ()
  "add logging"
  (interactive)
  (highlight-regexp "import logging; logging.error();" 'company-echo-common)
  (save-excursion (insert "import logging; logging.error();")))


(defun add-py-debug ()
  "add debug code and move line down"
  (interactive)
  (highlight-regexp "import pdb; pdb.set_trace();" 'company-echo-common)
  (save-excursion (insert "import pdb; pdb.set_trace();")))

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

(defun eww-new ()
  "Open new buffer by eww"
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "eww"))
    (eww-mode)
    (eww url)))

(defun my/apply-function-to-region-lines (fn)
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
          next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
          (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (funcall fn)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))

(defun my/wrap-qute-line()
  (move-beginning-of-line nil)
  (indent-for-tab-command)
  (insert "\"")
  (move-end-of-line nil)
  (insert "\","))

(defun my/wrap-qute-all-line ()
  (interactive)
  (my/apply-function-to-region-lines 'my/wrap-qute-line))

(defun my/start_vpn ()
  "Start a randomly chosen VPN in the background."
  (interactive)
  (let* ((default-directory (expand-file-name "~/Downloads/hideme_240095281740650/other os/"))
         (vpn-configs '("Belgium, Oostkamp S8.ovpn"
                        "Czech Republic, Prague S7.ovpn"
                        "Germany, Offenbach S3.ovpn"
                        "Netherlands, Amsterdam S2.ovpn"
                        "Spain, Madrid.ovpn"
                        "United Kingdom, London L1.ovpn"
                        "Canada, Chambly ROUTERS.ovpn"
                        "Denmark, Copenhagen S8.ovpn"
                        "Italy, Milan S2.ovpn"
                        "Norway, Fredrikstad ROUTERS.ovpn"
                        "Turkey, Istanbul ROUTERS.ovpn"
                        "Canada, Laval ROUTERS.ovpn"
                        "France, Gravelines.ovpn"
                        "Kazakhstan, Almaty S3.ovpn"
                        "Norway, Moss ROUTERS.ovpn"))
         (sudo-password (getenv "SUDOPASS"))
         (random-config (nth (random (length vpn-configs)) vpn-configs)))
    (message "Starting VPN with config: %s" random-config)
    (start-process
     "openvpn-process"
     "*openvpn-output*"
     "bash" "-c"
     (format "echo %s | sudo -S openvpn --config '%s'" sudo-password random-config))
    (message "VPN process started in background with config: %s" random-config)))

(defun my/stop_vpn ()
  "Kill OpneVPN."
  (interactive)
  (let* ((sudo-password (getenv "SUDOPASS")))
    (message "Kill openVPN")
    (start-process
     "openvpn-process"
     "*openvpn-output*"
     "bash" "-c"
     (format "echo %s | sudo -S killall openvpn" sudo-password))
    (message "Kill OpneVPN process.")))

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

(defun my/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun my/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

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
    (global-set-key (kbd "M-k") 'shrink-window)
    (global-set-key (kbd "M-j") 'enlarge-window)
    (global-set-key (kbd "M-h") 'shrink-window-horizontally)
    (global-set-key (kbd "M-l") 'enlarge-window-horizontally)
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)
    (global-set-key (kbd "s-V") 'consult-yank-from-kill-ring)
    (global-set-key (kbd "<f5>") 'revert-buffer)
    (global-set-key (kbd "s-r") 'iedit-mode)
    (global-set-key (kbd "C-x 2") 'my/split-window-vertically)
    (global-set-key (kbd "C-x 3") 'my/split-window-horizontally)

    (evil-define-key 'normal 'global (kbd "C-d") '(lambda () (interactive) (forward-line  10)))
    (evil-define-key 'normal 'global (kbd "C-u") '(lambda () (interactive) (forward-line  -10)))

    map))

(define-minor-mode my-keys-mode
  "Minor mode with the keys I use."
  :global t
  :init-value t
  :keymap my-keys-mode-map)

(map! :leader
        (:prefix "c"
                :desc "camel case to snake case" ";" #'my/toggle-camelcase-underscores
                :desc "python black region" "b" #'python-black-region
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
       :desc "execute sql region" "e" #'lsp-sql-execute-paragraph
       :desc "execute sql paragraph" "p" #'lsp-sql-execute-paragraph
       :desc "set logging" "l" #'my/set-logging
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
(map! :leader
      (:prefix "t"
       :desc "google-translate-at-point" "t" #'google-translate-at-point
       :desc "google-translate-at-point-reverse" "r" #'google-translate-at-point-reverse
       :desc "google-translate-at-point" "T" #'google-translate-query-translate
       :desc "google-translate-at-point-reverse" "R" #'google-translate-query-translate-reverse
       ))
(map! :leader
      (:prefix "w"
       :desc "google-translate-at-point" "w" #'ace-window
       ))
(map! :leader
      (:prefix "o"
       :desc "other-frame" "o" #'other-frame
       ))
(map! :leader
        (:prefix "s"
                :desc "eww" "g" #'eww-new
                ))
(map! :leader
        (:prefix "e"
                :desc "my/execute-python-region" "r" #'my/execute-python-region
                :desc "my/execute-python-buffer" "b" #'my/execute-python-buffer
                ))
(map! :leader
        (:prefix "y"
                :desc "my/requst-yandex-gpt-input" "i" #'my/requst-yandex-gpt-input
                :desc "my/open-yandex-gpt-log" "l" #'my/open-yandex-gpt-log
                :desc "my/requst-yandex-gpt-system" "s" #'my/requst-yandex-gpt-system
                :desc "ChatGPT" "y" #'chatgpt-shell
                :desc "ChatGPT region" "r" #'chatgpt-shell-send-and-review-region
                ))
(map! :leader
        (:prefix "\""
                :desc "my/wrap-qute-all-line" "\"" #'my/wrap-qute-all-line
                ))
(map! :leader
        (:prefix "o"
                :desc "other-frame" "o" #'other-frame
                ))
(map! :leader
        (:prefix "l"
                :desc "toggle-truncate-lines" "t" #'toggle-truncate-lines
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

;; lsp-pyright
(use-package! lsp-pyright
  :init
  (setq lsp-pyright-multi-root nil
        lsp-enable-file-watchers nil
        lsp-pyright-python-executable-cmd (or (executable-find "python3") python-shell-interpreter))
  :hook (python-mode . lsp-deferred))

;; lsp-mode
(use-package! lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred))
  :config
  (setq lsp-use-plists t
        lsp-enable-file-watchers nil
        lsp-completion-provider :capf
        lsp-idle-delay 0.05
        lsp-signature-doc-lines 5
        gc-cons-threshold (* 100 1024 1024)  ;; 100MB во время работы
        lsp-restart 'ignore)
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-completion-enable t))

;; company
(use-package! company
  :defer t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  :config
  (setq company-backends '(company-capf company-dabbrev company-keywords company-files)))

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
  (consult-customize +default/search-project :preview-key 'any))

;;fix doom format (temporary)
(use-package! python-black
  :demand t
  :after python)
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

;; Сброс gc-cons-threshold после загрузки LSP
(add-hook! 'lsp-after-initialize-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))


(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

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

;;yandex gpt integration
(load! "~/.doom.d/yandex_gpt.el")

;;=======================================================
;;=======================================================
;;#######################################################
;; load end
;;#######################################################
;;=======================================================
;;=======================================================
