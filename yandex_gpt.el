
(setq YANDEX_API_KEY (getenv "YANDEX_API_KEY"))
(setq YANDEX_FOLDER_ID (getenv "YANDEX_FOLDER_ID"))
(setq YANDEX_GPT_LOG_FILE (getenv "YANDEX_GPT_LOG_FILE"))
(setq BALANCE_API_KEY (getenv "BALANCE_API_KEY"))
(setq BILLING_ACCOUNT_ID (getenv "BILLING_ACCOUNT_ID"))
(setq temperature "0.6")
(setq maxTokens "2000")
(setq separator_request ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
(setq separator_response "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
(setq request_id nil)
(setq balance "unknow")


(defun my/open-yandex-gpt-log ()
  (interactive)
  (my/open-temp-buffer YANDEX_GPT_LOG_FILE))

(defun my/append-to-file (text file)
  (with-temp-buffer
    (insert text)
    (append-to-file (point-min) (point-max) file)))

(defun my/open-temp-buffer (file)
  (with-output-to-temp-buffer "*Yandex GPT*"
    (set-buffer "*Yandex GPT*")
    (insert-file-contents file)
    (markdown-mode))
  (end-of-buffer))

(defun my/insert-text-and-open-temp-buffer (text file)
  (my/append-to-file text file)
  (my/open-temp-buffer file))

(defun my/requst-yandex-gpt-region ()
  (interactive)
  (setq text (buffer-substring (region-beginning) (region-end)))
  (my/requst-yandex-gpt))

(defun my/requst-yandex-gpt-input ()
  (interactive)
  (setq text (read-from-minibuffer "Yandex gpt: "))
  (my/requst-yandex-gpt))

(defun my/requst-yandex-gpt-system ()
  (interactive)
  (setq text (buffer-substring (region-beginning) (region-end)))
  (my/requst-yandex-gpt-by-system (read-from-minibuffer "Yandex gpt: ")))

(defun my/get-balance ()
  (request "https://iam.api.cloud.yandex.net/iam/v1/tokens"
        :type "POST"
        :headers '(("Content-Type" . "application/json"))
        :data (json-encode `(("yandexPassportOauthToken" . ,BALANCE_API_KEY)))
        :parser 'json-read
        :sync t
        :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq iam_token (assoc-default 'iamToken data))
                  (my/request-get-balance)))))

(defun my/request-get-balance ()
  (request (concat "https://billing.api.cloud.yandex.net/billing/v1/billingAccounts/" BILLING_ACCOUNT_ID)
        :type "GET"
        :headers `(("Authorization" . ,(concat "Bearer " iam_token)))
        :parser 'json-read
        :sync t
        :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq balance (assoc-default 'balance data))))))

(defun my/response-yandex-gpt ()
  (interactive)
  (sit-for 1)
  (request (concat "https://llm.api.cloud.yandex.net/operations/" request_id)
    :type "GET"
    :headers `(("Authorization" . ,(concat "Api-Key " YANDEX_API_KEY)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (if (assoc-default 'response data)
                    (my/insert-text-and-open-temp-buffer (concat "\n" separator_response "\n" (assoc-default 'text (assoc-default 'message (aref (assoc-default 'alternatives (assoc-default 'response data)) 0))) "\n" separator_response "\n\n") YANDEX_GPT_LOG_FILE)
                  (message "wait response yandex gpt...")
                  (my/response-yandex-gpt))))))

(defun my/requst-yandex-gpt ()
  (interactive)
  (my/get-balance)
  (my/insert-text-and-open-temp-buffer (concat "\n" separator_request "\n" text) YANDEX_GPT_LOG_FILE)
  (request "https://llm.api.cloud.yandex.net/foundationModels/v1/completionAsync"
    :type "POST"
    :data (json-encode `(("modelUri" . ,(concat "gpt://" YANDEX_FOLDER_ID "/yandexgpt/latest"))
                         ("completionOptions" . (("stream" . nil)
                                                 ("temperature" . ,temperature)
                                                 ("maxTokens" . ,maxTokens)))
                         ("messages" . [(("role" . "user") ("text" . ,text))])))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Api-Key " YANDEX_API_KEY))
               ("x-folder-id" . YANDEX_FOLDER_ID))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq request_id (assoc-default 'id data))
                (my/insert-text-and-open-temp-buffer (concat "\n\nrequest id => `" request_id "` send time => `" (format-time-string "%Y-%m-%d %H:%M:%S") "` balance `" balance "`\n" separator_request) YANDEX_GPT_LOG_FILE)
                (my/response-yandex-gpt)))))

(defun my/requst-yandex-gpt-by-system (system)
  (interactive)
  (my/get-balance)
  (my/insert-text-and-open-temp-buffer (concat "\n" separator_request "\n" "system => " system "\n\n" "text => " text "\n") YANDEX_GPT_LOG_FILE)
  (request "https://llm.api.cloud.yandex.net/foundationModels/v1/completionAsync"
    :type "POST"
    :data (json-encode `(("modelUri" . ,(concat "gpt://" YANDEX_FOLDER_ID "/yandexgpt/latest"))
                         ("completionOptions" . (("stream" . nil)
                                                 ("temperature" . ,temperature)
                                                 ("maxTokens" . ,maxTokens)))
                         ("messages" . [(("role" . "system") ("text" . ,system))
                                        (("role" . "user") ("text" . ,text))])))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Api-Key " YANDEX_API_KEY))
               ("x-folder-id" . YANDEX_FOLDER_ID))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq request_id (assoc-default 'id data))
                (my/insert-text-and-open-temp-buffer (concat "\n\nrequest id => `" request_id "` send time => `" (format-time-string "%Y-%m-%d %H:%M:%S") "` balance `" balance "`\n" separator_request) YANDEX_GPT_LOG_FILE)
                (my/response-yandex-gpt)))))

