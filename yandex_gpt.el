
(setq YANDEX_API_KEY (getenv "YANDEX_API_KEY"))
(setq YANDEX_FOLDER_ID (getenv "YANDEX_FOLDER_ID"))
(setq YANDEX_GPT_LOG_FILE (getenv "YANDEX_GPT_LOG_FILE"))
(setq separator_request ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
(setq separator_response "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
(setq request_id nil)

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
  (my/insert-text-and-open-temp-buffer (concat "\n" separator_request "\n" text) YANDEX_GPT_LOG_FILE)
  (request "https://llm.api.cloud.yandex.net/foundationModels/v1/completionAsync"
    :type "POST"
    :data (json-encode `(
                         ("modelUri" . ,(concat "gpt://" YANDEX_FOLDER_ID "/yandexgpt/latest"))
                         ("completionOptions" . (
                                                 ("stream" . nil)
                                                 ("temperature" . "0.1")
                                                 ("maxTokens" . "2000")
                                                 ))
                         ("messages" . [(("role" . "user") ("text" . ,text))])))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Api-Key " YANDEX_API_KEY))
               ("x-folder-id" . YANDEX_FOLDER_ID))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq request_id (assoc-default 'id data))
                (my/insert-text-and-open-temp-buffer (concat "\n\nrequest id => `" request_id "` send time => `" (format-time-string "%Y-%m-%d %H:%M:%S") "`\n" separator_request) YANDEX_GPT_LOG_FILE)
                (my/response-yandex-gpt)))))

(defun my/requst-yandex-gpt-by-system (system)
  (interactive)
  (my/insert-text-and-open-temp-buffer (concat "\n" separator_request "\n" "system => " system "\n" "text => " text "\n") YANDEX_GPT_LOG_FILE)
  (request "https://llm.api.cloud.yandex.net/foundationModels/v1/completionAsync"
    :type "POST"
    :data (json-encode `(
                         ("modelUri" . ,(concat "gpt://" YANDEX_FOLDER_ID "/yandexgpt/latest"))
                         ("completionOptions" . (
                                                 ("stream" . nil)
                                                 ("temperature" . "0.1")
                                                 ("maxTokens" . "2000")
                                                 ))
                         ("messages" . [
                                        (("role" . "system") ("text" . ,system))
                                        (("role" . "user") ("text" . ,text))
                                        ])))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(concat "Api-Key " YANDEX_API_KEY))
               ("x-folder-id" . YANDEX_FOLDER_ID))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq request_id (assoc-default 'id data))
                (my/insert-text-and-open-temp-buffer (concat "\n\nrequest id => `" request_id "` send time => `" (format-time-string "%Y-%m-%d %H:%M:%S") "`\n" separator_request) YANDEX_GPT_LOG_FILE)
                (my/response-yandex-gpt)))))
