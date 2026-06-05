
;;; SQL Results Viewer — красивая таблица для результатов SQL-запросов

(require 'cl-lib)

(defvar my/sql-result-max-col-width 50
  "Максимальная ширина колонки (в символах) в таблице результатов.")

(map! :map my/sql-result-mode-map
      :n "RET" #'my/sql-result-show-cell
      :n "i"   #'my/sql-result-inspect-cell
      :n "q"   #'quit-window)

(define-derived-mode my/sql-result-mode tabulated-list-mode "SQL-Result"
  "Режим для просмотра результатов SQL-запросов в виде таблицы.

Горячие клавиши:
  `q' или `C-g' — закрыть окно результатов
  `RET'         — показать содержимое ячейки в minibuffer
  `i'           — открыть полное содержимое ячейки в отдельном буфере"
  (setq-local tabulated-list-padding 2))

(defun my/sql-result--column-at-point ()
  "Возвращает индекс колонки под курсором.
Использует текстовую метку `tabulated-list-column-name',
которую проставляет `tabulated-list-print-col' — работает
надёжнее, чем ручной расчёт позиций."
  (let ((name (get-text-property (point) 'tabulated-list-column-name)))
    (if name
        (tabulated-list--column-number name)
      0)))

(defun my/sql-result-show-cell ()
  "Показать содержимое текущей ячейки в echo area."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (col   (my/sql-result--column-at-point))
         (cell  (and entry col (elt entry col))))
    (if cell
        (message "Cell [Row %s, Col %d]: %s"
                 (tabulated-list-get-id) (1+ col)
                 (truncate-string-to-width cell 200 0 nil t))
      (message "Empty cell"))))


(map! :map my/sql-cell-detail-mode-map
      :n "RET" #'my/sql-result-show-cell
      :n "i"   #'my/sql-result-inspect-cell
      :n "C-g"   #'my/sql-result--close-cell-detail
      :n "q"   #'my/sql-result--close-cell-detail)

(define-derived-mode my/sql-cell-detail-mode special-mode "SQL-Cell"
  "Режим для просмотра полного содержимого ячейки.")

(defun my/sql-result--close-cell-detail ()
  "Закрыть буфер детализации ячейки и вернуться к результатам."
  (interactive)
  (let ((results-win (get-buffer-window "*SQL Results*")))
    ;; Убиваем буфер и удаляем окно
    (kill-buffer (current-buffer))
    (when (window-live-p results-win)
      (select-window results-win))
    ;; Если окна с результатами уже нет — покажем заново
    (unless (window-live-p results-win)
      (when (get-buffer "*SQL Results*")
        (pop-to-buffer "*SQL Results*")))))

(defun my/sql-result-inspect-cell ()
  "Показать полное содержимое текущей ячейки в popup-буфере."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (col   (my/sql-result--column-at-point))
         (cell  (and entry col (elt entry col))))
    (if (and cell (not (string-empty-p cell)))
        (let ((buf (get-buffer-create "*SQL Cell Detail*")))
          (with-current-buffer buf
            (my/sql-cell-detail-mode)
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert cell)
              (goto-char (point-min)))
            (setq-local header-line-format
                        (format "  Cell: Row %s, Col %d  (SPC/DEL — скролл, q — закрыть)"
                                (tabulated-list-get-id) (1+ col))))
          (display-buffer buf '((display-buffer-reuse-window
                                 display-buffer-below-selected)
                                (window-height . 0.3))))
      (message "Empty cell"))))

(defun my/sql-result--parse-result (result)
  "Преобразует RESULT из org-babel в (headers rows).
RESULT — список списков (org-table), возможно с `hline' между заголовком и данными."
  (if (not (listp result))
      (list nil (list (list (format "%s" result))))
    (let ((data (cl-remove-if (lambda (x) (eq x 'hline)) result)))
      (if (null data)
          (list nil nil)
        (if (and (car data) (listp (car data)))
            (list (car data) (cdr data))
          (list nil (list data)))))))

(defun my/sql-result--display (result)
  "Отображает RESULT в tabulated-list-mode буфере."
  (pcase-let ((`(,headers ,rows) (my/sql-result--parse-result result)))
    (let* ((ncols (max 1 (if headers (length headers)
                           (if rows (apply #'max 1 (mapcar #'length rows)) 1))))
           ;; Выравниваем все строки до одинакового числа колонок
           (normalized-rows
            (mapcar (lambda (row)
                      (let ((r (cl-subseq row 0 (min (length row) ncols))))
                        (append r (make-list (- ncols (length r)) ""))))
                    rows))
           ;; Вычисляем ширину каждой колонки (авто-подбор с ограничением)
           (col-widths
            (cl-loop for i below ncols
                     collect
                     (min my/sql-result-max-col-width
                          (max 8
                               (if headers (length (format "%s" (nth i headers))) 0)
                               (if normalized-rows
                                   (apply #'max (mapcar (lambda (r) (length (format "%s" (nth i r)))) normalized-rows))
                                 0)))))
           ;; Вектор формата колонок для tabulated-list-mode
           (fmt-vector
            (apply #'vector
                   (cl-loop for i below ncols
                            collect
                            (list (format "%s" (or (and headers (nth i headers)) ""))
                                  (nth i col-widths)
                                  2))))
           ;; Список записей (id . [значения колонок])
           (entries
            (cl-loop for row in normalized-rows
                     for idx from 1
                     collect
                     (list idx
                           (apply #'vector
                                  (cl-loop for i below ncols
                                           collect (format "%s" (nth i row))))))))
      ;; Настраиваем и показываем буфер
      (let ((buf (get-buffer-create "*SQL Results*")))
        (with-current-buffer buf
          (my/sql-result-mode)
          (setq tabulated-list-format fmt-vector
                tabulated-list-entries entries
                tabulated-list-sort-key nil)
          (tabulated-list-init-header)
          (tabulated-list-print t))
        ;; Открываем окно в нижней половине экрана
        (let ((win (display-buffer buf
                     '((display-buffer-reuse-window
                        display-buffer-below-selected)
                       (window-height . (lambda (win)
                                         (max 5 (/ (frame-height) 2))))))))
          (when win
            (with-selected-window win
              (goto-char (point-min)))))))))

(defun my/sql-popup (result)
  "Показать результат SQL-запроса RESULT в красивой таблице."
  (my/sql-result--display result))

(defun my/org-babel-execute-and-popup ()
  "Выполнить текущий src-block org-babel и показать результат в popup-таблице."
  (interactive)
  (let ((result (org-babel-execute-src-block)))
    (my/sql-popup result)))