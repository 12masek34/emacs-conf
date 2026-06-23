;;; database.el --- SQL Results Viewer with vtable -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025

;; Author: Dmitry Martys
;; Keywords: sql, database, org, babel
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; SQL Results Viewer — vtable-based display for SQL query results.
;; Designed for use with org-babel SQL src-blocks.
;; Requires Emacs 29+ with built-in vtable support.
;;
;; Основные возможности:
;; - Отображение результатов SQL-запросов в таблице
;; - Горизонтальный скролл заголовка вместе с данными
;; - Детальный просмотр содержимого ячейки без урезания
;; - Отображение времени выполнения запроса
;; - Сортировка по колонкам (S)
;; - Изменение ширины колонок ({ / })
;;
;; Использование:
;;   M-x my/org-babel-execute-and-popup — выполнить src-block и показать таблицу

;;; Code:

(require 'cl-lib)
(require 'vtable)

;; =============================================================================
;; Пользовательские настройки
;; =============================================================================

(defgroup my/sql nil
  "SQL results viewer."
  :group 'tools)

(defcustom my/sql-result-max-col-width 50
  "Максимальная ширина колонки в символах."
  :type 'integer
  :group 'my/sql)

(defcustom my/sql-result-show-timing t
  "Показывать время выполнения запроса в заголовке."
  :type 'boolean
  :group 'my/sql)

;; =============================================================================
;; Внутренние переменные
;; =============================================================================

(defvar-local my/sql--table nil
  "vtable object текущей таблицы результатов.")

(defvar-local my/sql--exec-time nil
  "Время выполнения последнего запроса (в секундах).")

;; =============================================================================
;; Работа с ячейками — доступ к полным (неурезанным) данным
;; =============================================================================

(defun my/sql--current-cell-data ()
  "Возвращает (row-number column-index raw-value) для ячейки под курсором.
Использует vtable-current-object и vtable-current-column для доступа
к оригинальным данным (неурезанным, в отличие от отображаемого текста)."
  (when-let* ((table my/sql--table)
              (col (vtable-current-column))
              (obj (vtable-current-object)))
    ;; obj — это список значений строки (оригинальные, неурезанные данные)
    ;; Вычисляем номер строки (1-based) из позиции в буфере
    ;; Строка 1 = заголовок, строки 2+ = данные
    (let ((row-num (- (line-number-at-pos (point)) 2)))
      (list row-num col (elt obj col)))))

(defun my/sql-result-show-cell ()
  "Показать содержимое текущей ячейки в echo area."
  (interactive)
  (pcase (my/sql--current-cell-data)
    (`(,row-num ,col ,val)
     (let ((val-str (if (stringp val) val (format "%s" val))))
       (message "Cell [Row: %d, Col: %d]: %s"
                row-num (1+ col)
                (truncate-string-to-width val-str 200 0 nil t))))
    (_ (message "Empty cell"))))

;; =============================================================================
;; Режим детализации ячейки
;; =============================================================================

(defvar-keymap my/sql-cell-detail-map
  "q"   #'my/sql--close-cell-detail
  "C-g" #'my/sql--close-cell-detail)

(defun my/sql--close-cell-detail ()
  "Закрыть буфер детализации и вернуться к результатам."
  (interactive)
  (let ((results-win (get-buffer-window "*SQL Results*")))
    ;; Закрываем окно и буфер детализации
    (quit-window t)
    ;; Возвращаемся в окно результатов
    (when (window-live-p results-win)
      (select-window results-win))))

(define-derived-mode my/sql-cell-detail-mode special-mode "SQL-Cell"
  "Режим для просмотра полного содержимого ячейки.

Горячие клавиши:
  `q' или `C-g' — закрыть и вернуться к результатам
  `SPC' — скролл вниз
  `DEL' — скролл вверх"
  (setq-local truncate-lines nil))

(defun my/sql-result-inspect-cell ()
  "Открыть полное содержимое ячейки в popup-буфере.
Показывает оригинальные (неурезанные) данные."
  (interactive)
  (pcase (my/sql--current-cell-data)
    (`(,row-num ,col ,val)
     (let* ((val-str (if (stringp val) val (format "%s" val)))
            (buf (get-buffer-create "*SQL Cell Detail*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert val-str)
           (goto-char (point-min))
           (my/sql-cell-detail-mode)
           (setq-local header-line-format
                       (format "  Cell: Row %d, Col %d  (q — close)"
                               row-num (1+ col)))
           (setq-local buffer-read-only t)))
       (display-buffer buf '((display-buffer-reuse-window
                              display-buffer-below-selected)
                             (window-height . 0.3)))))
    (_ (message "Empty cell"))))

;; =============================================================================
;; Режим SQL Results
;; =============================================================================

(define-derived-mode my/sql-result-mode special-mode "SQL-Result"
  "Режим для просмотра результатов SQL-запросов.

Горячие клавиши:
  `q'   — закрыть окно результатов
  `RET' — показать полное содержимое ячейки в отдельном буфере
  `i'   — показать содержимое ячейки в minibuffer
  `S'   — сортировать по колонке
  `{'   — сузить колонку
  `}'   — расширить колонку
  `g'   — перезагрузить таблицу"
  (setq-local truncate-lines t
              buffer-read-only t))

;;; Клавиатурные сокращения

(defvar-keymap my/sql-result-map
  :parent vtable-map
  "i"    #'my/sql-result-show-cell
  "RET"  #'my/sql-result-inspect-cell
  "q"    #'quit-window)

;; =============================================================================
;; Парсинг результата SQL-запроса
;; =============================================================================

(defun my/sql--parse-result (result)
  "Преобразовать RESULT из org-babel в (headers . rows).

RESULT может быть:
- nil / пустым
- списком списков (org-table), возможно с `hline'
- строкой
- списком значений одной строки"
  (cond
   ((null result)
    (list nil nil))

   ((and (listp result) (eq (car result) 'hline))
    ;; Формат (hline row1 row2 ...) — без заголовка
    (list nil (cl-remove-if (lambda (x) (eq x 'hline)) result)))

   ((and (listp result) (listp (car result)))
    ;; (header row1 row2 ...) или (row1 row2 ...)
    ;; Ищем hline
    (let* ((has-hline (memq 'hline result))
           (data (cl-remove-if (lambda (x) (eq x 'hline)) result)))
      (if has-hline
          (if data
              (list (car data) (cdr data))
            (list nil nil))
        ;; Без hline — первая строка может быть заголовком
        ;; но мы не можем быть уверены. Считаем все данные.
        (list (car data) (cdr data)))))

   ((stringp result)
    ;; Одна строка результата
    (list nil (list (list result))))

   (t
    ;; Единичное значение
    (list nil (list (list (format "%s" result)))))))

;; =============================================================================
;; Column formatter — урезание длинных значений
;; =============================================================================

(defun my/sql--cell-formatter (value _index _table)
  "Отформатировать VALUE для отображения в ячейке.
Урезает длинные строки до `my/sql-result-max-col-width' символов.
Оригинальное значение НЕ изменяется — оно хранится в vtable-object и доступно
для просмотра через `my/sql-result-inspect-cell'."
  (let* ((raw (if (stringp value) value (format "%s" value)))
         (max-chars my/sql-result-max-col-width))
    (if (> (length raw) max-chars)
        (truncate-string-to-width raw max-chars 0 nil t)
      raw)))

;; =============================================================================
;; Отображение таблицы
;; =============================================================================

(defun my/sql--display (result &optional exec-time)
  "Отобразить RESULT (результат org-babel SQL) в vtable.

RESULT — список списков (org-table), возможно с `hline'.
EXEC-TIME — время выполнения запроса в секундах (опционально).
           Если non-nil, отображается в заголовке буфера."
  (pcase-let* ((`(,headers ,rows) (my/sql--parse-result result))
               (ncols (max 1 (cond
                              (headers (length headers))
                              (rows (apply #'max 1 (mapcar #'length rows)))
                              (t 1))))
               ;; Нормализация всех строк к одной длине
               (normalized-rows
                (mapcar (lambda (row)
                          (let* ((row-list (if (listp row) row (list row)))
                                 (r (cl-subseq row-list 0 (min (length row-list) ncols))))
                            (append r (make-list (- ncols (length r)) ""))))
                        rows))
               ;; Имена колонок
               (col-names
                (cl-loop for i from 0 below ncols
                         collect (if (and headers (< i (length headers)))
                                     (format "%s" (nth i headers))
                                   (format "Col %d" (1+ i)))))
               ;; Спецификации колонок vtable
               (max-col-width my/sql-result-max-col-width)
               (col-specs
                (cl-loop for name in col-names
                         collect
                         (make-vtable-column
                          :name (truncate-string-to-width name max-col-width 0 nil t)
                          :width (format "%dex" (min max-col-width
                                                     (max 8 (length name))))
                          :max-width (format "%dex" max-col-width)
                          :min-width "8ex"
                          :align 'left)))
               ;; Форматирование времени выполнения
               (timing-str (when (and exec-time my/sql-result-show-timing)
                             (format "  [exec: %.2f ms]" (* exec-time 1000)))))
    ;; Создаём буфер и таблицу
    (let* ((buf (get-buffer-create "*SQL Results*")))
      (with-current-buffer buf
        (my/sql-result-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local my/sql--table
                      (make-vtable
                       :columns col-specs
                       :objects normalized-rows
                       :use-header-line nil  ;; заголовок — в буфере, скроллится
                       :formatter #'my/sql--cell-formatter
                       :keymap my/sql-result-map
                       :separator-width 1
                       :divider " │"
                       :ellipsis t
                       :sort-by nil))))
      ;; Кешируем заголовок для горизонтального скролла
      (with-current-buffer buf
        (setq-local my/sql--exec-time exec-time)
        (setq-local header-line-format
                    (propertize
                     (format "SQL Results%s" (or timing-str ""))
                     'face 'mode-line-buffer-id)))
      ;; Показываем окно
      (let ((win (display-buffer buf
                   '((display-buffer-reuse-window
                      display-buffer-below-selected)
                     (window-height . (lambda (win)
                                       (with-selected-window win
                                         (max 5
                                              (min (/ (frame-height) 2)
                                                   (1+ (length normalized-rows)))))))))))
        (when win
          (with-selected-window win
            (goto-char (point-min))
            (forward-line)  ;; пропускаем заголовок
            (set-window-start win (point)))
          ;; Force header line update right away
          (force-mode-line-update))))))

;; =============================================================================
;; Публичные функции
;; =============================================================================

(defun my/sql-popup (result &optional exec-time)
  "Показать результат SQL-запроса RESULT в таблице.
RESULT — в формате org-babel (список списков).
EXEC-TIME — время выполнения в секундах (опционально)."
  (my/sql--display result exec-time))

;;;###autoload
(defun my/org-babel-execute-and-popup ()
  "Выполнить текущий src-block org-babel и показать результат в таблице.
Замеряет и отображает время выполнения запроса.
Работает с любыми языками org-babel (sql, python, elisp, ...)."
  (interactive)
  (let ((start-time (float-time)))
    ;; Сохраняем параметры выполнения для оборачивания ошибки
    (condition-case err
        (let ((result (org-babel-execute-src-block)))
          (let ((exec-time (- (float-time) start-time)))
            (if (and (listp result) (listp (car result)))
                ;; Табличный результат — показываем в vtable
                (my/sql-popup result exec-time)
              ;; Нетабличный результат — показываем как есть
              (let ((msg (if (stringp result) result (format "%s" result))))
                (message "Query result (%.2f ms): %s"
                         (* exec-time 1000)
                         (truncate-string-to-width msg 200 0 nil t))))))
      (error
       (message "Query failed after %.2f ms: %s"
                (* (- (float-time) start-time) 1000)
                (error-message-string err))))))

(declare-function org-babel-execute-src-block "ob-core")

(provide 'database)
;;; database.el ends here