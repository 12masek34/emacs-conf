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

(defun my/sql-result--build-header ()
  "Build header-line string for tabulated-list, respecting horizontal scroll.
Preserves sort indicator and click-to-sort button properties."
  (let* ((hscroll (window-hscroll))
         (fmt (and (vectorp tabulated-list-format) tabulated-list-format))
         (padding (or tabulated-list-padding 0))
         (pos (1+ padding))
         (button-props `(help-echo "Click to sort by column"
                         mouse-face header-line-highlight
                         keymap ,tabulated-list-sort-button-map))
         result)
    ;; Safety guard: если fmt нет, отдаём просто отступ
    (unless fmt
      (cl-return-from my/sql-result--build-header
        (make-string (max 0 (- pos hscroll)) ?\s)))
    ;; Left padding, reduced by hscroll
    (setq result (make-string (max 0 (- pos hscroll)) ?\s))
    ;; Column headers
    (dotimes (n (length fmt))
      (let* ((col (aref fmt n))
             (name (nth 0 col))
             (label (format "%s" name))
             (width (nth 1 col))
             ;; Emacs 30+ формат: (NAME WIDTH SORTABLE &rest PROPS)
             (props (nthcdr 3 col))
             (pad-right (or (plist-get props :pad-right) 1))
             (label-end (+ pos width))
             (col-end (+ pos width pad-right)))    ; = label-end + pad-right
        ;; Если колонка целиком скроллом уехала влево — пропускаем
        (when (> col-end hscroll)
          (let* ((label-start (min (length label) (max 0 (- hscroll pos))))
                 (vis-label-len (max 0 (- label-end (max pos hscroll))))
                 (vis-pad-len (max 0 (- col-end (max label-end hscroll))))
                 (sort-col (and tabulated-list-sort-key
                                (equal (car col) (car tabulated-list-sort-key))))
                 (label-part (substring label label-start
                                        (min (length label)
                                             (+ label-start vis-label-len)))))
            ;; Добавляем индикатор сортировки (▲/▼)
            (when sort-col
              (setq label-part (concat label-part
                                       (if (cdr tabulated-list-sort-key) " ▼" " ▲"))))
            ;; Добиваем/обрезаем до vis-label-len символов
            (setq label-part (if (> vis-label-len 0)
                                 (truncate-string-to-width label-part vis-label-len 0 ?\s)
                               ""))
            ;; Текст-свойства для сортировки по клику
            (when (and (> (length label-part) 0)
                       (not (string-blank-p label-part)))
              (setq label-part
                    (if sort-col
                        (apply 'propertize label-part
                               'tabulated-list-column-name name
                               'face 'bold
                               button-props)
                      (apply 'propertize label-part
                             'tabulated-list-column-name name
                             button-props))))
            (setq result (concat result label-part))
            ;; Padding после колонки (только пробелы, синхронно с телом)
            (when (> vis-pad-len 0)
              (setq result (concat result (make-string vis-pad-len ?\s))))))
        (setq pos col-end)))
    result))

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
           (normalized-rows
            (mapcar (lambda (row)
                      (let ((r (cl-subseq row 0 (min (length row) ncols))))
                        (append r (make-list (- ncols (length r)) ""))))
                    rows))
           (sep-rows
            (mapcar (lambda (row)
                      (cl-loop for i from 0 below (length row)
                               collect (if (< i (1- (length row)))
                                           (concat (format "%s" (nth i row)) "│")
                                         (format "%s" (nth i row)))))
                    normalized-rows))
           (sep-headers
            (cl-loop for i from 0 below ncols
                     collect (if (< i (1- ncols))
                                 (concat (format "%s" (or (and headers (nth i headers)) "")) "│")
                               (format "%s" (or (and headers (nth i headers)) "")))))
           (col-widths
            (cl-loop for i below ncols
                     collect
                     (min my/sql-result-max-col-width
                          (max 8
                               (length (nth i sep-headers))
                               (if sep-rows
                                   (apply #'max (mapcar (lambda (r) (length (nth i r))) sep-rows))
                                   0)))))
           (fmt-vector
            (apply #'vector
                   (cl-loop for i below ncols
                            collect
                            (list (nth i sep-headers)
                                  (nth i col-widths)
                                  nil
                                  :pad-right 1))))
           (entries
            (cl-loop for row in sep-rows
                     for idx from 1
                     collect
                     (list idx
                           (apply #'vector
                                  (cl-loop for i below ncols
                                           collect (nth i row)))))))
      (let ((buf (get-buffer-create "*SQL Results*")))
        (with-current-buffer buf
          (my/sql-result-mode)
          (setq tabulated-list-format fmt-vector
                tabulated-list-entries entries
                tabulated-list-sort-key nil)
          (tabulated-list-init-header)
          (setq-local header-line-format
                      '(:eval (my/sql-result--build-header)))
          (tabulated-list-print t))
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