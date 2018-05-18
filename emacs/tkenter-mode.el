(defvar-local ensc/tkenter-idle-timer nil "TK enter timer")

(defun ensc/tkenter-get-non-null (row col)
  (let ((text nil))
    (while (and (not text)(> row 0))
      (setq text (org-table-get row col)
	    row (- row 1))
      (when (string-equal text "")
	(setq text nil)))

    (when text
      (set-text-properties 0 (length text) nil text))
    text))

(defun ensc/tkenter-convert-duraction (duration)
  (if (= 1 (length duration))
      (* 3600 (nth 0 duration))
    (let ((pos 0)
	  (res 0))
      (dolist (tm duration res)
	(setq res (+ res (* tm
			    (cond
			     ((= pos 0) 60)
			     ((= pos 1) 3600)
			     ((= pos 2) (* 3600 24))
			     (t (error "Too much elements in duration")))))
	      pos (+ pos 1))))))

(defun ensc/tkenter-parse-duration (duration)
  (let ((result 0)
	(orig-duration duration)
	(number 0)
	(is-exc nil)
	(has-num nil)
	(elems-tot '())
	(elems-exc '()))
  (loop for c across duration do
	(cond
	 ((= c ?+) nil)
	 ((= c ?:)
	  (progn
	    (if is-exc
		(setq elems-exc (append (list number) elems-exc))
	      (setq elems-tot (append (list number) elems-tot)))
	    (setq number 0
		  has-num t)))
	 ((= c ?X)
	  (progn
	    (when is-exc
	      (error "'X' already specified"))
	    (when has-num
	      (setq elems-tot (append (list number) elems-tot)
		    number 0
		    is-exc t
		    has-num nil))))
	 ((and (>= c ?0) (<= c ?9))
	  (setq number (+(* number 10) (- c ?0))
		has-num t))
	 (t
	  (error "Bad character"))))
  (when has-num
    (if is-exc
	(setq elems-exc (append (list number) elems-exc))
      (setq elems-tot (append (list number) elems-tot))))
  ;(message "duration %s -> %s / %s" orig-duration elems-tot elems-exc)
  (list (ensc/tkenter-convert-duraction elems-tot)
	(ensc/tkenter-convert-duraction elems-exc))))

(assert (equal (ensc/tkenter-parse-duration "+1")   '(3600 0)))
(assert (equal (ensc/tkenter-parse-duration "+:15") '( 900 0)))

(defun ensc/tkenter-summary (key col)
  (let ((row 1)
	(cur nil)
	(dur nil)
	(total-dur 0)
	(total-exc 0)
	(total-cnt 0)
	(tmp-dur nil)
	(is-match nil))
    (while (setq dur (org-table-get row 3)
		 cur (org-table-get row col))
      (setq is-match
	    (if (string-equal cur "")
		is-match
	      (string-equal cur key)))

      (when (and is-match dur)
	(setq tmp-dur (ensc/tkenter-parse-duration dur)
	      total-dur (+ total-dur (nth 0 tmp-dur))
	      total-exc (+ total-exc (nth 1 tmp-dur))
	      total-cnt (+ total-cnt 1))
	;(message "    -> %s" tmp-dur)
	)

      (setq row (+ row 1)))

    (list total-dur total-exc total-cnt)))

(defun ensc/tkenter-get-date (row)
  (ensc/tkenter-get-non-null row 1))

(defun ensc/tkenter-get-project (row)
  (ensc/tkenter-get-non-null row 2))

(defun ensc/tkenter-summary-date (day)
  (ensc/tkenter-summary day 1))

(defun ensc/tkenter-summary-project (project)
  (ensc/tkenter-summary project 2))

(defun ensc/tkenter-format-duraction-single (duration)
  (let ((res "")
	(fmt "%d:"))

    (when (>= duration (* 24 3600))
      (setq res (format "%s%d" res (/ duration (* 24 3600)))
	    fmt "%02d:"
	    duration (% duration (* 24 3600))))

    (setq res (format (concat "%s" fmt) res (/ duration 3600))
	  duration (% duration 3600))

    (setq res (format "%s%02d" res (/ duration 60))
	  duration (% duration 60))

    res))

(defun ensc/tkenter-parse-date (date &optional now)
  (let ((now (or now (decode-time)))
	(res '())
	(num 0)
	(has-num 0))
    (loop for c across date do
	  (cond
	   ((= c ?.)
	    (setq res (append res (list num))
		  has-num nil
		  num 0))
	   ((and (>= c ?0) (<= c ?9))
	    (setq num (+ (- c ?0) (* num 10))
		  has-num t))
	   (t error "Bad digit")))

    (when has-num
      (setq res (append res (list num))))

    (when (< (length res) 3)
      (setq res (append res (nthcdr (+ 3 (length res)) now))))

    (encode-time 0 0 0
		 (nth 0 res)
		 (nth 1 res)
		 (nth 2 res))))

(defun ensc/tkenter-format-date (date)
  (let ((res (format-time-string "%a %d.%m.%Y:" date)))
    (put-text-property 0 (length res) 'face '(font-lock-face '(:inherit bold)) res)
    res))

(defun ensc/tkenter-format-project (project)
  (let ((res project))
    (put-text-property 0 (length res) 'face '(font-lock-face '(:inherit bold)) res)
    res))


(defun ensc/tkenter-format-duraction (duration)
  (let* ((tot-str (ensc/tkenter-format-duraction-single (nth 0 duration)))
	 (exc     (nth 1 duration))
	 (exc-str (when (> exc 0) (concat "-" (ensc/tkenter-format-duraction-single exc)))))
    (put-text-property 0 (length tot-str) 'face '(font-lock-face '(:foreground "blue")) tot-str)
    (when exc-str
      (put-text-property 0 (length exc-str) 'face '(font-lock-face '(:foreground "red")) exc-str)
      (setq tot-str (concat tot-str exc-str)))
    tot-str))

(defun ensc/tkenter-run (col row)
  (let ((date (ensc/tkenter-get-date row))
	(project (ensc/tkenter-get-project row))
	(inhibit-modification-hooks t))
    (when (and date project)
      (let ((sum-day     (ensc/tkenter-summary-date    date))
	    (date-parsed (ensc/tkenter-parse-date date))
	    (sum-project (ensc/tkenter-summary-project project))
	    (message-log-max nil))
	(message "%s %s (#%d)    |     %s: %s (#%d)"
		 (ensc/tkenter-format-date date-parsed)
		 (ensc/tkenter-format-duraction sum-day)
		 (nth 2 sum-day)
		 (ensc/tkenter-format-project project)
		 (ensc/tkenter-format-duraction sum-project)
		 (nth 2 sum-project))))))

(defun ensc/tkenter-idle-fn (buf)
  (when (and ensc/tkenter-mode
	     (eq (current-buffer) buf)
	     (string-equal major-mode "org-mode"))
    (let ((col (org-table-current-column))
	  (row (org-table-current-line)))
      (when (and (/= col 0)(/= row 0))
	(ensc/tkenter-run col row)))))

(define-minor-mode ensc/tkenter-mode
  "mode for entering tk entries"
  :lighter " TM"
  (if ensc/tkenter-mode
      (progn
	(add-hook 'kill-buffer-hook
		  (lambda () (when (timerp ensc/tkenter-idle-timer)
			       (cancel-timer ensc/tkenter-idle-timer))))
	(setq ensc/tkenter-idle-timer
	     (run-with-idle-timer 0 t 'ensc/tkenter-idle-fn (current-buffer))))
    (cancel-timer ensc/tkenter-idle-timer))
  )
