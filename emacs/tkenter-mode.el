(require 'org-table)

(defvar-local ensc/tkenter-idle-timer nil "TK enter timer")
(defvar-local ensc/tkenter-skip-timer nil "skip idle timer the next time")
(defvar ensc/tkenter-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'ensc/tkenter-normalize-cell)
    (define-key map (kbd "C-c t") 'ensc/tkenter-transmit)
    map))

(defcustom ensc/tkenter-cli-program
  "tk-sc"
  "The tk-cli program"
  :type 'file
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-base-url
  "https://tk-sc.intern.sigma-chemnitz.de"
  "The TK uri"
  :type 'string
  :group 'enter/tkenter)

(defcustom ensc/tkenter-columns
  '((:date . 1)
    (:project . 2)
    (:effort . 3)
    (:desc . 4)
    (:note . 5)
    (:url . 6))
  "column; date, project, effort, description"
  :type 'sexp
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-idle-delay
  0.05
  "The TK idle delay"
  :type 'float
  :group 'ensc/tkenter)

(defun ensc/tkenter-column-get (key)
  (cdr (assoc key ensc/tkenter-columns)))

(defun ensc/tkenter-get-non-null (row col)
  (let ((text nil)
	(col-idx (ensc/tkenter-column-get col)))
    (while (and (not text)(> row 0))
      (setq text (substring-no-properties (org-table-get row col-idx))
	    row (1- row))
      (when (string= text "")
	(setq text nil)))
    text))

(defun ensc/tkenter-convert-effort (effort)
  (if (= 1 (length effort))
      (* 3600 (nth 0 effort))
    (let ((pos 0)
	  (res 0))
      (dolist (tm effort res)
	(setq res (+ res (* tm
			    (cond
			     ((= pos 0) 60)
			     ((= pos 1) 3600)
			     ((= pos 2) (* 3600 24))
			     (t (error "Too much elements in effort")))))
	      pos (1+ pos))))))

(defun ensc/tkenter-parse-effort (effort)
  (let ((result 0)
	(orig-effort effort)
	(number 0)
	(is-exc nil)
	(has-num nil)
	(elems-tot '())
	(elems-exc '()))
    (loop for c across effort do
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
					;(message "effort %s -> %s / %s" orig-effort elems-tot elems-exc)
    (list (ensc/tkenter-convert-effort elems-tot)
	  (ensc/tkenter-convert-effort elems-exc))))

(defun ensc/tkenter-summary (key col)
  (let ((row 2)
	(cur nil)
	(effort nil)
	(total-effort 0)
	(total-exc 0)
	(total-cnt 0)
	(tmp-effort nil)
	(is-match nil))
    (while (setq effort (org-table-get row (ensc/tkenter-column-get :effort))
		 cur (org-table-get row (ensc/tkenter-column-get col)))
      (setq is-match
	    (if (string-equal cur "")
		is-match
	      (string-equal cur key)))

      (when (and is-match effort)
	(setq tmp-effort (ensc/tkenter-parse-effort effort)
	      total-effort (+ total-effort (nth 0 tmp-effort))
	      total-exc (+ total-exc (nth 1 tmp-effort))
	      total-cnt (1+ total-cnt)))

      (setq row (1+ row)))

    (list total-effort total-exc total-cnt)))

(defun ensc/tkenter-get-date (row)
  (ensc/tkenter-get-non-null row :date))

(defun ensc/tkenter-get-project (row)
  (ensc/tkenter-get-non-null row :project))

(defun ensc/tkenter-summary-date (day)
  (ensc/tkenter-summary day :date))

(defun ensc/tkenter-summary-project (project)
  (ensc/tkenter-summary project :project))

(defun ensc/tkenter-format-effort-single (effort &optional split-days)
  (let ((res "")
	(fmt "%d:"))

    (when (and split-days (>= effort (* 24 3600)))
      (setq res (format "%s%d:" res (/ effort (* 24 3600)))
	    fmt "%02d:"
	    effort (% effort (* 24 3600))))

    (setq res (format (concat "%s" fmt) res (/ effort 3600))
	  effort (% effort 3600))

    (setq res (format "%s%02d" res (/ effort 60))
	  effort (% effort 60))

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
	   (t (error "Bad digit"))))

    (when has-num
      (setq res (append res (list num))))

    (when (< (length res) 3)
      (setq res (append res (nthcdr (+ 3 (length res)) now))))

    (encode-time 0 0 0
		 (nth 0 res)
		 (nth 1 res)
		 (nth 2 res))))

(defun ensc/tkenter-format-date (date)
  (let ((res (format-time-string "%a %d.%m.%Y:" date))
	(inhibit-modification-hooks t))
    (put-text-property 0 (length res) 'face '(:inherit bold) res)
    res))

(defun ensc/tkenter-format-project (project)
  (let ((res project)
	(inhibit-modification-hooks t))
    (put-text-property 0 (length res) 'face '(:inherit bold) res)
    res))


(defun ensc/tkenter-format-effort (effort)
  (let* ((tot-str (ensc/tkenter-format-effort-single (nth 0 effort)))
	 (exc     (nth 1 effort))
	 (exc-str (when (> exc 0)
		    (concat "-" (ensc/tkenter-format-effort-single exc))))
	 (inhibit-modification-hooks t))
    (put-text-property 0 (length tot-str) 'face '(:foreground "blue") tot-str)
    (when exc-str
      (put-text-property 0 (length exc-str) 'face '(:foreground "red") exc-str)
      (setq tot-str (concat tot-str exc-str)))
    tot-str))

(defun ensc/tkenter-run (col row)
  (let ((date (ensc/tkenter-get-date row))
	(project (ensc/tkenter-get-project row)))
    (when (and date project)
      (let ((sum-day     (ensc/tkenter-summary-date    date))
	    (date-parsed (ensc/tkenter-parse-date date))
	    (sum-project (ensc/tkenter-summary-project project))
	    (message-log-max nil))
	(message "%s %s (#%d)    |     %s: %s (#%d)"
		 (ensc/tkenter-format-date date-parsed)
		 (ensc/tkenter-format-effort sum-day)
		 (nth 2 sum-day)
		 (ensc/tkenter-format-project project)
		 (ensc/tkenter-format-effort  sum-project)
		 (nth 2 sum-project))))))

(defmacro ensc/tkenter-within-cell (buf &rest body)
  ""
  (declare (indent 0) (debug t))
  `(when (and ensc/tkenter-mode
	      (or (not ,buf) (eq (current-buffer) ,buf))
	      (string-equal major-mode "org-mode")
	      (org-at-table-p)
	      (not (org-at-table-hline-p)))
    (let ((col (org-table-current-column))
	  (row (org-table-current-line)))
      (when (and (/= col 0)(/= row 0))
	(progn ,@body)))))

(defun ensc/tkenter-idle-fn (buf)
  (ensc/tkenter-within-cell buf
			    (if ensc/tkenter-skip-timer
				(setq ensc/tkenter-skip-timer nil)
			      (ensc/tkenter-run col row))))

(defun ensc/tkenter-normalize-date (text-old)
  (format-time-string " %d.%m. " (ensc/tkenter-parse-date text-old)))

(defun ensc/tkenter-normalize-project (text-old)
  text-old)

(defun ensc/tkenter-normalize-effort (text-old)
  (let* ((effort    (ensc/tkenter-parse-effort text-old))
	 (text-pos  (ensc/tkenter-format-effort-single (nth 0 effort) t))
	 (text-neg  (ensc/tkenter-format-effort-single (nth 1 effort) t)))
    (if (> (nth 1 effort) 0)
	(concat "+" text-pos "X" text-neg)
      (concat "+" text-pos))))

(defun ensc/_tkenter-normalize-cell (col row)
  (let* ((text-old (org-table-get row col))
	 (text-new (when (and text-old
			      (not (string= "" text-old)))
		     (cond
		      ((= col (ensc/tkenter-column-get :date))
		       (ensc/tkenter-normalize-date text-old))
		      ((= col (ensc/tkenter-column-get :project))
		       (ensc/tkenter-normalize-project text-old))
		      ((= col (ensc/tkenter-column-get :effort))
		       (ensc/tkenter-normalize-effort text-old))))))
    (when (and text-new
	       (not (string= text-old text-new)))
      (org-table-put row col text-new nil))))

(defun ensc/tkenter-normalize-cell ()
  (interactive)
  (ignore-errors
    (ensc/tkenter-within-cell nil
			      (ensc/_tkenter-normalize-cell col row)))
  (org-cycle))

;;; autoload
(define-minor-mode ensc/tkenter-mode
  "mode for entering tk entries"
  :lighter " TM"
  :keymap ensc/tkenter-keymap
  (if ensc/tkenter-mode
      (progn
	(add-hook 'kill-buffer-hook
		  (lambda () (when (timerp ensc/tkenter-idle-timer)
			       (cancel-timer ensc/tkenter-idle-timer))))
	(setq ensc/tkenter-idle-timer
	      (run-with-idle-timer ensc/tkenter-idle-delay t
				   'ensc/tkenter-idle-fn (current-buffer))))
    (cancel-timer ensc/tkenter-idle-timer))
  )

(defun ensc/tkenter-unittest-parse-effort (effort exp)
  (assert (equal (ensc/tkenter-parse-effort effort) exp)))

(defun ensc/tkenter-translate-project (project)
  (let ((uuid (lax-plist-get
	       (org-table-get-remote-range "project-mapping" "@2$1..@>$2")
	       project)))
    (unless uuid
      (error "No such project %s" project))

    (substring-no-properties uuid)))

(defun ensc/_tkenter-transmit (col row)
  (let* ((date    (ensc/tkenter-parse-date (ensc/tkenter-get-non-null row :date)))
	 (project (ensc/tkenter-translate-project (ensc/tkenter-get-non-null row :project)))
	 (effort  (ensc/tkenter-parse-effort (ensc/tkenter-get-non-null row :effort)))
	 (desc    (org-table-get row (ensc/tkenter-column-get :desc)))
	 (note    (org-table-get row (ensc/tkenter-column-get :note)))
	 (url     (org-table-get row (ensc/tkenter-column-get :url)))
	 (result  nil))

    (when (not (string= url ""))
      (error "Already submitted!"))

    (setq result (with-temp-buffer
		   (let ((code (call-process ensc/tkenter-cli-program
					     nil t t
					     "--batch"
					     (concat "@" project)
					     (format-time-string "%d.%m.%Y" date)
					     (concat "+"
						     (ensc/tkenter-format-effort-single (nth 0 effort) t)
						     "X+"
						     (ensc/tkenter-format-effort-single (nth 1 effort) t))
					     (or desc "")
					     (or note ""))))
		     (append (list code) (split-string (buffer-string))))))
    (cond
     ((= 0 (nth 0 result))
      (when (not (string= "OK" (nth 1 result)))
	(error "Unexpected response: %s" result))

      (org-table-put row (ensc/tkenter-column-get :url)
		     (concat "[[" ensc/tkenter-base-url "/Times/Edit/" (nth 2 result) "][OK]]"))

      (message "Transmitted as %s" (nth 2 result))
      (org-table-align))
     (t
      (error "Failed to submit data: %s" result)))))

(defun ensc/tkenter-transmit ()
  (interactive)
  (ensc/tkenter-within-cell
    nil
    (setq ensc/tkenter-skip-timer t)
    (unwind-protect
	(let ((col (org-table-current-column))
	      (row (org-table-current-line)))
	  (when (and (/= col 0)(/= row 0))
	    (ensc/_tkenter-transmit col row)))
      (next-line)
      (while (and (org-at-table-p)
		  (org-at-table-hline-p))
	(next-line)))))

(defun ensc/tkenter-unittest ()
  (ensc/tkenter-unittest-parse-effort "1"        '(  3600     0))
  (ensc/tkenter-unittest-parse-effort "+1"       '(  3600     0))
  (ensc/tkenter-unittest-parse-effort ":15"      '(   900     0))
  (ensc/tkenter-unittest-parse-effort "+:15"     '(   900     0))
  (ensc/tkenter-unittest-parse-effort "+1:10:30" '(124200     0))
  (ensc/tkenter-unittest-parse-effort ":15X+:15" '(   900   900))
  (ensc/tkenter-unittest-parse-effort ":15X:15"  '(   900   900))
  (ensc/tkenter-unittest-parse-effort ":15X10"   '(   900 36000))
  (ensc/tkenter-unittest-parse-effort "0X10"     '(     0 36000))
  )

(ensc/tkenter-unittest)
