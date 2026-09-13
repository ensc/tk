;; -*- lexical-binding: t -*-
(require 'org-table)
(require 'dash)

(defvar-local ensc/tkenter-mode-now nil)
(defvar-local ensc/tkenter-idle-timer nil "TK enter timer")
(defvar-local ensc/tkenter-skip-timer nil "skip idle timer the next time")
(defvar ensc/tkenter-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'ensc/tkenter-normalize-cell)
    (define-key map (kbd "C-c t") 'ensc/tkenter-transmit)
    (define-key map [C-S-up] 'ensc/tkenter-find-todo-prev)
    (define-key map [C-S-down] 'ensc/tkenter-find-todo-next)
    map))

(defcustom ensc/tkenter-cli-program
  "tk-sc"
  "The tk-cli program"
  :type 'file
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-tfs-program
  "tk-tfs"
  "The tk-tfs program"
  :type 'file
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-base-url
  "https://tk-ses.intern.sigma-chemnitz.de"
  "The TK uri"
  :type 'string
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-num-header-rows
  2
  "The number of header lines"
  :type 'int
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-project-mapping-range
  "@2$2..@>>$3"
  "Subtable range with project-uuid mapping"
  :type 'string
  :group 'ensc/tkenter)

(defcustom ensc/tkenter-columns
  '((:date . 1)
    (:project . 2)
    (:effort . 3)
    (:desc . 4)
    (:note . 5)
    (:url . 6)
    (:mapping-project . 2)
    (:mapping-pushed  . 6)
    (:mapping-pending . 7)
    (:mapping-fee . 8)
    (:mapping-sales . 9))
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
      (setq text (org-table-get row col-idx)
	    row (1- row))
      (when text
	(setq text (substring-no-properties text)))
      (when (string= text "")
	(setq text nil)))
    text))

(defun ensc/tkenter-table-get (row col)
  "Wrapper around org-table-get.

I takes a textual `col` description (see `ensc/tkenter-columns`) and
 remove properties from the `org-table-get` result"
  (let ((s (when-let ((raw (org-table-get row (ensc/tkenter-column-get col))))
	     (string-trim (substring-no-properties raw)))))
    (when (and s (not (string-empty-p s)))
      s)))

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
  (let ((number 0)
	(is-exc nil)
	(has-num nil)
	(elems-tot '())
	(elems-exc '()))
    (cl-loop for c across effort do
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

(defun ensc/tkenter-summary (key col &optional filter)
  (let ((row 2)
	(cur nil)
	(tag nil)
	(effort nil)
	(total-effort 0)
	(total-exc 0)
	(total-cnt 0)
	(tmp-effort nil)
	(is-match nil)
	(is-key nil))
    (while (setq effort (org-table-get row (ensc/tkenter-column-get :effort))
		 tag (and filter (ensc/tkenter-get-desc-tag row))
		 cur (org-table-get row (ensc/tkenter-column-get col)))
      (setq is-key
	    (if (string-equal cur "")
		is-key
	      (string-equal cur key)))

      (setq is-match
	    (and
	     (or (not filter)
		 (string-equal tag filter))
	     is-key))

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

(defun ensc/tkenter-extract-desc-tag (desc)
  "Extract a tag from the beginning of DESC and return it, or nil.

Matches only when the first non-space characters are a bracketed tag"
  (when (and desc (string-match "^[ \\t]*\\[\\([^]]+\\)\\]" desc))
    (match-string 1 desc)))

(defun ensc/tkenter-get-desc-tag (row)
  "Get the tag from the description column at ROW.
Returns the tag found inside square brackets or nil.
Delegates to `ensc/tkenter-extract-desc-tag' for the extraction logic."
  (ensc/tkenter-extract-desc-tag (ensc/tkenter-get-non-null row :desc)))

(defun ensc/tkenter-summary-date (day)
  (ensc/tkenter-summary day :date))

(defun ensc/tkenter-summary-project (project)
  (ensc/tkenter-summary project :project))

(defun ensc/tkenter-summary-desc (project desc)
  (ensc/tkenter-summary project :project desc))

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

(defun ensc/tkenter-buffer-time ()
  (let* ((heading (plist-get (org-export-get-environment) :date))
	 (heading (and heading (substring-no-properties (car heading)))))
    (decode-time
     (when (and heading
		(string-match ".*\\<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)\\>" heading))
       (encode-time 0 0 0 1
		    (string-to-number (match-string 2 heading))
		    (string-to-number (match-string 1 heading)))))))

(defun ensc/tkenter-parse-date (date &optional now)
  (let ((res '())
	(num 0)
	(has-num 0))
    (cl-loop for c across date do
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
      (setq res (append res (nthcdr (+ 3 (length res)) (or now ensc/tkenter-mode-now)))))

    (encode-time 0 0 0
		 (nth 0 res)
		 (nth 1 res)
		 (nth 2 res))))

(defun ensc/tkenter-format-date (date)
  (let ((res (format-time-string "%a %d.%m.%Y:" date))
	(inhibit-modification-hooks t))
    (put-text-property 0 (length res) 'face '(:inherit bold) res)
    res))

(defun ensc/tkenter-format-tag (tag)
  (let ((res (format "[%s]" tag))
	(inhibit-modification-hooks t))
    (put-text-property 0 (length res) 'face '(:inherit bold) res)
    res))

(defun ensc/tkenter-format-project (project)
  (let ((res project)
	(inhibit-modification-hooks t))
    (put-text-property 0 (length res) 'face '(:inherit bold) res)
    (when (not (ensc/tkenter-translate-project-raw project))
      (put-text-property 0 (length res) 'face '(:foreground "red") res))
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

(defun ensc/tkenter-format-tag-sum (tag sum)
  (if (and tag sum)
      (format "    |     %s %s (#%d)"
	      (ensc/tkenter-format-tag     tag)
	      (ensc/tkenter-format-effort  sum)
	      (nth 2 sum))
    ""))

(defun ensc/tkenter-run (_col row)
  (let ((date (ensc/tkenter-get-date row))
	(project (ensc/tkenter-get-project row))
	(desc-tag (ensc/tkenter-get-desc-tag row)))
    (when (and date project)
      (let ((sum-day     (ensc/tkenter-summary-date    date))
	    (date-parsed (ensc/tkenter-parse-date date))
	    (sum-project (ensc/tkenter-summary-project project))
	    (sum-desc    (and desc-tag (ensc/tkenter-summary-desc project desc-tag)))
	    (message-log-max nil))
	(message "%s %s (#%d)    |     %s: %s (#%d)%s"
		 (ensc/tkenter-format-date date-parsed)
		 (ensc/tkenter-format-effort sum-day)
		 (nth 2 sum-day)
		 (ensc/tkenter-format-project project)
		 (ensc/tkenter-format-effort  sum-project)
		 (nth 2 sum-project)
		 (ensc/tkenter-format-tag-sum desc-tag sum-desc)
		 )))))

(defmacro ensc/tkenter-within-cell (&rest body)
  ""
  (declare (indent 0) (debug t))
  `(when (and (org-at-table-p)
	      (not (org-at-table-hline-p)))
     (let ((col (org-table-current-column))
	   (row (org-table-current-line)))
       (when (and (/= col 0)(/= row 0))
	 (progn ,@body)))))

(defun ensc/tkenter-idle-fn (buf)
  (when (and ensc/tkenter-mode
	     (or (not buf) (eq (current-buffer) buf))
	     (string= major-mode "org-mode"))
    (ignore-errors
      (ensc/tkenter-within-cell
	(if ensc/tkenter-skip-timer
	    (setq ensc/tkenter-skip-timer nil)
	  (ensc/tkenter-run col row))))))

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
    (ensc/tkenter-within-cell
      (ensc/_tkenter-normalize-cell col row)))
  (org-cycle))

;;;###autoload
(define-minor-mode ensc/tkenter-mode
  "mode for entering tk entries"
  :lighter " TM"
  :keymap ensc/tkenter-keymap
  (if ensc/tkenter-mode
      (progn
	(setq ensc/tkenter-mode-now (ensc/tkenter-buffer-time))

	(add-hook 'kill-buffer-hook
		  (lambda () (when (timerp ensc/tkenter-idle-timer)
			       (cancel-timer ensc/tkenter-idle-timer))))
	(setq ensc/tkenter-idle-timer
	      (run-with-idle-timer ensc/tkenter-idle-delay t
				   'ensc/tkenter-idle-fn (current-buffer))))
    (cancel-timer ensc/tkenter-idle-timer))
  )

(defun ensc/tkenter-unittest-parse-effort (effort exp)
  (cl-assert (equal (ensc/tkenter-parse-effort effort) exp)))

(defun ensc/tkenter-unittest-extract-desc-tag (desc exp)
  "Unit-test helper: ensure that extracting tag from DESC equals EXP.
Raises an error if the expectation is not met."
  (unless (equal (ensc/tkenter-extract-desc-tag desc) exp)
    (error "ensc/tkenter-unittest-extract-desc-tag failed: %S -> expected %S"
           desc exp)))

(defun ensc/tkenter-unittest-guess-note (desc exp)
  "Unit-test helper: ensure that guessing note from DESC equals EXP.
Raises an error if the expectation is not met."
  (unless (equal (ensc/tkenter-guess-note desc) exp)
    (error "ensc/tkenter-unittest-guess-note failed: %S -> expected %S"
           desc exp)))

(defun ensc/tkenter-translate-project-raw (project)
  (plist-get
   (org-table-get-remote-range "project-mapping" ensc/tkenter-project-mapping-range)
   project
   #'equal))

(defun ensc/tkenter-translate-project (project)
  (let ((uuid (ensc/tkenter-translate-project-raw project)))
    (unless uuid
      (error "No such project %s" project))

    (substring-no-properties uuid)))

(defun ensc/tkenter-guess-note (desc)
  "Guess a short note from DESC"
  (when (and desc (stringp desc))
    (let ((s (downcase (string-trim-left desc))))
      (cond
       ((string-match-p "^telko\\b" s) "telko")
       ((string-match-p "^tests?\\b" s) "test")
       ((string-match-p "^\\(?:dokumentation\\|documentation\\)\\b" s) "doc")
       (t nil)))))

(defun ensc/_tkenter-transmit (_col row &optional force)
  (let* ((date    (format-time-string "%d.%m.%Y" (ensc/tkenter-parse-date (ensc/tkenter-get-non-null row :date))))
	 (project (ensc/tkenter-translate-project (ensc/tkenter-get-non-null row :project)))
	 (effort  (ensc/tkenter-parse-effort (ensc/tkenter-get-non-null row :effort)))
	 (desc	  (ensc/tkenter-table-get row :desc))
	 (note	  (or (ensc/tkenter-table-get row :note)
		      (ensc/tkenter-guess-note desc)))
	 (url	  (ensc/tkenter-table-get row :url))
	 (tag	  (ensc/tkenter-extract-desc-tag desc)))

    ;; Report "Already submitted" early for both transmission modes
    (when (and (not force) url (not (string= url "")))
      (error "Already submitted!"))

    (if (and project (string-prefix-p "@" project))
	(ensc/_tkenter-transmit-tfs (substring project 1) date effort desc note tag row)
      (ensc/_tkenter-transmit-cli project date effort desc note row))))

(defun ensc/_tkenter-transmit-cli (project date effort desc note row)
  "Transmit using the CLI program for PROJECT and update table ROW.

This encapsulates the previous implementation: call the external CLI, parse
the response, validate it and write the resulting URL into the table. The
function signals errors for unexpected responses or failures, matching the
previous behaviour."
  (let ((result (with-temp-buffer
		  (let ((code (call-process ensc/tkenter-cli-program
					    nil t t
					    "--batch"
					    (concat "@" project)
					    date
					    (concat "+"
						    (ensc/tkenter-format-effort-single (nth 0 effort) t)
						    "X+"
						    (ensc/tkenter-format-effort-single (nth 1 effort) t))
					    (or desc "")
					    (or note ""))))
		    (append (list code) (split-string (buffer-string)))))))

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

(defun ensc/_tkenter-transmit-tfs (project date effort desc note tag &optional row)
  "Placeholder for external transmit handling for PROJECT.

When a project field starts with '@' this function is called instead of
 the
regular CLI path. Implementers should replace this stub with actual behaviour.
The function should return a RESULT compatible with the current caller's
expectations (a list whose first element is an exit code).

This stub raises an error that includes all received arguments so callers can
see what would be handed to a real implementation when debugging."

  (let* ((prjid (split-string project))
	 (prjid (or (when tag (nth 1 prjid))
		    (nth 0 prjid)))
	 (args  (list "-w" prjid
		      "-d" date
		      "-t"
		      (concat (ensc/tkenter-format-effort-single (nth 0 effort) t)
			      "X"
			      (ensc/tkenter-format-effort-single (nth 1 effort) t))

		      (when tag  (list "--work-item-type" "task" "--subtask" tag))
		      (when note (list "--activity-type" note))
		      desc))
	 (args (-flatten args))
	 (process-environment (cons "RUST_LOG=warn" process-environment))
	 (stderr-file (make-temp-file "tk-tfs"))
	 (result (with-temp-buffer
                   (let ((code (apply #'call-process ensc/tkenter-tfs-program
				      nil (list t stderr-file) t
				      "add" args)))
                     (list code (buffer-string))))))
    (unwind-protect
	(pcase result
          (`(0 ,url)
           (org-table-put row (ensc/tkenter-column-get :url)
                          (format "[[%s][OK]]" (string-trim url)))
           (message "Transmitted as %s" url)
           (org-table-align))
          (`(,code ,_)
           (error "Failed to submit data (exit %d): %s"
                  code
                  (with-temp-buffer
                    (insert-file-contents stderr-file)
                    (buffer-string)))))
      (delete-file stderr-file))))

(defun ensc/tkenter-transmit (&optional force)
  (interactive)
  (ensc/tkenter-within-cell
    (setq ensc/tkenter-skip-timer t)
    (unwind-protect
	(let ((col (org-table-current-column))
	      (row (org-table-current-line)))
	  (when (and (/= col 0)(/= row 0))
	    (ensc/_tkenter-transmit col row force)))
      (ensc/_tkenter-find-todo col row +1))))

(defun ensc/_tkenter-find-todo (col row rel)
  (let ((moved nil))
    (save-excursion
      ;; when we are at a hline, move to the next/prev line
      (while (org-at-table-hline-p)
	(setq row (+ row rel)
	      moved t)
	(org-table-goto-line row)))
    ;; unless we fixed our position already, go on step in the desired
    ;; direction
    (unless moved
      (setq row (+ row rel))))

  (let (result (url t))
    (while (and url (not result))
      (setq url (ensc/tkenter-table-get row :url))
      (if url
	  (setq row (+ row rel))
	(setq result row)))
    ;; when we are still inside the table and found an empty line,
    ;; change position
    (if (and result (> row ensc/tkenter-num-header-rows))
	(progn
	  (org-table-goto-line result)
	  (org-table-goto-column col))
      (setq ensc/tkenter-skip-timer t)
      (message "You are a hero! Everything is complete!"))))

(defun ensc/tkenter-find-todo (rel)
  (when (and (org-at-table-p))
    (let ((col (org-table-current-column))
	  (row (org-table-current-line)))
      (when (and (/= col 0)(/= row 0))
	(unwind-protect
	    (let ((col (org-table-current-column))
		  (row (org-table-current-line)))
	      (when (and (/= col 0)(/= row 0))
		(ensc/_tkenter-find-todo col row rel)))
	  nil)))))

(defun ensc/tkenter-find-todo-prev ()
  (interactive)
  (ensc/tkenter-find-todo -1))

(defun ensc/tkenter-find-todo-next ()
  (interactive)
  (ensc/tkenter-find-todo +1))

(defun ensc/tkenter-find-table (table-id)
  (let (id-loc buffer loc)
    (org-with-wide-buffer
     (goto-char (point-min))
     (if (re-search-forward
	  (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
		  (regexp-quote table-id) "[ \t]*$")
	  nil t)
	 (setq buffer (current-buffer) loc (match-beginning 0))
       (setq id-loc (org-id-find table-id 'marker))
       (unless (and id-loc (markerp id-loc))
	 (user-error "Can't find table \"%s\"" table-id))
       (setq buffer (marker-buffer id-loc)
	     loc (marker-position id-loc)))

     (with-current-buffer buffer
       (goto-char loc)
       (forward-char 1)
       (unless (and (re-search-forward "^\\(\\*+ \\)\\|^[ \t]*|" nil t)
		    (not (match-beginning 1)))
	 (user-error "Cannot find a table at NAME or ID %s" table-id))
       (setq loc (point))))

    (list buffer loc)))

(defun ensc/tkenter-get-project-stats (project)
  (let ((efforts-table-pos (ensc/tkenter-find-table "efforts"))
	(row 2)
	(effort-pushed 0)
	(effort-pending 0)
	(effort-neg 0)
	cur effort url tmp-effort)
    (with-current-buffer (car efforts-table-pos)
      (org-with-wide-buffer
       (goto-char (nth 1 efforts-table-pos))

       (while (setq effort (org-table-get row (ensc/tkenter-column-get :effort))
		    cur (ensc/tkenter-get-project row)
		    url (org-table-get row (ensc/tkenter-column-get :url)))
	 (when (string-equal cur project)
	   (setq tmp-effort (ensc/tkenter-parse-effort effort))
	   (setq effort-neg (+ effort-neg (nth 1 tmp-effort)))
	   (setq tmp-effort (car tmp-effort))

	   (if (string-equal url "")
	       (setq effort-pending (+ effort-pending tmp-effort))
	     (setq effort-pushed (+ effort-pushed tmp-effort))))

	 (setq row (1+ row)))))

    (list effort-pushed effort-pending effort-neg)))

(defun ensc/tkenter-format-mapping-effort (effort style)
  (let ((res (ensc/tkenter-format-effort-single effort)))
    (cl-case style
      (:pushed
       (when (= effort 0)
	 (put-text-property 0 (length res) 'face '(:foreground "red") res)
	 (setq res (concat "*" res "*"))))

      (:pending
       (unless (= effort 0)
	 (put-text-property 0 (length res) 'face '(:foreground "red") res)
	 (setq res (concat "*" res "*"))))
      )

    (concat " " res " ")))

(defun ensc/tkenter-format-mapping-sale (effort fee)
  (format "%.2f" (/ (* effort fee) 3600.0)))

(defun ensc/tkenter-update-mapping-table ()
  (interactive)
  (let ((mapping-table-pos (ensc/tkenter-find-table "project-mapping"))
	(project nil)
	(tmp-effort nil)
	(tmp-fee nil)
	(row 2))
    (with-current-buffer (car mapping-table-pos)
      (org-with-wide-buffer
       (goto-char (nth 1 mapping-table-pos))

       (while (setq project (org-table-get row (ensc/tkenter-column-get :mapping-project)))
	 (unless (string-equal project "")
	   (setq tmp-effort (ensc/tkenter-get-project-stats project)
		 tmp-fee (substring-no-properties (org-table-get row (ensc/tkenter-column-get :mapping-fee))))

	   (org-table-put row (ensc/tkenter-column-get :mapping-pushed)
			  (if (equal tmp-effort '(0 0 0))
			      ""
			    (ensc/tkenter-format-mapping-effort (nth 0 tmp-effort) :pushed))))

	   (org-table-put row (ensc/tkenter-column-get :mapping-pending)
			  (if (equal tmp-effort '(0 0 0))
			      ""
			    (ensc/tkenter-format-mapping-effort (nth 1 tmp-effort) :pending)))

	   (org-table-put row (ensc/tkenter-column-get :mapping-sales)
			  (if (or (not tmp-fee) (string-equal tmp-fee "") (equal tmp-effort '(0 0 0)))
			      ""
			    (ensc/tkenter-format-mapping-sale
			     (+ (nth 0 tmp-effort) (nth 1 tmp-effort) (- (nth 2 tmp-effort)))
			     (string-to-number tmp-fee))))

	   (setq row (1+ row)))
       (goto-char (nth 1 mapping-table-pos))
       (org-table-recalculate 'iterate t)
       (org-table-align)))))

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
  ;; tag extraction tests
  (ensc/tkenter-unittest-extract-desc-tag "[tag] some text" "tag")
  (ensc/tkenter-unittest-extract-desc-tag "[a][b] rest" "a")
  (ensc/tkenter-unittest-extract-desc-tag "prefix [abc] rest" nil)
  (ensc/tkenter-unittest-extract-desc-tag "no tag here" nil)
  (ensc/tkenter-unittest-extract-desc-tag "[] empty" nil)
  ;; guess-note tests
  (ensc/tkenter-unittest-guess-note "telko Gespräch" "telko")
  (ensc/tkenter-unittest-guess-note "Telko: something" "telko")
  (ensc/tkenter-unittest-guess-note "Dokumentation of feature" "doc")
  (ensc/tkenter-unittest-guess-note "documentation details" "doc")
  (ensc/tkenter-unittest-guess-note "test abc" "test")
  (ensc/tkenter-unittest-guess-note "tests xyz" "test")
  (ensc/tkenter-unittest-guess-note "other text" nil)
  (ensc/tkenter-unittest-guess-note "" nil)
  )

(ensc/tkenter-unittest)

(provide 'ensc-tkenter)
(provide 'ensc/tkenter)
