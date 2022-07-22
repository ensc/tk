;;; Copyright (C) 2015 Enrico Scholz <enrico.scholz@sigma-chemnitz.de>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; version 3 of the License.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'bindat)
(require 'cl)

(defcustom ensc/tk-idle-timer-limit 120
  "idle timeout in seconds")

(defvar ensc/tk-idle-timer-list nil)
(defvar ensc/_tk-check-idle-timer nil)
(defvar ensc/_tk-buffer-current nil)
(defvar ensc/_tk-buffer-updated-timer nil)
(defvar ensc/_tk-events nil)
(defvar ensc/_tk-network nil)
(defvar ensc/_tk-in-send nil)

(defconst ensc/_tk-event-hdr-spec
  '((:code u8)
    (:timestamp_hi u32)
    (:timestamp_lo u32)))

(defconst ensc/_tk-event-string-spec
  '((:len u32)
    (:str str (:len))))

(defconst ensc/_tk-event-buffer-spec
  '((:name struct ensc/_tk-event-string-spec)
    (:file struct ensc/_tk-event-string-spec)
    (:size u32)
    (:modified u8)))

(defconst ensc/_tk-event-connect
  '((:pid     u32)
    (:id      struct ensc/_tk-event-string-spec)
    (:detail  struct ensc/_tk-event-string-spec)))

(defun ensc/_tk-pack-string (str)
  "Pack a string"
  `((:len . ,(length str))
    (:str . ,str)))

(defcustom ensc/tk-ignore-buffers
  '(" *Minibuf-1*"
    "*Completions*"
    "*Messages*"
    "*Group*"
    "*vc-diff*"
    "*grep*"
    "*compilation*"
    "*Commit Message*"
    "*Summary INBOX*"
    "*Article INBOX*"
    "*git-status*")
  "buffers to be ignored from being logged")

(defun ensc/_tk-pack-buffer (buffer)
  "Pack information about a buffer"
  (let* ((name (when buffer (buffer-name buffer)))
	 (file (when buffer (buffer-file-name buffer)))
	 (size (when buffer (buffer-size buffer)))
	 (modified (when buffer (buffer-modified-p buffer)))
	 (args
	  `((:name . ,(ensc/_tk-pack-string (or name "")))
	    (:file . ,(ensc/_tk-pack-string (or file "")))
	    (:size . ,(or size 0))
	    (:modified . ,(if modified 1 0)))))
    (bindat-pack ensc/_tk-event-buffer-spec args)))

(defun ensc/_tk-pack-connect ()
  (bindat-pack ensc/_tk-event-connect
	       `((:pid    . ,(emacs-pid))
		 (:id     . ,(ensc/_tk-pack-string "emacs"))
		 (:detail . ,(ensc/_tk-pack-string (combine-and-quote-strings command-line-args))))))

(defun ensc/_tk-pack-event (code &rest args)
  "Pack an event"
  (cl-case code
    (?I '(?I nil))
    (?i '(?i nil))
    (?C (list ?C (ensc/_tk-pack-connect)))
    (?B (list ?B (apply 'ensc/_tk-pack-buffer args)))
    (?b (list ?b (apply 'ensc/_tk-pack-buffer args)))
    (progn
      (message "Unknown event %s" code)
      nil)))

(defun ensc/_tk-network-process-p (&optional proc)
  (let ((proc (or proc ensc/_tk-network)))
    (and proc
	 (process-live-p proc))))

(defun ensc/_tk-network-kill ()
  (let ((p ensc/_tk-network))
    (setq ensc/_tk-network nil)
    (when (ensc/_tk-network-process-p p)
      (delete-process p))))

(defun ensc/_tk-network-change (process event)
  (cond
   ((string= event "open\n")
    (progn
      (message "opening connection to tk server...")
      (ensc/_tk-network-kill)
      (setq ensc/_tk-network process)
      (ensc/tk-submit-event ?C)
      (ensc/_tk-trigger-send)))
   (t
    (progn
      (message "event: %s" event)
      (when (eq process ensc/_tk-network)
	(ensc/_tk-network-kill))))))

(defun ensc/_tk-network-connect ()
  (condition-case nil
      (make-network-process
       :name "tk"
       :type nil
       :host 'local
       :remote (concat (getenv "XDG_RUNTIME_DIR") "/tk.sock")
       :nowait t
       :coding 'binary
       :sentinel 'ensc/_tk-network-change
       :noquery t)
    nil))

(defun ensc/_tk-trigger-send-inner ()
  (let ((msg (car ensc/_tk-events)))
    ; (message "sending data %s" msg)
    (process-send-string ensc/_tk-network msg)
    (pop ensc/_tk-events)))

(defun ensc/_tk-trigger-send ()
  (unless ensc/_tk-in-send
    ;; TODO: is there a race?
    (setq ensc/_tk-in-send t)
    (condition-case err
	(progn
	  (unless (ensc/_tk-network-process-p)
	    (ensc/_tk-network-connect))
	  (while (and ensc/_tk-events (ensc/_tk-network-process-p))
	    (ensc/_tk-trigger-send-inner)))
	(error
	 (progn
	   (error-message-string err)
	   (ensc/_tk-network-kill))))
    (setq ensc/_tk-in-send nil)))

(defun ensc/_tk-enqueue-event (code tm data)
  "Enqueue an event"
  (let* ((tm (floor tm))
	 (hdr (bindat-pack ensc/_tk-event-hdr-spec
			   `((:code . ,code)
			     (:timestamp_hi . ,(lsh tm -32))
			     (:timestamp_lo . ,(logand tm #16rffffffff)))))
	 (payload (concat hdr data))
	 (sz (bindat-pack '((:sz u32)) `((:sz . ,(length payload)))))
	 (data (concat sz payload)))
    (setq ensc/_tk-events (nconc ensc/_tk-events (list data)))
    (ensc/_tk-trigger-send)))

(defun ensc/tk-submit-event-reltime (code tm-delta &rest args)
  "Submit an event which happened some time ago"
  (let ((tm (- (time-to-seconds (current-time))
	       (if (listp tm-delta)
		   (time-to-seconds tm-delta)
		 (or tm-delta 0))))
	(event (apply 'ensc/_tk-pack-event code args)))
    (when event
      (ensc/_tk-enqueue-event (nth 0 event) tm (nth 1 event)))))

(defun ensc/tk-submit-event (code &rest args)
  "Submit an event which happens now"
  (apply 'ensc/tk-submit-event-reltime code 0 args))

(defun ensc/_tk-leave-idle ()
  (when (or (not (current-idle-time))
	    (< (time-to-seconds (current-idle-time)) 2))
    (ensc/tk-submit-event-reltime ?i (current-idle-time))
    ; (message "leaving idle... %s" (current-idle-time))
    (when ensc/_tk-check-idle-timer
      (cancel-timer ensc/_tk-check-idle-timer)
      (setq ensc/_tk-check-idle-timer nil))))

(defun ensc/tk-go-idle (&optional time)
  ; (message "going idle")
  (ensc/tk-submit-event ?I)
  (when ensc/_tk-check-idle-timer
    (cancel-timer ensc/_tk-check-idle-timer))
  (setq ensc/_tk-check-idle-timer
	(run-with-idle-timer 0 t 'ensc/_tk-leave-idle)))

(defun ensc/_tk-buffer-ignored-p (&optional buf)
  (let* ((buf (or buf (current-buffer)))
	 (bufname (when buf (buffer-name buf))))
    (or (not (buffer-live-p buf))
	(member bufname ensc/tk-ignore-buffers))))

(defun ensc/_tk-buffer-updated-late ()
  (unless (ensc/_tk-buffer-ignored-p)
    (let ((buffer (current-buffer)))
      (when (and (bufferp ensc/_tk-buffer-current)
		 (not (eq ensc/_tk-buffer-current buffer)))
	(ensc/tk-submit-event ?b ensc/_tk-buffer-current))
      (when (or (not (bufferp ensc/_tk-buffer-current))
		(not (eq ensc/_tk-buffer-current buffer))
		;; prevent records caused by events triggered by
		;; autosave or backup features
		(and (buffer-modified-p buffer)
		     (< (time-to-seconds (current-idle-time)) 30)))
	(ensc/tk-submit-event ?B buffer))
      (setq ensc/_tk-buffer-current buffer))))

(defun ensc/tk-buffer-updated ()
  (when ensc/_tk-buffer-updated-timer
    (cancel-timer ensc/_tk-buffer-updated-timer))

  (setq ensc/_tk-buffer-updated-timer
	(run-with-timer 1 nil 'ensc/_tk-buffer-updated-late)))

(defun ensc/tk-start ()
  "start timekeeping timer"
  (interactive)

  (add-hook 'buffer-list-update-hook 'ensc/tk-buffer-updated)

  (add-to-list 'ensc/tk-idle-timer-list
	       (run-with-idle-timer ensc/tk-idle-timer-limit t
				    'ensc/tk-go-idle ensc/tk-idle-timer-limit))
  t)

(defun ensc/tk-stop ()
  "stop timekeeping timer"
  (interactive)

  (remove-hook 'buffer-list-update-hook 'ensc/tk-buffer-updated)

  (dolist (timer-object ensc/tk-idle-timer-list)
    (cancel-timer timer-object))

  (setq ensc/tk-idle-timer-list nil)
  (ensc/_tk-network-kill)
  (setq ensc/_tk-in-send nil)
  t)

; (ensc/tk-start)
