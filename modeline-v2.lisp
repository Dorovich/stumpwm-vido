;;; Asynchronous modeline redraw

(require :sb-concurrency)

(defparameter *mode-line-mailbox* (sb-concurrency:make-mailbox)
  "Queue of the pending requests to the thread handling modeline redraws.")

(defvar *mode-line-async-update* nil
  "If non-nil, the modeline is updated and redrawn asynchronously with a different thread.")

(defun update-modelines-loop ()
  "Loop of the thread handling modeline redraws through the requests list."
  (loop while *mode-line-async-update* do
    (let ((s (sb-concurrency:receive-message *mode-line-mailbox*)))
      (when (null s) (sb-thread:return-from-thread 0))
      (if (eq s -1)
          (mapc 'redraw-mode-line *mode-lines*)
          (dolist (mode-line (screen-mode-lines s))
            (redraw-mode-line mode-line))))))

(defparameter *mode-line-thread* nil
  "The thread that handles modeline redraws.")

(defun update-mode-lines (screen)
  "Update all mode lines on SCREEN."
  (if *mode-line-async-update*
      (sb-concurrency:send-message *mode-line-mailbox* screen)
      (dolist (mode-line (screen-mode-lines screen))
        (redraw-mode-line mode-line))))

(defun update-all-mode-lines ()
  "Update all mode lines."
  (if *mode-line-async-update*
      (sb-concurrency:send-message *mode-line-mailbox* -1)
      (mapc 'redraw-mode-line *mode-lines*)))

;;; Commands

(defcommand toggle-mode-line-async-on () ()
  "Turn on the multithreaded mode-line."
  (unless *mode-line-async-update*
    (setf *mode-line-async-update* t)
    (setf *mode-line-thread* (sb-thread:make-thread #'update-modelines-loop))))

(defcommand toggle-mode-line-async-off () ()
  "Turn off the multithreaded mode-line."
  (when *mode-line-async-update*
    (setf *mode-line-async-update* nil)
    (sb-concurrency:send-message *mode-line-mailbox* nil)
    (sb-thread:join-thread *mode-line-thread* :timeout 5)))
