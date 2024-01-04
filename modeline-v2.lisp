;;; Asynchronous modeline redraw

(require :sb-concurrency)

(defparameter *mode-line-mailbox* (sb-concurrency:make-mailbox)
  "Queue of the pending requests to the thread handling modeline redraws.")

(defvar *mode-line-async-update* nil
  "If non-nil, the the modeline is updated and redrawn asynchronously with a different thread.")

(defun update-modelines-loop ()
  "Loop of the thread handling modeline redraws through the requests list."
  (loop while *mode-line-async-update* do
    (let ((s (sb-concurrency:receive-message *mode-line-mailbox*)))
      (when (null s) (sb-ext:exit))
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
