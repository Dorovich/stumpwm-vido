;;; Asynchronous modeline redraw

(defparameter *mode-line-semaphore* (sb-thread:make-semaphore :count 0)
  "Semaphore to signal pending requests to the thread handling modeline redraws.")

(defparameter *mode-line-requests-lock* (sb-thread:make-mutex)
  "Lock to access the requests list atomically.")

(defvar *mode-line-async-update* nil
  "If non-nil, the the modeline is updated and redrawn asynchronously with a different thread.")

(defvar *mode-line-requests* nil
  "List of the screens the modeline should be redrawn for. If -1, modeline is redrawn for all screens")

(defun update-modelines-loop ()
  "Loop of the thread handling modeline redraws through the requests list."
  (loop while *mode-line-async-update* do
    (progn (sb-thread:wait-on-semaphore *mode-line-semaphore* :n 1)
           (let ((s nil))
             (sb-thread:with-mutex (*mode-line-requests-lock*)
               (setf s (car *mode-line-requests*))
               (setf *mode-line-requests* (cdr *mode-line-requests*)))
             (when (null s) (sb-ext:exit))
             (if (eq s -1)
                 (mapc 'redraw-mode-line *mode-lines*)
                 (dolist (mode-line (screen-mode-lines s))
                   (redraw-mode-line mode-line)))))))

(defparameter *mode-line-thread* nil)

(defun update-mode-lines (screen)
  "Update all mode lines on SCREEN"
  (if *mode-line-async-update*
      (progn
        (sb-thread:with-mutex (*mode-line-requests-lock*)
          (setf *mode-line-requests* (append *mode-line-requests* (cons screen nil))))
        (sb-thread:signal-semaphore *mode-line-semaphore* 1))
      (dolist (mode-line (screen-mode-lines screen))
        (redraw-mode-line mode-line))))

(defun update-all-mode-lines ()
  "Update all mode lines."
  (if *mode-line-async-update*
      (progn
        (sb-thread:with-mutex (*mode-line-requests-lock*)
          (setf *mode-line-requests* (append *mode-line-requests* (cons -1 nil))))
        (sb-thread:signal-semaphore *mode-line-semaphore* 1))
      (mapc 'redraw-mode-line *mode-lines*)))
