;;; Commands

(defcommand hsplit-and-focus () ()
  "create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "create a new frame below and focus it."
  (vsplit)
  (move-focus :down))

(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window."
  (delete-window)
  (remove-split))

(defcommand toggle-floating () ()
  "Toggle the floating state of the current window"
  (if (float-window-p (current-window))
      (unfloat-this)
    (float-this)))

(defcommand lock (&optional suspendp) ()
  "Lock the screen, and if @arg{suspendp} is non-nil, also suspend the system."
    (xlib:unmap-window (screen-message-window (current-screen)))
    (run-shell-command "slock")
    (when suspendp (run-shell-command "systemctl suspend")))

(defcommand screenshot () ()
  "Take a screenshot of the whole monitor."
  (make-script t "scrot" "full")
  (sleep 0.5)
  (message "Screenshot taken!"))

(defcommand smart-fullscreen () ()
  "Fullscreen the current window, only if there is one."
  (when (current-window)
    (fullscreen)))

(defcommand hdmi-side () ()
  (run-shell-command "xrandr --output HDMI-A-0 --auto --right-of DP-0")
  (run-shell-command "nitrogen --restore")
  (enable-mode-line-everywhere))

(defcommand hdmi-reset () ()
  (run-shell-command "xrandr --output DP-0 --auto"))

(defcommand clean-resize-direction (d)
  ((:direction "Direction: "))
  "Resize frame to direction @var{d} without showing frame borders."
  (resize-direction d)
  (clear-frame-outlines (current-group)))

(defcommand term (&optional program) ()
  "Invoke a terminal, possibly with a @arg{program}."
  (sb-thread:make-thread (lambda ()
                           (run-shell-command (if program
                                                  (format nil "st ~A" program)
                                                  "st")))))

(defcommand chromium () ()
  "Start Ungoogled Chromium or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "ungoogled-chromium" '(:class "Chromium-browser") t nil))))

(defcommand firefox () ()
  "Start Firefox or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "firefox" '(:class "firefox") t nil))))

(defcommand emacsclient () ()
  "Start the Emacs client or switch to it, if it is already running on any group. If no client is running, a new instance is created."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "emacsclient -a 'emacs' -c" '(:class "Emacs") t nil))))

(defcommand dired () ()
  "Start Dired on the Emacs client or switch to it, if it is already running on any group. If no client is running, a new instance is created."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "emacsclient -a 'emacs' -c" '(:class "Emacs") t nil)
                           (run-shell-command "emacsclient -e '(dired nil)'"))))

(defcommand file-manager () ()
  "Start PCManFM or switch to it, if it is already running on the current group (any screen)."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "pcmanfm" '(:class "Pcmanfm") nil t))))

(defcommand music-player () ()
  "Start Qmmp or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "qmmp" '(:class "qmmp") t nil))))

(defcommand mpc () ()
  "Start mpc-mode on the Emacs client or switch to it, if it is already running on any group. If no client is running, a new instance is created."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "emacsclient -a 'emacs' -c" '(:class "Emacs") t nil)
                           (run-shell-command "emacsclient -e '(mpc)'"))))

(defcommand steam () ()
  "Start Steam or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "steam" '(:class "steam") t nil))))

(defcommand spotify () ()
  "Start Spotify or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "spotify" '(:class "Spotify") t nil))))

(defcommand mail () ()
  "Start Thunderbird or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "thunderbird" '(:class "thunderbird") t nil))))

(defcommand discord () ()
  "Start Discord (WebCord) or switch to it, if it is already running on any group."
  (sb-thread:make-thread (lambda ()
                           (run-or-raise "webcord" '(:class "WebCord") t nil))))

(defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda ()
                           (slynk:create-server :port *slynk-port* :dont-close t)
                           (setf *slynk-started* t))))

(defcommand sly-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda ()
                           (slynk:stop-server *slynk-port*)
                           (setf *slynk-started* nil))))

(defcommand toggle-mode-line-async-on () ()
  "Turn on the multithreaded mode-line."
  (unless *mode-line-async-update*
    (setf *mode-line-async-update* t)
    (setf *mode-line-thread* (sb-thread:make-thread #'update-modelines-loop))))

(defcommand toggle-mode-line-async-off () ()
  "Turn off the multithreaded mode-line."
  (when *mode-line-async-update*
    (setf *mode-line-async-update* nil)
    (sb-thread:signal-semaphore *mode-line-semaphore* 1)
    (sb-thread:join-thread *mode-line-thread*)))
