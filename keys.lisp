;;; Key mappings

;; Root map
(set-prefix-key (kbd "s-SPC"))
(define-key *root-map* (kbd "ESC") "abort")
(define-key *root-map* (kbd "q") "logout")
(define-key *root-map* (kbd "v") "hsplit-and-focus")
(define-key *root-map* (kbd "s") "vsplit-and-focus")
(define-key *root-map* (kbd "L") "lock")
(define-key *root-map* (kbd "F") "fullscreen")

(undefine-key *root-map* (kbd "r"))
(define-key *root-map* (kbd "r") "remove")
(undefine-key *root-map* (kbd "R"))
(define-key *root-map* (kbd "R") "iresize")

;; Windows
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

(define-key *top-map* (kbd "s-M-h") "clean-resize-direction left")
(define-key *top-map* (kbd "s-M-j") "clean-resize-direction down")
(define-key *top-map* (kbd "s-M-k") "clean-resize-direction up")
(define-key *top-map* (kbd "s-M-l") "clean-resize-direction right")

(define-key *top-map* (kbd "s-c") "pull-hidden-next")
(define-key *top-map* (kbd "s-n") "fnext")
(define-key *top-map* (kbd "s-p") "fprev")
(define-key *top-map* (kbd "s-v") "windowlist")
(define-key *top-map* (kbd "s-f") "smart-fullscreen")
(define-key *top-map* (kbd "s-t") "toggle-floating")
(define-key *top-map* (kbd "s-x") *exchange-window-map*)

(define-key *top-map* (kbd "s-w") "delete")
(define-key *top-map* (kbd "s-q") "remove")

(defvar *window-split-map* nil
  "The keymap that frame splitting key bindings sit on.")

(fill-keymap *window-split-map*
             (kbd "s") "vsplit-and-focus"
             (kbd "v") "hsplit-and-focus"
             (kbd "o") "only"
             (kbd "i") "iresize"
             (kbd "r") "remove")
(define-key *top-map* (kbd "s-s") *window-split-map*)

;; Groups
(define-key *top-map* (kbd "s-1") (concat "gselect " (elt *group-names* 0)))
(define-key *top-map* (kbd "s-2") (concat "gselect " (elt *group-names* 1)))
(define-key *top-map* (kbd "s-3") (concat "gselect " (elt *group-names* 2)))
(define-key *top-map* (kbd "s-4") (concat "gselect " (elt *group-names* 3)))
(define-key *top-map* (kbd "s-5") (concat "gselect " (elt *group-names* 4)))

(define-key *top-map* (kbd "s-!") (concat "gmove " (elt *group-names* 0)))
(define-key *top-map* (kbd "s-\"") (concat "gmove " (elt *group-names* 1)))
(define-key *top-map* (kbd "s-periodcentered") (concat "gmove " (elt *group-names* 2)))
(define-key *top-map* (kbd "s-$") (concat "gmove " (elt *group-names* 3)))
(define-key *top-map* (kbd "s-%") (concat "gmove " (elt *group-names* 4)))

;; Utils
(define-key *top-map* (kbd "s-g") "abort")
(define-key *top-map* (kbd "s-R") "loadrc")
(define-key *top-map* (kbd "s-Q") "logout")
(define-key *top-map* (kbd "s-ESC") "lock")
(define-key *top-map* (kbd "s-BackSpace") "colon")

;; Programs
(define-key *top-map* (kbd "s-RET") "term")
(define-key *top-map* (kbd "s-b") "firefox")
(define-key *top-map* (kbd "s-e") "emacsclient")
(define-key *top-map* (kbd "s-a") "file-manager")
(define-key *top-map* (kbd "s-m") "music-player")
(define-key *top-map* (kbd "s-d") (if *is-laptop*
                                      "launch"
                                      "run-shell-command dmenu_run -h 12 -c -g 3 -l 10"))

;; Scripts
(define-key *top-map* (kbd "s-Print") "screenshot")
(define-key *top-map* (kbd "s-Insert") (make-script nil "snippet" "load"))
(define-key *top-map* (kbd "s-+") (make-script nil "volume" "up"))
(define-key *top-map* (kbd "s--") (make-script nil "volume" "down"))

(defvar *scripts-map* nil
  "The keymap that script-related key bindings sit on.")

(fill-keymap *scripts-map*
             (kbd "r") (make-script nil)
             (kbd "m") (make-script nil "music")
             (kbd "o") (make-script nil "sinks")
             (kbd "p") (make-script nil "pass")
             (kbd "s") (make-script nil "scrot")
             (kbd "k") (make-script nil "kill")
             (kbd "l") (make-script nil "steamapp")
             (kbd "i") (make-script nil "snippet load")
             (kbd "q") (make-script nil "quit"))
(define-key *top-map* (kbd "s-r") *scripts-map*)
