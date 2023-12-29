;;; Colorscheme

(defvar v/colors '((black . "#1C1B19")
                   (red . "#EF2F27")
                   (green . "#519F50")
                   (yellow . "#FBB829")
                   (blue . "#2C78BF")
                   (magenta . "#E02C6D")
                   (cyan . "#0AAEB3")
                   (white . "#BAA67F")
                   (bright-black . "#918175")
                   (bright-red . "#F75341")
                   (bright-green . "#98BC37")
                   (bright-yellow . "#FED06E")
                   (bright-blue . "#68A8E4")
                   (bright-magenta . "#FF5C8F")
                   (bright-cyan . "#2BE4D0")
                   (bright-white . "#FCE8C3")))

(defun v/color (name)
  "Get a color string from the `v/colors' alist. @arg{name} must be a symbol."
  (cdr (assoc name v/colors)))
