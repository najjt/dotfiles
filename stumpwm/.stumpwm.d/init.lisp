;; -*-lisp-*-

;; Initialize quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Needed for stumptray
(ql:quickload "xembed")

;; Use stumpwm package and declare it as the default
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Start background processes
(run-shell-command "~/scripts/autostart")

;; Modules
(set-module-dir "~/src/stumpwm-contrib/")

(load-module "end-session") ; Gracefully end programs when ending user session
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "stumptray")
(load-module "pianobar") ; Music modeline module

;; Don't show a startup message
(setf *startup-message* nil)

;; Font
;; (ql:quickload :clx-truetype)
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

;; Mouse settings
(setf *mouse-focus-policy*    :click  ; Focus follows mouse click
      *float-window-modifier* :SUPER) ; Move floating windows with super key and mouse

;; Load custom config files
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/keys.lisp")
(load "~/.stumpwm.d/modeline.lisp")
