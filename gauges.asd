(asdf:defsystem #:gauges
  :description "Fancy GUI gauges."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:lispbuilder-sdl
	       #:lispbuilder-sdl-ttf
	       #:lispbuilder-sdl-gfx)
  :components ((:file "package")
	       (:file "gauges")))
