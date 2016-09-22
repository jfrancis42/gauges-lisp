(asdf:defsystem #:gauges
  :depends-on (:lispbuilder-sdl
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-gfx)
  :components ((:file "package")
	       (:file "gauges")))
