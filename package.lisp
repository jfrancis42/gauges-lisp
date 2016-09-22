(defpackage :gauges
  (:use :cl :lispbuilder-sdl)
  (:export :deg-to-rad
	   :led
	   :seven-seg
	   :draw-sats
	   :initialize
	   :gauge
	   :led-color
	   :draw-gauge
	   :draw-value
	   :set-gauge-value))

