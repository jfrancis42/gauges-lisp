(in-package :gauges)

;; x=r*cos(a)
;; y=r*sin(a)

;; (sdl:color :r 255 :g 255 :b 255)

(defmacro deg-to-rad (d) `(/ (* ,d pi) 180))
(defmacro rad-to-deg (r) `(/ (* ,r 180) pi))

(defstruct box coords color)

(defun map-range (a1 a2 b1 b2 s)
  (+ b1
     (/ (* (- s a1)
	   (- b2 b1))
	(- a2 a1))))

(defun at-least (a b)
  (if (< b a) a b))

(defun at-most (a b)
  (if (> b a) a b))

(defclass seven-seg ()
  ((top-x :accessor top-x
	  :initarg :top-x
	  :initform nil)
   (top-y :accessor top-y
	  :initarg :top-y
	  :initform nil)
   (scale :accessor scale
	 :initarg :scale
	 :initform 2.0)
   (boxes :accessor boxes
	  :initarg :boxes
	  :initform nil)
   (segments :accessor segments
	     :initarg :segments
	     :initform nil)
   (bezel-color :accessor bezel-color
		:initarg :bezel-color
		:initform (sdl:color :r 127 :g 127 :b 127))
   (my-value  :accessor my-value
	      :initarg :my-value
	      :initform 0)))

(defmethod calc-segment ((g seven-seg) x y w h)
  (sdl:rectangle
   :x (sdl:cast-to-int (+ (* (scale g) x) (top-x g)))
   :y (sdl:cast-to-int (+ (* (scale g) y) (top-y g)))
   :w (sdl:cast-to-int (* (scale g) w))
   :h (sdl:cast-to-int (* (scale g) h))))

(defmethod draw-segment ((g seven-seg) seg color)
  (sdl-gfx:draw-box (cdr (assoc seg (segments g))) :color color))

(defmethod initialize ((g seven-seg))
  (setf (boxes g)
	(list
	 (make-box
	  :coords (sdl:rectangle :x (sdl:cast-to-int (top-x g))
				 :y (sdl:cast-to-int (top-y g))
				 :w (sdl:cast-to-int (* (scale g) 14))
				 :h (sdl:cast-to-int (* (scale g) 21)))
	  :color (bezel-color g))
	 (make-box
	  :coords (sdl:rectangle :x (sdl:cast-to-int (+ (top-x g) (* 2 (scale g))))
				 :y (sdl:cast-to-int (+ (top-y g) (* 2 (scale g))))
				 :w (sdl:cast-to-int (- (* (scale g) 14) (* (scale g) 4)))
				 :h (sdl:cast-to-int (- (* (scale g) 21) (* (scale g) 4))))
	  :color sdl:*black*)))
  (setf (segments g) nil)
  (setf (segments g) (cons (cons :a (calc-segment g 4 5 1 5)) (segments g)))
  (setf (segments g) (cons (cons :b (calc-segment g 5 4 4 1)) (segments g)))
  (setf (segments g) (cons (cons :c (calc-segment g 9 5 1 5)) (segments g)))
  (setf (segments g) (cons (cons :d (calc-segment g 5 10 4 1)) (segments g)))
  (setf (segments g) (cons (cons :e (calc-segment g 4 11 1 5)) (segments g)))
  (setf (segments g) (cons (cons :f (calc-segment g 9 11 1 5)) (segments g)))
  (setf (segments g) (cons (cons :g (calc-segment g 5 16 4 1)) (segments g))))

(defmethod draw-gauge ((g seven-seg))

  (sdl-gfx:draw-box
   (sdl:rectangle
    :x (sdl:cast-to-int (top-x g))
    :y (sdl:cast-to-int (top-y g))
    :w (sdl:cast-to-int (* (scale g) 14))
    :h (sdl:cast-to-int (* (scale g) 21)))
   :color (bezel-color g))

  (sdl-gfx:draw-box
   (sdl:rectangle
    :x (sdl:cast-to-int (+ (top-x g) (* 2 (scale g))))
    :y (sdl:cast-to-int (+ (top-y g) (* 2 (scale g))))
    :w (sdl:cast-to-int (- (* (scale g) 14) (* (scale g) 4)))
    :h (sdl:cast-to-int (- (* (scale g) 21) (* (scale g) 4))))
   :color sdl:*black*)

  (draw-segment g :a (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :b (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :c (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :d (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :e (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :f (sdl:color :r 63 :g 0 :b 0))
  (draw-segment g :g (sdl:color :r 63 :g 0 :b 0))
  
  (mapc (lambda (n)
	  (if (equal n 'a) (draw-segment g :a (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'b) (draw-segment g :b (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'c) (draw-segment g :c (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'd) (draw-segment g :d (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'e) (draw-segment g :e (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'f) (draw-segment g :f (sdl:color :r 255 :g 0 :b 0)))
	  (if (equal n 'g) (draw-segment g :g (sdl:color :r 255 :g 0 :b 0))))
	(cond
	  ((= 0 (my-value g)) (list 'a 'b 'c 'e 'f 'g))
	  ((= 1 (my-value g)) (list 'c 'f))
	  ((= 2 (my-value g)) (list 'b 'c 'd 'e 'g))
	  ((= 3 (my-value g)) (list 'b 'c 'd 'f 'g))
	  ((= 4 (my-value g)) (list 'a 'c 'd 'f))
	  ((= 5 (my-value g)) (list 'a 'b 'd 'f 'g))
	  ((= 6 (my-value g)) (list 'a 'b 'd 'e 'f 'g))
	  ((= 7 (my-value g)) (list 'a 'b 'c 'f))
	  ((= 8 (my-value g)) (list 'a 'b 'c 'd 'e 'f 'g))
	  ((= 9 (my-value g)) (list 'a 'b 'c 'd 'f 'g))
	  (t nil)))
    )

(defmethod set-gauge-value ((g seven-seg) value)
  (setf (my-value g) value))

(defclass led ()
  ((center-x :accessor center-x
	     :initarg :center-x
	     :initform nil)
   (center-y :accessor center-y
	     :initarg :center-y
	     :initform nil)
   (my-value  :accessor my-value
	      :initarg :my-value
	      :initform nil)
   (bezel-color :accessor bezel-color
		:initarg :bezel-color
		:initform sdl:*white*)
   (led-color :accessor led-color
	      :initarg :led-color
	      :initform :red)))

(defmethod initialize ((g led))
  t)

(defmethod draw-gauge ((g led))
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g)) (sdl:cast-to-int (center-y g)) 6
				:color (bezel-color g))
  (if (my-value g)
      (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g)) (sdl:cast-to-int (center-y g)) 4
				    :color (cond
					     ((equal :green (led-color g))
					      (sdl:color :r 0 :g 255 :b 0))
					     ((equal :yellow (led-color g))
					      (sdl:color :r 255 :g 255 :b 0))
					     ((equal :blue (led-color g))
					      (sdl:color :r 0 :g 0 :b 255))
					     ((equal :white (led-color g))
					      (sdl:color :r 255 :g 255 :b 255))
					     (t
					      (sdl:color :r 255 :g 0 :b 0))))
      (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g)) (sdl:cast-to-int (center-y g)) 4
				    :color (cond
					     ((equal :green (led-color g))
					      (sdl:color :r 0 :g 63 :b 0))
					     ((equal :yellow (led-color g))
					      (sdl:color :r 63 :g 63 :b 0))
					     ((equal :blue (led-color g))
					      (sdl:color :r 0 :g 0 :b 63))
					     ((equal :white (led-color g))
					      (sdl:color :r 15 :g 15 :b 15))
					     (t
					      (sdl:color :r 255 :g 0 :b 0))))
      )
  )

(defmethod set-gauge-value ((g led) value)
  (setf (my-value g) value))

(defclass gauge ()
  ((dial-min :accessor dial-min
	     :initarg :dial-min
	     :initform 0)
   (dial-max :accessor dial-max
	     :initarg :dial-max
	     :initform 100)
   (angle-min :accessor angle-min
	      :initarg :angle-min
	      :initform 180)
   (angle-max :accessor angle-max
	      :initarg :angle-max
	      :initform 360)
   (center-x :accessor center-x
	     :initarg :center-x
	     :initform nil)
   (center-y :accessor center-y
	     :initarg :center-y
	     :initform nil)
   (my-frame-rate :accessor my-frame-rate
		  :initarg :my-frame-rate
		  :initform nil)
   (tick-holder :accessor tick-holder
		:initarg :tick-holder
		:initform nil)
   (tick-marks :accessor tick-marks
	       :initarg :tick-marks
	       :initform nil)
   (draw-value :accessor draw-value
	       :initarg :draw-value
	       :initform nil)
   (my-value :accessor my-value
	      :initarg :my-value
	      :initform 0)
   (target :accessor target
	   :initarg :target
	   :initform nil)
   (value-loc :accessor value-loc
	      :initarg :value-loc
	      :initform nil)
   (title-loc :accessor title-loc
	      :initarg :title-loc
	      :initform nil)
   (title :accessor title
	   :initarg :title
	   :initform "")
   (radius :accessor radius
	   :initarg :radius
	   :initform nil)
   (hub-radius :accessor hub-radius
	       :initarg :hub-radius
	       :initform nil)
   (rad :accessor rad
	:initarg :rad
	:initform 0)
   (pointer-color :accessor pointer-color
		  :initarg :pointer-color
		  :initform sdl:*red*)
   (speedo-color :accessor speedo-color
		 :initarg :speedo-color
		 :initform sdl:*white*)))

(defmethod initialize ((g gauge))
  (setf (title-loc g) (list
		       (sdl:cast-to-int (- (center-x g) (* (length (title g)) 4)))
		       (sdl:cast-to-int (+ (center-y g) (radius g) 10))))
  (setf (value-loc g) (list
		       (sdl:cast-to-int (center-x g))
		       (sdl:cast-to-int (- (center-y g) (radius g) 15))))
  (setf (hub-radius g) (sdl:cast-to-int (at-least 3 (/ (radius g) 30))))
  (when (tick-marks g)
    (let ((ang (/ (- (deg-to-rad (angle-min g)) (deg-to-rad (angle-max g))) (tick-marks g))))
      (loop for i from 0 to (tick-marks g) do

	   (setf (tick-holder g) (cons (list (list (sdl:cast-to-int (+ (center-x g) (* 0.85 (radius g) (cos (* i ang)))))
						   (sdl:cast-to-int (+ (center-y g) (* 0.85 (radius g) (sin (* i ang))))))
					     (list (sdl:cast-to-int (+ (center-x g) (* (radius g) (cos (* i ang)))))
						   (sdl:cast-to-int (+ (center-y g) (* (radius g) (sin (* i ang)))))))
				       (tick-holder g)))))))

(defmethod draw-gauge ((g gauge))
  (cond
    ((> (target g) (rad g))
     (setf (rad g) (+ (rad g) (/ (- (target g) (rad g)) (my-frame-rate g)))))
    ((< (target g) (rad g))
     (setf (rad g) (- (rad g) (/ (- (rad g) (target g)) (my-frame-rate g))))))
  
  ;; Draw the background of the gauge.
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g)) (sdl:cast-to-int (center-y g)) (sdl:cast-to-int (radius g))
				:color (speedo-color g))

  ;; Fill in the middle with black, leaving a bright ring.
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g)) (sdl:cast-to-int (center-y g)) (sdl:cast-to-int (- (radius g) 2))
				:color sdl:*black*)

  ;; Put the specified number of tick marks around the inside of the
  ;; ring.
  (when (tick-marks g)
    (mapc (lambda (n)
	    (sdl-gfx:draw-line (sdl:point :x (first (first n))
					  :y (second (first n)))
			       (sdl:point :x (first (second n))
					  :y (second (second n)))
			       :color (speedo-color g)))
	  (tick-holder g)))
  
  ;; Draw the indicator.
  (sdl-gfx:draw-filled-trigon (sdl:point :x (sdl:cast-to-int (+ (center-x g) (* (hub-radius g) (cos (+ (rad g) 1.5707963267948966d0)))))
					 :y (sdl:cast-to-int (+ (center-y g) (* (hub-radius g) (sin (+ (rad g) 1.5707963267948966d0))))))
			      (sdl:point :x (sdl:cast-to-int (+ (center-x g) (* (hub-radius g) (cos (- (rad g) 1.5707963267948966d0)))))
					 :y (sdl:cast-to-int (+ (center-y g) (* (hub-radius g) (sin (- (rad g) 1.5707963267948966d0))))))
			      (sdl:point :x (sdl:cast-to-int (+ (center-x g) (* 0.8 (radius g) (cos (rad g)))))
					 :y (sdl:cast-to-int (+ (center-y g) (* 0.8 (radius g) (sin (rad g))))))
			      :color (pointer-color g))

  ;; Draw the "hub" in the middle of the gauge.
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (center-x g))
				(sdl:cast-to-int (center-y g))
				(hub-radius g)
				:color (speedo-color g))
  
  ;; Put the title underneath the gauge.
  (sdl:draw-string-solid-*
   (title g)
   (first (title-loc g))
   (second (title-loc g))
   :color (speedo-color g))

  ;; Put the current value above the gauge.
  (if (draw-value g)
      (sdl:draw-string-solid-*
       (format nil "~A" (round (my-value g)))
       (first (value-loc g))
       (second (value-loc g))
       :color (speedo-color g)))
)

(defmethod set-gauge-value ((g gauge) value)
  (setf (my-value g) value)
  (setf (target g) (* 1.0 (deg-to-rad (map-range (dial-min g) (dial-max g) (angle-min g) (angle-max g) value)))))

(defun draw-sats (satstuff width height)
  ;; Draw initial disc.
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (* 3 (/ width 4))) (sdl:cast-to-int (/ height 2)) 150
				:color sdl:*white*)

  ;; Fill in the middle with black, leaving a white ring.
  (sdl-gfx:draw-filled-Circle-* (sdl:cast-to-int (* 3 (/ width 4))) (sdl:cast-to-int (/ height 2)) 148
				:color sdl:*black*)

  ;; Draw a second circle for 45deg elevation.
  (sdl-gfx:draw-Circle-* (sdl:cast-to-int (* 3 (/ width 4))) (sdl:cast-to-int (/ height 2)) 75
				:color sdl:*white*)

  ;; Draw the horizontal line.
  (sdl-gfx:draw-line (sdl:point :x (sdl:cast-to-int (- (* 3 (/ width 4)) 150))
				:y (sdl:cast-to-int (/ height 2)))
		     (sdl:point :x (sdl:cast-to-int (+ (* 3 (/ width 4)) 150))
				:y (sdl:cast-to-int (/ height 2)))
		     :color sdl:*white*)

  ;; Draw the vertical line.
  (sdl-gfx:draw-line (sdl:point :x (sdl:cast-to-int (* 3 (/ width 4)))
				:y (sdl:cast-to-int (- (/ height 2) 150)))
		     (sdl:point :x (sdl:cast-to-int (* 3 (/ width 4)))
				:y (sdl:cast-to-int (+ (/ height 2) 150)))
		     :color sdl:*white*)

  ;; Draw each satellite.
  (mapc (lambda (n)
	  (sdl:draw-string-solid-*
	   (format nil "~A" (cdr (assoc :+prn+ n)))
	   (sdl:cast-to-int (+ (* 3 (/ width 4)) (* 150 (/ (- 90 (cdr (assoc :el n))) 90) (cos (gauges:deg-to-rad (- (cdr (assoc :az n)) 90))))))
	   (sdl:cast-to-int (+ (/ height 2) (* 150 (/ (- 90 (cdr (assoc :el n))) 90) (sin (gauges:deg-to-rad (- (cdr (assoc :az n)) 90))))))
	   :color (if (cdr (assoc :used n)) (sdl:color :r 0 :g (sdl:cast-to-int (* 255 (/ (cdr (assoc :ss n)) 50))) :b 0) sdl:*red*)))
	satstuff)
  )
