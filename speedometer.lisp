(ql:quickload :gauges)
(ql:quickload :gpsd)

(defparameter *text-color* sdl:*white*)

(defparameter *height* 400)
(defparameter *width* 800)
(defparameter *radius* 150)
(defparameter *frame-rate* 30)

(defparameter *max-speed* 85)
(defparameter *max-alt* 750)

(defun int-to-digits (value)
  (map 'list (lambda (c)(or (digit-char-p c) '-)) (prin1-to-string value)))

(defun speedo ()
  "Show the gps console."
  (let ((count 0) (clat "0") (clon "0") (ctime "0") (show-leds nil)
	(show-sats nil) (sat-info nil) (maidenhead "") (show-maidenhead nil))
    (sdl:with-init ()
      (sdl:window *width* *height* :double-buffer t :title-caption "Speedometer")
      (setf (sdl:frame-rate) *frame-rate*)

      (sdl:initialise-default-font)
;;      (sdl:initialise-default-font sdl:*ttf-font-vera*)

      (defparameter mode-led (make-instance 'gauges:led
					    :led-color :green
					    :center-x (/ *width* 2)
					    :center-y (+ 25 (/ *height* 2))))
      
      (defparameter mode-digit (make-instance 'gauges:seven-seg
					      :scale 1.5
					      :top-x (- (/ *width* 2) 9)
					      :top-y (- (/ *height* 2) 20)))
      
      (defparameter speedometer (make-instance 'gauges:gauge
					       :title "MPH"
					       :dial-min 0.0
					       :dial-max *max-speed*
					       :angle-min 90
					       :angle-max 360
					       :tick-marks 17
					       :center-x (* 1 (/ *width* 4))
					       :center-y (/ *height* 2)
					       :my-frame-rate *frame-rate*
					       :target 0
					       :radius *radius*))
      
      (defparameter altimeter (make-instance 'gauges:gauge
					     :title "FT"
					     :dial-min 0.0
					     :dial-max *max-alt*
					     :angle-min 180
					     :angle-max 360
					     :tick-marks 15
					     :center-x (* 3 (/ *width* 4))
					     :center-y (/ *height* 2)
					     :my-frame-rate *frame-rate*
					     :target 0
					     :radius *radius*))
      
      (defparameter signal-strength (make-instance 'gauges:gauge
						   :title "SNR"
						   :dial-min 0.0
						   :dial-max 50.0
						   :angle-min 180
						   :angle-max 360
						   :tick-marks 10
						   :center-x (* 9 (/ *width* 20))
						   :center-y (* 1 (/ *height* 5))
						   :my-frame-rate *frame-rate*
						   :target 0
						   :radius (/ *radius* 4)))
      
      (defparameter sat-count (make-instance 'gauges:gauge
					     :title "Sats"
					     :dial-min 0.0
					     :dial-max 12.0
					     :angle-min 180
					     :angle-max 360
					     :tick-marks 12
					     :center-x (* 11 (/ *width* 20))
					     :center-y (* 1 (/ *height* 5))
					     :my-frame-rate *frame-rate*
					     :target 0
					     :radius (/ *radius* 4)))
      
      (defparameter heading (make-instance 'gauges:gauge
					   :title "Heading"
					   :dial-min 0.0
					   :dial-max 360.0
					   :angle-min 0
					   :angle-max 360
					   :tick-marks 24
					   :center-x (* 2 (/ *width* 4))
					   :center-y (* 3 (/ *height* 4))
					   :my-frame-rate *frame-rate*
					   :target 0
					   :radius (/ *radius* 4)))

      (defparameter ss-digit-1 (make-instance 'gauges:seven-seg
					      :scale 1
					      :top-x (- (* 9 (/ *width* 20)) 12)
					      :top-y (+ (* 1 (/ *height* 5)) 10)))
      
      (defparameter ss-digit-2 (make-instance 'gauges:seven-seg
					      :scale 1
					      :top-x (+ (* 9 (/ *width* 20)) 0)
					      :top-y (+ (* 1 (/ *height* 5)) 10)))
      
      (defparameter sats-digit-1 (make-instance 'gauges:seven-seg
						:scale 1
						:top-x (- (* 11 (/ *width* 20)) 12)
						:top-y (+ (* 1 (/ *height* 5)) 10)))
      
      (defparameter sats-digit-2 (make-instance 'gauges:seven-seg
						:scale 1
						:top-x (+ (* 11 (/ *width* 20)) 0)
						:top-y (+ (* 1 (/ *height* 5)) 10)))

      (defparameter speed-digit-1 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (- (* 1 (/ *width* 4)) 36)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter speed-digit-2 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (- (* 1 (/ *width* 4)) 12)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter speed-digit-3 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (+ (* 1 (/ *width* 4)) 12)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter alt-digit-1 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (- (* 3 (/ *width* 4)) 60)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter alt-digit-2 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (- (* 3 (/ *width* 4)) 36)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter alt-digit-3 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (- (* 3 (/ *width* 4)) 12)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter alt-digit-4 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (+ (* 3 (/ *width* 4)) 12)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (defparameter alt-digit-5 (make-instance 'gauges:seven-seg
						 :scale 2
						 :top-x (+ (* 3 (/ *width* 4)) 36)
						 :top-y (+ (* 1 (/ *height* 2)) 20)))
      
      (gauges:initialize mode-led)
      (gauges:initialize mode-digit)
      (gauges:initialize speedometer)
      (gauges:initialize altimeter)
      (gauges:initialize heading)
      (gauges:initialize signal-strength)
      (gauges:initialize sat-count)
      (gauges:initialize ss-digit-1)
      (gauges:initialize ss-digit-2)
      (gauges:initialize sats-digit-1)
      (gauges:initialize sats-digit-2)
      (gauges:initialize speed-digit-1)
      (gauges:initialize speed-digit-2)
      (gauges:initialize speed-digit-3)
      (gauges:initialize alt-digit-1)
      (gauges:initialize alt-digit-2)
      (gauges:initialize alt-digit-3)
      (gauges:initialize alt-digit-4)
      (gauges:initialize alt-digit-5)

      (sdl:with-events ()
	(:quit-event ()
		     t)

	(:key-down-event (:key key)
			 (when (or (sdl:key= key :sdl-key-space)
				   (sdl:key= key :sdl-key-s))
			   (setf show-sats (not show-sats)))
			 (when (sdl:key= key :sdl-key-1)
			   (setf *frame-rate* 1)
			   (setf (sdl:frame-rate) *frame-rate*))
			 (when (sdl:key= key :sdl-key-2)
			   (setf *frame-rate* 10)
			   (setf (sdl:frame-rate) *frame-rate*))
			 (when (sdl:key= key :sdl-key-3)
			   (setf *frame-rate* 30)
			   (setf (sdl:frame-rate) *frame-rate*))
			 (when (sdl:key= key :sdl-key-4)
			   (setf *frame-rate* 60)
			   (setf (sdl:frame-rate) *frame-rate*))
			 (when (sdl:key= key :sdl-key-d)
 			   (setf show-leds (not show-leds)))
			 (when (sdl:key= key :sdl-key-m)
 			   (setf show-maidenhead (not show-maidenhead)))
			 (when (or (sdl:key= key :sdl-key-escape)
				   (sdl:key= key :sdl-key-q))
			   (sdl:push-quit-event)))
	
	(:video-expose-event ()
			     (sdl:update-display))
	
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       
	       ;; All of this stuff happens at 1hz.
	       (let ((cp (gpsd:get-current-location)))
		 (if (gpsd:point-crs cp)
		     (if (> (gpsd:point-spd cp) 0.5)
			 (gauges:set-gauge-value heading (- (gpsd:point-crs cp) 90))))
		 (when (eql 0 (mod (incf count 1) *frame-rate*))
		   (if show-leds
		       (progn
			 (setf (gauges:draw-value speedometer) nil)
			 (setf (gauges:draw-value altimeter) nil)
			 (setf (gauges:draw-value signal-strength) nil)
			 (setf (gauges:draw-value sat-count) nil))
		       (progn
			 (setf (gauges:draw-value speedometer) t)
			 (setf (gauges:draw-value altimeter) t)
			 (setf (gauges:draw-value signal-strength) t)
			 (setf (gauges:draw-value sat-count) t)))
		   (setf sat-info (gpsd:get-current-sats))
		   (if (gpsd:point-signal-strength cp)
		       (let* ((ss (gpsd:point-signal-strength cp))
			      (n (int-to-digits (round ss))))
			 (gauges:set-gauge-value signal-strength ss)
			 (if (= (length n) 1)
			     (progn
			       (gauges:set-gauge-value ss-digit-1 0)
			       (gauges:set-gauge-value ss-digit-2 (first n)))
			     (progn
			       (gauges:set-gauge-value ss-digit-1 (first n))
			       (gauges:set-gauge-value ss-digit-2 (second n)))))
		       (gauges:set-gauge-value signal-strength 0))

		   (if (gpsd:point-sats cp)
		       (let* ((sat-num (gpsd:point-sats cp))
			      (n (int-to-digits (round sat-num))))
			 (gauges:set-gauge-value sat-count sat-num)
			 (if (= (length n) 1)
			     (progn
			       (gauges:set-gauge-value sats-digit-1 0)
			       (gauges:set-gauge-value sats-digit-2 (first n)))
			     (progn
			       (gauges:set-gauge-value ss-digit-1 (first n))
			       (gauges:set-gauge-value ss-digit-2 (second n)))))
		       (gauges:set-gauge-value sat-count 0))

		   (if (gpsd:point-spd cp)
		       (let* ((spd (gpsd:ms-to-mph (gpsd:point-spd cp)))
			      (n (int-to-digits (round spd))))
			 (gauges:set-gauge-value speedometer spd)
			 (cond
			   ((= (length n) 1)
			    (gauges:set-gauge-value speed-digit-1 0)
			    (gauges:set-gauge-value speed-digit-2 0)
			    (gauges:set-gauge-value speed-digit-3 (first n)))
			   ((= (length n) 2)
			    (gauges:set-gauge-value speed-digit-1 0)
			    (gauges:set-gauge-value speed-digit-2 (first n))
			    (gauges:set-gauge-value speed-digit-3 (second n)))
			   ((= (length n) 3)
			    (gauges:set-gauge-value speed-digit-1 (first n))
			    (gauges:set-gauge-value speed-digit-2 (second n))
			    (gauges:set-gauge-value speed-digit-3 (third n))))))

		   (if (gpsd:point-alt cp)
		       (let* ((alt (gpsd:m-to-ft (gpsd:point-alt cp)))
			      (n (int-to-digits (round alt))))
			 (gauges:set-gauge-value altimeter alt)
			 (cond
			   ((= (length n) 1)
			    (gauges:set-gauge-value alt-digit-1 0)
			    (gauges:set-gauge-value alt-digit-2 0)
			    (gauges:set-gauge-value alt-digit-3 0)
			    (gauges:set-gauge-value alt-digit-4 0)
			    (gauges:set-gauge-value alt-digit-5 (first n)))
			   ((= (length n) 2)
			    (gauges:set-gauge-value alt-digit-1 0)
			    (gauges:set-gauge-value alt-digit-2 0)
			    (gauges:set-gauge-value alt-digit-3 0)
			    (gauges:set-gauge-value alt-digit-4 (first n))
			    (gauges:set-gauge-value alt-digit-5 (second n)))
			   ((= (length n) 3)
			    (gauges:set-gauge-value alt-digit-1 0)
			    (gauges:set-gauge-value alt-digit-2 0)
			    (gauges:set-gauge-value alt-digit-3 (first n))
			    (gauges:set-gauge-value alt-digit-4 (second n))
			    (gauges:set-gauge-value alt-digit-5 (third n)))
			   ((= (length n) 4)
			    (gauges:set-gauge-value alt-digit-1 0)
			    (gauges:set-gauge-value alt-digit-2 (first n))
			    (gauges:set-gauge-value alt-digit-3 (second n))
			    (gauges:set-gauge-value alt-digit-4 (third n))
			    (gauges:set-gauge-value alt-digit-5 (fourth n)))
			   ((= (length n) 5)
			    (gauges:set-gauge-value alt-digit-1 (first n))
			    (gauges:set-gauge-value alt-digit-2 (second n))
			    (gauges:set-gauge-value alt-digit-3 (third n))
			    (gauges:set-gauge-value alt-digit-4 (fourth n))
			    (gauges:set-gauge-value alt-digit-5 (fifth n))))))

		   (if (gpsd:point-mode cp) (gauges:set-gauge-value mode-digit (gpsd:point-mode cp)))

		   (setf clat (format nil "~A" (gpsd:point-lat cp)))
		   (setf clon (format nil "~A" (gpsd:point-lon cp)))
		   (if show-maidenhead (setf maidenhead (gpsd:maidenhead cp)))
		   (setf ctime (format nil "~A" (local-time:unix-to-timestamp (gpsd:point-creation-time cp))))
		   (cond
		     ((>= (gpsd:point-mode cp) 3)
		      (gauges:set-gauge-value mode-led t)
		      (setf (gauges:led-color mode-led) :green))
		     ((= (gpsd:point-mode cp) 2)
		      (gauges:set-gauge-value mode-led t)
		      (setf (gauges:led-color mode-led) :yellow))
		     (t
		      (gauges:set-gauge-value mode-led t)
		      (setf (gauges:led-color mode-led) :red)))
		   ))
	       
	       (gauges:draw-gauge speedometer)
	       (gauges:draw-gauge heading)
	       (gauges:draw-gauge signal-strength)
	       (gauges:draw-gauge sat-count)
	       (gauges:draw-gauge mode-led)
	       (when show-leds
		 (gauges:draw-gauge mode-digit)
		 (gauges:draw-gauge ss-digit-1)
		 (gauges:draw-gauge ss-digit-2)
		 (gauges:draw-gauge sats-digit-1)
		 (gauges:draw-gauge sats-digit-2)
		 (gauges:draw-gauge speed-digit-1)
		 (gauges:draw-gauge speed-digit-2)
		 (gauges:draw-gauge speed-digit-3))

	       (if show-sats
		   (gauges:draw-sats sat-info *width* *height*)
		   (progn
		     (gauges:draw-gauge altimeter)
		     (when show-leds
		       (gauges:draw-gauge alt-digit-1)
		       (gauges:draw-gauge alt-digit-2)
		       (gauges:draw-gauge alt-digit-3)
		       (gauges:draw-gauge alt-digit-4)
		       (gauges:draw-gauge alt-digit-5))))
	       
	       (sdl:draw-string-solid-* ctime 5 5 :color *text-color*)
	       (sdl:draw-string-solid-* clat (sdl:cast-to-int (* 2 (/ *width* 5))) 5 :color *text-color*)
	       (sdl:draw-string-solid-* clon (sdl:cast-to-int (* 3 (/ *width* 5))) 5 :color *text-color*)
	       (if show-maidenhead
		   (sdl:draw-string-solid-* maidenhead (sdl:cast-to-int (* 4 (/ *width* 5))) 5 :color *text-color*))
	       
	       (sdl:update-display))))))

(defun main ()
  (format t "Starting GPSD connection...~%")
  (gpsd:start-gpsd)
  (format t "Sleeping for three seconds to allow GPSD startup...~%")
  (sleep 3)
  (speedo))

(defun bin ()
  (sb-ext:save-lisp-and-die "gofast" :toplevel #'main :executable t :purify t :compression 9))
