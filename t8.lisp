(in-package :qt-troll-tutorial8)
(named-readtables:in-readtable :qt)

;;; LCD Range
(defclass lcd-range ()
  ((slider :reader slider))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setValue(int)" set-value)
          ("setRange(int,int)" set-range))
  (:signals ("valueChanged(int)")))

(defmethod initialize-instance :after ((instance lcd-range) &key parent)
  (if parent
      (new instance parent)
      (new instance))

  (with-slots (slider) instance
	      (let ((lcd    (#_new QLCDNumber 2))
		    (layout (#_new QVBoxLayout)))
		(setf (slot-value instance 'slider)
		      (#_new QSlider (#_Horizontal "Qt")))
		(#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
		(#_setRange slider 0 99)
		(#_setValue slider 0)
		(connect slider "valueChanged(int)"
			 lcd "display(int)")
		(connect slider "valueChanged(int)"
			 instance "valueChanged(int)")

		(#_addWidget layout lcd)
		(#_addWidget layout slider)
		(#_setLayout instance layout))
              (#_setFocusProxy instance slider)))

(defmethod value ((lcd lcd-range))
  (#_value (slider lcd)))

(defmethod set-value ((lcd lcd-range) value)
  (declare (integer value))
  (#_setValue (slider lcd) value))

(defmethod set-range ((lcd lcd-range) min max)
  (declare (type integer min max))
  (if (or (> min max)
          (minusp min)
          (> max 99))
      (warn "invalid SET-RANGE(~D, ~D)" min max)
      (#_setRange (slider lcd) min max)))


;;; Cannonfield

(defclass cannon-field ()
  ((current-angle :accessor current-angle
                  :initform 45
                  :initarg :current-angle))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("setAngle(int)" set-angle))
  (:signals ("angleChanged(int)"))
  (:override ("paintEvent" paint-event)))

(defmethod initialize-instance :after ((instance cannon-field) &key parent)
  (if parent
      (new instance parent)
      (new instance))

  (with-objects ((color (#_new QColor 250 250 200))
                 (palette (#_new QPalette color)))
    (#_setPalette instance palette)
    (#_setAutoFillBackground instance t)))

(defun clamp (angle min max)
  (max (min angle max) min))

(defmethod set-angle ((cannon cannon-field) angle)
  (declare (integer angle))
  (let ((angle (clamp angle 5 70)))
    (unless (= (current-angle cannon) angle)
      (setf (current-angle cannon) angle)
      (#_update cannon)
      (emit-signal cannon "angleChanged(int)" angle))))

(defmethod paint-event ((cannon cannon-field) event)
  (let ((painter (#_new QPainter cannon)))
    ;; T8
    ;; (#_drawText painter 200 200 (format nil "Angle = ~D"
    ;;                                     (current-angle cannon)))

    ;; T9
    (#_setPen painter (#_NoPen "Qt"))
    (#_setBrush painter (#_blue "Qt"))
    
    (#_translate painter 0 (#_height (#_rect cannon)))
    (with-objects ((pie    (#_new QRect -35 -35 70 70))
                   (barrel (#_new QRect 30 -5 20 10)))
      (#_drawPie painter pie 0 (* 90 16))
      (#_rotate painter (- (current-angle cannon)))
      (#_drawRect painter barrel))
    
    (#_end painter)))

;;; Main window

(defclass widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((object widget) &key parent)
  ;; Must call the c++ constructer first!
  (if parent
      (new object parent)
      (new object))

  (let ((quit   (#_new QPushButton "&Quit" object))
        (font   (#_new QFont "Times" 18 (#_Bold "QFont")))
        (angle  (make-instance 'lcd-range))
        (cannon-field (make-instance 'cannon-field))
        (grid   (#_new QGridLayout)))

    (#_setFont quit font)
    (connect quit "clicked()" *qapplication* "quit()")

    (set-range angle 5 70)
    
    (connect angle "valueChanged(int)" cannon-field "setAngle(int)")
    (connect cannon-field "angleChanged(int)" angle "setValue(int)")
    
    (#_addWidget grid quit 0 0)
    (#_addWidget grid angle 1 0)
    (#_addWidget grid cannon-field 1 1 2 1)
    (#_setColumnStretch grid 1 10)

    (#_setLayout object grid)
    
    (set-value angle 60)
    (#_setFocus angle)))

(defun main ()
  "http://doc.qt.digia.com/4.3/tutorial-t8.html"
  (with-main-window
      (window (make-instance 'widget))))
