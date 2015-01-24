(in-package :qt-troll-tutorial)
(named-readtables:in-readtable :qt)

(defun hello ()
  "http://doc.qt.digia.com/4.3/tutorial-t1.html"
  (let ((app (make-qapplication))
        (button (#_new QPushButton "Hello World!")))
      (#_resize button 100 30)
      (#_show button)
      (#_exec app)))


(defun quit ()
  "http://doc.qt.digia.com/4.3/tutorial-t2.html"
  (let ((app (make-qapplication))
        (quit (#_new QPushButton "Quit"))
        (font (#_new QFont "Times" 18 (#_Bold "QFont"))))
    (#_resize quit 75 30)
    (#_setFont quit font)

    (connect quit "clicked()" app "quit()")

    (#_show quit)
    (#_exec app)))


(defun family-values ()
  "http://doc.qt.digia.com/4.3/tutorial-t3.html"
  (let ((app (make-qapplication)))
    (with-objects ((window (#_new QWidget)))
      (#_resize window 200 120)

      (let ((quit (#_new QPushButton "Quit" window))
            (font (#_new QFont "Times" 18 (#_Bold "QFont"))))
        (#_setFont quit font)
        (#_setGeometry quit 10 40 180 40)

        (connect quit "clicked()" app "quit()")

        (#_show window)
        (#_exec app)))))

;; Getting more complex...
(defclass my-widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((object my-widget) &key parent)
  ;; Must call the c++ constructer first!
  (if parent
      (new object parent)
      (new object))

  (#_setFixedSize object 200 100)

  (let ((quit (#_new QPushButton "Quit" object))
        (font (#_new QFont "Times" 18 (#_Bold "QFont"))))
    (#_setGeometry quit 62 40 75 30)
    (#_setFont quit font)

    (connect quit "clicked()" *qapplication* "quit()")))

(defun multiplicity ()
  "http://doc.qt.digia.com/4.3/tutorial-t4.html"
  (let ((app (make-qapplication))
        (my-widget (make-instance 'my-widget)))
    (#_show my-widget)
    (#_exec app)))  


;;; Building blocks
(defclass bb-widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((object my-widget) &key parent)
  ;; Must call the c++ constructer first!
  (if parent
      (new object parent)
      (new object))

  (let ((quit   (#_new QPushButton "Quit" object))
        (lcd    (#_new QLCDNumber 2))
        (slider (#_new QSlider (#_Horizontal "Qt")))
        (font   (#_new QFont "Times" 18 (#_Bold "QFont")))
        (layout (#_new QVBoxLayout)))

    (#_setFont quit font)
    (#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
    (#_setRange slider 0 99)
    (#_setValue slider 0)

    (connect quit "clicked()" *qapplication* "quit()")
    (connect slider "valueChanged(int)" lcd "display(int)")

    (#_addWidget layout quit)
    (#_addWidget layout lcd)
    (#_addWidget layout slider)
    (#_setLayout object layout)))

(defun building-blocks ()
  "http://doc.qt.digia.com/4.3/tutorial-t5.html"
  (let ((app (make-qapplication))
        (bb-widget (make-instance 'my-widget)))
    (#_show bb-widget)
    (#_exec app)))

;;; BB Galore!
(defclass lcd-range ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance lcd-range) &key parent)
  (if parent
      (new instance parent)
      (new instance))

  (let ((lcd    (#_new QLCDNumber 2))
        (slider (#_new QSlider (#_Horizontal "Qt")))
        (layout (#_new QVBoxLayout)))
    (#_setSegmentStyle lcd (#_Filled "QLCDNumber"))
    (#_setRange slider 0 99)
    (#_setValue slider 0)
    (connect slider "valueChanged(int)" lcd "display(int)")

    (#_addWidget layout lcd)
    (#_addWidget layout slider)
    (#_setLayout instance layout)))

(defclass bbg-widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((object bbg-widget) &key parent)
  ;; Must call the c++ constructer first!
  (if parent
      (new object parent)
      (new object))

  (let ((quit   (#_new QPushButton "Quit" object))
        (font   (#_new QFont "Times" 18 (#_Bold "QFont")))
        (layout (#_new QVBoxLayout))
        (grid   (#_new QGridLayout)))

    (#_setFont quit font)
    (connect quit "clicked()" *qapplication* "quit()")

    (dotimes (row 3)
      (dotimes (column 3)
        (let ((lcd-range (make-instance 'lcd-range)))
          (#_addWidget grid lcd-range
                       row column))))

    (#_addWidget layout quit)
    (#_addLayout layout grid)
    (#_setLayout object layout)))


(defun bb-galore ()
  "http://doc.qt.digia.com/4.3/tutorial-t6.html"
  (let ((app (make-qapplication))
        (bbg-widget (make-instance 'bbg-widget)))
    (#_show bbg-widget)
    (#_exec app)))
