;;;; qt-hello-world.asd

(asdf:defsystem #:qt-troll-tutorial
  :serial t
  :description "qt trolltech tutorial in lisp"
  :author "Nicholas Patrick <npatrick04@gmail.com>"
  :license "BSD"
  :depends-on (:qt)
  :components ((:file "package")
               (:file "tutorial")))

