(in-package :numericals/basic-math/impl)

(macrolet ((def (&rest names)
             `(progn
                ,@(loop :for name :in names
                        :for name! := (find-symbol (format nil "~A!" name) :nu)
                        :collect `(progn
                                    (define-polymorphic-function ,name! (x) :overwrite t)
                                    (defpolymorph (,name! :inline t) (x) t
                                      (,name x :out x :broadcast nil)))))))
  (def nu:abs nu:fround nu:ftruncate nu:ffloor nu:fceiling))

(macrolet ((def (&rest names)
             `(progn
                ,@(loop :for name :in names
                        :for name! := (find-symbol (format nil "~A!" name) :nu)
                        :collect `(progn
                                    (define-polymorphic-function ,name! (x y &key broadcast)
                                      :overwrite t)
                                    (defpolymorph (,name! :inline t)
                                        (x y &key (broadcast nu:*broadcast-automatically*))
                                        t
                                      (,name x y :out x :broadcast broadcast)))))))
  (def nu:add nu:subtract nu:multiply nu:divide))
