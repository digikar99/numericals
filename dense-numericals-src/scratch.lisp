     (defpolymorph (,name :inline t)
         ((x (simple-array single-float)) &key ((out (simple-array single-float))
                                                (zeros-like x)))
         (simple-array single-float)
       ,(let* ((num-threads 4)
               (threads     (make-gensym-list num-threads "FUTURE"))
               (cache-size  10000))
          `(let* ((total-size    (array-total-size x))
                  (max-work-size (ceiling total-size ,num-threads))
                  ,@(loop :for thread :in threads
                          :for thread-idx :from 0
                          :collect
                          `(,thread (,(if (zerop thread-idx)
                                          'progn
                                          'bordeaux-threads:make-thread)
                                     (lambda ()
                                       (let* ((work-size (min max-work-size
                                                              (- total-size
                                                                 (* ,thread-idx max-work-size))))
                                              (ptr-x (cffi:inc-pointer (ptr x)
                                                                       (* 4 ,thread-idx)))
                                              (ptr-out (cffi:inc-pointer (ptr out)
                                                                         (* 4 ,thread-idx))))
                                         (loop :while (> work-size 0)
                                               :for n := (min work-size ,cache-size)
                                               :do (,single-float-c-name n
                                                                         ptr-x 1
                                                                         ptr-out 1)
                                                   (cffi:incf-pointer ptr-x
                                                       (* 4
                                                          ,cache-size
                                                          ,num-threads))
                                                   (cffi:incf-pointer ptr-out
                                                       (* 4
                                                          ,cache-size
                                                          ,num-threads))
                                                   (decf work-size ,cache-size))))))))
             (declare (ignorable ,@threads))
             ,@(loop :for thread :in threads
                     :for thread-idx :from 0
                     :collect `(,(if (zerop thread-idx)
                                     'funcall
                                     'bordeaux-threads:join-thread)
                                ,thread)
                     ;; :collect `(funcall ,thread)
                     )))
       out)
     
