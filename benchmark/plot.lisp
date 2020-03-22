;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(py4cl2:defpymodule "matplotlib.pyplot" nil :lisp-package "PLT")


(in-package :cl-user)
(progn  
  (plt:plot (loop for i from 1 to 10 collect (write-to-string i))
            '(0.367 0.233 0.184 0.157 0.142 0.132 0.125 0.142 0.116 0.113)
            :label "SBCL (SSE)")

  (plt:plot (loop for i from 1 to 10 collect (write-to-string i))
            '(0.306 0.193 0.159 0.113 0.098 0.088 0.081 0.075 0.076 0.071)
            :label "SBCL (AVX)")
  (plt:xlabel "Size of Vector (x100)")
  (plt:ylabel "Time to execute 10^8 double '+' operations")
  (plt:ylim 0)
  (plt:legend)
  (plt:show))

(progn  
  (plt:plot (loop for i from 1 to 10 collect (write-to-string i))
            '(0.13 0.131 0.121 0.134 0.133 0.125 0.125 0.138 0.13 0.131)
            :label "SBCL (SSE)")

  (plt:plot (loop for i from 1 to 10 collect (write-to-string i))
            '(0.077 0.116 0.11 0.11 0.113 0.113 0.114 0.113 0.117 0.126)
            :label "SBCL (AVX)")
  (plt:xlabel "Size of Vector (x1000)")
  (plt:ylabel "Time to execute 10^8 double '+' operations")
  (plt:ylim 0)
  (plt:legend)
  (plt:show))

(progn  
  (plt:plot (loop for i from 1 to 10 collect (write-to-string (* 10000 i)))
            '(0.147 0.192 0.202 0.202 0.202 0.202 0.202 0.202 0.202 0.203)
            :label "SBCL (SSE)")

  (plt:plot (loop for i from 1 to 10 collect (write-to-string (* 10000 i)))
            '(0.125 0.181 0.196 0.197 0.196 0.196 0.196 0.195 0.197 0.196)
            :label "SBCL (AVX)")
  (plt:xlabel "Size of Vector (x10000)")
  (plt:ylabel "Time to execute 10^8 double '+' operations")
  (plt:ylim 0)
  (plt:legend)
  (plt:show))

(progn
  (plt:plot (loop for i from 2 to 7 collect
                 (concatenate 'string "10^" (write-to-string i)))
            #(1.898927 0.32075238 0.16599727 0.1875503 0.27427316 0.34775853)
            :label "Numpy")
  (plt:plot (loop for i from 2 to 7 collect
                 (concatenate 'string "10^" (write-to-string i)))
            '(0.346 0.106 0.149 0.203 0.277 0.294)
            :label "SBCL (SSE)")
  (plt:plot (loop for i from 2 to 7 collect
                 (concatenate 'string "10^" (write-to-string i)))
            '(0.313 0.068 0.126 0.196 0.269 0.348)
            :label "SBCL (AVX)")
  (plt:xlabel "Size of Vector")
  (plt:ylabel "Time to execute 10^8 double '+' operations")
  (plt:ylim 0)
  (plt:legend)
  (plt:show))
