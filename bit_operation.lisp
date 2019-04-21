(defun plus1 (a)
 (cond
  ((zerop a) 1)
  ((zerop (~ a)) 0)
  ((zerop (& a 1)) (| a 1))
  (t (<< (plus1 (>> a 1)) 1))
 )
)

(defun plus (a b)
 (cond
  ((zerop a) b)
  ((zerop b) a)
  ((zerop (~ a)) (minus1 b))
  ((zerop (~ b)) (minus1 a))
  ((zerop (& (& a 1) b))
   (| (<< (plus (>> a 1) (>> b 1)) 1) (plus (& a 1) (& b 1))))
  (t (<< (plus (plus1 (>> a 1)) (>> b 1)) 1))
 )
)

(defun neg (a)
 (plus1 (~ a))
)

(defun minus1 (a)
 (cond
  ((zerop a) -1)
  ((zerop (& a 1)) (| (<< (minus1 (>> a 1)) 1) 1))
  (t (^ a 1))
 )
)

(defun minus (a b)
 (cond
  ((zerop a) (neg b))
  ((zerop b) a)
  ((zerop (~ a)) (minus1 (neg b)))
  ((zerop (~ b)) (plus1 a))
  ((zerop (& b 1))
   (cond
    ((zerop (& a 1)) (<< (minus (>> a 1) (>> b 1)) 1))
    (t               (| (<< (minus (>> a 1) (>> b 1)) 1) 1))
   )
  )
  (t 
   (cond
    ((zerop (& a 1)) (| (<< (minus (>> a 1) (plus1 (>> b 1))) 1) 1))
    (t               (<< (minus (>> a 1) (>> b 1)) 1))
   )
  )
 )
)

