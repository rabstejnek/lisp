;;; 2a
(defun known (x bindings)
    (cond 
        ((null bindings) (print "null bidings"))
        ((null x) (print "null x"))
        ((assoc x bindings) (known (cdr (assoc x bindings)) bindings))
        (t x)
    )
)



;;; 2b
;;; helper method to flatten the list, eg. flatten ((((a)) b) c) = (a b c) 
;;; note: flatten method is from online source
;;; https://stackoverflow.com/questions/41843094/flattening-lists-while-removing-nil-and-keeping-atoms-after-in-lisp
(defun flatten (x)
    (cond 
        ((null x) x)
        ((atom x) (list x))
        (t (nconc (flatten (car x)) (flatten (cdr x))))
    )
)

(defun has-vars (lst) 
    (setq values nil) 
    (loop for x in (flatten lst)
        if (string-equal "?"(subseq(string x)0 1))
        do (pushnew x values)
    )
    values
)


































