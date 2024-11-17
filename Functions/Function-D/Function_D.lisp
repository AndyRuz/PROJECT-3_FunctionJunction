;;;; Function D: set-diff
;;;; Returns the difference of set-1 and set-2
(defun set-diff (set-1 set-2)
  (cond
    ((not set-1) nil)                        ; empty set-1 case
    ((not (set-member set-2 (first set-1)))  ; if first element not in set-2
     (cons (first set-1)                     ; include it in result
           (set-diff (cdr set-1) set-2)))    ; and recurse on rest
    (t (set-diff (cdr set-1) set-2))))      ; else skip element and recurse
