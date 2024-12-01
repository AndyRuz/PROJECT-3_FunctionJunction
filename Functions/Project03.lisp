;;;; Function 1: set-member
;;;; Checks if an item is a member of a set
(defun set-member (set item)
  (cond
    ((not set) nil)                          ; empty set case
    ((equal (first set) item) t)             ; found item
    (t (set-member (cdr set) item))))        ; recurse on rest of set





;; FUNCTION 3: Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
 (if (equal set-1 '()) ;check if set-1 is empty (means empty intersection).
      '()
      (if (set-member set-2 (car set-1)) ;checks if the first element of set-1 is also a member of set-2
          (cons (car set-1) (set-intersection (cdr set-1) set-2)) ;first item in set-1 is a member of set-2, creates a new list, checks rest of list.
          (set-intersection (cdr set-1) set-2))) ;first item fo set-1 is not a member of set-2, check rest of list


;;;; Function 4: set-diff
;;;; Returns the difference of set-1 and set-2
(defun set-diff (set-1 set-2)
  (cond
    ((not set-1) nil)                        ; empty set-1 case
    ((not (set-member set-2 (first set-1)))  ; if first element not in set-2
     (cons (first set-1)                     ; include it in result
           (set-diff (cdr set-1) set-2)))    ; and recurse on rest
    (t (set-diff (cdr set-1) set-2))))      ; else skip element and recurse



;; FUNCTION 5:
;; Return the exclusive or of a and b
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil

(defun boolean-xor (a b)
 (and (not (and a b)) (or a b))
)



;;;; Function 6: boolean-implies
;;;; Implements logical implication (a → b)
(defun boolean-implies (a b)
  (or (not a) b))                           ; checks (!a||b)








;; FUNCTION 9: Perform merge sort on the lists.
;; Parameters:
;;   list: The list to sort
;;   predicate: A function to compare elements of the list
;; Examples:
;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)
;;     (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0


(defun merge-sort (list predicate)
  (labels ((split (lst n acc)
             (cond 
               ((= n 0) (list acc lst))
               ((not lst) (list acc nil))
               (t (split (cdr lst) 
                        (- n 1)
                        (cons (car lst) acc)))))
           
           (merge-lists (left right)
             (cond ((not left) right)
                   ((not right) left)
                   ((funcall predicate (car left) (car right))
                    (cons (car left) (merge-lists (cdr left) right)))
                   (t (cons (car right) (merge-lists left (cdr right)))))))
    
    (cond 
      ((not list) nil)
      ((not (cdr list)) list)
      (t (let* ((len (length list))
                (mid (/ len 2))
                (halves (split list mid nil)))
           (merge-lists (merge-sort (first halves) predicate)
                       (merge-sort (second halves) predicate)))))))
