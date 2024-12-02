;;;; Function 1: set-member
;;;; Checks if an item is a member of a set
(defun set-member (set item)
  (cond
    ((not set) nil)                          ; empty set case
    ((equal (first set) item) t)             ; found item
    (t (set-member (cdr set) item))))        ; recurse on rest of set

;;FUNCTION 2 Union of set-1 and set-2
;; Set Union Function 
(defun set-union (set-1 set-2)
  (cond
    ((null set-2) set-1)
    ((set-member set-1 (car set-2))     ; Swapped argument order to make compatible with set-member
     (set-union set-1 (cdr set-2)))
    (t
     (set-union (cons (car set-2) set-1) (cdr set-2)))))



;; FUNCTION 3: Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
 (if (equal set-1 '()) ; See if set-1 is empty (empty intersection).
      '()
      (if (set-member set-2 (car set-1)) ; Checks if the first element of set-1 is also a member of set-2
          (cons (car set-1) (set-intersection (cdr set-1) set-2)) ; First item in set-1 is a member of set-2
                                                                  ; Creates a new list, checks rest of list.
          (set-intersection (cdr set-1) set-2)))  ; First item fo set-1 is not a member of set-2.
                                                  ; Check rest of list.


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
 (and (not (and a b)) ; Returns true if both inputs are not true
      (or a b)) ; Returns true if at least one input is true
  ;; Combined effect: returns true only when exactly one input is true
)



;;;; Function 6: boolean-implies
;;;; Implements logical implication (a → b)
(defun boolean-implies (a b)
  (or (not a) b))                           ; checks (!a||b)


;; Function 7 Boolean AND,OR,NOT,XOR,IF,IFF

;; Helper function for AND evaluation - evaluates raw expressions 

(defun eval-and (lst) 
  (cond  
    ((null lst) t) 
    ((equal (car lst) nil) nil) 
    ((equal (car lst) t) (eval-and (cdr lst))) 
    ((equal (car (car lst)) 'not)  
     (if (eval-not (car (cdr (car lst)))) 
         (eval-and (cdr lst)) 
         nil)) 
    ((equal (car (car lst)) 'and)  
     (if (eval-and (cdr (car lst))) 
         (eval-and (cdr lst)) 
         nil)) 
    ((equal (car (car lst)) 'or)  
     (if (eval-or (cdr (car lst))) 
         (eval-and (cdr lst)) 
         nil)) 
    (t nil))) 

  

;; Helper function for OR evaluation - evaluates raw expressions 

(defun eval-or (lst) 
  (cond  
    ((null lst) nil) 
    ((equal (car lst) t) t) 
    ((equal (car lst) nil) (eval-or (cdr lst))) 
    ((equal (car (car lst)) 'not)  
     (if (eval-not (car (cdr (car lst)))) 
         t 
         (eval-or (cdr lst)))) 
    ((equal (car (car lst)) 'and)  
     (if (eval-and (cdr (car lst))) 
         t 
         (eval-or (cdr lst)))) 
    ((equal (car (car lst)) 'or)  
     (if (eval-or (cdr (car lst))) 
         t 
         (eval-or (cdr lst)))) 
    (t nil))) 

  

;; Helper function for NOT evaluation 

(defun eval-not (exp) 
  (cond 
    ((equal exp t) nil) 
    ((equal exp nil) t) 
    ((equal (car exp) 'not) (not (eval-not (car (cdr exp))))) 
    ((equal (car exp) 'and) (not (eval-and (cdr exp)))) 
    ((equal (car exp) 'or) (not (eval-or (cdr exp)))) 
    (t nil))) 

  

;; Boolean Evaluation Function with additional operations 

(defun boolean-eval (exp)
  (cond
    ((equal exp t) t)
    ((equal exp nil) nil)
    ((equal (car exp) 'not)
     (eval-not (car (cdr exp))))
    ((equal (car exp) 'and)
     (eval-and (cdr exp)))
    ((equal (car exp) 'or)
     (eval-or (cdr exp)))
    ((equal (car exp) 'xor)
     (boolean-xor (boolean-eval (first (cdr exp)))     ; Using first instead of cadr
                  (boolean-eval (second (cdr exp)))))  ; Using second instead of caddr
    ((equal (car exp) 'implies)
     (boolean-implies (boolean-eval (first (cdr exp)))
                     (boolean-eval (second (cdr exp)))))
    ((equal (car exp) 'iff)
     (boolean-iff (boolean-eval (car (cdr exp)))
                  (boolean-eval (car (cdr (cdr exp))))))
    (t nil)))

  
 
;;FUNCTION 8: Boolean Bi-Implication 

(defun boolean-iff (a b) 
  (or (and (equal a t) (equal b t)) 
      (and (equal a nil) (equal b nil))))




;; FUNCTION 9: Perform merge sort on the lists.
;; Parameters:
;;   list: The list to sort
;;   predicate: A function to compare elements of the list
;; Examples:
;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)
;;     (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0

defun merge-sort (list predicate)
  ;; Using labels to define helper functions that are only used within merge-sort
  (labels (
           ;; Helper function to merge two sorted lists into one sorted list
           ;; Takes two lists and returns a single sorted list
           (merge-lists (left right)
             (cond 
               ((not left) right)    ; If left list is empty, use right list
               ((not right) left)    ; If right list is empty, use left list
               ;; Compare first elements using predicate (< or >)
               ((funcall predicate (car left) (car right))
                ;; If left element should come first, add it to result
                ;; and merge rest of left list with right list
                (cons (car left) (merge-lists (cdr left) right)))
               ;; If right element should come first, add it to result
               ;; and merge left list with rest of right list
               (t (cons (car right) (merge-lists left (cdr right))))))
           
           ;; Helper function to split a list into two parts
           ;; Returns a cons cell where car is first element and cdr is rest of list
           (split (lst)
             ;; Handle empty list or single element list
             (if (or (not lst) (not (cdr lst)))
                 (cons lst nil)
                 ;; For longer lists, split after first element
                 (let* ((rest (cdr lst)))
                   (cons (cons (car lst) nil) rest)))))
    
    ;; Main merge-sort logic
    (cond 
      ((not list) nil)              ; Empty list is already sorted
      ((not (cdr list)) list)       ; Single element list is already sorted
      (t (let* ((split-result (split list))     ; Split the input list
                (left (car split-result))        ; Get left part
                (right (cdr split-result)))      ; Get right part
           ;; Recursively sort both parts and merge them together
           (merge-lists (merge-sort left predicate)
                       (merge-sort right predicate)))))))
