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

    ((my-member (car set-2) set-1)  

     (set-union set-1 (cdr set-2))) 

    (t  

     (set-union (cons (car set-2) set-1) (cdr set-2))))) 

;; Helper function for set membership 

(defun my-member (x set) 

  (cond  

    ((null set) nil) 

    ((equal x (car set)) t) 

    (t (my-member x (cdr set)))))





;; FUNCTION 3: Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
 (if (equal set-1 '()) ;check if set-1 is empty (empty intersection).
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
     (eval-xor (cdr exp))) 
    ((equal (car exp) 'implies) 
     (eval-implies (car (cdr exp)) (car (cdr (cdr exp))))) 
    ((equal (car exp) 'iff) 
     (boolean-iff (boolean-eval (car (cdr exp)))  
                  (boolean-eval (car (cdr (cdr exp)))))) 
    (t nil))) 

  

;; XOR evaluation helper 

(defun eval-xor (lst) 
  (cond 
    ((null lst) nil) 
    ((null (cdr lst)) (boolean-eval (car lst))) 
    (t (let ((first-val (boolean-eval (car lst))) 
             (second-val (boolean-eval (car (cdr lst))))) 
         (and (or first-val second-val) 
              (not (and first-val second-val))))))) 

  

;; Implication helper 

(defun eval-implies (p q) 
  (or (not (boolean-eval p)) 
      (boolean-eval q)))  

 
;;FUNCTION 8 Boolean Bi-Implication 

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
