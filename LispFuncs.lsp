;; Helper function for set membership 

(defun my-member (x set) 
  (cond  
    ((null set) nil) 
    ((equal x (car set)) t) 
    (t (my-member x (cdr set))))) 

  

;; Set Union Function 

(defun set-union (set-1 set-2) 

  (cond  

    ((null set-2) set-1) 

    ((my-member (car set-2) set-1)  

     (set-union set-1 (cdr set-2))) 

    (t  

     (set-union (cons (car set-2) set-1) (cdr set-2))))) 

  

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

 

;; Biconditional Function 

(defun boolean-iff (a b) 

  (or (and (equal a t) (equal b t)) 

      (and (equal a nil) (equal b nil)))) 

 

;;Testing: 

;;Union: 

(set-union '(1 2 3) '(3 4 5)) 

(set-union '(1 2 3) '(1 2 3)) 

(set-union '(1 2 3) '(4 5 6)) 

(set-union '() '(3 4 5)) 

Boolean: 
;; Nested With recursion:
(boolean-eval '(or t (or nil (xor nil t))))
(boolean-eval '(and (and t nil) (or nil (xor nil t))))
(boolean-eval '(and t (or nil (iff t t))))

;;and 

 (boolean-eval '(and t nil)) 

(boolean-eval '(and t t)) 

(boolean-eval '(and nil nil)) 

(boolean-eval '(and nil t)) 

;;or 

(boolean-eval '(or t nil)) 

(boolean-eval '(or t t)) 

(boolean-eval '(or nil nil)) 

(boolean-eval '(or nil t)) 

;;Xor 

(boolean-eval '(xor t nil))  

(boolean-eval '(xor t t)) 

(boolean-eval '(xor nil nil)) 

(boolean-eval '(xor nil t)) 


 

;;Implies     

(boolean-eval '(implies t nil))   

(boolean-eval '(implies t t))   

(boolean-eval '(implies nil nil))   

(boolean-eval '(implies nil t))   

;;IFF 

(boolean-eval '(iff t t))  

(boolean-eval '(iff t nil)) 

(boolean-eval '(iff nil t)) 

(boolean-eval '(iff nil nil)) 

 

;;Bi-Implication     

 

(boolean-iff t nil) 

(boolean-iff t t) 

(boolean-iff nil nil) 

(boolean-iff nil t) 