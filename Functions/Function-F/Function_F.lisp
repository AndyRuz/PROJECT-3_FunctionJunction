;;;; Function F: boolean-implies
;;;; Implements logical implication (a → b)
(defun boolean-implies (a b)
  (or (not a) b))                           ; checks (!a||b)
