;;;; Function A: set-member
;;;; Checks if an item is a member of a set
(defun set-member (set item)
  (cond
    ((not set) nil)                          ; empty set case
    ((equal (first set) item) t)             ; found item
    (t (set-member (cdr set) item))))        ; recurse on rest of set
