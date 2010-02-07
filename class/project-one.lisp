;;; Lisp Project #1
;;; Written by Gibi (unnecessarily snarky comments were placed into this file by someone else, though)
;;; It was the Lisp Gnomes
;;; See accompanying mini-Lisp implementation + file

;; Problem 1: 
;; This function takes two inputs and returns their average if they are both numbers, otherwise returns nil.
;; The right way to do this function would be to make it generic:
;; (defun average (&rest args)
;;     (let ((numeric (remove-if-not #'numberp args)))
;;         (/ (reduce #'+ numeric) (length numeric))))
;; The problem with that is that we haven't been taught a) higher order functions and sharpquotes b) remove-if-not c) variable length argument lists. 
;; Thus we will make a less generic and significantly uglier function called average-2.
(defun average-2 (x y)
    (cond 
        ((and (numberp x) (numberp y)) (/ (+ x y) 2))
        (t nil)))

;; Problem 2:
;; This recursive function takes one input and returns the average of all the numeric elements of the input; it returns nil if none of the elements are numeric.
;; Note: due to the remove-if-not which makes things that much simpler, this is about as inefficient as it gets. Luckily, we have a few GHz to spare.
(defun average-r (input-list)
    (let ((only-numeric (remove-if-not #'numberp input-list)))  ; This is really silly here because we're doing it every time, even when we know it's guaranteed
        (cond ((eq only-numeric nil) nil)                       ; But Ms. Piper said 'recursive function' and we are obedient students because we like points
              ((= (length only-numeric) 2) (average-2 (car only-numeric) (cadr only-numeric)))
              (t (/ (+ (car only-numeric) (* (1- (length only-numeric)) (average-r (cdr only-numeric)))) (length only-numeric)))))) 
;; Is 131 characters per column too much? ^


;; Problem 3:
;; This recursive function takes one list as an input and returns the last element of that input list
(defun our-last (input-list)
    (if (cdr input-list)
        (our-last (cdr input-list)) ; If there are more elements and cdr != nil, cut off first and recurse
        (car input-list)))
        
;; Problem 4:
;; This recursive predicate function takes two inputs; the second is a list. It's purpose is 
;; to search the list looking for a match between a list element and the first argument. 
;; If not found, return nil, otherwise, return first argument. No member!
(defun our-member-p (element input-list) 
    (cond
        ((eq input-list nil) nil)
        ((eq element (car input-list)) element)
        (t (our-member-p element (cdr input-list)))))

;; Problem 5: 
;; This recursive function takes inputs x and y; it returns the number of times atom x exists as an element in list y.
;; The easy way:
;;    (defun count-x-in-y (element input-list)
;;        (length (remove-if-not (lambda (x) (equalp x element)))))
;; The hard way that doesn't use any library functions: (library functions are the root of all evil, unless you happen to know them)
(defun count-x-in-y (el input-list)
    (cond 
        ((not input-list) 0)
        ((equalp (car input-list) el) (1+ (count-x-in-y el (cdr input-list))))
        (t (count-x-in-y el (cdr input-list)))))

;; Problem 6:
;; Find the length of a list. Without using them evil library functions (that would be 
;; cheating! since we're writing a library function. Half the library functions use (length ..) anyway.)
(defun my-length (input-list)
    (if (not input-list)
        0
        (1+ (my-length input-list))))

;; Problem 7: 
;; Base conversion... from base 10 to bases 2..9 inclusive. 
;; The prompt says to name the function 'Convfr10' but that's not Lispy at all... Let's rename it
;; to to-base-10 (Kind of like that Australian couple that tried to name their child "4-realz")
;;
;; Why, why is this always the classic example of everything? Grrgh.
(defun to-base-10 (base num)
    (digits-to-int base (int-to-list num)))

;; Problem 8:
;; It should really be called problem -7. Or 1/7. It's 7 backwards.
(defun from-base-10 (base num)
    (list-to-string (base-convert base 10 (int-to-list num))))


;; Helper functions for problems 7,8

;; Do the actual conversion between bases
;; Procedure:
;;   Convert to value, find highest power that goes into it and how many times
;;   That is the first element of the new list
;;   Find what's left over, and either finish (all taken care of) or recurse
(defun base-convert (desired-base start-base digit-list)
    (let* ((val (digits-to-int start-base digit-list))
           (pow (highest-power desired-base val))
           (times (truncate (/ val (expt desired-base pow))))
           (subval (* times (expt desired-base pow))))

        (cons times
            (if (= 0 (- val subval)) 
                (fill-list (1- pow) 0)
                (base-convert desired-base start-base (int-to-list (- val subval)))))))


;; Find the highest power of x that goes into y
(defun highest-power (x y)
  (truncate (log y x)))

;; Create a list of length n filled with element X
(defun fill-list (n x)
    (if (<= n 0) 
        nil
        (cons x (fill-list (1- n) x))))

;; Convert that digit list into a number of the given base
(defun digits-to-int (base digit-list)
    (if (not digit-list) 0
        (+ (* (car digit-list) (expt base (1- (length digit-list)))) 
           (digits-to-int base (cdr digit-list)))))

;; Convert a list (a b c d ...) of integers into a string "abcd"
(defun list-to-string (int-list)
    (format nil "~{~D~}" int-list))

;; Convert an int ABCD to a list '(A B C D) of its digits
;; It works by converting the int to a string with format,
;; then converting that to a list of characters with coerce,
;; then converting that to an int list with map.
;; Yes, it's ugly. :-(
(defun int-to-list (int)
    (map 'list (lambda (digit) (- (char-code digit) 48)) 
        (coerce (format nil "~D" int) 'list)))
        

        
