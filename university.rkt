(define-struct lecturer (name salary faculty))
;; A Lecturer is a (make-lecturer Sym Num Sym)
;; requires: salary is a non-negative number
;;
(define-struct professor (name salary faculty research-area))
;; A Professor is a(make-professor Sym Num Sym)
;; requires: salary is a non-negative number
;;
(define-struct staff (name salary faculty position))
;; A staff is a (make-staff Str Num Str Str)
;; requires: salary is a non-negative number
;;
;; An Employee is one of
;; A lecturer
;; A Professor
;; A Staff
;;
;; A University Faculty (UFaculty) is one of
;; empty
;; (cons Employee UFaculty)
;;
;; A University is one of
;; empty
;; (cons (list Str UFaculty) University)
;; Str is a faculty name
;; All faculties' names in Unversity are unique
;; Note that University is an Association List
;;
;; 
;; Template function for a University
;; my-university-temp: University -> Any
(define (my-university-temp uni)
  (cond
    [(empty? uni) ...]
    [else (...(first (first uni))...; faculty of first
              ...(my-ufaculty-temp (second (first uni)))...; Ufaculty of first
              (my-university-temp (rest lst)))]))

;; my-ufaculty-temp: UFaculty -> Any
(define (my-ufaculty-temp ufac)
  (cond
    [(empty? ufac)...]
    [else (...(my-emp-temp (first ufac))...
              (my-ufaculty-temp (rest ufac))...)]))

;; my-emp-temp: Employee -> Any
(define (my-emp-temp emp)
  (cond
    [(lecturer? emp) (...(lecturer-name emp)...
                         (lecturer-salary emp)...
                         (lecturer-faculty emp)...)]
    [(professor? emp) (...(professor-name emp)...
                          (porfessor-salary emp)...
                          (professor-faculty emp)...
                          (professor-research-area emp)...)]
    [(staff? emp) (...(staff-name emp)...
                      (staff-salary emp)...
                      (staff-faculty emp)...
                      (staff-position emp)...)]))

        

;; (emp-faculty emp)produces faculty of the Employee(emp)
;; emp-faculty: Employee -> Str
(define (emp-faculty emp)
  (cond
    [(lecturer? emp) (lecturer-faculty emp)]
    [(professor? emp) (professor-faculty emp)]
    [(staff? emp) (staff-faculty emp)]))

;; (alst-newemp emp) consumes an Employee value(emp)and produces
;; an Association List of emp
;; alst-newemp: Employee -> AL
(define (alst-newemp emp)
  (list (emp-faculty emp) (list emp)))

;; (add-new-employee olduni newemp) consumes a University value(olduni)
;; and an Employee value(newemp) and produces a new University
;; add-new-employee: University Employee -> University
;; requires: add newemp to the beginning of the Employee's faculty list
;; Example:
(check-expect (add-new-employee
               (list
                (list "cs" (list (make-lecturer "a" 100000 "cs"))))
               (make-staff "d" 40000 "cs" "admin assist"))
              (list
               (list "cs"
                     (list (make-staff "d" 40000 "cs" "admin assist")
                           (make-lecturer "a" 100000 "cs"))))) ;same fac
(check-expect (add-new-employee
               empty
               (make-staff "d" 40000 "cs" "admin assist")) ;insert empty
              (list
               (list "cs"
                     (list (make-staff "d" 40000 "cs" "admin assist")))))

(define (add-new-employee olduni newemp) ;Wrapper function
  (cond
    [(empty? olduni) (list (alst-newemp newemp))]
    [(string=? (emp-faculty newemp) (first (first olduni)))
     (cons (list (first (first olduni))
                 (cons newemp (second (first olduni))))
           (rest olduni))]
    [else (cons (first olduni) (add-new-employee (rest olduni) newemp))]))

;; Test:
(check-expect (add-new-employee
               (list
                (list "cs" (list (make-lecturer "a" 100000 "cs"))))
               (make-staff "c" 40000 "math" "admin assist")) ;differ fac
              (list
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))
               (list "math"
                     (list (make-staff "c" 40000 "math" "admin assist")))))

;; (closing-a-faculty olduni afaculty)consumes a University value(olduni) 
;; and a String(afaculty) and produces a new University value,
;; which remove the afaculty and all associated employees
;; closing-a-faculty: University String -> University
;; requires: afaculty exists in the University
;; Example:
(check-expect (closing-a-faculty
               (list (list "cs"
                           (list (make-lecturer "a" 100000 "cs")))
                     (list "math"
                           (list (make-professor "b" 150000 "math" "HCI"))))
               "math")
              (list (list "cs" (list (make-lecturer "a" 100000 "cs")))))

(define (closing-a-faculty olduni afaculty)
  (cond
    [(empty? olduni) empty]
    [(equal? afaculty (first (first olduni)))
     (closing-a-faculty (rest olduni) afaculty)] 
    [else (cons (first olduni)
                (closing-a-faculty (rest olduni) afaculty))])) 
;; Tests:
(check-expect (closing-a-faculty
               (list (list "cs" (list (make-lecturer "a" 100000 "cs"))))
               "cs")
              empty) ;same-faculty
(check-expect (closing-a-faculty empty "cs") empty) ;empty case
(check-expect (closing-a-faculty
               (list (list "cs" (list (make-lecturer "a" 100000 "cs"))))
               "art")
              (list (list "cs" (list (make-lecturer "a" 100000 "cs"))))) ;none
;;
;;
;; (how-many-employees uni) consumes a University value(uni)
;; and produces the total number of employees across all faculties
;; how-many-employees: University -> Nat
;; Example: 
(check-expect (how-many-employees
               (list (list "cs"
                           (list (make-lecturer "a" 100000 "cs")))
                     (list "math"
                           (list (make-staff "c" 40000 "math" "admin assist")
                                 (make-professor "b" 150000 "math" "HCI")))))
              3) ;2

(define (how-many-employees uni)
  (cond
    [(empty? uni) 0] ;1
    [else (+ (length (second (first uni)))
             (how-many-employees (rest uni)))])) ;2
;; Test:
(check-expect (how-many-employees empty) 0) ;1
;;
;; Helper function for 2e:
;; (insert-university lst lolos) inserts the list(lst) into the list(lolos)
;; so that the resulting list is sorted.
;; insert-university: (listof AL) (listof AL) -> (listof AL)
(define (insert-university lst lolos)
  (cond
    [(empty? lolos) (list lst)]
    [(string<? (first lst) (first (first lolos)))
     (cons lst lolos)]
    [else (cons (first lolos) (insert-university lst (rest lolos)))]))

;; (sort-university origin) consumes a University value(origin) and produces
;; a University value that is sorted by faculty name in lexicographical order
;; sort-university: University -> University
;; Example:
(check-expect (sort-university
               (list
                (list "math"
                      (list (make-staff "c" 40000 "math" "admin assist")
                            (make-professor "b" 150000 "math" "HCI")))
                (list "cs"
                      (list (make-lecturer "a" 100000 "cs")))
                (list "physicis" empty)))
              (list
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))
               (list "math"
                     (list (make-staff "c" 40000 "math" "admin assist")
                           (make-professor "b" 150000 "math" "HCI")))
               (list "physicis" empty)))
(define (sort-university origin)
  (cond
    [(empty? origin) empty]
    [else (insert-university (first origin) (sort-university (rest origin)))]))
;; Test:
(check-expect (sort-university
               (list
                (list "math"
                      (list (make-staff "c" 40000 "math" "admin assist")
                            (make-professor "b" 150000 "math" "HCI")))
                (list "cs"
                      (list (make-lecturer "a" 100000 "cs")))
                (list "art"
                      (list (make-lecturer "bca" 100000 "art")))))
              (list
               (list "art"
                     (list (make-lecturer "bca" 100000 "art")))
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))
               (list "math"
                     (list (make-staff "c" 40000 "math" "admin assist")
                           (make-professor "b" 150000 "math" "HCI")))))

;; (merge-universities uni1 uni2) consumes two University values(uni1,uni2)
;; and produces a new University value which contains all value of uni1,uni2
;; merge-universities: University University -> University
;; requires: uni1 uni2 do not have the same faculty
;; Example:
(check-expect (merge-universities
               (list
                (list "cs"
                      (list (make-lecturer "a" 100000 "cs"))))
               (list
                (list "art"
                      (list (make-lecturer "bca" 100000 "art"))))) ;5
              (list
               (list "art"
                     (list (make-lecturer "bca" 100000 "art")))
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))))
(define (merge-universities uni1 uni2)
  (cond
    [(and (empty? uni1) (empty? uni2)) empty] ;1
    [(and (empty? uni1) (cons? uni2)) uni2] ;2
    [(and (cons? uni1) (empty? uni2)) uni1] ;3
    [(and (cons? uni1) (cons? uni2))
     (cond [(string<? (first (first uni1)) (first (first uni2)))
            (cons (first uni1) (merge-universities (rest uni1) uni2))] ;4
           [else (cons (first uni2) (merge-universities uni1 (rest uni2)))])])) ;5

;; Tests:
(check-expect (merge-universities empty empty) empty) ;1
(check-expect (merge-universities
               empty
               (list
                (list "art"
                      (list (make-lecturer "bca" 100000 "art"))))) ;2
              (list
               (list "art"
                     (list (make-lecturer "bca" 100000 "art")))))
(check-expect (merge-universities
               (list
                (list "cs"
                      (list (make-lecturer "a" 100000 "cs"))))
               empty) ;3
              (list
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))))
(check-expect (merge-universities
               (list
                (list "art"
                      (list (make-lecturer "bca" 100000 "art"))))
               (list
                (list "cs"
                      (list (make-lecturer "a" 100000 "cs"))))) ;4
              (list
               (list "art"
                     (list (make-lecturer "bca" 100000 "art")))
               (list "cs"
                     (list (make-lecturer "a" 100000 "cs")))));
