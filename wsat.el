;;; wsat.el -- Walksat solver

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1

;;; Commentary:

;; Solves SAT problem

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun argmin (items)
  "Return min value index for ITEMS."
  (let ((min-index 0)
        (min-value (car items))
        (current-index 1))
    (dolist (item (cdr items))
      (when (< item min-value)
        (setq min-index current-index)
        (setq min-value item))
      (setq current-index (+ 1 current-index)))
    min-index))

(defun wsat-clause-true? (clause assignment)
  "Check for truth of the CLAUSE under given ASSIGNMENT."
  (if (equal (length clause) 0) nil
    (if (wsat-literal-value (car clause) assignment) t
      (wsat-clause-true? (cdr clause) assignment))))

(defun wsat-statement-true? (statement assignment)
  "Check for truth of the STATEMENT under given ASSIGNMENT."
  (if (equal (length statement) 0) t
    (if (wsat-clause-true? (car statement) assignment)
        (wsat-statement-true? (cdr statement) assignment)
      nil)))

(defun wsat-literal-symbol (literal)
  "Get the symbol used in LITERAL. Assume a list of form '(not P) for negation."
  (if (sequencep literal)
      (cadr literal)
    literal))

(defun wsat-literal-value (literal assignment)
  "Get the value of LITERAL under given ASSIGNMENT.
Assume a list of form '(not P) for negation."
  (if (sequencep literal)
      (not (gethash (cadr literal) assignment))
    (gethash literal assignment)))

(defun wsat-random-truth ()
  "Get t or nil randomly."
  (if (equal (random 2) 1) t nil))

(defun wsat-random-assigment (symbols)
  "Generate a random assignment map for given SYMBOLS."
  (let ((assignment (make-hash-table)))
    (dolist (sym symbols)
      (puthash sym (wsat-random-truth) assignment))
    assignment))

(defun wsat-satisfied-clause (statement assignment)
  "Get all satisfied clause from the STATEMENT under ASSIGNMENT."
  (cl-remove-if (lambda (c) (not (wsat-clause-true? c assignment))) statement))

(defun wsat-unsatisfied-clause (statement assignment)
  "Get all unsatisfied clause from the STATEMENT under ASSIGNMENT."
  (cl-remove-if (lambda (c) (wsat-clause-true? c assignment)) statement))

(defun wsat-count-unsatisfaction (statement assignment sym)
  "Count the number of unsatisfied clause in STATEMENT with an ASSIGNMENT flip of SYM."
  (let ((new-assignment (wsat-flip-assignment assignment sym)))
    (length (wsat-unsatisfied-clause statement new-assignment))))

(defun wsat-select-clause (statement assignment)
  "Return a random unsatisfied clause from the STATEMENT under ASSIGNMENT."
  (let ((unsatisfied-cls (wsat-unsatisfied-clause statement assignment)))
    (nth (random (length unsatisfied-cls)) unsatisfied-cls)))

(defun wsat-select-symbol (clause statement assignment)
  "Select a symbol from the CLAUSE which minimizes the newer unassigned clauses.
Compared to given STATEMENT under ASSIGNMENT."
  (if (equal 0 (random 10))
      (wsat-literal-symbol
       (nth (random (length clause)) clause))
    (let* ((unassigned-cls (wsat-unsatisfied-clause statement assignment))
           (unassigned-counts
            (mapcar
             (lambda (lit)
               (wsat-count-unsatisfaction unassigned-cls assignment (wsat-literal-symbol lit))) clause)))
      (wsat-literal-symbol (nth (argmin unassigned-counts) clause)))))

(defun wsat-flip-assignment (assignment sym)
  "Return a new ASSIGNMENT by flipping the given SYM."
  (let ((current (gethash sym assignment))
        (new-assignment (copy-hash-table assignment)))
    (puthash sym (not current) new-assignment)
    new-assignment))

(defun wsat-solve (statement symbols max-iterations max-restarts)
  "Solve the problem STATEMENT by working on SYMBOLS.
Go for MAX-ITERATIONS mutations and MAX-RESTARTS resets."
  (if (>= max-restarts 0)
      (let ((assignment (wsat-random-assigment symbols)))
        (print (format "Initial assignment %s -> %s"
                       (hash-table-keys assignment)
                       (hash-table-values assignment)))
        (print (format "%d restarts left" max-restarts))
        (catch 'solved
          (dotimes (i max-iterations)
            (if (wsat-statement-true? statement assignment)
                (progn
                  (print (format "Solved in %d iterations" i))
                  (throw 'solved assignment))
              (setq assignment
                    (wsat-flip-assignment
                     assignment (wsat-select-symbol
                                 (wsat-select-clause statement assignment)
                                 statement
                                 assignment)))))
          ;; Restart with another random position
          (wsat-solve statement symbols max-iterations (- max-restarts 1))))
    (print "Max number of restarts reached")
    nil))

(provide 'wsat)

;;; wsat.el ends here
