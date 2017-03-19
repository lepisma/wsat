(defun argmin (items)
  "There is no argmin in elisp!"
  (let ((min-index 0)
        (min-value (car items))
        (current-index 1))
    (dolist (item (cdr items))
      (if (< item min-value)
          (progn
            (setq min-index current-index)
            (setq min-value item)))
      (setq current-index (+ 1 current-index)))
    min-index))

(defun wsat-clause-true? (clause assignment)
  "Check for truth of a clause"
  (if (equal (length clause) 0) nil
    (if (wsat-literal-value (car clause) assignment) t
      (wsat-clause-true? (cdr clause) assignment))))

(defun wsat-statement-true? (statement assignment)
  (if (equal (length statement) 0) t
    (if (wsat-clause-true? (car statement) assignment)
        (wsat-statement-true? (cdr statement) assignment)
      nil)))

(defun wsat-literal-symbol (literal)
  "Get the symbol used in literal"
  (if (sequencep literal)
      (second literal)
    literal))

(defun wsat-literal-value (literal assignment)
  "Get the value of literal"
  (if (sequencep literal)
      (not (gethash (second literal) assignment))
    (gethash literal assignment)))

(defun wsat-random-truth ()
  (if (equal (random 2) 1) t nil))

(defun wsat-random-assigment (symbols)
  "Generate a random assignemnt map for given symbols"
  (let ((assignment (make-hash-table)))
    (dolist (sym symbols)
      (puthash sym (wsat-random-truth) assignment))
    assignment))

(defun wsat-select-clause (statement)
  "Return a random unsatisfied clause from the statement"
  (nth (random (length statement)) statement))

(defun wsat-n-unsatisfied-clause (statement assignment)
  (apply '+ (mapcar (lambda (c) (if (wsat-clause-true? c assignment) 1 0)) statement)))

(defun wsat-select-symbol (clause statement assignment)
  "TODO This doesn't check for previously unassigned"
  (if (equal 0 (random 10))
      (wsat-literal-symbol
       (nth (random (length clause)) clause))
    (let ((unassigned-counts
           (mapcar
            (lambda (lit)
              (wsat-n-unsatisfied-clause
               statement (wsat-flip-assignment assignment (wsat-literal-symbol lit)))) clause)))
      (wsat-literal-symbol (nth (argmin unassigned-counts) clause)))))

(defun wsat-flip-assignment (assignment sym)
  (let ((current (gethash sym assignment))
        (new-assignment (copy-hash-table assignment)))
    (puthash sym (not current) new-assignment)
    new-assignment))

(defun wsat-solve (statement symbols max-iterations max-restarts)
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
                                 (wsat-select-clause statement)
                                 statement
                                 assignment)))))
          ;; Restart with another random position
          (wsat-solve statement symbols max-iterations (- max-restarts 1))))
    (print "Max number of restarts reached")
    nil))

(provide 'wsat)
