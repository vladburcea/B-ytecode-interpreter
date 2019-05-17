#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:

(define empty-stack null)

(define (make-stack)
  empty-stack)

(define (push element stack)
  (cons element stack))
  
(define (top stack)
  (if (null? stack)
      stack
      (car stack)))
  
(define (pop stack)
  (if (null? stack)
      stack
      (drop stack 1)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC)
  )

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (first stack-machine))

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (second stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (third stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (fourth stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (fifth stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (sixth stack-machine))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

(define (get-symbol-index symbol)
  (cond
    [(equal? symbol 'STACK) 0]
    [(equal? symbol 'CO-VARNAMES) 1]
    [(equal? symbol 'CO-CONSTS) 2]
    [(equal? symbol 'CO-NAMES) 3]
    [(equal? symbol 'CO-CODE) 4]
    [(equal? symbol 'INSTRUCTION-COUNTER) 5]
    [else -1]
    ))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-names (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item))

;; For testing purposes
;; (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0)))
;; (get-stack (update-stack-machine "new-names" 'STACK (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0)))


;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;; For testing purposes
;; (define stack-machine (make-stack-machine (list 1 2 3) (hash) 'dummy-co-consts (hash) (list) 0))
;; (push-exec-stack 3 stack-machine)


;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; For testing purposes
;; (pop-exec-stack stack-machine)



;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

;; For debugging purposes
;; (load_global 1)
;; (load_fast 1)
;; (store_fast 1)
;; stack-machine
;; (load_const stack-machine 1)


;; IC is updated with every operation
;; Function that takes an element (at index idx) from the co-consts and adds it to the stack
(define (load_const stack-machine idx)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (hash-ref (get-consts stack-machine) idx) stack-machine)))

;; Function that adds a function from co_names to stack. The same as "load_const"
(define (load_global stack-machine idx)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (hash-ref (get-names stack-machine) idx) stack-machine)))

;; Function that takes TOS (top of stack) and stores it in co-varnames at index idx
(define (store_fast stack-machine idx)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (pop-exec-stack (update-stack-machine
                    (hash-set (get-varnames stack-machine)
                              idx
                              (top (get-stack stack-machine)))
                    'CO-VARNAMES
                    stack-machine))
   ))

;; Function that adds an element from co_varnames to stack. The same as "load_const"
(define (load_fast stack-machine idx)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (hash-ref (get-varnames stack-machine) idx) stack-machine)
   ))

;; Function that pops two elements from the stack and pushes their sum back onto the stack
(define (binary_add stack-machine)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (+
                     (top (cdr (get-stack stack-machine)))
                     (top (get-stack stack-machine)))
                    (pop-exec-stack (pop-exec-stack stack-machine)))
   ))

;; Function that pops two elements from the stack and pushes their difference back onto the stack
(define (binary_subtract stack-machine)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (-
                     (top (cdr (get-stack stack-machine)))
                     (top (get-stack stack-machine)))
                    (pop-exec-stack (pop-exec-stack stack-machine)))
   ))

;; Function that pops two elements from the stack and pushes their modulo back onto the stack
(define (binary_modulo stack-machine)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack (modulo
                     (top (cdr (get-stack stack-machine)))
                     (top (get-stack stack-machine)))
                    (pop-exec-stack (pop-exec-stack stack-machine)))
   ))

;; Function that pops two elements from the stack and and executes the given function
(define (compare_op stack-machine idx)
  (update-stack-machine ;; IC Update
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER
   (push-exec-stack ((get-cmpop idx)
                     (top (cdr (get-stack stack-machine)))
                     (top (get-stack stack-machine)))
                    (pop-exec-stack (pop-exec-stack stack-machine)))
   ))


;; Function that tests TOS. If TOS is true it jumps to the IC provided.
;; The IC is given as a byte-code index, therefor it needs to be divided by two
;; (the bytecode starts from 0 and contains only multiples of 2)
;; If the statement is false, then the function does nothing (except from incrementing IC)
(define (jmp_true stack-machine idx)
  (if (top (get-stack stack-machine))
      (update-stack-machine 
       (/ idx 2)
       'INSTRUCTION-COUNTER
       (pop-exec-stack stack-machine))
      (update-stack-machine ;; IC Update
       (add1 (get-IC stack-machine))
       'INSTRUCTION-COUNTER
       (pop-exec-stack stack-machine))
      ))


;; This function has the same purpose as jmp_true, but for a flase statement.
(define (jmp_false stack-machine idx)
  (if (not (top (get-stack stack-machine)))
      (update-stack-machine
       (/ idx 2)
       'INSTRUCTION-COUNTER
       (pop-exec-stack stack-machine))
      (update-stack-machine
       (add1 (get-IC stack-machine))
       'INSTRUCTION-COUNTER ;; IC Update 
       (pop-exec-stack stack-machine))
      ))

;; Function that performs an unconditional jump to the IC given.
(define (jmp_abs stack-machine idx)
  (update-stack-machine
   (/ idx 2)
   'INSTRUCTION-COUNTER ;; IC Update 
   stack-machine))

;; This function iterates over a list as long as it can.
;; If the list isn't empty, then the first element of the list is pushed onto stack
;; and eliminated from the list. The list is push onto the stack before the element.
;; For the list '(1 2 3 4) after two iterations the stack would look like this:
;; 1st iter:                 2nd iter:
;;
;;     |    1    | <- TOS        |    2    | <- TOS
;;     | (2 3 4) |               |  (3 4)  |
;;     |   ...   |               |   ...   |
;;     |_________|               |_________|
;;
;; If the list is empty the function just jumps at (IC + (delta + 2)/2)
(define (for_iter stack-machine delta)
  (define current car)
  (define next cdr)
  (define iter (car (get-stack stack-machine)))

  (if (null? iter)
      (jmp_abs 
       (pop-exec-stack stack-machine)
       (+ delta 2 (* (get-IC stack-machine) 2))) ;; Jumping at (IC + (delta + 2)/2)
      (update-stack-machine
       (add1 (get-IC stack-machine))
       'INSTRUCTION-COUNTER ;; IC Update 
       (push-exec-stack (current iter) (push-exec-stack (next iter) (pop-exec-stack stack-machine)))
       )))

;; Function that takes a n arguments from the stack and then apllies a function
;; found at TOS on them
;; It push onto the stack the result of the function
;; Example:
;;
;;     |„Hello world“| <- TOS           |   void   | <- TOS
;;     |    print    |                  |   ....   |
;;     |     ...     |          =>      |   ....   |
;;     |_____________|                  |__________|
;;
(define (call_f stack-machine n)
  (update-stack-machine
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER  ;; IC Update 
   (push-exec-stack
    (let ((f (get-function (car (drop (get-stack stack-machine) n))))
          (arg (take (get-stack stack-machine) n)))
      (f arg))           ;; Applying the function on the arguments
    (update-stack-machine
     (drop (get-stack stack-machine) (add1 n))
     'STACK              ;; Popping n+1 elements from the stack (n arguments and the function itself)
     stack-machine))
   ))

;; Function that pops an element from the stack
(define (pop_top stack-machine)
  (update-stack-machine
   (add1 (get-IC stack-machine))
   'INSTRUCTION-COUNTER  ;; IC Update 
   (pop-exec-stack stack-machine)))

;; Instruction parser
(define (run-stack-machine stack-machine)  
  (let loop ((sm stack-machine))
    (define instr (list-ref (get-code sm) (get-IC sm)))
    (case (car instr)
      ['RETURN_VALUE                          sm]
      ['LOAD_CONST                            (loop (load_const sm (cdr instr)))]
      ['LOAD_GLOBAL                           (loop (load_global sm (cdr instr)))]
      ['LOAD_FAST                             (loop (load_fast sm (cdr instr)))]
      ['STORE_FAST                            (loop (store_fast sm (cdr instr)))]
      [(BINARY_ADD INPLACE_ADD)               (loop (binary_add sm))]
      [(BINARY_SUBTRACT INPLACE_SUBTRACT)     (loop (binary_subtract sm))]
      [(BINARY_MODULO INPLACE_MODULO)         (loop (binary_modulo sm))]
      ['COMPARE_OP                            (loop (compare_op sm (cdr instr)))]
      ['POP_JUMP_IF_FALSE                     (loop (jmp_false sm (cdr instr)))]
      ['POP_JUMP_IF_TRUE                      (loop (jmp_true sm (cdr instr)))]
      ['JUMP_ABSOLUTE                         (loop (jmp_abs sm (cdr instr)))]
      ['FOR_ITER                              (loop (for_iter sm (cdr instr)))]
      ['CALL_FUNCTION                         (loop (call_f sm (cdr instr)))]
      ['POP_TOP                               (loop (pop_top sm))]

      ;; If the instrunction is not implemented I skip it and update the IC
      [else (loop (update-stack-machine (add1 (get-IC sm)) 'INSTRUCTION-COUNTER sm))]
      )
    )
  )