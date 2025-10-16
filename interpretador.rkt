#lang eopl

;; Taller 3 — Intérprete extendido
;; Cadenas entre /.../, identificadores con @, y todas las construcciones solicitadas.

;******************************************************************************************

(define lexico
  '(
    (espacio (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)

    ;; Identificadores que empiezan con @
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)

    ;; Números: enteros y flotantes
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ;; TEXTO entre /.../
    ;; (Excluimos "/" del interior; y usamos " " en vez de space)
    (texto ("/" (arbno (or letter digit " " "," ":" "-" "?" "." "@" "(" ")" "+" "~" "*" "\\")) "/") string)
  ))

;******************************************************************************************

(define gramatica
  '(
    (programa (expresion) un-programa)

    (expresion (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    ;; Condicionales
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSi") condicional-exp)

    ;; Declaración local
    (expresion ("declarar" "(" lista-decl ")" "{" expresion "}") variableLocal-exp)

    ;; Procedimientos
    (expresion ("procedimiento" "(" lista-ids ")" "haga" expresion "finProc") procedimiento-ex)

    ;; Procedimientos recursivos
    (expresion ("procedimiento-rec" identificador "(" lista-ids ")" "haga" expresion "finProcRec") procedimiento-rec-ex)

    ;; Aplicación
    (expresion ("evaluar" expresion "(" lista-exps-opt ")" "finEval") app-exp)

    ;; =======================
    ;; Listas factorizadas LL(1)
    ;; =======================

    ;; Lista de identificadores: id ("," id)*   — permite vacío
    (lista-ids () lista-ids-vacia)
    (lista-ids (identificador lista-ids-tail) lista-ids-primero)

    (lista-ids-tail () lista-ids-tail-vacia)
    (lista-ids-tail ("," identificador lista-ids-tail) lista-ids-tail-mas)

    ;; Lista de expresiones: exp ("," exp)*     — **NO** permite vacío (1+)
    (lista-exps (expresion lista-exps-tail) lista-exps-primera)

    (lista-exps-tail () lista-exps-tail-vacia)
    (lista-exps-tail ("," expresion lista-exps-tail) lista-exps-tail-mas)

    ;; Lista de expresiones opcional (0 o más) — el vacío solo aquí
    (lista-exps-opt () lista-exps-opt-vacia)
    (lista-exps-opt (lista-exps) lista-exps-opt-directa)

    ;; Lista de declaraciones: id "=" exp (";" id "=" exp)* — permite vacío
    (lista-decl () lista-decl-vacia)
    (lista-decl (identificador "=" expresion lista-decl-tail) lista-decl-primera)

    (lista-decl-tail () lista-decl-tail-vacia)
    (lista-decl-tail (";" identificador "=" expresion lista-decl-tail) lista-decl-tail-mas)

    ;;  Primitivas binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)  ; si tu resta binaria es "~"
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;;  Primitivas unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
  ))

;******************************************************************************************
; Generación de datatypes y parsers
(sllgen:make-define-datatypes lexico gramatica)

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

(define just-scan
  (sllgen:make-string-scanner lexico gramatica))

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa pgm))
    (sllgen:make-stream-parser lexico gramatica)))

;******************************************************************************************
; Evaluación de programas
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp) (evaluar-expresion exp (init-env))))))

;******************************************************************************************
;; Verdadero/falso: 0 es falso; otro número es verdadero
(define valor-verdad?
  (lambda (x) (and (number? x) (not (zero? x)))))

;******************************************************************************************
;; Ambiente
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env ambiente?)))

(define scheme-value? (lambda (v) #t))

(define empty-env (lambda () (empty-env-record)))
(define extend-env (lambda (syms vals env) (extended-env-record syms vals env)))

(define init-env
  (lambda ()
    (extend-env '(@a @b @c @d @e)
                '(1 2 3 "hola" "FLP")
                (empty-env))))

;******************************************************************************************
;; Búsqueda de variables
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (empty-env-record ()
        (eopl:error 'buscar-variable "Variable no definida: ~s" sym))
      (extended-env-record (syms vals old-env)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
              (list-ref vals pos)
              (buscar-variable old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
        (let ([pos (list-find-position sym proc-names)])
          (if (number? pos)
              (cerradura (list-ref idss pos)
                         (list-ref bodies pos)
                         env)
              (buscar-variable old-env sym)))))))

;******************************************************************************************
;; Procedimientos
(define-datatype procVal procVal?
  (cerradura (lista-ID (list-of symbol?))
             (exp expresion?)
             (amb ambiente?))
  (cerradura-rec (nombre symbol?)
                 (lista-ID (list-of symbol?))
                 (exp expresion?)
                 (amb ambiente?)))

;******************************************************************************************
;; Evaluador
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) (substring txt 1 (- (string-length txt) 1))) ; quita / inicial y final
      (var-exp (id) (buscar-variable env id))

      (primapp-bin-exp (e1 prim e2)
        (aplicar-primitiva-binaria prim
          (evaluar-expresion e1 env)
          (evaluar-expresion e2 env)))

      (primapp-un-exp (prim e)
        (aplicar-primitiva-unaria prim (evaluar-expresion e env)))

      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (evaluar-expresion test-exp env))
            (evaluar-expresion true-exp env)
            (evaluar-expresion false-exp env)))

      (variableLocal-exp (decls body)
        (let* ([pair (eval-decls decls env)]
               [ids (car pair)]
               [vals (cadr pair)])
          (evaluar-expresion body (extend-env ids vals env))))

      (procedimiento-ex (ids cuerpo)
        (cerradura (ids->list ids) cuerpo env))

      (procedimiento-rec-ex (name ids cuerpo)
        (cerradura-rec name (ids->list ids) cuerpo env))

      (app-exp (rator rands-opt)
        (let* ([proc (evaluar-expresion rator env)]
               [args (eval-list-exps rands-opt env)])
          (apply-procedure proc args))))))

;******************************************************************************************
;; Listas auxiliares

;; ---------- IDS ----------
(define ids->list
  (lambda (ids)
    (cases lista-ids ids
      (lista-ids-vacia () '())
      (lista-ids-primero (id tail)
        (cons id (ids-tail->list tail))))))

(define ids-tail->list
  (lambda (tail)
    (cases lista-ids-tail tail
      (lista-ids-tail-vacia () '())
      (lista-ids-tail-mas (id rest)
        (cons id (ids-tail->list rest))))))

;; ---------- EXPS (1+), y OPT ----------
(define exps->list
  (lambda (lx)
    (cases lista-exps lx
      (lista-exps-primera (e tail)
        (cons e (exps-tail->list tail))))))

(define exps-tail->list
  (lambda (tail)
    (cases lista-exps-tail tail
      (lista-exps-tail-vacia () '())
      (lista-exps-tail-mas (e rest)
        (cons e (exps-tail->list rest))))))

(define eval-list-exps
  (lambda (lx env)
    (let ([lst
           (cases lista-exps-opt lx
             (lista-exps-opt-vacia () '())
             (lista-exps-opt-directa (lst) (exps->list lst)))])
      (map (lambda (e) (evaluar-expresion e env)) lst))))

;; ---------- DECLARACIONES ----------
(define eval-decls
  (lambda (decls env)
    (cases lista-decl decls
      (lista-decl-vacia () (list '() '()))
      (lista-decl-primera (id exp tail)
        (let* ([v (evaluar-expresion exp env)]
               [pair (eval-decls-tail tail env)])
          (list (cons id (car pair))
                (cons v  (cadr pair))))))))

(define eval-decls-tail
  (lambda (tail env)
    (cases lista-decl-tail tail
      (lista-decl-tail-vacia () (list '() '()))
      (lista-decl-tail-mas (id exp rest)
        (let* ([v (evaluar-expresion exp env)]
               [pair (eval-decls-tail rest env)])
          (list (cons id (car pair))
                (cons v  (cadr pair))))))))

;******************************************************************************************
;; Aplicación de procedimientos
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
        (unless (= (length ids) (length args))
          (eopl:error 'apply-procedure "Aridad incorrecta"))
        (evaluar-expresion body (extend-env ids args env)))
      (cerradura-rec (name ids body env)
        (unless (= (length ids) (length args))
          (eopl:error 'apply-procedure "Aridad incorrecta"))
        (let ([self proc])
          (evaluar-expresion body
            (extend-env (cons name ids) (cons self args) env)))))))

;******************************************************************************************
;; Primitivas
(define aplicar-primitiva-binaria
  (lambda (t-primitiva-binaria exp-1 exp-2)
    (cases primitiva-binaria t-primitiva-binaria
      (primitiva-suma () (+ exp-1 exp-2))
      (primitiva-resta () (- exp-1 exp-2))
      (primitiva-div () (/ exp-1 exp-2))
      (primitiva-multi () (* exp-1 exp-2))
      (primitiva-concat () (string-append exp-1 exp-2)))))

(define aplicar-primitiva-unaria
  (lambda (t-primitiva-unaria exp-1)
    (cases primitiva-unaria t-primitiva-unaria
      (primitiva-longitud () (string-length exp-1))
      (primitiva-add1 () (+ exp-1 1))
      (primitiva-sub1 () (- exp-1 1)))))

;******************************************************************************************
;; Utilidades
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (s) (eqv? s sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else (let ([r (list-index pred (cdr ls))])
              (and r (+ r 1)))])))

;******************************************************************************************
;; Helper para pruebas
(define (interpretar str)
  (evaluar-programa (scan&parse str)))

;******************************************************************************************
;; Fin del archivo
