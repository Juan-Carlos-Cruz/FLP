#lang eopl

;; Taller 3 — Intérprete extendido
;; Cadenas entre /.../, identificadores con @, y todas las construcciones solicitadas.

;******************************************************************************************
;; ESPECIFICACIÓN LÉXICA
;; Define los patrones para reconocer tokens del lenguaje
;******************************************************************************************

(define lexico
  '(
    ;; Ignorar espacios en blanco durante el análisis léxico
    (espacio (whitespace) skip)
    ;; Ignorar comentarios que comienzan con % y continúan hasta el fin de línea
    (comentario ("%" (arbno (not #\newline))) skip)

    ;; Identificadores que empiezan con @ seguido de una letra y opcionalmente más caracteres alfanuméricos o "?"
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)

    ;; Números: enteros positivos, negativos, y flotantes con punto decimal
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ;; Texto entre /.../: cadenas delimitadas por barras que pueden contener varios caracteres
    ;; (Excluimos "/" del interior; y usamos " " en vez de space)
    (texto ("/" (arbno (or letter digit " " "," ":" "-" "?" "." "@" "(" ")" "+" "~" "*" "\\")) "/") string)
  ))

;******************************************************************************************
;; ESPECIFICACIÓN SINTÁCTICA (GRAMÁTICA)
;; Define la estructura del lenguaje mediante reglas de producción BNF
;******************************************************************************************

(define gramatica
  '(
    ;; Un programa consiste en una única expresión a evaluar
    (programa (expresion) un-programa)

    ;; Expresiones básicas del lenguaje: números, texto y variables
    (expresion (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expresion (identificador) var-exp)
    ;; Expresiones con operadores binarios en notación infija: (exp op exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    ;; Expresiones con operadores unarios: op(exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    ;; Condicional: evalúa test-exp, si es verdadero evalúa true-exp, sino false-exp
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSi") condicional-exp)

    ;; Declaración local: permite definir variables con ámbito limitado
    (expresion ("declarar" "(" lista-decl ")" "{" expresion "}") variableLocal-exp)

    ;; Procedimientos: crea closures (funciones) con parámetros y cuerpo
    (expresion ("procedimiento" "(" lista-ids ")" "haga" expresion "finProc") procedimiento-ex)

    ;; Procedimientos recursivos: permiten autorreferencia mediante un nombre
    (expresion ("procedimiento-rec" identificador "(" lista-ids ")" "haga" expresion "finProcRec") procedimiento-rec-ex)

    ;; Aplicación de procedimientos: evalúa un procedimiento con argumentos
    (expresion ("evaluar" expresion "(" lista-exps-opt ")" "finEval") app-exp)

    ;; =======================
    ;; Listas factorizadas LL(1)
    ;; =======================

    ;; Lista de identificadores: secuencia de ids separados por comas (permite vacío)
    (lista-ids () lista-ids-vacia)
    (lista-ids (identificador lista-ids-tail) lista-ids-primero)

    (lista-ids-tail () lista-ids-tail-vacia)
    (lista-ids-tail ("," identificador lista-ids-tail) lista-ids-tail-mas)

    ;; Lista de expresiones: secuencia de exps separadas por comas (mínimo 1 elemento)
    (lista-exps (expresion lista-exps-tail) lista-exps-primera)

    (lista-exps-tail () lista-exps-tail-vacia)
    (lista-exps-tail ("," expresion lista-exps-tail) lista-exps-tail-mas)

    ;; Lista de expresiones opcional: puede estar vacía o tener elementos
    (lista-exps-opt () lista-exps-opt-vacia)
    (lista-exps-opt (lista-exps) lista-exps-opt-directa)

    ;; Lista de declaraciones: asignaciones de variables separadas por punto y coma
    (lista-decl () lista-decl-vacia)
    (lista-decl (identificador "=" expresion lista-decl-tail) lista-decl-primera)

    (lista-decl-tail () lista-decl-tail-vacia)
    (lista-decl-tail (";" identificador "=" expresion lista-decl-tail) lista-decl-tail-mas)

    ;; Primitivas binarias: operaciones que toman dos operandos
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)  ; si tu resta binaria es "~"
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;; Primitivas unarias: operaciones que toman un solo operando
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
  ))

;******************************************************************************************
;; GENERACIÓN DE DATATYPES Y PARSERS
;; Crea automáticamente los tipos de datos abstractos y funciones de parsing
;******************************************************************************************
(sllgen:make-define-datatypes lexico gramatica)

;; Parsea un string completo y devuelve un árbol de sintaxis abstracta
(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;; Solo realiza el análisis léxico (tokenización) sin construir el AST
(define just-scan
  (sllgen:make-string-scanner lexico gramatica))

;; Intérprete interactivo (Read-Eval-Print Loop) para probar el lenguaje
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa pgm))
    (sllgen:make-stream-parser lexico gramatica)))

;******************************************************************************************
;; EVALUACIÓN DE PROGRAMAS
;; Función principal que inicia la evaluación de un programa completo
;******************************************************************************************
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp) (evaluar-expresion exp (init-env))))))

;******************************************************************************************
;; MANEJO DE VALORES DE VERDAD
;; Define la semántica de verdad: 0 es falso, cualquier otro número es verdadero
;******************************************************************************************
(define valor-verdad?
  (lambda (x) (and (number? x) (not (zero? x)))))

;******************************************************************************************
;; SISTEMA DE AMBIENTES (ENVIRONMENTS)
;; Maneja el contexto de variables y sus valores mediante ámbitos anidados
;******************************************************************************************
(define-datatype ambiente ambiente?
  (empty-env-record)  ;; Ambiente vacío base
  (extended-env-record (syms (list-of symbol?))     ;; Lista de nombres de variables
                       (vals (list-of scheme-value?))   ;; Lista de valores asociados
                       (env ambiente?))             ;; Ambiente padre (para ámbitos anidados)
  (recursively-extended-env-record (proc-names (list-of symbol?))     ;; Nombres de procedimientos recursivos
                                   (idss (list-of (list-of symbol?))) ;; Listas de parámetros formales
                                   (bodies (list-of expresion?))      ;; Cuerpos de los procedimientos
                                   (env ambiente?)))                  ;; Ambiente padre

;; Predicado que acepta cualquier valor de Scheme (para flexibilidad en los valores almacenados)
(define scheme-value? (lambda (v) #t))

;; Constructor del ambiente vacío
(define empty-env (lambda () (empty-env-record)))

;; Extiende un ambiente con nuevos bindings (asociaciones variable-valor)
(define extend-env (lambda (syms vals env) (extended-env-record syms vals env)))

;; Ambiente inicial predefinido con variables de ejemplo
(define init-env
  (lambda ()
    (extend-env '(@a @b @c @d @e)
                '(1 2 3 "hola" "FLP")
                (empty-env))))

;******************************************************************************************
;; BÚSQUEDA DE VARIABLES
;; Busca un símbolo en el ambiente y retorna su valor asociado
;******************************************************************************************
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
;; PROCEDIMIENTOS (CLOSURES)
;; Representa funciones como closures que capturan su ambiente de definición
;******************************************************************************************
(define-datatype procVal procVal?
  ;; Closure regular: captura parámetros, cuerpo y ambiente de definición
  (cerradura (lista-ID (list-of symbol?))
             (exp expresion?)
             (amb ambiente?))
  ;; Closure recursivo: incluye nombre para permitir autorreferencia
  (cerradura-rec (nombre symbol?)
                 (lista-ID (list-of symbol?))
                 (exp expresion?)
                 (amb ambiente?)))

;******************************************************************************************
;; EVALUADOR PRINCIPAL
;; Evalúa una expresión en un ambiente dado, manejando todos los tipos de expresiones
;******************************************************************************************
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      ;; Literales numéricos: se evalúan a sí mismos
      (numero-lit (num) num)
      ;; Literales de texto: remueve las barras delimitadoras
      (texto-lit (txt) (substring txt 1 (- (string-length txt) 1))) ; quita / inicial y final
      ;; Variables: buscan su valor en el ambiente
      (var-exp (id) (buscar-variable env id))

      ;; Aplicación de primitivas binarias: evalúa ambos operandos y aplica la operación
      (primapp-bin-exp (e1 prim e2)
        (aplicar-primitiva-binaria prim
          (evaluar-expresion e1 env)
          (evaluar-expresion e2 env)))

      ;; Aplicación de primitivas unarias: evalúa el operando y aplica la operación
      (primapp-un-exp (prim e)
        (aplicar-primitiva-unaria prim (evaluar-expresion e env)))

      ;; Condicional: evalúa la condición y decide qué rama evaluar
      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (evaluar-expresion test-exp env))
            (evaluar-expresion true-exp env)
            (evaluar-expresion false-exp env)))

      ;; Declaración local: evalúa las definiciones y extiende el ambiente para el cuerpo
      (variableLocal-exp (decls body)
        (let* ([pair (eval-decls decls env)]
               [ids (car pair)]
               [vals (cadr pair)])
          (evaluar-expresion body (extend-env ids vals env))))

      ;; Creación de procedimiento: construye un closure con el ambiente actual
      (procedimiento-ex (ids cuerpo)
        (cerradura (ids->list ids) cuerpo env))

      ;; Creación de procedimiento recursivo: closure con capacidad de autorreferencia
      (procedimiento-rec-ex (name ids cuerpo)
        (cerradura-rec name (ids->list ids) cuerpo env))

      ;; Aplicación de procedimiento: evalúa el procedimiento y los argumentos, luego aplica
      (app-exp (rator rands-opt)
        (let* ([proc (evaluar-expresion rator env)]
               [args (eval-list-exps rands-opt env)])
          (apply-procedure proc args))))))

;******************************************************************************************
;; FUNCIONES AUXILIARES PARA MANEJO DE LISTAS
;; Convierten estructuras de listas anidadas a listas planas de Scheme
;******************************************************************************************

;; ---------- MANEJO DE LISTAS DE IDENTIFICADORES ----------
;; Convierte una lista estructurada de IDs a una lista plana de símbolos
(define ids->list
  (lambda (ids)
    (cases lista-ids ids
      (lista-ids-vacia () '())
      (lista-ids-primero (id tail)
        (cons id (ids-tail->list tail))))))

;; Procesa recursivamente la cola de una lista de identificadores
(define ids-tail->list
  (lambda (tail)
    (cases lista-ids-tail tail
      (lista-ids-tail-vacia () '())
      (lista-ids-tail-mas (id rest)
        (cons id (ids-tail->list rest))))))

;; ---------- MANEJO DE LISTAS DE EXPRESIONES ----------
;; Convierte una lista estructurada de expresiones a lista plana
(define exps->list
  (lambda (lx)
    (cases lista-exps lx
      (lista-exps-primera (e tail)
        (cons e (exps-tail->list tail))))))

;; Procesa recursivamente la cola de una lista de expresiones
(define exps-tail->list
  (lambda (tail)
    (cases lista-exps-tail tail
      (lista-exps-tail-vacia () '())
      (lista-exps-tail-mas (e rest)
        (cons e (exps-tail->list rest))))))

;; Evalúa una lista opcional de expresiones en el ambiente dado
(define eval-list-exps
  (lambda (lx env)
    (let ([lst
           (cases lista-exps-opt lx
             (lista-exps-opt-vacia () '())
             (lista-exps-opt-directa (lst) (exps->list lst)))])
      (map (lambda (e) (evaluar-expresion e env)) lst))))

;; ---------- MANEJO DE DECLARACIONES ----------
;; Evalúa una lista de declaraciones, retornando dos listas: identificadores y valores
(define eval-decls
  (lambda (decls env)
    (cases lista-decl decls
      (lista-decl-vacia () (list '() '()))
      (lista-decl-primera (id exp tail)
        (let* ([v (evaluar-expresion exp env)]
               [pair (eval-decls-tail tail env)])
          (list (cons id (car pair))
                (cons v  (cadr pair))))))))

;; Procesa recursivamente la cola de una lista de declaraciones
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
;; APLICACIÓN DE PROCEDIMIENTOS
;; Aplica un procedimiento (closure) a una lista de argumentos
;******************************************************************************************
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      ;; Closure regular: extiende el ambiente con los parámetros y evalúa el cuerpo
      (cerradura (ids body env)
        (unless (= (length ids) (length args))
          (eopl:error 'apply-procedure "Aridad incorrecta"))
        (evaluar-expresion body (extend-env ids args env)))
      ;; Closure recursivo: extiende el ambiente incluyendo autorreferencia
      (cerradura-rec (name ids body env)
        (unless (= (length ids) (length args))
          (eopl:error 'apply-procedure "Aridad incorrecta"))
        (let ([self proc])
          (evaluar-expresion body
            (extend-env (cons name ids) (cons self args) env)))))))

;******************************************************************************************
;; PRIMITIVAS DEL LENGUAJE
;; Implementa las operaciones básicas disponibles en el lenguaje
;******************************************************************************************

;; Aplica una primitiva binaria a dos valores ya evaluados
(define aplicar-primitiva-binaria
  (lambda (t-primitiva-binaria exp-1 exp-2)
    (cases primitiva-binaria t-primitiva-binaria
      (primitiva-suma () (+ exp-1 exp-2))
      (primitiva-resta () (- exp-1 exp-2))
      (primitiva-div () (/ exp-1 exp-2))
      (primitiva-multi () (* exp-1 exp-2))
      (primitiva-concat () (string-append exp-1 exp-2)))))

;; Aplica una primitiva unaria a un valor ya evaluado
(define aplicar-primitiva-unaria
  (lambda (t-primitiva-unaria exp-1)
    (cases primitiva-unaria t-primitiva-unaria
      (primitiva-longitud () (string-length exp-1))
      (primitiva-add1 () (+ exp-1 1))
      (primitiva-sub1 () (- exp-1 1)))))

;******************************************************************************************
;; UTILIDADES GENERALES
;; Funciones auxiliares para búsqueda y manipulación de listas
;******************************************************************************************

;; Encuentra la posición de un símbolo en una lista, retorna #f si no se encuentra
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (s) (eqv? s sym)) los)))

;; Busca el primer elemento que satisface un predicado y retorna su posición
(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else (let ([r (list-index pred (cdr ls))])
              (and r (+ r 1)))])))

;******************************************************************************************
;; FUNCIÓN HELPER PARA PRUEBAS
;; Facilita la prueba de expresiones sin usar el REPL interactivo
;******************************************************************************************
(define (interpretar str)
  (evaluar-programa (scan&parse str)))

;******************************************************************************************
;; FIN DEL ARCHIVO
;******************************************************************************************