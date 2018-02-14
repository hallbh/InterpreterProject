;; Adam C. Foltzer
;; pc2j ParentheC to Java converter

;; For starters, try:
;; > (load "pc2j.scm")
;; > (pc2j "interp.pc")

;; If you have javac and java in your PATH:
;; > (compile/run "interp.pc")

;;; pmatch code written by Oleg Kiselyov
;; (http://pobox.com/~oleg/ftp/)
;;;
;;; Taken from leanTAP.scm
;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.

; (pmatch exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- comparison with exp (using equal?)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

; Modified by Adam C. Foltzer for Chez 8 compatibility

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch v cs ...)))
    ((_ v) (errorf 'pmatch "failed: ~s" v))
    ((_ v (else e0 e ...)) (begin e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (uscore quote unquote)
    ((_ v uscore kt kf)
     ; _ can't be listed in literals list in R6RS Scheme
     (and (identifier? #'uscore) (free-identifier=? #'uscore #'_))
     kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))


;; From the compiler class helpers; handy for building error msgs

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k name defn ... expr)
       (with-syntax ([who (datum->syntax-object #'k 'who)])
         #'(define name
             (let ([who 'name])
               defn ...
               expr)))])))

;; Reads a file containing Scheme data as a list
(define read-file-as-list
  (lambda (filename)
    (letrec ([read-loop
              (lambda (dat)
                (cond
                  [(eof-object? dat) '()]
                  [else (cons dat (read-loop (read)))]))])
      (with-input-from-file
        filename
        (lambda () (read-loop (read)))))))

;; Prefix for all generated Java identifiers to avoid keyword collisions
(define java-identifier-prefix "_")

;; Produces a safe identifier string from a Scheme symbol, string, or integer
(define-who safe
  (define char->safe
    (lambda (char)
      (case char
        [(#\?) "$q"]
        [(#\!) "$b"]
        [(#\.) "$d"]
        [(#\+) "$p"]
        [(#\-) "$_"]
        [(#\_) "_"]
        [(#\*) "$t"]
        [(#\/) "$di"]
        [(#\<) "$lt"]
        [(#\>) "$gt"]
        [(#\:) "$co"]
        [(#\%) "$pe"]
        [(#\^) "$ex"]
        [(#\&) "$am"]
        [(#\~) "$ti"]
        [else (string char)])))
  (lambda (x)
    (cond
      [(symbol? x)
       (apply
         string-append
         java-identifier-prefix
         (map char->safe
              (string->list (symbol->string x))))]
      [(integer? x) (format "~d" x)]
      [(string? x) (safe (string->symbol x))]
      [(eq? (car x) 'quote) (safe (cadr x))]
      [else (errorf who "can only handle symbols, strings, and integers")])))

;; Gets all of the labels, excluding the special label main
(define labels
  (lambda (exp*)
    (pmatch exp*
      [() '()]
      [((define-label ,name . ,body) . ,exp*)
       (guard (not (eq? name 'main)))
       `(,name . ,(labels exp*))]
      [(,exp . ,exp*) (labels exp*)])))

;; Makes the declarations for the label constants, given a list of labels
(define label-declarations
  (lambda (label*)
    (let loop ([label* label*] [i 0])
      (cond
        [(null? label*) ""]
        [else (let* ([label (safe (car label*))]
                     [decl (format "static final int ~a = ~d;\n" label i)])
                (string-append decl (loop (cdr label*) (add1 i))))]))))
      

;; Makes a list of all the registers defined at the top level
(define registers
  (lambda (exp*)
    (pmatch exp*
      [() '()]
      [((define-registers . ,reg*) . ,exp*)
       (append reg* (registers exp*))] ;; support more than one def?
      [(,exp . ,exp*) (registers exp*)])))

;; Makes the declaration of the registers, warning if none exist
(define register-declarations
  (lambda (reg*)
    (cond
      [(null? reg*)
       (warningf who "no registers defined")
       ""]
      [else (object-declarations reg*)]))) 

;; Declares a list of variables as Objects
(define object-declarations
  (lambda (var*)
    (cond
      [(null? var*) ""]
      [else
       (apply string-append
              "Object "
              (let loop ([var* var*])
                (cond
                  [(null? var*) '()]
                  [(null? (cdr var*))
                   `(,(format "~a;\n" (safe (car var*))))]
                  [else (cons
                         (format "~a, " (safe (car var*)))
                         (loop (cdr var*)))])))])))

;; Gets the program counter, and errors out if there is not exactly one
(define program-counter
  (lambda (exp*)
    (letrec ([loop
              (lambda (exp*)
                (pmatch exp*
                  [() #f]
                  [((define-program-counter ,pc) . ,exp*)
                   (and (not (loop exp*)) pc)]
                  [(,exp . ,exp*) (loop exp*)]))])
      (let ([pc (loop exp*)])
        (when (not pc)
          (errorf 'pc2j "exactly one program counter must be defined"))
        pc))))

;; Makes the program counter declaration string
(define program-counter-declaration
  (lambda (pc)
    (format "int ~a;\n" (safe pc))))

;; Makes a list of all define-union expressions at the top
;; level of the given expression
(define unions
  (lambda (exp*)
    (pmatch exp*
      [() '()]
      [((define-union . ,def) . ,exp*)
       `((define-union . ,def) . ,(unions exp*))]
      [(,exp . ,exp*) (unions exp*)])))

;; Generates a sequence of assignments, usually to map arguments to
;; fields in a constructor or to map fields to local variables. Takes
;; a *safe* string prefix for each side, a list of corresponding
;; variables for each side. The variable lists must each be the same
;; length.

;; Example:
;; > (printf (generate-assignments "this." '(body env) "" '(body env)))
;; this._body = _body;
;; this._env = _env;
;; > (printf (generate-assignments "Object " '(body env) "((clos_closure) _p$t)." '(body env)))
;; Object _body = ((clos_closure) _p$t)._body;
;; Object _env = ((clos_closure) _p$t)._env;

(define-who generate-assignments
  (lambda (lhs-prefix lhs-var* rhs-prefix rhs-var*)
    (pmatch `(,lhs-var* ,rhs-var*)
      [(() ()) ""]
      [(() ,x) (errorf who "variable lists must be the same length")]
      [(,x ()) (errorf who "variable lists must be the same length")]
      [((,lhs . ,lhs*) (,rhs . ,rhs*))
       (string-append
         (format "~a~a = ~a~a;\n"
                 lhs-prefix
                 (safe lhs)
                 rhs-prefix
                 (safe rhs))
         (generate-assignments lhs-prefix lhs* rhs-prefix rhs*))])))
      

;; Makes the declarations for a single union type Most of the work is
;; done by helpers: variant-declaration handles each class
;; declaration, and calls off to generate-constructor for help
(define-who union-declaration
  (define variant-declaration
    (lambda (variant type)
      (pmatch variant
        [(,tag . ,field*)
         (let* ([name (safe
                       (string-append (symbol->string type)
                                      "_"
                                      (symbol->string tag)))]
                [field-decls (object-declarations field*)]
                [constructor (generate-constructor name field*)])
           (format "static class ~a {\n~a\n~a\n}\n\n"
                   name
                   field-decls
                   constructor))])))
  (define generate-constructor
    (lambda (name field*)
      (cond
        [(null? field*) ""]
        [else
         (let ([arg* (let loop ([field* field*])
                       (cond
                         [(null? field*) ""]
                         [(null? (cdr field*))
                          (format "Object ~a" (safe (car field*)))]
                         [else (string-append
                                 (format "Object ~a, "
                                         (safe (car field*)))
                                 (loop (cdr field*)))]))]
               [assign* (generate-assignments "this." field* "" field*)])
           (format "~a(~a) {\n~a}"
                name
                arg*
                assign*))])))
  (lambda (union)
    (pmatch union
      [(define-union ,type . ,variants)
       (apply string-append
              (map (lambda (v) (variant-declaration v type)) variants))])))

;; Builds a complete variant name from the type and the tag
(define build-type-name
  (lambda (type tag)
    (string->symbol
      (format "~a_~a" type tag))))

;; Makes a list of all the union types based on output from unions
(define-who union-types
  (lambda (union*)
    (pmatch union*
      [() '()]
      [((define-union ,type . ,variants) . ,union*)
       (let* ([variants (map car variants)]
              [types (map (lambda (v)
                            (build-type-name type v))
                          variants)])
         (append types (union-types union*)))]
      [,x (errorf who "Input not a list of define-union expressions: ~s" x)])))

;; Returns a list of fields for a particular type/variant combo
(define-who union-variant-fields
  (lambda (type tag union*)
    (pmatch union*
      [() (errorf who "invalid type ~a" type)]
      [((define-union ,t . ,variants) . ,union*)
       (guard (eq? type t))
       (cond
         [(assq tag variants) => cdr]
         [else (errorf who "invalid tag ~a for type ~a" tag type)])]
      [(,union . ,union*) (union-variant-fields type tag union*)])))

;; label-exprs returns a list of all the non-main define-label expressions
(define label-exprs
  (lambda (expr*)
    (pmatch expr*
      [() '()]
      [((define-label ,label . ,body) . ,expr*)
       (guard (not (eq? label 'main)))
       `((define-label ,label . ,body) . ,(label-exprs expr*))]
      [(,expr . ,expr*) (label-exprs expr*)])))

;; Returns the expressions in the main label
(define main-body
  (lambda (expr*)
    (letrec ([loop
              (lambda (expr*)
                (pmatch expr*
                  [() #f]
                  [((define-label main . ,body) . ,expr*)                   
                   (and (not (loop expr*)) body)]
                  [(,expr . ,expr*) (loop expr*)]))])
      (let ([body (loop expr*)])
        (when (not body)
          (errorf 'pc2j "exactly one main label must be defined"))
        body))))

;; generate-body takes a non-main define-label form and generates Java
;; code for its body to be used in the trampoline's switch
(define generate-label-body
  (lambda (label-expr types)
    (pmatch label-expr
      [(define-label ,label . ,body)
       (format "case ~a:\n~a\nbreak;\n"
               (safe label)
               (apply string-append (map (generate-statement types) body)))])))

;; generate-statement implements individual expressions, including
;; primitives, control flow, union type constructors, and more. Needs
;; access to names of union types in order to call their constructors
;; where appropriate.
(define-who generate-statement
  (lambda (types)
    (define type-names (union-types types))
    (define constructor?
      (lambda (sym)
        (memq sym type-names)))
    ;; only handles single-character escapes like ~a
    (define convert-format-string
      (lambda (fmt)
        (let ([fmt
               (let loop ([fmt (string->list fmt)])
                 (cond
                   [(null? fmt) '()]
                   [(and (eq? (car fmt) #\~)
                         (not (null? (cdr fmt)))
                         (char-alphabetic? (cadr fmt)))
                    `(#\% #\s . ,(loop (cddr fmt)))]
                   [else (cons (car fmt) (loop (cdr fmt)))]))])
          (list->string fmt))))
    ;; adds commas and safeifies a list of arguments
    (define arg*->safe
      (lambda (arg*)
        (cond
          [(null? arg*) ""]
          [(null? (cdr arg*)) (safe (car arg*))]
          [else (format "~a, ~a"
                        (safe (car arg*))
                        (arg*->safe (cdr arg*)))])))
    (define generate-arg*
      (lambda (arg*)
        (cond
          [(null? arg*) ""]
          [(null? (cdr arg*)) ((generate-statement types) (car arg*))]
          [else (format "~a, ~a"
                        ((generate-statement types) (car arg*))
                        (generate-arg* (cdr arg*)))])))
    ;; generates the if-then-else chain for union-case statements
    (define generate-union-case
      (lambda (reg type case*)
        (pmatch case*
          [() (warningf who "empty union-case converted to noop") ""]
          [(((,tag . ,field*) . ,body))
           (format "if (~a instanceof ~a) {\n~a~a\n} else { throw new Exception((new Formatter()).format(\"Error in union-case: could not match %s against type ~a\", ~a.getClass().getName()).out().toString());\n}\n"
                   (safe reg)
                   (safe (build-type-name type tag))
                   (generate-assignments
                     "Object "
                     field*
                     (format "((~a) ~a)."
                             (safe (build-type-name type tag))
                             (safe reg))
                     (union-variant-fields type tag types))
                   (apply string-append
                          (map (generate-statement types) body))
                   (safe type)
                   (safe reg))]
          [(((,tag . ,field*) . ,body) . ,case*)
           (let ([this (format "if (~a instanceof ~a) {\n~a~a\n} else "
                   (safe reg)
                   (safe (build-type-name type tag))
                   (generate-assignments
                     "Object "
                     field*
                     (format "((~a) ~a)."
                             (safe (build-type-name type tag))
                             (safe reg))
                     (union-variant-fields type tag types))
                   (apply string-append
                          (map (generate-statement types) body)))])
             (string-append this (generate-union-case reg type case*)))])))    
    (lambda (expr)
      (let ([gs (generate-statement types)])
        (pmatch expr
          [,x (guard (symbol? x)) (safe x)]
          [,n (guard (integer? n))
           (format "Integer.valueOf(~a)" n)]
          [#t "Boolean.valueOf(true)"]
          [#f "Boolean.valueOf(false)"]          
          [(void) "null"]
          [(,constructor . ,arg*) (guard (constructor? constructor))
           (format "new ~a(~a)" (safe constructor) (generate-arg* arg*))]
          [(begin . ,expr*)
           (apply string-append (map gs expr*))]
          [(if ,test ,conseq, altern)
           (format "if ((Boolean)~a) {\n~a } else {\n~a }\n"
                   (gs test)
                   (gs conseq)
                   (gs altern))]
          [(set! ,var ,expr)
           (format "~a = ~a;\n" (safe var) (gs expr))]
          [(union-case ,reg ,type . ,case*)
           (generate-union-case reg type case*)]
          [(mount-trampoline ,constructor ,k-reg ,pc)
           (format "~a\ntrampoline();\n"
                   (gs `(set! ,k-reg (,constructor (void)))))]
          [(dismount-trampoline ,dismount)
           (format "break ~a;\n" trampoline-label)]
          [(error . ,rest)
           (gs `(errorf . ,rest))]
          [(errorf ,who ,fmt . ,arg*)
           (format "throw new Exception((new Formatter()).format(\"Error in ~a: ~a\"~a).out().toString());\n"
                   (safe who)
                   (convert-format-string fmt)
                   (if (null? arg*)
                       ""
                       (string-append ", " (arg*->safe arg*))))]
          [(printf ,fmt . ,arg*)
           (format "System.out.printf(~s~a);"
                   (convert-format-string fmt)
                   (if (null? arg*)
                       ""
                       (string-append ", " (arg*->safe arg*))))]
          [(and ,a ,b)
           (format "((Boolean)~a && (Boolean)~a)" (gs a) (gs b))]
          [(or ,a ,b)
           (format "((Boolean)~a || (Boolean)~a)" (gs a) (gs b))]
          [(not ,a)
           (format "(! (Boolean)~a)" (gs a))]          
          [(add1 ,n)
           (format "((Integer)~a + 1)" (gs n))]          
          [(sub1 ,n)
           (format "((Integer)~a - 1)" (gs n))]          
          [(zero? ,n)
           (format "((Integer)~a == 0)" (gs n))]
          [(random ,n)
           (format "(new java.util.Random()).nextInt(~a)" (gs n))]
          [(,op ,n1 ,n2)
           (guard (memq op '(< > <= >= + * - /)))
           (format "((Integer)~a ~a (Integer)~a)" (gs n1) op (gs n2))]
          [,x (errorf who "invalid expression ~a" x)])))));

;; Emits the program, the main portion of which is a labeled
;; while(true) infinite loop. The loop switches on the contents of the
;; program counter, and the cases are provided by generate-body of
;; each non-main label.

(define trampoline-label "tramp")

(define-who emit-program
  (lambda (name
           label-decl*
           register-decl*
           pc-decl
           pc
           union-decl*
           label-body*
           main)
    (printf
     "import java.util.Formatter;

public class ~a {
//labels
~a
//registers
~a
//program counter
~a
//union types
~a
//trampoline
void trampoline() throws Exception {
~a: while(true) {
switch (~a) {
~a
default: throw new Exception(\"Invalid label \" + ~a);
}
}
}
//run corresponds to main label from ParentheC
public void run() throws Exception {
~a
}\n
public static void main(String[] args) throws Exception {
new ~a().run();\n}\n}\n"
     name
     label-decl*
     register-decl*
     pc-decl
     union-decl*
     trampoline-label
     pc
     label-body*
     pc
     main
     name)))

;; Gets the program name from the file input. This amounts to
;; stripping the file extension off the file, and then safeifying it,
;; i.e.:
;; > (safe-program-name "~/c311/interp.pc")
;; "_interp"

(define safe-program-name
  (lambda (filename)
    (safe (string->symbol (path-root (path-last filename))))))


;; Output conversion to current-output-port
(define pc2j*
  (lambda (source-file)
    (let ([source (read-file-as-list source-file)])
      (emit-program
        (safe-program-name source-file)
        (label-declarations (labels source))
        (register-declarations (registers source))
        (program-counter-declaration (program-counter source))
        (safe (program-counter source))
        (apply string-append (map union-declaration (unions source)))
        (apply string-append
               (map (lambda (label)
                      (generate-label-body
                        label
                        (unions source)))
                    (label-exprs source)))
        (apply string-append
               (map (generate-statement (unions source))
                    (main-body source)))))))

;; Output conversion to <safe-program-name>.java
(define pc2j
  (lambda (source-file)
    (let ([output-file (format "~a.java" (safe-program-name source-file))])
      (delete-file output-file)
      (with-output-to-file output-file (lambda () (pc2j* source-file)))
      (printf "pc2j: generated ~a\n" output-file)
      (flush-output-port))))

;; Converts, compiles, and runs, with output collected back to Scheme
(define-who compile/run
  (lambda (source-file)
    (let* ([tempdir (gensym->unique-string (gensym "temp"))]
           [compile-command
            (format "javac -d ~a ~a.java"
                    tempdir
                    (safe-program-name source-file))])
      (pc2j source-file)
      (mkdir tempdir)
      (let ([compile-value (system compile-command)])
        (unless (zero? compile-value)
          (errorf who
                  "compiler command failed:\nInvoked: ~a\n Returned:\n~a"
                  compile-command
                  compile-value))
        (system (format "java -cp ~a ~a"
                        tempdir
                        (safe-program-name source-file)))
        (for-each
          (lambda (f)
            (let ([filename (format "~a~a~a"
                     tempdir
                     (directory-separator)
                     f)])
              (delete-file filename)))
          (directory-list tempdir))
        (delete-directory tempdir)
        (void)))))
