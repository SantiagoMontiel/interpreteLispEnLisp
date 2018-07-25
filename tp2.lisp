(defun insertarEnAmb (clave valor amb)
	(if (null clave) amb
		(append (list (car clave) (car valor)) (insertarEnAmb (cdr clave) (cdr valor) amb))
	)
)

(defun procesarAmb (expresion amb)
	(if (null amb) 'valor_no_asociado
		(if (equal (car amb) expresion) (car (cdr amb))
			(procesarAmb expresion (cdr (cdr amb)))
		)
	)
)

(defun procesarAtomo (expresion amb)
	(cond
		((equal expresion T) T)
		((numberp expresion) expresion)
		(T(procesarAmb expresion amb))
	)
)

(defun procesarAnd (expresion amb)
	(if (evaluar (car (cdr expresion)) amb) 
			(evaluar (car (cdr (cdr expresion))) amb)
			nil
	)
)

(defun procesarOr (expresion amb)
	(if (evaluar (car (cdr expresion)) amb)	T
			(evaluar (car (cdr expresion)) amb)
	)
)

(defun procesarIf (expr amb)
	(if (evaluar (car (cdr expr)) amb)
		(evaluar (car (cdr (cdr expr))) amb)
		(evaluar (car (cdr (cdr (cdr expr)))) amb)
	)
)

(defun procesarCond (condlist amb)
	(if (null condlist) nil
		(if (caar condlist) (evaluar (cadar condlist) amb)
			(procesarCond (cdr condlist) amb)
		)
	)
)


(defun my-mapcar (funcion args amb)
	(if (null args) nil
		;Si es una lista o esta en el ambiente, entonces evalua
		(if (or (listp funcion) (not (eq (procesarAmb funcion amb) 'valor_no_asociado )))
			(cons (evaluar (cons funcion (list (car args))) amb) (my-mapcar funcion (cdr args) amb))
			(cons (funcall funcion (car args))  (my-mapcar funcion (cdr args) amb))))
)

(defun evaluarlista (lista amb)
	(if (null lista) nil
		(cons (evaluar (car lista) amb) (evaluarlista (cdr lista) amb))
	)
)

(defun aplicar (funcion args amb)
	(if (atom funcion)
		(cond
			((equal funcion 'atom)(atom (car args)))
			((equal funcion 'listp)(listp (car args)))
			((equal funcion 'symbolp)(symbolp (car args)))
			((equal funcion 'numberp)(numberp (car args)))
			((equal funcion 'null)(null (car args)))
			((equal funcion 'length)(length (car args)))
			((equal funcion 'car)(car (car args)))
			((equal funcion 'cdr)(cdr (car args)))
			((equal funcion 'list) args)
			((equal funcion 'cons)(cons (car args) (car(cdr args)))) 
			((equal funcion 'not)(not (car args)))
			((equal funcion '+)(+ (car args) (car (cdr args))))
			((equal funcion '-)(- (car args) (car(cdr args))))
			((equal funcion '*)(* (car args) (car (cdr args))))
			((equal funcion '/)(/ (car args) (car (cdr args))))
			((equal funcion 'expt)(expt (car args) (car (cdr args))))
			((equal funcion 'rem)(rem (car args) (car (cdr args))))
			((equal funcion 'sqrt)(sqrt (car args) (car (cdr args))))
			((equal funcion '<)(< (car args) (car (cdr args))))
			((equal funcion '>)(> (car args) (car (cdr args))))
			((equal funcion '>=)(>= (car args) (car (cdr args))))
			((equal funcion '>=)(>= (car args) (car (cdr args))))
			((equal funcion '=)(= (car args) (car (cdr args))))
			((equal funcion 'eq)(eq (car args) (car (cdr args))))
			((equal funcion 'equal)(equal (car args) (car (cdr args))))
			((equal funcion 'mapcar)  (my-mapcar (car args) (car (cdr args)) amb))
			((equal funcion 'apply) (apply (car args) (car (cdr args))))
			((equal funcion 'funcall) (apply (car args) (cdr args)))
			(T
				(if (equal (procesarAmb funcion amb) 'valor_no_asociado)
					'funcion_inexistente
					(aplicar (procesarAmb funcion amb) args amb) 
				)
			)
		)
		(cond
			((equal (car funcion) 'lambda) (evaluar (car (cdr (cdr funcion))) (insertarEnAmb (car (cdr funcion)) args amb)) )
			(T (aplicar (evaluar funcion amb) args amb ))
		)
	)
)

(defun evaluar (expresion &optional amb)
	(if (null expresion) nil
		(if (atom expresion) (procesarAtomo expresion amb)
			(cond
				((equal (car expresion) 'quote) (car (cdr expresion)))
				((equal (car expresion) 'and ) (procesarAnd expresion amb))
				((equal (car expresion) 'or ) (procesarOr expresion amb))
				((equal (car expresion) 'if ) (procesarIf expresion amb))
				((equal (car expresion) 'cond ) (procesarCond (cdr expresion) amb))
				((equal (car expresion) 'lambda ) expresion)
				(T (aplicar (car expresion) (evaluarlista (cdr expresion) amb) amb))
			)
		)
	)
)

; funcion para comparar el resultado de la evaluacion del interprete del tp
; contra la evaluacion que realiza el interprete real mediante la funcion eval
;(defun testexpr (expr)
;	(if (equal (eval expr) (evaluar expr nil)) T
;		(list expr (eval expr) (evaluar expr nil))
;	)
;)

; tests
;(testexpr nil)
;(testexpr '(quote ( a b c)) )
;(testexpr '(and t t) )
;(testexpr '(or t t))
;(testexpr '(or nil t))
;(testexpr '(or t nil))
;(testexpr '(or nil nil))
;(testexpr '(if nil 'verdadero 'falso) )
;(testexpr '(if t 'verdadero 'falso) )
;(testexpr '(car (quote (1 2 3))) )
;(testexpr '(cdr (quote (1 2 3))) )
;(testexpr '(cons 1 (quote (2 3 4))) )
;(testexpr '((lambda (x) (+ x 1)) 1) )
;(testexpr '(cond (nil 'cond1) (nil 'cond2) ) )
;(testexpr '(mapcar 'atom (quote ((1) 2 a))) )
;(testexpr '(apply 'cons (quote (a (1 2))) ))
;(testexpr '(funcall 'cons 1 (quote (a b)) ))
;(testexpr '(atom 1))
;(testexpr '(listp (quote (1 2 3)) ))
;(testexpr '(atom (quote (1 2 3)) ))
;(testexpr '(listp 1 ))
;(testexpr '(length (quote (1 2 3))))
;(testexpr '(null nil ))
;(testexpr '(list 1 2 3 ))

;ejemplos de corridas varios

;(evaluar '(quote ((2 3) (4 5))) )
;(evaluar '((lambda (x) (* x 2)) 2) nil )
;(evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil)
;(evaluar '(lambda (x) (* x 2)) nil)
;(evaluar '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3)) nil)
;(evaluar '(mapcar 'numberp (quote (4))) '(t))
;(evaluar '(mapcar 'numberp (quote (4 5 6 nil))))
;(evaluar '(mapcar 'car (quote ( (2 3) (4 5)))))
;(evaluar '(fact 5) '(fact (lambda(n)(if(eq n 0) 1 (* n (fact (- n 1))))) ) )
;(evaluar '(mapcar 'fact (quote ( 2 3 4 5 ) )) '(fact (lambda(n)(if(eq n 0) 1 (* n (fact (- n 1)))))) )
