;Funcion principal
(defun evaluar (expresion &optional amb)
	(if (null expresion) nil
		(if (atom expresion) (procesarAtomo expresion amb)
			(cond
				((equal (car expresion) 'quote) (car (cdr expresion)))
				((equal (car expresion) 'and ) (procesarAnd expresion amb))
				((equal (car expresion) 'or ) (procesarOr expresion amb))
				((equal (car expresion) 'if ) (procesarIf expresion amb))
				((equal (car expresion) 'lambda ) expresion)
				((equal (car expresion) 'while) (procesarWhile (list (car expresion) (car (cdr expresion)) (car (cdr (cdr expresion))) (aplicar (car (car (cdr (cdr (cdr expresion)))))  (cdr (car (cdr (cdr (cdr expresion)))))amb)) amb))
				(T (aplicar (car expresion) (evaluarlista (cdr expresion) amb) amb))
			)
		)
	)
)


;Aplico la funcion a los argumentos 
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
				;Si llegue aca es porque la funcion esta en memoria o no existe.
				(if (equal (procesarAmb funcion amb) 'valor_no_asociado)
					;'funcion_inexistente
					funcion
					(aplicar (procesarAmb funcion amb) args amb) 
				)
			)
		)
		;Si llegue aca es porque estoy procesando una lista 
		(cond
			((equal (car funcion) 'lambda) (evaluar (car (cdr (cdr funcion))) (insertarEnAmb (car (cdr funcion)) args amb)) )
			(T (aplicar (evaluar funcion amb) args amb ))
		)
	)
)


;Evalua todos los elementos de la lista pasada por parametro
(defun evaluarlista (lista amb)
	(if (null lista) nil
		(cons (evaluar (car lista) amb) (evaluarlista (cdr lista) amb))
	)
)


;Devuelve T , un numero o busca la variable en memoria.
(defun procesarAtomo (expresion amb)
	(cond
		((equal expresion T) T)
		((numberp expresion) expresion)
		(T(procesarAmb expresion amb))
	)
)


;Busca la expresion en el ambiente. Si no la encuentra devuelve un Error.
(defun procesarAmb (expresion amb)
	(if (null amb) 'valor_no_asociado
		(if (equal (car amb) expresion) (car (cdr amb))
			(procesarAmb expresion (cdr (cdr amb)))
		)
	)
)


;Ingresa un par clave valor dentro del ambiente. El ambiente es una lista de listas.
(defun insertarEnAmb (clave valor amb)
	(if (null clave) amb
		(append (list (car clave) (car valor)) (insertarEnAmb (cdr clave) (cdr valor) amb))
	)
)


;Devuelve T si ambas expresiones son verdaderas sino devuelve nil
(defun procesarAnd (expresion amb)
	(if (evaluar (car (cdr expresion)) amb) 
			(evaluar (car (cdr (cdr expresion))) amb)
			nil
	)
)


;Devuelve T si alguna expresion es verdadera sino devuelve nil
(defun procesarOr (expresion amb)
	(if (evaluar (car (cdr expresion)) amb)	T
			(evaluar (car (cdr expresion)) amb)
	)
)


;Dependiendo la evaluacion del condicional evaluo el if o el else.
(defun procesarIf (expr amb)
	(if (evaluar (car (cdr expr)) amb)
		(evaluar (car (cdr (cdr expr))) amb)
		(evaluar (car (cdr (cdr (cdr expr)))) amb)
	)
)


(defun procesarWhile (expr amb)
	(if (aplicar (car (cdr expr)) (cdr (cdr (cdr expr))) amb) 
		(procesarWhile (list (car expr) (car (cdr expr)) (car (cdr (cdr expr))) (aplicar (car (cdr (cdr expr))) (cdr (cdr (cdr expr))) amb)) amb)
		(car (cdr (cdr (cdr expr))))
	)
)


;Redefino el mapcar 
(defun my-mapcar (funcion args amb)
	(if (null args) nil
		;Si es una lista o esta en el ambiente entonces evalua, sino aplico la funcion a cada uno de los argumentos
		(if (or (listp funcion) (not (eq (procesarAmb funcion amb) 'valor_no_asociado )))
			(cons (evaluar (cons funcion (list (car args))) amb) (my-mapcar funcion (cdr args) amb))
			(cons (funcall funcion (car args))  (my-mapcar funcion (cdr args) amb))))
)


(defun nana()
(evaluar	'(while 		(lambda (x) (NoCero (car x)))
								(lambda (x) (list (Restar1 (car x)) (* (car x) (car (cdr x)))))
								(car ( (5 1) 8 7)))

				'(NoCero (lambda (x) (not (eq x 0))) Restar1 (lambda (x) (- x 1)))
)
)
