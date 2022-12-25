(defun find_para(l)(cond 
	((null l) nil)
	((numberp (car l))(find_para (cdr l)))
	((eq (car l) '+)(find_para(cdr l)))
	((eq (car l) '-)(find_para(cdr l)))
	((eq (car l) '=)(find_para(cdr l)))
	(T(car l))
	)
)

(defun right_part(l)(cond
	((not (eq (car l) '=))(right_part(cdr l)))
	(T(cdr l ))
	)
)

(defun left_part(l)(cond
	((eq (car l) '=) nil)
	(T(cons (car l) (left_part(cdr l))))
	)
)

(defun find_var(l para sum_multi sum_res)(cond   ;(L sum_multi=0(still 0,wrong!!!!) sum_res=0)
	((null l)(cons sum_multi (cons sum_res ())))
	
	((eq (car l) '-)(cond;;;-_condition
		((numberp (cadr l))(cond;;;-n_condition
			((eq (caddr l) para)(cond  ;;-nY_condition
				((numberp (cadddr l))(find_var (cddddr l) para (+ sum_multi (* (* -1 (cadr l)) (cadddr l))) sum_res));;;-nYn 
				(T(find_var (cdddr l) para  (- sum_multi (cadr l)) sum_res));;;-nY
			))  
			(T(find_var (cddr l) para sum_multi (- sum_res (cadr l))));;;-n
		))
		(T(find_var(cddr l) para (+ -1 sum_multi) sum_res));;;-Y
	))
	
	((eq (car l) '+)(cond  ;;;+_condition
		((numberp (cadr l))(cond;;;+n_condition
			((eq (caddr l) para)(cond  ;;+nY_condition
				((numberp (cadddr l))(find_var (cddddr l) para (+ sum_multi (* (cadr l) (cadddr l))) sum_res));;;+nYn 
				(T(find_var (cdddr l) para  (+ sum_multi (cadr l)) sum_res));;;+nY
			))  
			(T(find_var (cddr l) para sum_multi (+ sum_res (cadr l))));;;+n
		))
		(T (find_var(cddr l) para (+ 1 sum_multi) sum_res));;;+Y
	))
	
	((eq (car l) para)(cond ;;;Y_condition
		((numberp (cadr l))(find_var (cddr l) para (+ sum_multi (cadr l)) sum_res))  ;;;Yn
		(T(find_var (cdr l) para (+ 1 sum_multi) sum_res ));;;Y
		))
		
	((numberp (car l))(cond  ;;;n_condition
			((eq (cadr l) para)(cond  ;;nY_condition
				((numberp (caddr l))(find_var (cdddr l) para (+ sum_multi (*(car l) (caddr l))) sum_res));;;nYn 	
				(T(find_var (cddr l) para (+ (car l) sum_multi) sum_res));;;ny
			))
			(T (find_var (cdr l) para sum_multi (+ sum_res (car l))));;;n
		))	
	(T(print '(неправильный ввод)))
))

(defun calculate (l1 l2 para)(cond
	(T (append(cons(- (car l1) (car l2)) (cons para ()) )   (cons '= (cons (- (cadr l2) (cadr l1)) ()))))
))

(defun main(l)(cond
		((null l) nil)
		(T(calculate (find_var (left_part l) (find_para l) 0 0)(find_var (right_part l) (find_para l) 0 0) (find_para l)))
	
	))


(print (main '(2 x = 4)))
(print (main '(2 x 4 = 1)))
(print (main '(- 2 x - 2 = 2 x 3 + 7)))
(print (main '(0 = - 3 x + 72)))
(print (main '(- 3 x 3 - 3 = - 4 - 3 x 3 )))

