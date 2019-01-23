(defvar keywords 	'("and"     "not"     "set"    "if"   
					"append"  "concat"  "equal"  
					"deffun"  "for"     "while"  "exit"
					"true"    "false"   "or" "defvar" "null"))

(setq operators '("+" "-" "/" "*" "**" "'"))

(defvar EXPB '("and" "or" "not" "equal" "true" "false"))

(defvar EXPI '("set"  "+" "-" "/" "*" "(" ")" "**" "if"   
					"deffun"  "for"  "defvar"   "while"  "exit"))
(defvar EXPLISTI '("concat" "append" "null" "'(" "'()"))

(setq indent 0)

(setq idListCount 0)

(setq parseList '())


(defun write_to_file (stream)
	"liste halinde sıralanmış bir grup parse edilmiş elemanları dosyaya yazan fonksiyon"
	(cond
		((null parseList) nil)
		((equal (car parseList) nil) nil)
		((equal (car parseList) "(") ; ilk eleman ( ise tab sayısını arttırarak yazacağımız için bu duruma özel şart yazdım.
			(incf indent)
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab) ; çoklu şekilde print yapabilmek için yazılan format komutu (arka arkaya verilen sayı kadar tab yazıyor)
	   		(format stream "(") ; gerekli sayıda  tab yapıldıktan sonra ( dosyaya yazılır.
	   		(terpri stream)
	   		(setq parseList (cdr parseList)) 
	   			; recursive şekilde parseList gezilir. Parametre olarak verildiğinde nedense nil olarak okuyordu.
				; Ben de çözüm olarak global yaptım ve recursive de azaltarak base case i sağladım.
	   		(write_to_file stream) ; recursive çağrı
		)
		((equal (car parseList) ")") ; ) görünce de  dosyaya  ) yazıp tab sayısını azaltıyorum.
			(format stream "~v@{~A~:*~}" indent #\tab)
	   		(format stream ")")
	   		(terpri stream)
	   		(decf indent)
	   		(setq idListCount 0)
	   		(setq parseList (cdr parseList))
	   		(write_to_file stream)
		)
		((equal (car parseList) "IDLIST")
			(cond
				((equal (car (cdr parseList)) "(")  ; IDLIST den sonra ( geldiği durumda  IDLIST -> (IDLIST)  kuralı için kontrol
					(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
			   		(format stream "IDLIST")
			   		(terpri stream)
			   		(setq parseList (cdr parseList))
	   				(write_to_file stream)
				)
				(t ; diğer durumlarda içiçe ID ler basılacağı için her seferinde ID lere özel tab arttırması yapılacak. 
					;Bu yüzden idListCount diye ayrı bir count tutuldu.
					(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
			   		(format stream "IDLIST")
			   		(terpri stream)
			   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
			   		(format stream "ID")
			   		(terpri stream)
			   		(format stream "~v@{~A~:*~}" (+ indent idListCount 2) #\tab)
			   		(format stream (car (cdr (cdr parseList))))
			   		(terpri stream)
			   		(incf idListCount)
			   		(setq parseList (cdr (cdr (cdr parseList))))
	   				(write_to_file stream)
				)
			)
		)
		((equal (car parseList) "ID")  ; ID görünce gerekli tab sayısı kadar +  içiçe olan ID ler için belirlenen tab sayısı kadar tab bırakılarak dosyaya yazılır.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "ID")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
	   		(format stream (car (cdr parseList)))
	   		(terpri stream)
	   		(setq parseList (cdr (cdr parseList)))
			(write_to_file stream)
		)
		((equal (car parseList) "null")
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "null")
	   		(terpri stream)
	   		(setq parseList (cdr parseList))
	   		(write_to_file stream)
		)
		((equal (car parseList) "VALUES") ; sayı görünce  içiçe tablar yaparak sayı değerini dosyaya yazar.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream "VALUES")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 1) #\tab)
	   		(format stream "IntegerValue")
	   		(terpri stream)
	   		(format stream "~v@{~A~:*~}" (+ indent idListCount 2) #\tab)
	   		(format stream (car (cdr (cdr parseList))))
	   		(terpri stream)
	   		(setq parseList (cdr (cdr (cdr parseList))))
	   		(write_to_file stream)
		)
		(t  ; diğer durumlar için o anki tab sayısı kadar tab yapılıp  yazma işlemi yapılır.
			(format stream "~v@{~A~:*~}" (+ indent idListCount) #\tab)
	   		(format stream (car parseList))
	   		(terpri stream)
	   		(setq parseList (cdr parseList))
	   		(write_to_file stream)
		)
	)
)


(defun parserHelper (tokens stream)
	" recursive olarak tüm tokenleri sırayla gezerek parseList i doldurur ve gerekli durumlarda dosyaya da recursive olarak yazması için write_to_file fonksiyonunu çağırır."
	(cond ((null tokens) nil)
		(t (setq token (car tokens))) ; işlem kolaylığı açısından o anki token token değişkeninde tutuldu.
	)

	(cond 
		((null tokens) nil)
		((null token) nil)
		((equal (car token) "operator") ; token in tipine göre şartlar kontrol edilir ve gerekli rule için işlemler yapılır.
			(cond
				((not (equal (find (car (cdr token)) operators :test #'string=) nil)) ; ( veya ) haricindeki operatörler'den biri ise EXPI dir.
					(setq parseList (cons "EXPI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((equal (car (cdr token)) "(") ; ( görüldüğünde o ana kadar yapılmış parse işlemini dosyaya yazar. 
											;	Çünkü parse tree'de ( den sonra başka bir rule a geçilir ve 1 tab içeri girilir.
					(write_to_file stream)
					(setq parseList '("("))
				)
				((equal (car (cdr token)) ")") ; ) görüldüğünde de o operasyon biter ve bir tab geri gidilir. Bu yüzden parseList boşaltılmak için dosyaya yazılır ve sıfırlanır.
					(setq parseList (append parseList (list ")")))
					(write_to_file stream)
					(setq parseList '())
				)
			)
			(parserHelper (cdr tokens) stream)
		)
		((equal (car token) "keyword")
			(cond
				((not (equal (find (car (cdr token)) EXPI :test #'string=) nil)) ; gelen keyword EXPI EXPB veya EXPLISTI keyword lerinden herhangi biri olabilir. Kontrol ediyoruz.
					(setq parseList (cons "EXPI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((not (equal (find (car (cdr token)) EXPB :test #'string=) nil))
					(setq parseList (cons "EXPB" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
				((not (equal (find (car (cdr token)) EXPLISTI :test #'string=) nil))
					(setq parseList (cons "EXPLISTI" parseList))
					(setq parseList (append parseList (list (car (cdr token)))))
				)
			)
			(parserHelper (cdr tokens) stream)
		)
		((equal (car token) "identifier")
			(cond
				((and (equal (car parseList) "(") (equal (car (car (cdr tokens))) "identifier")) 
				; ID den önce ( varsa ve sonra da ID geliyorsa IDLIST 'dir yani (x y) gibi devam etmektedir.
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("IDLIST") '("ID") (list (car (cdr token)))))
					(write_to_file stream)
					(setq parseList '())
				)
				((or (equal (car (car (cdr tokens))) "identifier") (> idListCount 0))
					; bir sonraki  token de ID ise veya bir IDLIST 'in içinde isek bu şarta girer. (x y z t)
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("ID") (list (car (cdr token)))))
					(write_to_file stream)
					(setq parseList '())
				)

				((and (equal (car parseList) "(") (equal (car (cdr (car (cdr tokens)))) ")")) ; tek ID li bir IDLIST ise -> (x)
					(setq parseList (cons "IDLIST" parseList))
					(setq parseList (append parseList '("IDLIST") '("ID") (list (car (cdr token)))))
				)
				(t
					(setq parseList (append parseList '("ID") (list (car (cdr token)))))
				)
			)
			(parserHelper (cdr tokens) stream)
		)
		((equal (car token) "integer") ; sayı ise
			(setq parseList (append parseList '("VALUES") '("IntegerValue") (list (car (cdr token)))))
			(parserHelper (cdr tokens) stream)
		)
		(t (parserHelper (cdr tokens) stream))
	)
)

(defun parser (tokenList)
	(with-open-file (stream "141044050.tree" :direction :output :if-does-not-exist :create :if-exists :supersede)
   		(format stream "; DIRECTIVE: parse tree")
   		(terpri stream)

   		(format stream "START")
   		(terpri stream)
   		(incf indent)
   		(format stream "~v@{~A~:*~}" indent #\tab)
   		(format stream "INPUT")
   		(terpri stream)
   		(incf indent)
   		(parserHelper tokenList stream)
	)
	(setq idListCount 0)
	(setq indent 0)
	(setq parseList '())
)

(parser '(("operator" "(") ("keyword" "deffun") ("identifier" "sumup") ("operator" "(") ("identifier" "x") ("operator" ")") ("operator" "(") ("keyword" "if") ("operator" "(") ("keyword" "equal") ("identifier" "x") ("integer" "0") ("operator" ")") ("integer" "1") ("operator" "(") ("operator" "+") ("identifier" "x") ("operator" "(") ("identifier" "sumup") ("operator" "(") ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")")))
