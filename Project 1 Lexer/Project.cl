(defvar keywords 	'("and"     "not"     "set"    "if"   
					"append"  "concat"  "equal"  
					"deffun"  "for"     "while"  "exit"
					"true"    "false"   "or"))

(setq operators '("+" "-" "/" "*" "(" ")" "**"))


(defun readFile (str)
	"str ile gelen dosyayı açar ve okuyup liste halinde geri döndürür."
	(setq tokenList '())
	(let ((in (open str :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
	      	while line do (setq tokenList (cons line tokenList)))
	      (close in)
	   )
	)
	(reverse tokenList)
)

(defun is_digit (chr)
	"Gelen karakterin rakam olup olmadığını kontrol edip t veya nil döndürür."
	(setq ascii (char-code chr))
	(setq isDigit nil)
	(if (and (> ascii 47) (< ascii 58))
		(setq isDigit t))
	isDigit
)

(defun is_alpha (chr)
	"Gelen karakterin harf olup olmadığını kontrol edip t veya nil döndürür."
	(setq ascii (char-code chr))	
	(setq isAlpha nil)

	(if (or (and (> ascii 64) (< ascii 91)) (and (> ascii 96) (< ascii 123)))
		(setq isAlpha t))
	isAlpha
)

(defun combineList (nested) 
 "içiçe (1 (2 3 (4))) gibi olan listeleri tek liste haline getiren fonksiyon."
	(cond
		((null nested) nil)
		((listp (car nested)) (append (combineList (car nested)) (combineList (cdr nested))))
		(t (cons (car nested) (combineList (cdr nested))))
	)
)

(defun getWord (lst)  
	"lst ile gelen satır stringinde whitespace veya ( veya ) karakterleri görene kadar olan string i döndürür."
	(cond 
		((null lst) nil)

	((not (equal (find  (car lst) '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout #\( #\) ) :test #'char=) nil))
		nil)
		(t (append (list (car lst)) (getWord (cdr lst))))
	)
)


(defun tokenizer (line)
	"gelen satır stringindeki tokenleri tek tek ayrıştırıp liste halinde geri döndürür."
	(setq line (coerce line 'list))
	(cond 
		((null line)
			nil)
		(t 	(setq word (getWord line)) ; whitespace karakterlere veya ( ) operatörlerine kadar olan string i recursive olarak bulup word e  atıyoruz.
			(setq wordSize (list-length word))  ; word un size ını alıyoruz ki token lara eklersek size kadar satırda ilerleyerek devam edelim.
			(setq word (coerce word 'string)) ; word char listesi olarak geliyor ve burda string e çeviriyoruz.
			;(terpri)
			;(write word) (write "  size = ") (write wordSize)
			;(terpri)
		)
	)
	(cond 
		((null line) nil)
		((equal (car line) nil) nil)
		((not (equal (find (string (car line)) operators :test #'string=) nil))
			(cond 
				((and (equal (car line) #\*) (equal (car (cdr line)) #\*))
	; * ve ** operatorlerini kontrol ederek recursive call yapıyoruz. Çünkü harf harf baktığımızdan dolayı bu kontrolü
	; yapmassak eğer ** olduğu durumda ilk * ı bir operatör ve 2. * ı bir operatör olarak alacaktı. Burda bunu engelliyoruz.
			 		(append '("**") (list (tokenizer (cdr (cdr line))))))
				((and (equal (car line) #\-) (> wordSize 1))
; Negatif sayıları Integer olarak alabilmek ve integer olması için gerekli regular expression u sağlaması için gerekli if kontrolü
					(if (equal (every #'is_digit (string-left-trim "-" word)) t) ; - den sonra gelenler tamamen sayı olmalı harf içermemeli
						(append (list word) (list (tokenizer (nthcdr wordSize line))))
						(append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))) 
					)
				)
				((> wordSize 1)  ; *ab /tmp gibi identifier tanımlamalarına izin vermemek için kontrol yapıyoruz.
					(append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line)))))
			 	(t (append (list (string (car line))) (list (tokenizer (cdr line)))))
			)
		)
		((equal wordSize 0)  ; tab newline bosluk gibi karakterler olduğunda size 0  hesaplanacaktır ve atlayarak recursive call lara devam ediyoruz.
		; ( ve ) operatörlerinde de  size 0 olacaktır fakat yukarıdaki if kontrolüne gireceği için buraya girmeyecektir.
			(tokenizer (cdr line))
		)
		( (equal (every #'is_alpha word) t)   ; kelimenin her karakteri harf ise identifier olabilir. Aksi halde olamaz.
			(append (list word) (list (tokenizer (nthcdr wordSize line)))))
		( (equal (every #'is_digit word) t)  ; her karakter rakam ise integer sayi olabilir. (Negatif sayı kontrolü operatör condition unda yapılıyor.)
			(append (list word) (list (tokenizer (nthcdr wordSize line)))))
		((and (equal wordSize 1) (or (is_alpha (coerce word 'character)) (is_digit (coerce word 'character))))
		; x veya 5 gibi tek harfli sayı veya identifier ları ekliyoruz. Bu kontrolü ? : gibi identifier tanımlanmasını engellemek için yapıyoruz.
			(append (list (string (car line))) (list (tokenizer (cdr line))))
		)
		((not (equal (find word keywords :test #'string=) nil))
	; bosluga kadar okunan kelime keyword lerden biriyse listeye ekliyoruz.
			(append (list word) (list (tokenizer (nthcdr wordSize line))))
		)
		; Hatalı durumlarda token listesine hatalı olduğu belirtilerek eklenip devam ediliyor.
		(t (append (list (format nil "~A  [Syntax Error !]" word)) (list (tokenizer (nthcdr wordSize line))))) 
	)
)

(defun lexer (filename)
	"lexer işlemlerini yapar.Dosyayı okuyup listeyi alır ve satır satır tokenizer fonksiyonuna göndererek token listesini oluşturur."
	(setq list_from_file (readFile filename))
	(setq size (list-length list_from_file))

	(setq tokenList (combineList (mapcar #'tokenizer list_from_file)))

	tokenList
)

(defun tokensPrint (filename)
	"Token ları tipleri ile birlikte ekrana sıralı şekilde basan print fonksiyonudur."
	(setq tokenList (lexer filename))
	(mapcar (lambda (token) 
			(cond
				((not (equal (find token operators :test #'string=) nil))
					(format t "Token -> ~A ->> [Operator]~%" token))
				((not (equal (find token keywords :test #'string=) nil))
					(format t "Token -> ~A ->> [Keyword]~%" token))
				((and (equal (length token) 1) (equal (is_digit (character token)) t))
					(format t "Token -> ~A ->> [Digit]~%" token))
				((not (equal (parse-integer token :junk-allowed t) nil))
					(format t "Token -> ~A ->> [Integer]~%" token))
				((equal (search "Syntax Error" token) nil)
					(format t "Token -> ~A ->> [Identifier]~%" token))
				(t (format t "Token -> ~A ->> [SYNTAX ERROR IN THIS TOKEN !!]~%" token))
			)
		)
	tokenList)
	tokenList
)

;(lexer "test.coffee")

; farklı dosya ile test etmek için aşağıdaki string'in yerine dosyanın adını yazınız.
(tokensPrint "test.coffee")