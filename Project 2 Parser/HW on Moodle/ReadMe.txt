Tüm fonksiyonlar recursive şekilde çalışmaktadır.

REPL üzerinden Load işlemi yapılınca otomatik olarak Proje 1 in output unu input olarak kabul eden Parse List oluşturulup .tree dosyasına yazılır.

'()  için ' veya '( işaretlerinin operatör mü keyword mü olduğu ile alakalı ne moodle da ne slaytta bir kaynak, açıklama olmadığı için
'()  şeklinde verilen inputlar ile ilgili herhangi bir kod yazmadım.


Parse  Tree doğru şekilde çalışmaktadır. Tek eksiği bazı durumlarda root'dan itibaren en içe doğru yazması gerekirken bazır seviyeleri yazmıyor.
EXPI
	VALUE
		IntegerValue
			5
; yerine

VALUE
	IntegerValue
		5
gibi yazdığı durumlar olabiliyor. Bunun haricinde göze batan bir eksiklik görmedim. 
Testlerinizde yardımcı olması açısından eksikliklerimi belirtmek istedim.
Kolay Gelsin.