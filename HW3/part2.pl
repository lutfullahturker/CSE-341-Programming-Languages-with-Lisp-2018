flight(istanbul,izmir,3).          % fact: Istanbul and Izmir has a flight with cost 3.
flight(istanbul,ankara,2).
flight(edirne,erzurum,5).
flight(erzurum,antalya,2).
flight(antalya,izmir,1).
flight(antalya,diyarbakir,5).
flight(izmir,ankara,6).
flight(ankara,diyarbakir,8).
flight(ankara,trabzon,6).
flight(ankara,kars,3).
flight(istanbul,trabzon,3).
flight(kars,gaziantep,3).

connected(X,Y,C) :-    /* Undirected bir graf oldugundan dolayi yukardaki tum edge leri  ters sekilde de tek tek yazmak yerine bu kurali yaziyoruz ve  ornegin
						  Istanbul,Trabzon,3  ile birlikte  Trabzon,Istanbul,3  de  gecerli olmus oluyor. */
	flight(X,Y,C) ; flight(Y,X,C).

route(X,Y,C) :-   /* Tek il ilerleyerek  gidilecegi durumda  basit olarak yukardaki flight lardan uygun olani veriyoruz. */
    flight(X,Y,C).

route(X,Y,C) :-  /* visited  array i tutmak icin bos bir liste ile birlikte recursive call yapiyoruz. */
	route(X,Y,C,[]). 

route(X,Y,C,V) :-  /* Recursive seklinde varilacak sehri arar ve toplam cost u ilerledikce  toplar. En kisa yolu bulmaz. yukaridaki tanimlardan ilk saglanana girerek  sonucu verir. */
	connected(X,Y,C) ; not(member(X,V)),
	connected(X,B,C2),
	not(B=X),
	route(B,Y,C3,[X|V]),not(member(B,V)),not(X=Y),
	C is C2 + C3.