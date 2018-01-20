# HaskellBeadandok

sudoku.hs Feladata : Sudoku megoldó       http://lambda.inf.elte.hu/fp/Sudoku.xml

A feladat a Sudoku logikai játékról, szabályainak megfogalmazásáról és végül a megoldásáról szól. 
A Sudoku játékot egy 9x9-es négyzetrácson játsszák, amelynek az üresen hagyott mezőit kell kitölteni az [1-9]
intervallumból vett értékek valamelyikével.
A kitöltés szabálya, hogy egy tetszőleges sorban, oszlopban vagy blokkban sem szerepeljen ugyanaz a szám egynél többször.

bignum.hs Feladata : Karatsuba-algoritmus  http://lambda.inf.elte.hu/fp/Karatsuba.xml

A Karatsuba-algoritmus egy gyors szorzást megvalósító algoritmus. Az algoritmus két, n számjegyből álló szám szorzatát 
állítja elő hatékonyan. Ilyenkor ugyanis a szorzat legfeljebb nlog23 ≈ n1, 585 számjegy szorzásából áll elő, 
ellentétben a hagyományos szorzás algoritmusával, amely n2 számjegy szorzásából határozható meg.

Például a Karatsuba-algoritmus két, 1024 számjegyből álló szám szorzatát 310 = 59049 lépésből tudja előállítani,
ellentétben a hagyományos algoritmus négyzetes, azaz (210)2 = 1,048,576 darab szorzásával.

A számok, amelyekkel a feladatban foglalkozunk, tetszőleges alapú számrendszerben adottak. 
Az egyszerűség kedvéért a számrendszer alapszámát és az számrendszerhez tartozó “számjegyeket” is egész számokkal adjuk meg.

A számokat listákkal ábrázoljuk, ahol a hatványok együtthatói helyiérték szerinti növekvő sorrendben adottak
(vagyis a lista elején szerepel a legkisebb helyiérték, a végén pedig a legnagyobb).

Például a 2015-ös egész szám 10-es számrendszerbeli ábrázolása [5,1,0,2], azaz
5 * 100 + 1 * 101 + 0 * 102 + 2 * 103. Ugyanez az érték 8-as számrendszerben felírva 
[7,3,7,3], azaz 7 * 80 + 3 * 81 + 7 * 82 + 3 * 83.
