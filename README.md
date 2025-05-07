### W tym zadaniu rozszerzamy język z Zadania 2 o konstruktory wartości i dopasowanie wzorca. Tym niemniej nadal nie wprowadzamy żadnej kontroli typów - każda wartość może być zastosowana do dowolnych argumentów.

Na przykład
```
one = S Z
two = S one
add Z     n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add one two)
S (S (add Z two))
S (S two)
S (S (S one))
S (S (S (S Z)))
```
Tak jak w Haskellu, nazwy z wielkiej litery oznaczają konstruktory.
## Składnia

Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów - każda wartość może być zastosowana do dowolnych argumentów.

Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki haskell-src. Wynik parseModule musimy przekształcić do naszej uproszczonej składni
```
type Name = String
data Def = Def { defMatches :: [Match] }
data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  ::Expr
    }

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
data Pat = PVar Name | PApp Name [Pat]
```
## Dopasowania

Stwierdzenie, czy argument pasuje do wzorca może wymagać wykonania jednego lub więcej kroków redukcji tego argumentu.
W językach leniwych istotnie zwykle to dopasowanie wzorca wymusza redukcje.
Tym niemniej argument jest redukowany "tylko tyle ile potrzeba", czyli aż do momentu rozstrzygnięcia czy pasuje do wzorca.
```
one = S Z
two = S one
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add one two)
S (S (add Z two))
S (S two)
S (S (S one))
S (S (S (S Z)))
```
## Poziom 1

Przy prostej implementacji dopasowania wzorca może się zdarzyć że takie "wymuszone" redukcje pozostaną nieodnotowane, np.
```
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add (S Z) two)
S (S (add Z two))
S (S two)
S (S (S (S Z)))
```

Na tym poziomie można poprzestać na rozwiązaniu, które w takich przypadkach łączy niektóre kroki, oczywiście pod warunkiem że redukcja jest ogólnie poprawna. Takie rozwiązania mogą liczyć (o ile nie mają innych braków) na ok. 50-60% punktów.
## Poziom 2

Jednym ze sposobów rozwiązania tego problemu jest precyzyjne odnotowywanie wszystkich kroków wraz z kontekstem w jaki się odbywają. Można wykorzystać do tego celu monadę stanu, która będzie przechowywać historię redukcji (wraz z ich kontekstami). Stan może ponadto zawierać także listę definicji i ilość pozostałego "paliwa" (kroków, po których uznamy, że obliczenie jest zapętlone lub za długie do wyświetlenia), tudzież inne informacje które uznamy za potrzebne. Do reprezentacji kontekstu i nawigacji wewnątrz wyrażen można uzyć techniki "zipper" (ewentualnie po prostu ścieżki będącej listą elementów "lewo-prawo", ale to słabsze rozwiązanie).
```
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
{main}
{add two two}
add {S (S Z)} two
{S (add (S Z) two)}
S {add (S Z) two}
S {S (add Z two)}
S (S {add Z two})
S (S {two})
S (S {S (S Z)})
S (S (S {S Z}))
S (S (S (S {Z})))
```
Postać wyjścia nie musi być dokładnie taka jak powyżej, ważne aby w poprawny i czytelny sposób ilustrowała proces redukcji.
## Sekwencje

Do przechowywania historii przyda się typ sekwencji, w którym dodawanie elementu na końcu odbywa się w czasie stałym

Zdefiniuj typ
```
newtype SnocList a = SnocList {unSnocList :: [a]}
toList :: SnocList a -> [a]
fromList :: [a] -> SnocList a
snoc :: SnocList a -> a -> SnocList a
```
oraz instancje Eq, Show, Semigroup, Monoid, Functor, Applicative, Alternative.
