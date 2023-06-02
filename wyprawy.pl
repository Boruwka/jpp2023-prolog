:- ensure_loaded(library(lists)).

user:runtime_entry(start):-
    (current_prolog_flag(argv, [File]) ->
        set_prolog_flag(fileerrors, off),
        (compile(File) -> true
         ;
	 format('Error opening file ~p.\n', [File])
        ),
        prompt(_Old, ''),         % pusty prompt
	przetwarzaj
     ;
	write('Incorrect usage, use: program <file>\n')
    ).
    
/* Plan działania: 
1. Znajdowanie wszystkich ścieżek olewając kierunki i warunki
2. Uwzględnienie kierunków
3. Wypisywanie długości
4. Uwzględnienie długości
5. Uwzględnienie rodzaju
6. Uwzględnianie rodzaju - żaden podany
7. nil jako warunki
8. Nieprzerywanie po jednym zapytaniu 
9. nil jako miejsce

10. Obsługa wypisywania koniec
11. Sprawdzanie poprawności wejścia
12. Poprawne wypisywanie
13. Niepowtarzające się wyniki
14. Testy rocznikowe
15. Styl kodu - linijki do 80 znaków, wcięcia
16. Komentarze
17. Komentarz z nazwiskiem autora, prawidłowa nazwa
*/
    
% funkcje pomocnicze

nie_ma_rodzaju_w_warunkach([]).
nie_ma_rodzaju_w_warunkach([rodzaj(_)|_]) :- fail.
nie_ma_rodzaju_w_warunkach([X|T]) :- X \= rodzaj(_), nie_ma_rodzaju_w_warunkach(T).

tuple_to_list((X, T), [X | L]) :- tuple_to_list(T, L).
tuple_to_list(X, [X]) :- X \= (_, _).
tuple_to_list(nil, []).

% dodaj_na_koniec(Elem, T, Res)
% dodaje elem na koniec t zwraca w res
dodaj_na_koniec(Elem, [], [Elem]).
dodaj_na_koniec(Elem, [Hd|Tl], [Hd|Restl]) :- dodaj_na_koniec(Elem, Tl, Restl).

dlugosc_spelnia_warunek(D, dlugosc(eq, K)) :- D is K.
dlugosc_spelnia_warunek(D, dlugosc(lt, K)) :- (D < K).
dlugosc_spelnia_warunek(D, dlugosc(le, K)) :- (D =< K).
dlugosc_spelnia_warunek(D, dlugosc(gt, K)) :- (D > K).
dlugosc_spelnia_warunek(D, dlugosc(ge, K)) :- (D >= K).


dlugosc_spelnia_warunki(_, []).
dlugosc_spelnia_warunki(D, [dlugosc(War, K)|_]) :- dlugosc_spelnia_warunek(D, dlugosc(War, K)).
dlugosc_spelnia_warunki(D, [rodzaj(_)|T]) :- dlugosc_spelnia_warunki(D, T).

wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa(Trasy, Start, Meta, D, Warunki), dlugosc_spelnia_warunki(D, Warunki). 

wyprawa(T, S, M, D, W) :- wyprawa(T, S, M, D, W, []).

% nil oba końce

wyprawa([], nil, nil, 0, _, _).

wyprawa([Id|T], nil, nil, D, War, Was) :- 
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([Id|T], nil, nil, D, War, Was) :- 
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).

% nil jako miejsce koncowe

wyprawa([], _, nil, 0, _, _).

wyprawa([Id|T], S, nil, D, War, Was) :- 
    (X \= nil),
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([Id|T], S, nil, D, War, Was) :- 
    (X \= nil),
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
 
% nil jako miejsce startowe

wyprawa([], nil, _, 0, _, _).

wyprawa(L, nil, M, D, War, Was) :- 
    trasa(Id, X, M, R, _K, Dl), 
    nonmember((X, M), Was), 
    wyprawa(T, nil, X, Dl1, War, [(X, M)|Was]), 
    D is (Dl + Dl1),
    dodaj_na_koniec(Id, T, L),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa(L, nil, M, D, War, Was) :- 
    trasa(Id, M, X, R, oba, Dl), 
    nonmember((X, M), Was), 
    wyprawa(T, nil, X, Dl1, War, [(X, M)|Was]), 
    D is (Dl + Dl1),
    dodaj_na_koniec(Id, T, L),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
 
% określone miejsce startu i końca

wyprawa([], X, X, 0, _, _).

wyprawa([Id|T], S, M, D, War, Was) :- 
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, M, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([Id|T], S, M, D, War, Was) :- 
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((S, X), Was), 
    wyprawa(T, X, M, Dl1, War, [(S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).

wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), wypisz_wyprawe(Trasy, D).

wypisz_wyprawe([], D) :- format('Dlugosc: ~p \n', [D]).
wypisz_wyprawe([Id|Trasy], D) :- trasa(Id, Start, Meta, _Rodzaj, _Kierunek, _Km), format('Id: ~p Trasa: ~p -> ~p ', [Id, Start, Meta]), wypisz_wyprawe(Trasy, D).

wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, D, Warunki) :- wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), nl, fail.


% właściwe przetwarzanie

przetwarzaj :-
    write('Podaj miejsce startu: '),
    read(Start),
    (
        Start == koniec -> 
        write('Koniec programu. Milych wedrowek!\n');
        (
            write('Podaj koniec: '),
            read(Meta),
            (
                Meta == koniec ->
                write('Koniec programu. Milych wedrowek!\n');
                (
                    write('Podaj warunki: '),
                    read(Warunki),
                    (
                        Warunki == koniec -> 
                        write('Koniec programu. Milych wedrowek!\n');
                        (
                            (
                            (tuple_to_list(Warunki, WarunkiTab),
                            wypisz_wszystkie_wyprawy_spelniajace_warunki(_Trasy, Start, Meta, _D, WarunkiTab));
                            write('Brak trasy \n')
                            ),
                            przetwarzaj
                        )
                    )
                )
            )
        )
    ).
