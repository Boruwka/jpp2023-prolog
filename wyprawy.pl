% Przykładowy program główny w Prologu

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

5. Uwzględnienie rodzaju - tylko jeden możliwy 
6. Uwzględnianie rodzaju - różne i żaden
7. Obsługa stałej nil
8. Obsługa wypisywania koniec
9. Sprawdzanie poprawności wejścia
10. Poprawne wypisywanie, przeanalizowanie treści
11. Niepowtarzające się wyniki
12. Testy rocznikowe
*/
    
% funkcje pomocnicze

tuple_to_list((X, T), [X | L]) :- tuple_to_list(T, L).
tuple_to_list(X, [X]) :- X \= (_, _).

dlugosc_spelnia_warunek(D, dlugosc(eq, K)) :- D is K.
dlugosc_spelnia_warunek(D, dlugosc(lt, K)) :- (D < K).
dlugosc_spelnia_warunek(D, dlugosc(le, K)) :- (D =< K).
dlugosc_spelnia_warunek(D, dlugosc(gt, K)) :- (D > K).
dlugosc_spelnia_warunek(D, dlugosc(ge, K)) :- (D >= K).


dlugosc_spelnia_warunki(_, []).
dlugosc_spelnia_warunki(D, [dlugosc(War, K)|_]) :- dlugosc_spelnia_warunek(D, dlugosc(War, K)).
dlugosc_spelnia_warunki(D, [rodzaj(_)|T]) :- dlugosc_spelnia_warunki(D, T).

wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa(Trasy, Start, Meta, D), dlugosc_spelnia_warunki(D, Warunki). 
 
 
wyprawa(T, S, M, D) :- wyprawa(T, S, M, D, []).
wyprawa([], X, X, 0, _).
% wyprawa([Id], S, M, D, Was) :- trasa(Id, S, M, _R, _K, D), nonmember((S, M), Was).
wyprawa([Id|T], S, M, D, Was) :- trasa(Id, S, X, _R, _K, Dl), nonmember((S, X), Was), wyprawa(T, X, M, Dl1, [(S, X)|Was]), D is (Dl + Dl1).
wyprawa([Id|T], S, M, D, Was) :- trasa(Id, X, S, _R, oba, Dl), nonmember((S, X), Was), wyprawa(T, X, M, Dl1, [(S, X)|Was]), D is (Dl + Dl1).

wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), wypisz_wyprawe(Trasy, D).

wypisz_wyprawe([], D) :- format('Dlugosc: ~p \n', [D]).
wypisz_wyprawe([Id|Trasy], D) :- trasa(Id, Start, Meta, _Rodzaj, _Kierunek, _Km), format('Id: ~p Trasa: ~p -> ~p ', [Id, Start, Meta]), wypisz_wyprawe(Trasy, D).

wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, D, Warunki) :- wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), nl, fail.


% właściwe przetwarzanie

przetwarzaj :-
    write('Podaj miejsce startu: '),
    read(Start),
    write('Podaj koniec: '),
    read(Meta),
    write('Podaj warunki: '),
    read(Warunki),
    /*(
      trasa(_Id, Start, Meta, _Rodzaj, _Kierunek, Km) ->
      format('Istnieje trasa dlugosci ~d.~n', [Km])
    ;
      format('Brak trasy z ~p do ~p.~n', [Start, Meta])
    ).*/
    ( 
      (tuple_to_list(Warunki, WarunkiTab),
      wypisz_wszystkie_wyprawy_spelniajace_warunki(_Trasy, Start, Meta, _D, WarunkiTab));
      write('Brak trasy')
    ).
