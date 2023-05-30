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

4. Uwzględnienie rodzaju - tylko jeden możliwy 
5. Uwzględnianie rodzaju - różne i żaden
6. Uwzględnienie długości
7. Obsługa stałej nil
8. Obsługa wypisywania koniec
9. Sprawdzanie poprawności wejścia
10. Poprawne wypisywanie, przeanalizowanie treści
11. Niepowtarzające się wyniki
12. Testy rocznikowe
*/
    
% funkcje pomocnicze

% na razie olewamy warunki
wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa(Trasy, Start, Meta, D). 
 
 % na razie olewamy kierunki
 
wyprawa(T, S, M, Dl) :- wyprawa(T, S, M, Dl, []).
wyprawa([], X, X, 0, Was).
% wyprawa([Id], S, M, D, Was) :- trasa(Id, S, M, _R, _K, D), nonmember((S, M), Was).
wyprawa([Id|T], S, M, D, Was) :- trasa(Id, S, X, _R, _K, Dl), nonmember((S, X), Was), wyprawa(T, X, M, Dl1, [(S, X)|Was]), D is (Dl + Dl1).
wyprawa([Id|T], S, M, D, Was) :- trasa(Id, X, S, _R, oba, Dl), nonmember((S, X), Was), wyprawa(T, X, M, Dl1, [(S, X)|Was]), D is (Dl + Dl1).

wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), wypisz_wyprawe(Trasy, D).

wypisz_wyprawe([], D) :- format('Dlugosc: ~p \n', [D]).
wypisz_wyprawe([Id|Trasy], D) :- trasa(Id, Start, Meta, Rodzaj, Kierunek, Km), format('Id: ~p Trasa: ~p -> ~p ', [Id, Start, Meta]), wypisz_wyprawe(Trasy, D).

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
      wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, D, Warunki);
      write('Brak trasy')
    ).
