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
3. Uwzględnienie rodzaju
4. Wypisywanie długości
5. Uwzględnienie długości
6. Poprawne wypisywanie
7. Niepowtarzające się wyniki
8. Testy rocznikowe
*/
    
% funkcje pomocnicze

% notmembertuple = not(member)
notmembertuple((Elem1, Elem2), []).
notmembertuple((Elem1, Elem2), [(Hd1, Hd2)|L]) :- ((Elem1 =\= Hd1); (Elem2 =\= Hd2)),  notmembertuple((Elem1, Elem2), L).

% na razie olewamy warunki
wyprawa_spelnia_warunki(Trasy, Start, Meta, Warunki) :- wyprawa(Trasy, Start, Meta). 
 
 % na razie olewamy kierunki
 
wyprawa(T, S, M) :- wyprawa(T, S, M, []).
wyprawa([], X, X, Was).
% wyprawa([Id], S, M, Was) :- trasa(Id, S, M, _R, _K, _D), nonmember((S, M), Was).
wyprawa([Id|T], S, M, Was) :- trasa(Id, S, X, _R, _K, _D), nonmember((S, X), Was), wyprawa(T, X, M, [(S, X)|Was]).

wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, Warunki) :- wyprawa_spelnia_warunki(Trasy, Start, Meta, Warunki), wypisz_wyprawe(Trasy).

wypisz_wyprawe([]) :- write('\n').
wypisz_wyprawe([Id|Trasy]) :- trasa(Id, Start, Meta, Rodzaj, Kierunek, Km), format('Id: ~p Trasa: ~p -> ~p ', [Id, Start, Meta]), wypisz_wyprawe(Trasy).

wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, Warunki) :- wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, Warunki), nl, fail.


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
      wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, Warunki);
      write('Brak trasy')
    ).
