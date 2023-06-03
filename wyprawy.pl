% Jagoda Bracha

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
    
    
% funkcje pomocnicze

% jest prawdą, jeśli w tablicy Warunków nie występuje element postaci rodzaj(R)
nie_ma_rodzaju_w_warunkach([]).
nie_ma_rodzaju_w_warunkach([rodzaj(_)|_]) :- fail.
nie_ma_rodzaju_w_warunkach([X|T]) :- 
    X \= rodzaj(_), 
    nie_ma_rodzaju_w_warunkach(T).

% tuple_to_list(T, L) przerabia krotkę T na listę L
tuple_to_list((X, T), [X | L]) :- tuple_to_list(T, L).
tuple_to_list(X, [X]) :- X \= (_, _).
tuple_to_list(nil, []).


% sprawdza, czy warunek jest poprawny 
dobry_warunek(dlugosc(eq, K)) :- (K \= (_,_)).
dobry_warunek(dlugosc(le, K)) :- (K \= (_,_)).
dobry_warunek(dlugosc(lt, K)) :- (K \= (_,_)).
dobry_warunek(dlugosc(ge, K)) :- (K \= (_,_)).
dobry_warunek(dlugosc(gt, K)) :- (K \= (_,_)).
dobry_warunek(rodzaj(R)) :- (R \= (_, _)).
dobry_warunek(nil).

% sprawdza, czy tablica warunków zawiera jedynie poprawne warunki
sprawdz_warunki([]).
sprawdz_warunki([X|T]) :- dobry_warunek(X), sprawdz_warunki(T).
sprawdz_warunki([X|_]) :- 
    \+ dobry_warunek(X),
    format('Error: niepoprawny warunek - ~p\n', [X]),
    fail.
    
% first_member(E, L) zwraca to co member,
% ale tylko raz nawet jeśli E występuje wiele razy w liście.
first_member(_, []) :- fail.
first_member(E, [H|_]) :- (E == H).
first_member(E, [H|T]) :- (E \= H), first_member(E, T).
    
% usun_duplikaty(Tab, TabBezDuplikatow) 
% usuwa z Tab powtarzające się elementy.
usun_duplikaty([], []).
usun_duplikaty([H|T], [H|L]) :- nonmember(H, T), usun_duplikaty(T, L).
usun_duplikaty([H|T], L) :- 
    first_member(H, T),
    usun_duplikaty(T, L).

% dodaj_na_koniec(Elem, T, Res)
% dodaje Elem na koniec T i zwraca wynik w Res
dodaj_na_koniec(Elem, [], [Elem]).
dodaj_na_koniec(Elem, [Hd|Tl], [Hd|Restl]) :- dodaj_na_koniec(Elem, Tl, Restl).

dlugosc_spelnia_warunek(D, dlugosc(eq, K)) :- D is K.
dlugosc_spelnia_warunek(D, dlugosc(lt, K)) :- (D < K).
dlugosc_spelnia_warunek(D, dlugosc(le, K)) :- (D =< K).
dlugosc_spelnia_warunek(D, dlugosc(gt, K)) :- (D > K).
dlugosc_spelnia_warunek(D, dlugosc(ge, K)) :- (D >= K).

% sprawdza, czy wyprawa podanej długości spełnia warunki z tablicy

dlugosc_spelnia_warunki(_, []).

dlugosc_spelnia_warunki(D, [dlugosc(War, K)|_]) :- 
    dlugosc_spelnia_warunek(D, dlugosc(War, K)).
    
dlugosc_spelnia_warunki(D, [rodzaj(_)|T]) :- 
    dlugosc_spelnia_warunki(D, T).


wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :- 
    wyprawa(Trasy, Start, Meta, D, Warunki), 
    dlugosc_spelnia_warunki(D, Warunki). 

wyprawa(T, S, M, D, W) :- wyprawa(T, S, M, D, W, []).

% nil oba końce

wyprawa([], nil, nil, 0, _, _).

wyprawa([(Id, S, X, R)|T], nil, nil, D, War, Was) :- 
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([(Id, S, X, R)|T], nil, nil, D, War, Was) :- 
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).

% nil jako miejsce koncowe

wyprawa([], _, nil, 0, _, _).

wyprawa([(Id, S, X, R)|T], S, nil, D, War, Was) :- 
    (X \= nil),
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([(Id, S, X, R)|T], S, nil, D, War, Was) :- 
    (X \= nil),
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, nil, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
 
% nil jako miejsce startowe

wyprawa([], nil, _, 0, _, _).

wyprawa(L, nil, M, D, War, Was) :- 
    trasa(Id, X, M, R, _K, Dl), 
    nonmember((Id, X, M), Was), 
    wyprawa(T, nil, X, Dl1, War, [(Id, X, M)|Was]), 
    D is (Dl + Dl1),
    dodaj_na_koniec((Id, X, M, R), T, L),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa(L, nil, M, D, War, Was) :- 
    trasa(Id, M, X, R, oba, Dl), 
    nonmember((Id, X, M), Was), 
    wyprawa(T, nil, X, Dl1, War, [(Id, X, M)|Was]), 
    D is (Dl + Dl1),
    dodaj_na_koniec((Id, X, M, R), T, L),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
 
% określone miejsce startu i końca

wyprawa([], X, X, 0, _, _).

wyprawa([(Id, S, X, R)|T], S, M, D, War, Was) :- 
    trasa(Id, S, X, R, _K, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, M, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1),
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).
    
wyprawa([(Id, S, X, R)|T], S, M, D, War, Was) :- 
    trasa(Id, X, S, R, oba, Dl), 
    nonmember((Id, S, X), Was), 
    wyprawa(T, X, M, Dl1, War, [(Id, S, X)|Was]), 
    D is (Dl + Dl1), 
    (member(rodzaj(R), War) ; nie_ma_rodzaju_w_warunkach(War)).

wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki) :-
    wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), 
    (D > 0), 
    wypisz_wyprawe(Trasy, D).


wypisz_wyprawe([], D) :- 
    format('Dlugosc trasy: ~p.\n', [D]).
    
wypisz_wyprawe([(Id, A, B, R)], D) :- 
    format('~p -(~p,~p)-> ~p\n', [A, Id, R, B]), 
    wypisz_wyprawe([], D).
    
wypisz_wyprawe([(Id, A, _, R)|Trasy], D) :- 
    (Trasy \= []), 
    format('~p -(~p,~p)-> ', [A, Id, R]), 
    wypisz_wyprawe(Trasy, D).

wypisz_wszystkie_wyprawy_spelniajace_warunki(Trasy, Start, Meta, D, Warunki) :-
    wypisz_wyprawa_spelnia_warunki(Trasy, Start, Meta, D, Warunki), nl, fail.


% właściwe przetwarzanie


przetwarzaj :-
    write('Podaj miejsce startu: '),
    read(Start),
    (
        Start == koniec -> 
        write('Koniec programu. Milych wedrowek!\n');
        (
            write('Podaj miejsce koncowe: '),
            read(Meta),
            (
                Meta == koniec ->
                write('Koniec programu. Milych wedrowek!\n');
                (
                    write('Podaj warunki: '),
                    read(Warunki),
                    nl,
                    (
                        Warunki == koniec -> 
                        write('Koniec programu. Milych wedrowek!\n');
                        (
                            
			    (tuple_to_list(Warunki, WarunkiTab),
                            sprawdz_warunki(WarunkiTab),
                            usun_duplikaty(WarunkiTab, WarunkiTabBezDuplikatow),
                            wypisz_wszystkie_wyprawy_spelniajace_warunki(_Trasy, Start, Meta, _D, WarunkiTabBezDuplikatow));
                            przetwarzaj
                        )
                    )
                )
            )
        )
    ).
