:- ensure_loaded(attributi).
:- ensure_loaded(test_set).
:- ensure_loaded(training_set).
:- ensure_loaded(albero).
:- op(300, xfx, <==).
:- dynamic alb/1.

% Programma Prolog che effettua la potatura di un albero di decisione
% per valutare il cambiamento delle sue prestazioni.
%
% NOTA: per eseguire correttamente il programma e' necessario aumentare
% lo stack size disponibile (3 GB in questo caso) con il seguente
% comando: set_prolog_stack(global, limit(3*10**9)).
% Potrebbe essere necessario modificare in base alla propria
% configurazione. Assicurarsi di avere il file albero.pl che viene
% generato dopo aver lanciato l'induzione dell'albero con il criterio di
% gini o dell'entropia, nella propria workspace.
%
% Il programma prima calcola tutti i sotto-alberi procedendo in
% profondita'. Successivamente, il programma scorre la lista dei
% sotto-alberi e rimuove ognuno di questi dall'albero originale,
% mantenendo la potatura solo se questa ha migliorato le prestazioni,
% valutate con il test del chi-quadro. Infine, viene mostrata la matrice
% di confusione finale dell'albero potato.

% Lancia la potatura dell'albero originale
lancia(MigliorAlbero, Punteggio) :-
    findall(t(Att,Val), t(Att,Val), Albero),
    lancia_sottoalberi(Albero, [], [], ListaFinale),
    length(ListaFinale, Length),
    inverti(ListaFinale, ListaPotatura,[]),
    lancia_potatura(Albero, ListaPotatura, MigliorAlbero, 100,Punteggio,Length),
    assert(alb(MigliorAlbero)),
    stampa_matrice_di_confusione(MigliorAlbero).

% inverte una lista
inverti([],Z,Z).
inverti([H|T],Z,Acc) :- inverti(T,Z,[H|Acc]).


% Prova la potatura per ogni sottoalbero mantenendola solo se questa ha
% migliorato le predizione (utilizzando il test del chi-quadro)
lancia_potatura(Albero,_,AlberoVincente,PunteggioCorrente, PunteggioFinale, 0) :-
    AlberoVincente = Albero,
    PunteggioFinale = PunteggioCorrente.
lancia_potatura([Albero|_], [SubAlbero|Resto], MigliorAlbero, PunteggioCorrente, PunteggioFinale, Limite) :-
    Limite > 0,
    writeln(Limite),
    rimuovi_sottoalbero(Albero, SubAlbero, AlberoPotato), % pota l'albero rimuovendo il sottoalbero
    valuta_chi_quadro(Albero, AlberoPotato, AlberoVincente, Punteggio), % vede se la potatura ha migliorato l'accuratezza
    Punteggio =< PunteggioCorrente,
    NuovoLimite is Limite - 1,
    lancia_potatura([AlberoVincente], Resto, MigliorAlbero, Punteggio, PunteggioFinale, NuovoLimite).

% Aggiunge i sottoalberi ad una lista procedendo per livelli di
% profondita' nell'albero originale
lancia_sottoalberi([], [], ListaFinale, ListaFinale).
lancia_sottoalberi([], _, ListaCorrente, ListaFinale) :-
    lancia_sottoalberi(ListaCorrente, [], ListaCorrente, ListaFinale).
lancia_sottoalberi([Albero|Resto], ListaLivelloCorrente, ListaCorrente, ListaFinale) :-
    ricava_sottoalberi(Albero, [], SottoAlberi),
    (SottoAlberi = [[]] -> (ListaLivello = [],
                            ListaRisultante = ListaCorrente);
    append(ListaLivelloCorrente, SottoAlberi, ListaLivello),
    append(ListaCorrente, SottoAlberi, ListaRisultante)),
    lancia_sottoalberi(Resto, ListaLivello, ListaRisultante, ListaFinale).

% Predicati per ricavare i sottoalberi
ricava_sottoalberi(t(_,[]),ListaCorrente,ListaFinale) :-
    reverse(ListaCorrente, ListaFinale).
ricava_sottoalberi(t(Att, [Val|Resto]), ListaCorrente, ListaFinale) :-
    (Resto = [], ListaCorrente = [] -> (controlla_se_lista(Val, ValAggiustato),
                                        (rimuovi_valore(ValAggiustato, t(Att2,[Val2|Resto2])),
                                         costruisci_albero(Att2, [Val2], Risultato),
                                         ricava_sottoalberi(t(Att2,Resto2), [Risultato|ListaCorrente], ListaFinale))
                                        ;
                                        (rimuovi_valore([Val], [l(_)|Resto]);rimuovi_valore([Val],l(_));rimuovi_valore([Val],null)),
                                        ricava_sottoalberi(t(Att,Resto),[Resto|ListaCorrente], ListaFinale))
    ;
    controlla_se_lista(Val, ValAggiustato),
    costruisci_albero(Att, ValAggiustato, Risultato),
    ricava_sottoalberi(t(Att,Resto), [Risultato|ListaCorrente], ListaFinale)).

% Costruisce la sintassi per un albero o sottoalbero partendo da
% attributo e valore
costruisci_albero(Att, Valore, AlberoRisultante) :-
    AlberoRisultante = t(Att,Valore).

% Predicato ausiliario per la rimozione del valore dalla struttura di un
% sottoalbero
rimuovi_valore([Valore|_], SubAlb) :-
    compound(Valore),
    arg(2, Valore, Arg1),
    SubAlb = Arg1.

% Predicato ausiliario che controlla se Valore � una lista, in caso
% contrario lo converte in una lista
controlla_se_lista(Valore, NuovoValore) :-
    \+is_list(Valore) -> NuovoValore = [Valore] ;
    NuovoValore = Valore.

% Predicato per la potatura effettiva di un sottoalbero da Albero
rimuovi_sottoalbero(Albero, SottoalberoDaRimuovere, AlberoRisultante) :-
    rimuovi_sottoalbero_helper(Albero, SottoalberoDaRimuovere, AlberoRisultante), !.

rimuovi_sottoalbero_helper(t(Attributo, Valori), SottoalberoDaRimuovere, t(Attributo, ValoriAggiornati)) :-
    remove_subtree_from_list(Valori, Attributo, SottoalberoDaRimuovere, ValoriAggiornati), !.

rimuovi_sottoalbero_helper(l(Classe), _, l(Classe)) :- !.

rimuovi_sottoalbero_helper(null, _, null) :- !.

remove_subtree_from_list([],_, _, []).
remove_subtree_from_list([H|T], Att, SottoalberoDaRimuovere, T1) :-
    (t(Att,[H]) == SottoalberoDaRimuovere
    ->  remove_subtree_from_list(T, Att,SottoalberoDaRimuovere, T1)
    ;   H = Tier:SubTree,
        rimuovi_sottoalbero_helper(SubTree, SottoalberoDaRimuovere, SubTreeAggiornato),
        T1 = [Tier:SubTreeAggiornato|T2],
        remove_subtree_from_list(T, Att, SottoalberoDaRimuovere, T2)
    ).
remove_subtree_from_list([H|T], Att ,SottoalberoDaRimuovere, [H|T1]) :-
    H \= _:_,
    remove_subtree_from_list(T, Att, SottoalberoDaRimuovere, T1).

% Predicato che decide qual e' l'albero "migliore" in base al punteggio
% ottenuto con il test del chi-quadro
valuta_chi_quadro(Albero, AlberoPotato, AlberoVincente, MigliorCosto) :-
    costo_albero(Albero,CostoOriginale),
    costo_albero(AlberoPotato,CostoPotato),
    (	CostoOriginale < CostoPotato -> AlberoVincente = Albero, MigliorCosto = CostoOriginale
    ;
    (
     AlberoVincente = AlberoPotato, MigliorCosto = CostoPotato)
    ).

% Valuta un albero con il test del chi-quadro
costo_albero(Albero, Costo) :-
    findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
    length(TestSet,N),
    findall(sano/Oggetto,s(sano,Oggetto),SaniAttesi),
    length(SaniAttesi, Ns),
    InfAttesi is N - Ns,
    valuta(Albero,TestSet,VN,0,VP,0,FN,0,FP,0,_,0),
    SaniOsservati is VN + FN,
    InfOsservati is VP + FP,
    chi_quadro(InfOsservati, SaniOsservati, InfAttesi, Ns, Costo).

% Predicato che effettua il test del chi-quadro
chi_quadro(InfOss, SaniOss, InfAtt, SaniAtt, Costo) :-
    CostoInf is ((InfOss - InfAtt) ** 2) / InfAtt,
    CostoSani is ((SaniOss - SaniAtt) ** 2) / SaniAtt,
    Costo is CostoInf + CostoSani.

% Predicatii per calcolare vari positivi, falsi positivi, veri negativi
% e falsi negativi
% Predicati per calcolare veri positivi, falsi positivi, veri negativi, e falsi negativi
valuta(_, [], VPqso, VPqso, VPstar, VPstar, VPgalaxy, VPgalaxy, FPqso, FPqso, FPstar, FPstar, FPgalaxy, FPgalaxy, NC, NC).
valuta(Albero, [qso/Oggetto | Coda], VPqso, VPqsoA, VPstar, VPstarA, VPgalaxy, VPgalaxyA, FPqso, FPqsoA, FPstar, FPstarA, FPgalaxy, FPgalaxyA, NC, NCA) :-
    (classifica(Oggetto, qso, Albero) ->
        VPqsoA1 is VPqsoA + 1,
        FPqsoA1 = FPqsoA,
        FPstarA1 = FPstarA,
        FPgalaxyA1 = FPgalaxyA
    ;
    (classifica(Oggetto, star, Albero) ->
        FPstarA1 is FPstarA + 1,
        VPqsoA1 = VPqsoA,
        FPgalaxyA1 = FPgalaxyA
    ;
    (classifica(Oggetto, galaxy, Albero) ->
        FPgalaxyA1 is FPgalaxyA + 1,
        VPqsoA1 = VPqsoA,
        FPstarA1 = FPstarA
    ;
    NCA1 is NCA + 1,
    VPqsoA1 = VPqsoA,
    FPqsoA1 = FPqsoA,
    FPstarA1 = FPstarA,
    FPgalaxyA1 = FPgalaxyA))),
    valuta(Albero, Coda, VPqso, VPqsoA1, VPstar, VPstarA, VPgalaxy, VPgalaxyA, FPqso, FPqsoA1, FPstar, FPstarA1, FPgalaxy, FPgalaxyA1, NC, NCA1).

classifica(Oggetto,nc,t(Att,Valori)) :-
	member(Att=Val,Oggetto),
        member(Val:null,Valori).

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),
        member(Val:l(Classe),Valori).

classifica(Oggetto, Classe, t(Att, Valori)) :-
    member(Att = Val, Oggetto),
    (member(Val: l(Classe), Valori);
    (member(Val: t(AttFiglio, ValoriFiglio), Valori),
    delete(Oggetto, Att = Val, Resto),
    classifica(Resto, Classe, t(AttFiglio, ValoriFiglio)))).

% Predicato per mostrare la matrice di confusione
stampa_matrice_di_confusione([Albero | _]) :-
    findall(Classe/Oggetto, s(Classe, Oggetto), TestSet),
    length(TestSet, N),
    valuta(Albero, TestSet, VPqso, 0, VPstar, 0, VPgalaxy, 0, FPqso, 0, FPstar, 0, FPgalaxy, 0, NC, 0),
    write('Test effettuati: '), writeln(N),
    write('Test non classificati: '), writeln(NC),
    write('QSO: Veri Positivi '), write(VPqso), write('   Falsi Positivi '), writeln(FPqso),
    write('Star: Veri Positivi '), write(VPstar), write('   Falsi Positivi '), writeln(FPstar),
    write('Galaxy: Veri Positivi '), write(VPgalaxy), write('   Falsi Positivi '), writeln(FPgalaxy).

% Predicato ausiliario per arrotondare un numero decimale
arrotonda(Numero, Arrotondamento) :-
    Temp is round(Numero * 100),
    Arrotondamento is Temp / 100.
