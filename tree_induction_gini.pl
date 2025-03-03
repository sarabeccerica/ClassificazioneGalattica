:- ensure_loaded(attributi).
:- ensure_loaded(training_set).
:- ensure_loaded(test_set).
:- op(300, xfx, <==).

% Programma Prolog di apprendimento automatico per la classificazione di
% stelle, galassie e quasar attraverso la misura delle frequenze.
% Con il predicato lancia_apprendi(Classe) (Classe = GALAXY, QSO, STAR),
% viene lanciato l'apprendimento relativo alla classe specificata.
% Una volta concluso l'apprendimento, con il predicato
% classifica_oggetto e' possibile far classificare al programma
% l'oggetto specificato.
% Inoltre, con il predicato lancia_induzione, il programma calcola
% l'albero di decisione con il criterio di Gini.
% Con il predicato stampa_matrice_di_confusione e' possibile
% visualizzare le prestazioni dell'albero indotto.

% Cambiare percorso con quello della propria workspace
file_output('./albero.pl').


% Predicato per lanciare l'apprendimento
lancia_apprendi(Classe) :-
    file_output(NomeFile),
    tell(NomeFile),
    apprendi(Classe),
    told.

% Predicato per lanciare la classificazione di un oggetto
classifica_oggetto(Oggetto,Classe) :-
	Classe <== Descrizione,
	member(CongiunzioneAttributi,Descrizione),
	soddisfa(Oggetto,CongiunzioneAttributi).

% Predicato per lanciare l'induzione dell'albero di decisione con il
% criterio di Gini
lancia_induzione(Albero) :-
    file_output(NomeFile),
    induce_albero(Albero),
    tell(NomeFile),
    write(Albero),
    write('.'),
    told,
    assert(alb(Albero)).

apprendi(Classe) :-
    findall(e(C,O),e(C,O), Esempi),      % raccoglie  gli Esempi
    apprendi(Esempi, Classe, Descrizione), % induce la Descrizione della Classe
    nl,write(Classe),write('<=='),nl,      % la stampa
    writelist(Descrizione),
    assert( Classe<==Descrizione ).      % e la asserisce, ovvero APPRENDE

% apprendi(Esempi,Classe,Descrizione)
% Descrizione copre esattamente gli esempi di Classe nella lista Esempi
apprendi(Esempi,Classe,[]) :-
    \+ member(e(Classe,_),Esempi). % non ci sono piu' esempi da coprire
apprendi(Esempi,Classe,[Congie|Congi]) :-
    apprendi_cong(Esempi,Classe,Congie),
    rimuovi(Esempi,Congie,RestoEsempi), % rimuove gli esempi utilizzati
    apprendi(RestoEsempi,Classe,Congi). % copre gli esempi restanti

% apprendi_cong(Esempi,Classe,Cong)
% Cong e' una lista di coppie attributo-valore soddisfatti da alcuni
% esempi di Classe e da nessun esempio di un'altra classe
apprendi_cong(Esempi,Classe,[]) :-
    \+ (member(e(ClasseX,_),Esempi), % non ci sono esempi di altre classi
    ClasseX \== Classe),!.
apprendi_cong(Esempi,Classe,[Cond|Conds]) :-
    scegli_cond(Esempi,Classe,Cond), % sceglie attributo-valore
    filtra(Esempi,[Cond],NuoviEsempi), % filtra esempi in base a Cond
    apprendi_cong(NuoviEsempi,Classe,Conds).

scegli_cond(Esempi,Classe,AttVal) :-
    findall(AV/Punti,punti(Esempi,Classe,AV,Punti),AVs),
    best(AVs,AttVal). % trova tutte le coppie Att-Val degli esempi di Classe
                      % assegnandogli un punteggio e sceglie quella migliore

punti(Esempi,Classe,AttVal,Punti) :-
    candidato(Esempi,Classe,AttVal), % un attributo-valore adatto
    filtra(Esempi,[AttVal],NuoviEsempi), % NuoviEsempi che soddisfano Att=Val
    length(NuoviEsempi,N1),
    conta_pos(NuoviEsempi,Classe,Npos), % numero di esempi positivi
    Npos > 0,
    Punti is (Npos + 1) / (N1 + 2).

candidato(Esempi,Classe,Att=Val) :-
    a(Att, Valori), % prende un attributo e i suoi valori
    member(Val,Valori), % controlla che Val sia all'interno di Valori
    adatto(Att=Val,Esempi,Classe). % adatto per classificare Classe

adatto(AttVal,Esempi,Classe) :-
    member(e(ClasseX,OggX),Esempi), % cerca un esempio negativo che non matchi
    ClasseX \== Classe,             % con AttVal
    \+ soddisfa(OggX,[AttVal]),!. % esempio negativo che non matcha

soddisfa(Oggetto,Congiunzione) :-
    \+ (member(Att=Valx,Congiunzione), % soddisfa ha successo se non � vero che
        member(Att=Valy,Oggetto),      % esiste all'interno di congiunzione un
        Valx \== Valy).                % valore Valx associato ad Att che sia
                                       % diverso dal valore Valy associato ad Att
                                       % in Oggetto

best([AttVal/_],AttVal).
best([AV0/S0,AV1/S1|AVSlist],AttVal) :-
    S1 > S0,!, % AV1 � meglio di AV0
    best([AV1/S1|AVSlist],AttVal) % if S1 > S0
    ;
    best([AV0/S0|AVSlist],AttVal). % else if S0 > S1

conta_pos([],_,0).
conta_pos([e(ClasseX,_)|Esempi],Classe,N) :-
    conta_pos(Esempi,Classe,N1),
    (ClasseX=Classe,!,N is N1+1 ; N=N1). % aumenta il contatore N1 se l'esempio
                                         % e' relativo a Classe

filtra(Esempi,Cond,NuoviEsempi) :-
    findall(e(Classe,Ogg),(member(e(Classe,Ogg),Esempi),soddisfa(Ogg,Cond)),NuoviEsempi).

% rimuovi(Esempi,Cong,RestoEsempi)
% rimuove da Esempi quelli coperti da Cong e restituisce Esempi1

rimuovi([],_,[]).
rimuovi([e(_,Ogg)|Es],Conge,Es1) :-
	soddisfa(Ogg,Conge), !, % il primo esempio matcha Conge
	rimuovi(Es,Conge,Es1).  % lo rimuove

rimuovi([E|Es],Conge,[E|Es1]) :- % mantiene il primo esempio
	rimuovi(Es,Conge,Es1).

writelist([]).
writelist([X|L]) :-
	tab(2), writeq(X), nl,
	writelist(L).

% ================Induzione====================

:- dynamic alb/1.

induce_albero( Albero ) :-
	findall( e(Classe,Oggetto), e(Classe,Oggetto), Esempi),
        findall( Att,a(Att,_), Attributi),
        induce_albero( Attributi, Esempi, Albero),
	mostra( Albero ),
	assert(alb(Albero)).

% induce_albero( +Attributi, +Esempi, -Albero):
% l'Albero indotto dipende da questi tre casi:
% (1) Albero = null: l'insieme degli esempi è vuoto
% (2) Albero = l(Classe): tutti gli esempi sono della stessa classe
% (3) Albero = t(Attributo, [Val1:SubAlb1, Val2:SubAlb2, ...]):
%     gli esempi appartengono a più di una classe
%     Attributo è la radice dell'albero
%     Val1, Val2, ... sono i possibili valori di Attributo
%     SubAlb1, SubAlb2,... sono i corrispondenti sottoalberi di
%     decisione.
% (4) Albero = l(Classi): non abbiamo Attributi utili per
%     discriminare ulteriormente
induce_albero( _, [], null ) :- !.			         % (1)
induce_albero( _, [e(Classe,_)|Esempi], l(Classe)) :-	         % (2)
	\+ ( member(e(ClassX,_),Esempi), ClassX \== Classe ),!.  % no esempi di altre classi (OK!!)
induce_albero( Attributi, Esempi, t(Attributo,SAlberi) ) :-	 % (3)
	sceglie_attributo( Attributi, Esempi, Attributo), !,     % implementa la politica di scelta
	del( Attributo, Attributi, Rimanenti ),			 % elimina Attributo scelto
	a( Attributo, Valori ),					 % ne preleva i valori
	induce_alberi( Attributo, Valori, Rimanenti, Esempi, SAlberi).
induce_albero( _, Esempi, l(Classi)) :-                          % finiti gli attributi utili (KO!!)
	findall( Classe, member(e(Classe,_),Esempi), Classi).

% sceglie_attributo( +Attributi, +Esempi, -MigliorAttributo):
% seleziona l'Attributo che meglio discrimina le classi; si basa sul
% concetto della "Gini-disuguaglianza"; utilizza il setof per ordinare
% gli attributi in base al valore crescente della loro disuguaglianza
% usare il setof per far questo è dispendioso e si può fare di meglio ..
sceglie_attributo( Attributi, Esempi, MigliorAttributo )  :-
	setof( Disuguaglianza/A,
	      (member(A,Attributi) , disuguaglianza(Esempi,A,Disuguaglianza)),
	      [_/MigliorAttributo|_] ).

% disuguaglianza(+Esempi, +Attributo, -Dis):
% Dis � la disuguaglianza combinata dei sottoinsiemi degli esempi
% partizionati dai valori dell'Attributo
disuguaglianza( Esempi, Attributo, Dis) :-
	a( Attributo, AttVals),
	somma_pesata( Esempi, Attributo, AttVals, 0, Dis).

% somma_pesata( +Esempi, +Attributo, +AttVals, +SommaParziale, -Somma)
% restituisce la Somma pesata delle disuguaglianze
% Gini = sum from{v} P(v) * sum from{i <> j} P(i|v)*P(j|v)
somma_pesata( _, _, [], Somma, Somma).
somma_pesata( Esempi, Att, [Val|Valori], SommaParziale, Somma) :-
	length(Esempi,N),                                            % quanti sono gli esempi
	findall( C,						     % EsempiSoddisfatti: lista delle classi ..
		 (member(e(C,Desc),Esempi) , soddisfa(Desc,[Att=Val])), % .. degli esempi (con ripetizioni)..
		 EsempiSoddisfatti ),				     % .. per cui Att=Val
	length(EsempiSoddisfatti, NVal),			     % quanti sono questi esempi
	NVal > 0, !,                                                 % almeno uno!
	findall(P,			           % trova tutte le P robabilità
                (bagof(1,		           %
                       member(_,EsempiSoddisfatti),
                       L),
                 length(L,NVC),
                 P is NVC/NVal),
                ClDst),
        gini(ClDst,Gini),
	NuovaSommaParziale is SommaParziale + Gini*NVal/N,
	somma_pesata(Esempi,Att,Valori,NuovaSommaParziale,Somma)
	;
	somma_pesata(Esempi,Att,Valori,SommaParziale,Somma). % nessun esempio soddisfa Att = Val

% gini(ListaProbabilità, IndiceGini)
%    IndiceGini = SOMMATORIA Pi*Pj per tutti i,j tali per cui i\=j
%    E' equivalente a 1 - SOMMATORIA Pi*Pi su tutti gli i
gini(ListaProbabilita,Gini) :-
	somma_quadrati(ListaProbabilita,0,SommaQuadrati),
	Gini is 1-SommaQuadrati.
somma_quadrati([],S,S).
somma_quadrati([P|Ps],PartS,S)  :-
	NewPartS is PartS + P*P,
	somma_quadrati(Ps,NewPartS,S).

% induce_alberi(Attributi, Valori, AttRimasti, Esempi, SAlberi):
% induce decisioni SAlberi per sottoinsiemi di Esempi secondo i Valori
% degli Attributi
induce_alberi(_,[],_,_,[]).     % nessun valore, nessun sottoalbero
induce_alberi(Att,[Val1|Valori],AttRimasti,Esempi,[Val1:Alb1|Alberi])  :-
	attval_subset(Att=Val1,Esempi,SottoinsiemeEsempi),
	induce_albero(AttRimasti,SottoinsiemeEsempi,Alb1),
	induce_alberi(Att,Valori,AttRimasti,Esempi,Alberi).

% attval_subset( Attributo = Valore, Esempi, Subset):
%   Subset è il sottoinsieme di Examples che soddisfa la condizione
%   Attributo = Valore
attval_subset(AttributoValore,Esempi,Sottoinsieme) :-
	findall(e(C,O),(member(e(C,O),Esempi),soddisfa(O,[AttributoValore])),Sottoinsieme).



del(T,[T|C],C) :- !.
del(A,[T|C],[T|C1]) :-
	del(A,C,C1).

mostra(T) :-
	mostra(T,0).
mostra(null,_) :- writeln(' ==> ???').
mostra(l(X),_) :- write(' ==> '),writeln(X).
mostra(t(A,L),I) :-
	nl,tab(I),write(A),nl,I1 is I+2,
	mostratutto(L,I1).
mostratutto([],_).
mostratutto([V:T|C],I) :-
	tab(I),write(V), I1 is I+2,
	mostra(T,I1),
	mostratutto(C,I).


% ================================================================================
% classifica( +Oggetto, -Classe, t(+Att,+Valori))
%  Oggetto: [Attributo1=Valore1, .. , AttributoN=ValoreN]
%  Classe: classe a cui potrebbe appartenere un oggetto caratterizzato da quelle coppie
%  Attributo=Valore
%  t(-Att,-Valori): Albero di Decisione
% presuppone sia stata effettuata l'induzione dell'Albero di Decisione

classifica(Oggetto,nc,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto e' della Classe
	member(Att=Val,Oggetto),  % se Att=Val e' elemento della lista Oggetto
        member(Val:null,Valori). % e Val:null e' in Valori

classifica(Oggetto,Classe,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto e' della Classe
	member(Att=Val,Oggetto),  % se Att=Val e' elemento della lista Oggetto
        member(Val:l(Classe),Valori). % e Val:l(Classe) e' in Valori

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),  % se Att=Val e' elemento della lista Oggetto
	delete(Oggetto,Att=Val,Resto),
	member(Val:t(AttFiglio,ValoriFiglio),Valori),
	classifica(Resto,Classe,t(AttFiglio,ValoriFiglio)).


stampa_matrice_di_confusione :-
	alb(Albero),
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	length(TestSet,N),
	valuta(Albero,TestSet,VN,0,VP,0,FN,0,FP,0,NC,0),
	A is (VP + VN) / (VP+VN+FP+FN), % Accuratezza
	E is 1 - A,		   % Errore
        P is VP / (VP + FN), % Precisione
	write('Test effettuati :'),  writeln(N),
	write('Test non classificati :'),  writeln(NC),
	write('Veri negativi  '), write(VN), write('   Falsi positivi '), writeln(FP),
	write('Falsi negativi '), write(FN), write('   Veri positivi  '), writeln(VP),
	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E),
        write('Precisione: '), writeln(P).


valuta(_,[],VN,VN,VP,VP,FN,FN,FP,FP,NC,NC).            % testset vuoto -> valutazioni finali

valuta(Albero,[qso/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,qso,Albero), !,                 % prevede correttamente Quasar
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
valuta(Albero,[star/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,qso,Albero), !,                 % prevede erroneamente Quasar (con STAR)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
 valuta(Albero,[galaxy/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,qso,Albero), !,                 % prevede erroneamente Quasar (con GALAXY)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).

valuta(Albero,[star/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,star,Albero), !,                 % prevede correttamente Star
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
valuta(Albero,[qso/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,star,Albero), !,                 % prevede erroneamente Star (con QSO)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
 valuta(Albero,[galaxy/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,star,Albero), !,                 % prevede erroneamente Star (con GALAXY)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).

valuta(Albero,[galaxy/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,galaxy,Albero), !,                 % prevede correttamente Galaxy
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
valuta(Albero,[qso/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,galaxy,Albero), !,                 % prevede erroneamente Galaxy (con QSO)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
 valuta(Albero,[star/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
    classifica(Oggetto,galaxy,Albero), !,                 % prevede erroneamente Galaxy (con STAR)
    VNA1 is VNA + 1,
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
