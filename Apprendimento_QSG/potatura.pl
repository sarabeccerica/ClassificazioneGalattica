:- ensure_loaded(attributi).
:- ensure_loaded(test_set).
:- ensure_loaded(training_set).
:- ensure_loaded(albero).
% operatore personalizzato
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
% Gini o dell'entropia, nella propria workspace.
%
% Il programma prima calcola tutti i sotto-alberi procedendo in
% profondita'. Successivamente, il programma scorre la lista dei
% sotto-alberi e rimuove ognuno di questi dall'albero originale,
% mantenendo la potatura solo se questa ha migliorato le prestazioni,
% valutate con il test del chi-quadro. Infine, viene mostrata la matrice
% di confusione finale dell'albero potato.