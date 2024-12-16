:- ensure_loaded(attributi).
:- ensure_loaded(training_set).
:- ensure_loaded(test_set).
% operatore personalizzato
:- op(300, xfx, <==).

% Questo programma Prolog è utilizzato per l'apprendimento automatico
% per la classificazione di corpi celesti in Quasar, Stelle o Galassie.
% Con il predicato lancia_apprendi(Classe) (Classe = Quasar/Stelle/Galassie),
% viene lanciato l'apprendimento relativo alla classe specificata.
% Una volta concluso l'apprendimento, con il predicato
% classifica_oggetto e' possibile far classificare al programma
% l'oggetto specificato.
% Inoltre, con il predicato lancia_induzione, il programma calcola
% l'albero di decisione con il criterio di Gini.
% Con il predicato stampa_matrice_di_confusione e' possibile
% visualizzare le prestazioni dell'albero indotto.

% Cambiare percorso con quello della propria workspace
file_output('C:\Users\matte\OneDrive\Desktop\Università 2\1 - Intelligenza Artificiale (Dragoni)\Stellar Classification\Classificazione Galattica\Apprendimento_QSG\albero.pl').

% Predicato per lanciare l'apprendimento
lancia_apprendi(Classe) :-
    file_output(NomeFile),
    tell(NomeFile),
    apprendi(Classe),
    told.