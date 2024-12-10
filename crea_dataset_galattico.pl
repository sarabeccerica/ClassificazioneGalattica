/*

METADATI DEL DATASET

a(obj_ID,alpha,delta,u,g,r,i,z,run_ID,rerun_ID,cam_col,field_ID,
  spec_obj_ID,class,redshift,plate,MJD,fiber_ID)


obj_ID     : valore unico che identifica gli oggetti nel dataset
alpha      : angolo di ascensione retta (coordinate celesti)
delta      : angolo di declinazione (coordinate celesti)
u          : valore numerico del filtro ultravioletto del sistema
             fotometrico
g          : valore numerico del filtro verde del sistema fotometrico
r          : valore numerico del filtro rosso del sistema
             fotometrico
i          : valore numerico del filtro del vicino infrarosso del
             sistema fotometrico
z          : valore numerico del filtro infrarosso del sistema
             fotometrico
run_ID     : numero del ciclo di scansioni, utilizzato per identificare
             una scansione specifica
rerun_ID   : numero di rielaborazione, specifica come è stata processata
             l'immagine
cam_col    : colonna della camera, utilizzata per identificare la
             linea di scansione all'interno del ciclo
field_ID   : numero del campo, utilizzato per identificare ogni singolo
             campo osservato
spec_obj_ID: identificatore univoco per oggetti spetroscopici ottici
class      : classe dell'oggetto (galassia, stella o quasar)
redshift   : valore del redshift, ovvero lo spostamento verso il rosso
             della lunghezza d'onda
plate      : identificatore della piastra
MJD        : data giuliana modificata
fiber_ID   : identificatore della fibra ottica

Esempio:
a(1237660961327743232,135.6891066036,32.4946318397087,23.87882,22.2753,20.39501,
  19.16573,18.79371,3606,301,2,79,6543777369295181824,"GALAXY",0.6347936,5812,
  56354,171)
*/

:- ensure_loaded(star_classification).

/*
Il campo "obj_ID" è univoco, quindi inutile per l'analisi e verrà
eliminato.

I campi "alpha", "delta", "MJD", "plate", "cam_col", "field_ID",
"spec_obj_ID" e "fiber_ID" non sono utili al fine della classificazione.

Il campo "rerun_ID" contiene un solo valore per tutte le righe, quindi
verrà eliminato.

Il campo "redshift" ha valori con la virgola ed è troppo specifico.
Per questo motivo verranno definite tre classi:
*   : (< 0,01)          per Stelle
**  : (>= 0,01 e < 0,5) per Quasar
*** : (>= 0,5)          per Galassie

I valori dei campi "u","g","r","i","z" verranno suddivisi in 5 fasce
per semplificazione.

*/


start :-
    tell('star_classification_database.pl'),
    a(_,_,_,u,g,r,i,z,_,_,_,_,_,class,redshift,_,_,_),
    write('aa('), write(





