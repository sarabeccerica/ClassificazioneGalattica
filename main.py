from pyswip import Prolog
from pyswip import Functor
from tkinter import ttk
import tkinter as tk
import time
import numpy as np
import pandas as pd
import os

FOLDER_PATH = "C:\Users\matte\OneDrive\Desktop\Università 2\1 - Intelligenza Artificiale (Dragoni)\Stellar Classification\Classificazione Galattica"
INDUCTION_FILE = "tree_induction_entropy.pl"

def switchPath(filename):
    with open(os.getcwd() + '/Apprendimento_QSG/' + filename, 'r') as file, open(os.getcwd() + '/Apprendimento_QGS/' + filename + '.tmp', 'w') as file_temp:
        # Itera ogni riga del file
        for line in file:
            # Controlla se la riga inizia con 'file_output' e se necessario la sostituisce
            if line.startswith('file_output'):
                line = "file_output('"+os.getcwd() + "/Apprendimento_QSG/albero.pl').\n"
            # Scrive la riga nel file temporaneo
            file_temp.write(line)

    # Rimuove il file originale
    os.remove(os.getcwd()+'/Apprendimento_QSG/'+filename)
    # Rinomina
    os.rename(os.getcwd()+'/Apprendimento_QSG/'+filename + '.tmp', os.getcwd()+'/Apprendimento_QSG/'+filename)

def on_combobox_change():
    selected = combobox.get()
    if selected == "Gini": INDUCTION_FILE = "tree_induction_gini.pl"
    else: INDUCTION_FILE = "tree_induction_entropy.pl"
    print(f"Hai selezionato: {INDUCTION_FILE}")

def format_value(value):
    output = ""
    if isinstance(value, list):
        output = "[ " + ", ".join([format_value(val) for val in value]) + " ]"
    elif isinstance(value, Functor) and value.arity == 2:
        output = "{0}{1}{2}".format(value.args[0], value.name, value.args[1])
    else:
        output = "{}".format(value)
    return output    

def format_result(result):
    result = list(result)
    if len(result) == 0:
        return "false."
    if len(result) == 1 and len(result[0]) == 0:
        return "true."
    output = ""
    for res in result:
        tmpOutput = []
        for var in res:
            tmpOutput.append(var + " = " + format_value(res[var]))
        output += ", ".join(tmpOutput) + " ;\n"
    output = output[:-3] + " ."
    return output

def interroga():
    switchPath('tree_induction_entropy.pl')
    switchPath('tree_induction_gini.pl')
    prolog = Prolog()
    #Determino i valori inseriti dall'utente
    values = [entry.get() for entry in entry_widgets]
    values[5] = str(float(values[5]) / 100 )#Normalizzo la percentuale
    #Carico il file da consultare
    prolog.consult(FOLDER_PATH+"Apprendimento_QSG/"+INDUCTION_FILE)
    #Costruisco la query ricavandomi il tier
    query = "["
    for chiave, user_input in zip(attributi_dict.keys(), values):
        if len(user_input) == 0: user_input = 0
        tier_result = get_tier_for_value(chiave, attributi_dict[chiave], float(user_input))
        print(f"Il tier corrispondente per "+chiave+" è: "+tier_result)
        query = query+chiave+"="+tier_result+","
    query = query[:-1]
    query=query+"]"
    print(query)

    print("Inizio l'apprendimento dei quasar")
    tempo_inizio=time.time()
    answer = prolog.query("lancia_apprendi(QSO).")
    print(format_result(answer))
    tempo_fine=time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

    print("Inizio l'apprendimento delle stelle")
    tempo_inizio = time.time()
    answer = prolog.query("lancia_apprendi(STAR).")
    print(format_result(answer))
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

    print("Inizio l'apprendimento delle galassie")
    tempo_inizio = time.time()
    answer = prolog.query("lancia_apprendi(GALAXY).")
    print(format_result(answer))
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")

    print("Interrogo il programma")
    tempo_inizio = time.time()
    answer = prolog.query(("classifica_oggetto(" + query + ", Classe)."))
    print(format_result(answer))
    tempo_fine = time.time()
    tempo_totale = tempo_fine - tempo_inizio
    print(f"Tempo totale di esecuzione: {tempo_totale} secondi")





root = tk.Tk()
root.title("Apprendimento intelligente QSG")
labels = ["Classe", "U", "G", "R", "I", "Z", "Redshift"]

entry_widgets = []

for i, label_text in enumerate(labels):
    label = tk.Label(root, text=label_text, font=("Arial", 12))
    label.grid(row=i, column=0, padx=10, pady=5, sticky="w")
    entry = tk.Entry(root, width=10, font=("Arial", 12))
    entry.grid(row=i, column=1, padx=10, pady=5)
    entry_widgets.append(entry)

button = tk.Button(root, text="Costruisci la query", command=interroga, font=("Arial", 12))
button.grid(row=len(labels), column=1, pady=10)

label=tk.Label(root, text="Metodo di induzione:", font=("Arial",12))
label.grid(padx=5, pady=5, sticky="w")
combobox = ttk.Combobox(root, values=["Entropia", "Gini"])
combobox.current(0)
combobox.bind("<<ComboboxSelected>>", on_combobox_change)
combobox.grid(column=0, padx=5, pady=5, sticky="w")

root.mainloop()
