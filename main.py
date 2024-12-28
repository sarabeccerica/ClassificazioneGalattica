from pyswip import Prolog
from pyswip import Functor
from tkinter import ttk
import tkinter as tk
import time
import numpy as np
import pandas as pd
import os
from cleaning import assign_bin
from cleaning import bins_u
from cleaning import bins_g
from cleaning import bins_r
from cleaning import bins_i
from cleaning import bins_z
from cleaning import bins_redshift

FOLDER_PATH = "C:/Users/matte/OneDrive/Desktop/ClassificazioneGalattica/"
INDUCTION_FILE = "tree_induction_entropy.pl"

def switchPath(filename):
    input_path = FOLDER_PATH + "Apprendimento_QSG/" + filename
    temp_path = FOLDER_PATH + "Apprendimento_QSG/" + filename + ".tmp"
    print("Input Path:", input_path)
    print("Temp Path:", temp_path)
    # Apertura dei file di input e temporaneo
    with open(input_path, 'r') as file, open(temp_path, 'w') as file_temp:
        # Itera ogni riga del file
        for line in file:
            # Controlla se la riga inizia con 'file_output' e, se necessario, la sostituisce
            if line.startswith('file_output'):
                line = f"file_output('{FOLDER_PATH}Apprendimento_QSG/albero.pl').\n"
            file_temp.write(line)

    # Rimuove il file originale e rinomina il file temporaneo
    os.remove(input_path)
    os.rename(temp_path, input_path)

def on_combobox_change(event):
    global INDUCTION_FILE
    selected = combobox.get()
    if selected == "Gini":
        INDUCTION_FILE = "tree_induction_gini.pl"
    else:
        INDUCTION_FILE = "tree_induction_entropy.pl"
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
   
    # Determina i valori inseriti dall'utente
    values = [entry.get() for entry in entry_widgets]
    path = FOLDER_PATH + 'Apprendimento_QSG/'+ INDUCTION_FILE
    query_consult = f"consult('{path}')"
    print(f"Consulting Prolog file: {query_consult}")
    prolog.query(query_consult)

    # Costruisco la query direttamente usando i valori dell'utente
    query = "["
    for chiave, user_input in zip(attributi_dict.keys(), values):
        if len(user_input) == 0:  # Gestione di input vuoto
            user_input = "0"  # Predefinisci un valore per sicurezza

        # Determina il tier corrispondente per l'attributo
        if chiave == "u":
            bin = assign_bin(float(user_input), bins_u)
        elif chiave == "g":
            bin = assign_bin(float(user_input), bins_g)
        elif chiave == "r":
            bin = assign_bin(float(user_input), bins_r)
        elif chiave == "i":
            bin = assign_bin(float(user_input), bins_i)
        elif chiave == "z":
            bin = assign_bin(float(user_input), bins_z)
        elif chiave == "redshift":
            bin = assign_bin(float(user_input), bins_redshift)


        query += f"{chiave}=bin{bin},"  # Appendo l'attributo e il valore
    query = query.rstrip(",") + "]"  # Rimuovo la virgola finale e chiudo

    print(f"Query costruita: {query}")

    # Fasi di apprendimento
    categorie = ["qso", "star", "galaxy"]
    for cat in categorie:
        print(f"Inizio l'apprendimento per la classe {cat}")
        tempo_inizio = time.time()
        answer = prolog.query(f"lancia_apprendi({cat}).")
        print(format_result(answer))
        tempo_fine = time.time()
        print(f"Tempo totale per {cat}: {tempo_fine - tempo_inizio:.2f} secondi")

    print("Classificazione dell'oggetto...")
    tempo_inizio = time.time()
    answer = prolog.query(("classifica_oggetto(" + query + ", Classe)."))
    print(format_result(answer))
    tempo_fine = time.time()
    print(f"Tempo totale di esecuzione: {tempo_fine - tempo_inizio:.2f} secondi")

root = tk.Tk()
root.title("Apprendimento intelligente QSG")
labels = ["U", "G", "R", "I", "Z", "Redshift"]

entry_widgets = []

attributi_dict = {
    'u': 10,
    'g': 10,
    'r': 10,
    'i': 10,
    'z': 10,
    'redshift': 3
}

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
