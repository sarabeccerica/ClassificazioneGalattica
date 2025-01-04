from pyswip import Prolog
from pyswip import Functor
from tkinter import ttk
import tkinter as tk
import time
import numpy as np
import pandas as pd
import os
from cleaning import assign_bin
from cleaning import bins_alpha
from cleaning import bins_delta
from cleaning import bins_u
from cleaning import bins_g
from cleaning import bins_r
from cleaning import bins_i
from cleaning import bins_z
from cleaning import bins_redshift

INDUCTION_FILE = "tree_induction_entropy.pl"

def on_combobox_change(event):
    global INDUCTION_FILE
    selected = combobox.get()
    INDUCTION_FILE = f"tree_induction_{selected.lower()}.pl"
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
    prolog = Prolog()

    # Determina i valori inseriti dall'utente
    values = [entry.get() for entry in entry_widgets]
    if not os.path.isfile(INDUCTION_FILE):
        print(f"Errore: Il file Prolog {INDUCTION_FILE} non esiste!")
        return

    try:
        prolog.consult(INDUCTION_FILE)
    except Exception as e:
        print(f"Errore nel consultare il file Prolog: {e}")
        return

    # Costruisco la query direttamente usando i valori dell'utente
    query = "["
    for chiave, user_input in zip(attributi_dict.keys(), values):
        if len(user_input) == 0:  # Gestione di input vuoto
            user_input = "0"  # Predefinisci un valore per sicurezza

        # Determina il tier corrispondente per l'attributo
        if chiave == "alpha":
            bin = assign_bin(float(user_input), bins_alpha)
        elif chiave == "delta":
            bin = assign_bin(float(user_input), bins_delta)
        elif chiave == "u":
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
        try:
            answer = list(prolog.query(f"lancia_apprendi({cat})."))

            print(f"Risultati per {cat}: {format_result(answer)}")
        except Exception as e:
            print(f"Errore durante l'apprendimento per {cat}: {e}")

        tempo_fine = time.time()
        print(f"Tempo totale per {cat}: {tempo_fine - tempo_inizio:.2f} secondi")

    print(list(prolog.query("lancia_induzione(Albero).")))
    print("Classificazione dell'oggetto...")
    tempo_inizio = time.time()
    try:
        answer = list(prolog.query("classifica_oggetto("+query+",Classe)."))
        if not answer:
            print("Errore: Nessuna risposta ricevuta dalla query Prolog.")
            return

        print(f"Risultati per {query} : {format_result(answer)}")
    except Exception as e:
        print(f"Errore durante la classificazione di {query}: {e}")

    tempo_fine = time.time()
    print(f"Tempo totale di esecuzione: {tempo_fine - tempo_inizio:.2f} secondi")

root = tk.Tk()
root.title("Apprendimento intelligente QSG")
labels = ["ALPHA","DELTA","U", "G", "R", "I", "Z", "Redshift"]

entry_widgets = []

attributi_dict = {
    'alpha': 6,
    'delta': 6,
    'u': 5,
    'g': 5,
    'r': 5,
    'i': 5,
    'z': 5,
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
