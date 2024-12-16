from pyswip import Prolog
from pyswip import Functor
from tkinter import ttk
import tkinter as tk
import time
import numpy as np
import pandas as pd
import os

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


def interroga():
    switchPath('tree_induction_entropy.pl')
    switchPath('tree_induction_gini.pl')
    prolog = Prolog()

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
