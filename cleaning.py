import os
import pandas as pd
from sklearn.model_selection import train_test_split # type: ignore

# Carica il dataset
df = pd.read_csv('star_classification.csv', nrows=1000)

# Sostituisce -9999 con NaN (una riga contiene valori -9999)
df.replace(-9999, pd.NA, inplace=True)

# Rimuove le righe che contiene NaN
df.dropna(inplace=True)

# Seleziona le colonne desiderate
columns = ['alpha','delta','class', 'u', 'g', 'r', 'i', 'z', 'redshift']
clean_df = df[columns]

min_count = clean_df['class'].value_counts().min()

# Campiona un numero uguale di elementi per ogni classe
balanced_df = pd.concat([
    clean_df[clean_df['class'] == 'QSO'].sample(n=min_count, random_state=42),
    clean_df[clean_df['class'] == 'STAR'].sample(n=min_count, random_state=42),
    clean_df[clean_df['class'] == 'GALAXY'].sample(n=min_count, random_state=42)
])

# Suddividi i dati in training e test in modo stratificato
train_data, test_data = train_test_split(
    balanced_df,
    test_size=0.3,
    stratify=balanced_df['class'],
    random_state=42
)
# Definizione delle fasce per ogni attributo

bins_alpha = [(0,60),(60,120),(120,180),(180,240),(240,300),(300,360)]

bins_delta = [(-90,-60),(-60,-30),(-30,-1),(-0.9999999,30),(30,60),(60,90)]

bins_u = [(-float('inf'), 15.353262), (15.353263, 19.710294), (19.710295, 24.067326),
          (24.067327, 28.424358), (28.424359, float('inf'))]

bins_g = [(-float('inf'), 14.719009), (14.719010, 18.939819), (18.939820, 23.160629),
          (23.160630, 27.381439), (27.381440, float('inf'))]

bins_r = [(-float('inf'), 13.775732), (13.775733, 17.724764), (17.724765, 21.673796),
          (21.673797, 25.622828), (25.622829, float('inf'))]

bins_i = [(-float('inf'), 14.004215), (14.004216, 18.538529), (18.538530, 23.072843),
          (23.072844, 27.606156), (27.606157, float('inf'))]

bins_z = [(-float('inf'), 13.566615), (13.566616, 17.520897), (17.520898, 21.475179),
          (21.475180, 25.429461), (25.429462, float('inf'))]


bins_redshift = [(-float('inf'), 0.01999), (0.02, 0.49999), (0.5, float('inf'))]


# Funzione per dividere un valore in fasce
def assign_bin(value, bins):
    for i, (lower, upper) in enumerate(bins):
        if lower <= value <= upper:
            return f"{i + 1}"  # Etichetta della fascia (1-based index)
    return f"Out of range: {value}"  # Se il valore non rientra in nessuna fascia

def asign_class(value):
    if value == "QSO":
        return "qso"
    elif value == "STAR":
        return "star"
    elif value == "GALAXY":
        return "galaxy"

# Funzione per creare il file Prolog con le fasce
def create_prolog_file_with_bins(data, filename, columns):

    with open(filename, 'w') as f:
        for index, row in data.iterrows():
            # Assegna le fasce per ogni colonna
            alpha_bin = assign_bin(row['alpha'], bins_alpha)
            delta_bin = assign_bin(row['delta'], bins_delta)            
            u_bin = assign_bin(row['u'], bins_u)
            g_bin = assign_bin(row['g'], bins_g)
            r_bin = assign_bin(row['r'], bins_r)
            i_bin = assign_bin(row['i'], bins_i)
            z_bin = assign_bin(row['z'], bins_z)
            redshift_bin = assign_bin(row['redshift'], bins_redshift)

            # Classe dell'oggetto
            class_label = asign_class(row['class'])

            # Scrivi nel file Prolog con il formato richiesto
            if filename == 'training_set.pl':
                f.write(        
                    f'e({class_label}, [alpha=bin{alpha_bin},delta=bin{delta_bin},u=bin{u_bin},g=bin{g_bin},r=bin{r_bin},i=bin{i_bin},z=bin{z_bin},redshift=bin{redshift_bin}]).\n')
            elif filename == 'test_set.pl':
                f.write(
                    f's({class_label}, [alpha=bin{alpha_bin},delta=bin{delta_bin},u=bin{u_bin},g=bin{g_bin},r=bin{r_bin},i=bin{i_bin},z=bin{z_bin},redshift=bin{redshift_bin}]).\n')

        for column in columns:
            if filename == 'attributi.pl':
                if column == 'u' or column == 'g' or column == 'r' or column == 'i' or column == 'z':
                    f.write(f'a({column}, [bin1,bin2,bin3,bin4,bin5]).\n')
                elif column == 'alpha' or column == 'delta':
                    f.write(f'a({column}, [bin1,bin2,bin3,bin4,bin5,bin6]).\n')
                elif column == 'redshift':
                    f.write(f'a({column}, [bin1,bin2,bin3]).\n')

create_prolog_file_with_bins(train_data, 'training_set.pl', columns)
create_prolog_file_with_bins(test_data, 'test_set.pl', columns)
create_prolog_file_with_bins(test_data, 'attributi.pl', columns)