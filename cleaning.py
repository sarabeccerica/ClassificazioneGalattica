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
columns = ['class', 'u', 'g', 'r', 'i', 'z', 'redshift']
clean_df = df[columns]

# Suddividi i dati in training e test
train_data, test_data = train_test_split(clean_df, test_size=0.3)

# Definizione delle fasce per ogni attributo
bins_u = [(-float('inf'), 13.174746), (13.174747, 15.353262), (15.353263, 17.531778),
          (17.531779, 19.710294), (19.710295, 21.888810), (21.888811, 24.067326),
          (24.067327, 26.245842), (26.245843, 28.424358), (28.424359, 30.602874),
          (30.602874, float('inf'))]

bins_g = [(-float('inf'), 12.608604), (12.608605, 14.719009), (14.719010, 16.829414),
          (16.829415, 18.939819), (18.939820, 21.050224), (21.050225, 23.160629),
          (23.160630, 25.271034), (25.271035, 27.381439), (27.381440, 29.491844),
          (29.491845, float('inf'))]

bins_r = [(-float('inf'), 11.801216), (11.801217, 13.775732), (13.775733, 15.750248),
          (15.750249, 17.724764), (17.724765, 19.69928), (19.699281, 21.673796),
          (21.673797, 23.648312), (23.648313, 25.622828), (25.622829, 27.597344),
          (27.597345, float('inf'))]

bins_i = [(-float('inf'), 11.737059), (11.737060, 14.004215), (14.004216, 16.271372),
          (16.271373, 18.538529), (18.538530, 20.805686), (20.805687, 23.072843),
          (23.072844, 25.339999), (25.340000, 27.606156), (27.606157, 29.873313),
          (29.873314, float('inf'))]

bins_z = [(-float('inf'), 11.589474), (11.589475, 13.566615), (13.566616, 15.543756),
          (15.543757, 17.520897), (17.520898, 19.498038), (19.498039, 21.475179),
          (21.475180, 23.452320), (23.452321, 25.429461), (25.429462, 27.406602),
          (27.406603, float('inf'))]


bins_redshift = [(-float('inf'), 0.01999), (0.02, 0.49999), (0.5, float('inf'))]


# Funzione per dividere un valore in fasce
def assign_bin(value, bins):
    for i, (lower, upper) in enumerate(bins):
        if lower <= value < upper:
            return f"{i + 1}"  # Etichetta della fascia (1-based index)
    return "Out of range"  # Se il valore non rientra in nessuna fascia

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
                    f'e({class_label},[u=bin{u_bin}, g=bin{g_bin}, r=bin{r_bin}, i=bin{i_bin}, z=bin{z_bin}, redshift=bin{redshift_bin}]).\n')
            elif filename == 'test_set.pl':
                f.write(
                    f's({class_label},[u=bin{u_bin}, g=bin{g_bin}, r=bin{r_bin}, i=bin{i_bin}, z=bin{z_bin}, redshift=bin{redshift_bin}]).\n')

        for column in columns:
            if filename == 'attributi.pl':
                if column == 'u' or column == 'g' or column == 'r' or column == 'i' or column == 'z':
                    f.write(f'a({column}, [bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10]).\n')
                elif column == 'redshift':
                    f.write(f'a({column}, [bin1, bin2, bin3]).\n')

create_prolog_file_with_bins(train_data, 'training_set.pl', columns)
create_prolog_file_with_bins(test_data, 'test_set.pl', columns)
create_prolog_file_with_bins(test_data, 'attributi.pl', columns)