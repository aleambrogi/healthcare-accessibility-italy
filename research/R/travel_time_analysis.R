
# 📦 Librerie
library(ggplot2)
library(readr)
library(sf)
library(dplyr)

# 🗂️ Percorso del file principale (modifica se necessario)
file_path <- "C://Users//samsung//Documents//MAGISTRALE//PRIMO ANNO//APPLIED STATISTICS//GRINS Indice SSN//Scripts//DATASET_DIST//points_with_people.gpkg"

# 🔁 --- Caricamento dati
if (grepl("\\.csv$", file_path)) {
  dati <- read_csv(file_path)
} else if (grepl("\\.gpkg$", file_path)) {
  dati <- st_read(file_path)
} else {
  stop("Formato non supportato. Usa .csv o .gpkg")
}

# ✅ --- Controllo colonne
if (!all(c("grid_SIGLA_PROVINCIA", "travel_time_min") %in% colnames(dati))) {
  stop("Assicurati che il file contenga le colonne 'grid_SIGLA_PROVINCIA' e 'travel_time_min'")
}

# 🧹 --- Pulizia dei dati
dati_clean <- dati %>%
  filter(!is.na(grid_SIGLA_PROVINCIA), !is.na(travel_time_min))

# 🔗 --- Unione dei dati con la colonna 'gruppo' al dataframe principale senza geometria
dati_clean_no_geom <- st_drop_geometry(dati_clean)  # Rimuove la geometria

# Unione dei dati (colonna 'gruppo' è già stata calcolata)
dati_clean_joined <- left_join(dati_clean_no_geom, province_ordinate, by = "grid_SIGLA_PROVINCIA")

# 📊 --- Funzione per creare il boxplot
plot_box <- function(df, titolo) {
  ggplot(df, aes(x = reorder(as.factor(grid_SIGLA_PROVINCIA), travel_time_min, FUN = median), 
                 y = travel_time_min)) +
    geom_boxplot(fill = "#FF9800", color = "black", outlier.color = "red", outlier.size = 1.2) +
    coord_flip() +
    theme_minimal(base_size = 10) +
    labs(
      title = titolo,
      x = "PROVINCE",
      y = "TRAVEL TIMES (minutes)"
    )
}

# 🎨 --- Stampa i 3 grafici separati
for (i in 1:3) {
  df_g <- dati_clean_joined %>% filter(gruppo == i)
  print(plot_box(df_g, paste("Boxplot Group", i)))
}

# ─────────────────────────────────────────────────────
# 🗺️ MAPPA STATICA – Tempo Mediano per Provincia
# ─────────────────────────────────────────────────────

# 📁 Carica shapefile delle province (modifica il percorso e nome colonna se necessario)
province_sf <- st_read("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Datasets\\ISTAT shape file\\Limiti2021\\ProvCM2021\\ProvCM2021.shp")

# 🛠️ Correzione delle geometrie non valide (ripara le geometrie)
province_sf <- st_make_valid(province_sf)

# 🔄 Trasforma il CRS dei dati in modo che entrambi abbiano lo stesso sistema di riferimento spaziale
# Assicurati di scegliere il CRS corretto. Qui uso EPSG:4326 come esempio.
province_sf <- st_transform(province_sf, crs = 4326)

# 🔗 Join spaziale: assegna tempo mediano a ciascuna provincia
mappa_sf <- st_join(province_sf, province_ordinate, join = st_intersects)

# 🖼️ Plot della mappa statica
mappa_plot <- ggplot(mappa_sf) +
  geom_sf(aes(fill = mediana), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "MEDIAN TRAVEL TIME",
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10)
  )

# 👁️ Visualizza
print(mappa_plot)

# 💾 Salva per slide/report
ggsave("mappa_statica_tempi.png", mappa_plot, width = 10, height = 7, dpi = 300)


##--------------------------------------------------------------------------------
# 📦 Librerie (già caricate prima)
library(ggplot2)
library(dplyr)
library(viridis)
library(sf)

# 📉 Sample del dataset (es. 10.000 righe o il 10%)
set.seed(123)  # Per riproducibilità
dati_sample <- dati_clean %>% 
  sample_frac(0.1)  # oppure: sample_n(10000)

# 🧮 Calcolo della difficoltà sul campione
dati_con_popolazione <- dati_sample %>%
  mutate(
    difficolta = log1p(travel_time_min * (Y_LT15 + 0.8 * Y1564 + Y_GE65)),
    grid_SIGLA_PROVINCIA = as.character(grid_SIGLA_PROVINCIA)
  )

# 📉 Soglia + percentile sul sample
soglia_difficolta <- mean(dati_con_popolazione$difficolta, na.rm = TRUE) +
  2 * sd(dati_con_popolazione$difficolta, na.rm = TRUE)

percentile_99 <- quantile(dati_con_popolazione$difficolta, 0.99, na.rm = TRUE)

# 🛠️ Colonna coerente anche nello shapefile
province_sf <- province_sf %>%
  mutate(grid_SIGLA_PROVINCIA = as.character(grid__NCIA))

# 🔗 Join geometrie + dati del sample
dati_sf_con_difficolta <- province_sf %>%
  left_join(
    st_drop_geometry(dati_con_popolazione),
    by = "grid_SIGLA_PROVINCIA"
  ) %>%
  mutate(
    difficolta_limited = pmin(difficolta, percentile_99)
  )

# 🎨 Mappa finale
mappa_difficolta <- ggplot(dati_sf_con_difficolta) +
  geom_sf(aes(fill = difficolta_limited), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Difficulty Index",
    option = "magma",
    limits = c(min(dati_sf_con_difficolta$difficolta_limited, na.rm = TRUE), percentile_99),
    na.value = "grey90"
  ) +
  labs(
    title = "Difficulty Index by Province (Sample)",
    subtitle = paste("Limited to 99th percentile. Threshold >", round(soglia_difficolta, 2)),
    caption = "Difficulty = log(1 + Travel Time × Weighted Population)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(mappa_difficolta)

ggsave("mappa_indice_difficolta.png", mappa_difficolta, width = 10, height = 7, dpi = 300, limitsize = FALSE)



























# Calcola il prodotto tra Popolazione Totale e Tempi di Percorrenza
dati_clean <- dati_clean %>%
  mutate(pop_time_product = TOT_P_2021 * travel_time_min)

# 📊 --- Scatter plot tra Popolazione Totale, Tempi di Percorrenza e colore in base al prodotto
ggplot(dati_clean, aes(x = TOT_P_2021, y = travel_time_min, color = pop_time_product)) +
  geom_point(alpha = 0.7) +  # Aggiungi i punti con trasparenza
  scale_x_continuous(labels = scales::comma) +  # Separatore delle migliaia sull'asse X
  scale_y_continuous(labels = scales::comma) +  # Separatore delle migliaia sull'asse Y
  scale_color_viridis_c(option = "plasma", name = "Pop × Time") +  # Scala di colori per il prodotto
  labs(
    title = "Scatter Plot: Total Population vs Travel Time",
    x = "Total Population (2021)",
    y = "Travel Time (minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 12),  # Aumenta la dimensione del titolo della legenda
    legend.text = element_text(size = 8),   # Aumenta la dimensione del testo della legenda
    legend.key.size = unit(2, "lines")     # Aumenta la dimensione dei "key" della legenda
  )



