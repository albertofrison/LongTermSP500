# ==============================================================================
# PROGETTO: REPORT FINANZIARIO S&P 500 TR - ANALISI DI CONVERGENZA
# PERIODO DI INVESTIMENTO: 1993 - 2024 (Base calcolo dal 1992)
# OBIETTIVO: Analisi delle performance storiche, mitigazione del rischio nel tempo e generazione asset per Carousel LinkedIn.
# ==============================================================================

# Reset completo dell'ambiente di lavoro e della console visiva
cat("\014")
rm(list = ls())

# --- FASE 0: CARICAMENTO DELLE LIBRERIE ---
library(quantmod)   # Scaricamento dati finanziari da Yahoo Finance
library(dplyr)      # Manipolazione e trasformazione dati
library(tidyr)      # Rimodellamento matrici (pivot e gestione NA)
library(lubridate)  # Gestione avanzata delle componenti temporali (date/anni)
library(ggplot2)    # Motore grafico per visualizzazioni professionali
library(gridExtra)  # Conversione di tabelle in oggetti grafici (grob)
library(grid)       # Gestione del layout grafico di basso livello

# --- FASE 1: ACQUISIZIONE E PREPARAZIONE DATI ---
ticker_target <- "^SP500TR"
data_inizio   <- "1900-01-01"

message("Download dei dati storici S&P 500 TR...")
sp500_xts <- getSymbols(ticker_target, src = "yahoo", from = data_inizio, auto.assign = FALSE)

# Estrazione dell'ultimo prezzo di chiusura per ogni anno solare
sp500_annuale <- data.frame(Date = index(sp500_xts), coredata(sp500_xts)) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Price = last(SP500TR.Close)) %>%
  ungroup()

# --- FASE 2: ELABORAZIONE MATRICE DEI RENDIMENTI (CAGR) ---
anni_report <- 1989:2024

dati_triangolo <- expand_grid(
  StartYear = anni_report,
  EndYear = anni_report
) %>%
  # Filtro per mantenere solo le combinazioni cronologicamente valide
  filter(EndYear >= StartYear) %>%
  # Calcolo dell'orizzonte effettivo di detenzione (es. inizio 1993 a fine 1993 = 1 anno)
  mutate(
    HorizonNum = EndYear - StartYear + 1,
    BaseYear   = StartYear - 1
  ) %>%
  # Associazione prezzo di acquisto (Chiusura dell'anno precedente)
  left_join(sp500_annuale, by = c("BaseYear" = "Year")) %>%
  rename(Price_Start = Price) %>%
  # Associazione prezzo di vendita (Chiusura dell'anno finale)
  left_join(sp500_annuale, by = c("EndYear" = "Year")) %>%
  rename(Price_End = Price) %>%
  # Calcolo del CAGR arrotondato per la visualizzazione in Heatmap
  mutate(
    Return = round(((Price_End / Price_Start)^(1 / HorizonNum) - 1) * 100, 0),
    Horizon = paste0(HorizonNum, if_else(HorizonNum == 1, " Anno", " Anni"))
  )

# --- FASE 3: GENERAZIONE TABELLA TRIANGOLARE PER DISPOSITIVO GRAFICO ---
tabella_wide <- dati_triangolo %>%
  arrange(HorizonNum) %>%
  select(Horizon, StartYear, Return) %>%
  pivot_wider(names_from = StartYear, values_from = Return) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ replace_na(., "")))

# Configurazione del tema grafico per inserire la tabella nel PDF Carousel
tema_tabella <- ttheme_default(
  base_size = 5.5, 
  core = list(bg_params = list(fill = c("#F8FAFC", "#FFFFFF"), col = "#CBD5E1")),
  colhead = list(bg_params = list(fill = "#0F172A"), fg_params = list(col = "white", fontface = "bold"))
)
grob_tabella <- tableGrob(tabella_wide, theme = tema_tabella)

# Stampa di controllo rapido della tabella nella console di R
message("\n--- STRUTTURA MATRICE DEI RENDIMENTI ANNUALIZZATI (CAGR %) ---")
print(knitr::kable(tabella_wide, format = "simple", align = "l"))

# --- FASE 4: GRAFICO 1 - HEATMAP DEI RENDIMENTI ---
etichette_y <- paste0(1:36, if_else(1:36 == 1, " Anno", " Anni"))

plot_heatmap <- ggplot(data = dati_triangolo, aes(x = factor(StartYear), y = HorizonNum)) +
  geom_tile(aes(fill = Return), color = "white", linewidth = 0.2) +
  geom_text(aes(label = paste0(Return, "%")), size = 2.5, color = "#1E293B", fontface = "bold") +
  scale_y_reverse(breaks = 1:36, labels = etichette_y) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient2(low = "#EF4444", mid = "#F8FAFC", high = "#0D9488", midpoint = 9) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Rendimenti Annualizzati dell'Indice S&P 500 (Total Return)",
    subtitle = "Performance annuale media di un investimento lump sum (PIC) per anno di ingresso",
    x = "Anno di Inizio Investimento",
    y = "Orizzonte Temporale",
    fill = "CAGR %",
    caption = "Elaborazione dati storici - Fonte: Yahoo Finance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#0F172A"),
    plot.subtitle = element_text(size = 10, color = "#475569"),
    axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank(), 
    legend.position = "right"
  )

# --- FASE 5: GRAFICO 2 - IMPATTO RISCHIO / RENDIMENTO ---
metriche_orizzonte <- dati_triangolo %>%
  filter(EndYear <= 2020) %>%
  group_by(HorizonNum) %>%
  summarise(
    Rendimento_Medio = mean(Return),
    Deviazione_Standard = sd(Return)
  ) %>%
  ungroup() %>%
  mutate(Deviazione_Standard = replace_na(Deviazione_Standard, 0))

metriche_long <- metriche_orizzonte %>%
  pivot_longer(
    cols = c(Rendimento_Medio, Deviazione_Standard),
    names_to = "Indicatore",
    values_to = "Valore"
  )

plot_rischio_rendimento <- ggplot(data = metriche_long, aes(x = HorizonNum, y = Valore, color = Indicatore)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(1, max(metriche_long$HorizonNum), by = 5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(-5, 40, by = 5)) +
  scale_color_manual(
    values = c("Rendimento_Medio" = "#0D9488", "Deviazione_Standard" = "#EF4444"),
    labels = c("Rendimento_Medio" = "Rendimento Annuo Medio (CAGR)", "Deviazione_Standard" = "Rischio (Deviazione Standard)")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "L'Impatto del Tempo su Rischio e Rendimento (S&P 500 TR)",
    subtitle = "All'allungarsi dell'orizzonte temporale il rendimento medio si stabilizza e la volatilità scende",
    x = "Anni di Detenzione dell'Investimento",
    y = "Valore Percentuale (%)",
    caption = "Serie storica bloccata a 32 anni per avere almeno 5 osservazioni",
    color = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#0F172A"),
    plot.subtitle = element_text(size = 10, color = "#475569"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# --- FASE 6: GRAFICO 3 - CONVERGENZA STORICA (RENDIMENTI REALI) ---
anni_target <- 1989:2000

convergenza_esatta <- expand_grid(
  StartYear = anni_target,
  EndYear = 1989:2024
) %>%
  filter(EndYear >= StartYear) %>%
  mutate(
    HorizonNum = EndYear - StartYear + 1,
    BaseYear   = StartYear - 1
  ) %>%
  filter(HorizonNum <= 25) %>% 
  left_join(sp500_annuale, by = c("BaseYear" = "Year")) %>%
  rename(Price_Start = Price) %>%
  left_join(sp500_annuale, by = c("EndYear" = "Year")) %>%
  rename(Price_End = Price) %>%
  mutate(
    Return = ((Price_End / Price_Start)^(1 / HorizonNum) - 1) * 100,
    StartYear = factor(StartYear)
  )

plot_convergenza_storica <- ggplot(data = convergenza_esatta, 
                                   aes(x = HorizonNum, y = Return, color = StartYear, group = StartYear)) +
  geom_line(linewidth = 1, alpha = 0.85) +
  geom_point(size = 1.5, alpha = 0.7) +
  annotate("rect", xmin = 20, xmax = 25, ymin = 8, ymax = 10, alpha = 0.15, fill = "#0D9488") +
  scale_x_continuous(breaks = seq(1, 25, by = 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(-15, 40, by = 5)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Convergenza Storica dei Rendimenti Reali",
    subtitle = "Evoluzione del CAGR% per i vintage 1989-2000: il tempo riduce la volatilità",
    x = "Orizzonte Temporale dell'Investimento (Anni)",
    y = "Rendimento Annuale Composto (CAGR %)",
    caption = "Analisi focalizzata sui vintage 1989-2000 con proiezione a 25 anni",
    color = "Anno di Ingresso"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#0F172A"),
    plot.subtitle = element_text(size = 10, color = "#475569"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# --- FASE 7: ESPORTAZIONE PER CAROUSEL LINKEDIN (PDF MULTIPAGINA) ---
message("\nGenerazione del Carousel PDF per LinkedIn...")
file_output_carousel <- "Carousel_SP500_Analisi_Completa.pdf"

# Apertura del dispositivo PDF con dimensioni quadrate standard (10x10 pollici)
pdf(file = file_output_carousel, width = 10, height = 10)

# Slide 1: La Mappa di Calore (Heatmap)
print(plot_heatmap)

# Slide 2: La Tabella Triangolare dei Rendimenti (Rendering Grafico)
#grid.newpage()
#grid.draw(grob_tabella)

# Slide 3: La Convergenza Storica delle Curve Real
print(plot_convergenza_storica)

# Slide 4: L'Impatto Rischio vs Rendimento
print(plot_rischio_rendimento)

# Chiusura formale del dispositivo grafico e salvataggio su disco
dev.off()

message(paste0("Processo completato! Il file '", file_output_carousel, "' è pronto nella directory corrente."))
