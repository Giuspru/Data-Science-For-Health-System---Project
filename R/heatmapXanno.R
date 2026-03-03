library(tidyverse)
library(scales)

# ── Palette colori e stile ────────────────────────────────────────────────
BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"

CMAP <- colorRampPalette(c("#0D1B2A", "#0A3D62", "#1A6E8A", "#06D6A0",
                           "#FFD166", "#F4A261", "#E05C5C"))(256)

SHORT <- c(
  "Acute lower respiratory infections" = "ALRI",
  "Birth asphyxia and birth trauma" = "Asphyxia / Trauma",
  "Congenital anomalies" = "Congenital anomalies",
  "Diarrhoeal diseases" = "Diarrhoeal diseases",
  "HIV/AIDS" = "HIV/AIDS",
  "Injuries" = "Injuries",
  "Malaria" = "Malaria",
  "Measles" = "Measles",
  "Meningitis/encephalitis" = "Meningitis / Enceph.",
  "Prematurity" = "Prematurity",
  "Sepsis and other infectious conditions of the newborn" = "Sepsis neonato",
  "Tetanus" = "Tetanus",
  "Tuberculosis" = "Tuberculosis"
)

# ── Funzioni per preparare dati ───────────────────────────────────────────
make_pivot <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Cause, Period) %>%
    summarise(Total = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Period, values_from = Total, values_fill = 0)
}

row_normalize <- function(pivot_df) {
  mat <- as.matrix(pivot_df[,-1])
  row_max <- apply(mat, 1, max)
  norm_mat <- mat / row_max
  norm_mat[is.na(norm_mat)] <- 0
  data.frame(Cause = pivot_df$Cause, norm_mat)
}

reshape_for_heatmap <- function(norm_df, raw_df) {
  norm_long <- norm_df %>%
    pivot_longer(-Cause, names_to = "Year", values_to = "Norm") %>%
    mutate(Year = as.numeric(Year))
  
  raw_long <- raw_df %>%
    pivot_longer(-Cause, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(Year))
  
  combined <- left_join(norm_long, raw_long, by = c("Cause", "Year"))
  combined$CauseShort <- SHORT[combined$Cause]
  combined
}

# ── Lettura dati ──────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

p1 <- make_pivot(df1)
p2 <- make_pivot(df2)
n1 <- row_normalize(p1)
n2 <- row_normalize(p2)

# ── Ordine cause per somma totale decrescente ──────────────────────────────
order1 <- p1 %>% mutate(TotalSum = rowSums(select(., -Cause))) %>% arrange(TotalSum) %>% pull(Cause)
order2 <- p2 %>% mutate(TotalSum = rowSums(select(., -Cause))) %>% arrange(TotalSum) %>% pull(Cause)

n1 <- n1 %>% slice(match(order1, Cause))
n2 <- n2 %>% slice(match(order2, Cause))
p1 <- p1 %>% slice(match(order1, Cause))
p2 <- p2 %>% slice(match(order2, Cause))

# ── Prepara dati long per ggplot ───────────────────────────────────────────
heat1 <- reshape_for_heatmap(n1, p1)
heat2 <- reshape_for_heatmap(n2, p2)

# ── Funzione generica heatmap ─────────────────────────────────────────────
plot_heatmap <- function(df, accent, title, subtitle) {
  ggplot(df, aes(x = factor(Year), y = fct_rev(factor(CauseShort)), fill = Norm)) +
    geom_tile(color = PANEL_BG) +
    geom_text(aes(label = case_when(
      Value >= 1e6 ~ sprintf("%.1fM", Value/1e6),
      Value >= 1e3 ~ sprintf("%.0fK", Value/1e3),
      TRUE ~ sprintf("%.0f", Value)
    )), color = TEXT_C, size = 2.8, fontface = "bold") +
    scale_fill_gradientn(colors = CMAP, limits = c(0,1)) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = BG, color = NA),
      panel.background = element_rect(fill = PANEL_BG, color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = TEXT_C, size = 8),
      axis.text.y = element_text(color = TEXT_C, size = 9),
      plot.title = element_text(color = accent, face = "bold", hjust = 0),
      legend.background = element_rect(fill = PANEL_BG),
      legend.text = element_text(color = TEXT_C),
      legend.title = element_text(color = TEXT_C)
    ) +
    annotate("text", x = 1, y = length(unique(df$CauseShort)) + 0.5,
             label = subtitle, color = TEXT_C, size = 2.8, hjust = 0, alpha = 0.6)
}

# ── Costruzione heatmap ────────────────────────────────────────────────────
p1_plot <- plot_heatmap(heat1, C1,
                        "< 1 Mese  |  Heatmap Causa × Anno  (valori normalizzati per riga)",
                        "Neonati 0–27 giorni")

p2_plot <- plot_heatmap(heat2, C2,
                        "< 1 Anno  |  Heatmap Causa × Anno  (valori normalizzati per riga)",
                        "Bambini 1–59 mesi")

# ── Combina due heatmap verticalmente ──────────────────────────────────────
library(patchwork)
p_final <- p1_plot / p2_plot +
  plot_annotation(
    title = "Heatmap — Evoluzione Relativa di Ogni Causa nel Tempo",
    theme = theme(plot.title = element_text(color = TEXT_C, face = "bold", size = 15, hjust = 0.5))
  )

# ── Salvataggio ─────────────────────────────────────────────────────────────
ggsave("analysis_07_heatmap.png", p_final, width = 17, height = 11, dpi = 180, bg = BG)
cat("✓ Grafico salvato: analysis_07_heatmap.png\n")