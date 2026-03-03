library(tidyverse)
library(patchwork)
library(scales)

# ── Lettura dati ─────────────────────────────────────────────────────────────
df1 <- read_csv("../datasetsUtilizzabiliPerAnalisi/deathsByCauseLess1Month.csv")
df2 <- read_csv("../datasetsUtilizzabiliPerAnalisi/deathsByCauseLess1Year.csv")

# ── Funzione per dati Pareto ────────────────────────────────────────────────
pareto_data <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Cause) %>%
    summarise(Total = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Total)) %>%
    mutate(
      BarPct = Total / sum(Total) * 100,
      CumPct = cumsum(Total) / sum(Total) * 100
    )
}

pd1 <- pareto_data(df1)
pd2 <- pareto_data(df2)

# ── Palette ────────────────────────────────────────────────────────────────
BG        <- "#0D1B2A"
PANEL_BG  <- "#0F2034"
GRID_C    <- "#1A3148"
TEXT_C    <- "#C8D8E8"
C1        <- "#E05C5C"
C2        <- "#4A90D9"
LINE_C1   <- "#FFD166"
LINE_C2   <- "#06D6A0"
THRESH_C  <- "#FF6B6B"
OUT_C     <- "#2A4A6A"

theme_dark_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = GRID_C),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C)
  )

# ── Funzione per creare pannello Pareto ─────────────────────────────────────
create_pareto <- function(pd, bar_col, line_col, title, subtitle) {
  
  n <- nrow(pd)
  threshold_idx <- max(which(pd$CumPct <= 80))
  
  pd <- pd %>%
    mutate(
      Cause = factor(Cause, levels = Cause),
      Color = ifelse(row_number() <= threshold_idx, bar_col, OUT_C)
    )
  
  ggplot(pd, aes(x = seq_len(n), y = BarPct)) +
    
    # Barre percentuali
    geom_col(aes(fill = Color), width = 0.65, color = NA, alpha = 0.85) +
    
    # Valori sulle barre
    geom_text(
      data = pd %>% filter(BarPct >= 2.5) %>% mutate(x = row_number()),
      aes(x = x, y = BarPct, label = sprintf("%.1f%%", BarPct)),
      vjust = -0.5,
      size = 3.2,
      color = TEXT_C,
      fontface = "bold"
    ) +
    
    # Linea cumulativa
    geom_line(aes(y = CumPct, group = 1), color = line_col, size = 1.2) +
    geom_point(aes(y = CumPct), color = line_col, size = 2.5) +
    
    # Soglia 80%
    geom_hline(yintercept = 80, linetype = "dashed", color = THRESH_C, size = 0.8) +
    
    # Linea verticale al punto soglia
    geom_vline(xintercept = threshold_idx, linetype = "dotted", color = THRESH_C, size = 0.6) +
    
    # Annotazione 80%
    annotate(
      "text", x = threshold_idx + 0.6, y = 68,
      label = paste0(threshold_idx, " cause\n→ 80% dei decessi"),
      color = THRESH_C, fontface = "bold", size = 3
    ) +
    
    # Asse X con etichette corte
    scale_x_continuous(
      breaks = seq_len(n),
      labels = str_replace_all(pd$Cause, 
                               c("and other infectious conditions of the newborn" = "& inf. neonato",
                                 "Acute lower respiratory infections" = "ALRI",
                                 "Birth asphyxia and birth trauma" = "Asphyxia/Trauma",
                                 "Congenital anomalies" = "Cong. anomalies",
                                 "Meningitis/encephalitis" = "Meningitis/Enceph.")),
      expand = expansion(add = c(0.5, 1))
    ) +
    
    scale_y_continuous(
      name = "% sul totale decessi",
      limits = c(0, max(pd$BarPct)*1.22),
      labels = function(x) paste0(round(x), "%"),
      sec.axis = sec_axis(~ ., name = "% cumulativa")
    ) +
    
    labs(title = title, subtitle = subtitle) +
    
    theme_dark_custom +
    theme(
      plot.title = element_text(color = bar_col, face = "bold", hjust = 0),
      plot.subtitle = element_text(color = TEXT_C, size = 9),
      axis.text.x = element_text(angle = 38, hjust = 1, size = 8.5)
    )
}

# ── Creazione pannelli ──────────────────────────────────────────────────────
p1 <- create_pareto(pd1, C1, LINE_C1, "< 1 Mese  |  Curva di Pareto per Causa", "Neonati 0–27 giorni")
p2 <- create_pareto(pd2, C2, LINE_C2, "< 1 Anno  |  Curva di Pareto per Causa", "Bambini 1–59 mesi")

# ── Layout affiancato ───────────────────────────────────────────────────────
final_plot <- p1 + p2 +
  plot_annotation(
    title = "Analisi di Pareto — Quante cause spiegano l'80% dei decessi infantili?",
    theme = theme(
      plot.title = element_text(color = TEXT_C, size = 15, face = "bold"),
      plot.background = element_rect(fill = BG)
    )
  )

# ── Salvataggio ─────────────────────────────────────────────────────────────
ggsave(
  "analysis_04_pareto.png",
  final_plot,
  width = 18,
  height = 7,
  dpi = 180,
  bg = BG
)

cat("✓ Grafico salvato: analysis_04_pareto.png\n")