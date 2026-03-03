# ── Librerie ────────────────────────────────────────────────────────────────
library(tidyverse)
library(patchwork)
library(scales)

# ── Lettura dati ─────────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

# ── Funzione di preparazione ────────────────────────────────────────────────
prepare <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX",
           FactValueNumeric > 0)
}

d1 <- prepare(df1)
d2 <- prepare(df2)

# ── Ordine cause: mediana decrescente DS1 ───────────────────────────────────
cause_order <- d1 %>%
  group_by(Cause) %>%
  summarise(med = median(FactValueNumeric, na.rm = TRUE)) %>%
  arrange(desc(med)) %>%
  pull(Cause)

# Aggiungi eventuali cause mancanti da DS2
missing_causes <- setdiff(unique(d2$Cause), cause_order)
cause_order <- c(cause_order, missing_causes)

# ── Palette & stile ─────────────────────────────────────────────────────────
BG        <- "#0D1B2A"
PANEL_BG  <- "#0F2034"
GRID_C    <- "#1A3148"
TEXT_C    <- "#C8D8E8"
C1        <- "#E05C5C"
C2        <- "#4A90D9"
MED_C1    <- "#FFD166"
MED_C2    <- "#06D6A0"

theme_dark_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = GRID_C),
    panel.grid.minor   = element_blank(),
    axis.text  = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    plot.title = element_text(face = "bold"),
    legend.background = element_rect(fill = BG),
    legend.key = element_rect(fill = BG)
  )

# ── Funzione per creare boxplot ─────────────────────────────────────────────
create_boxplot <- function(data, color, med_color, title) {
  
  data <- data %>%
    mutate(Cause = factor(Cause, levels = cause_order))
  
  counts <- data %>%
    group_by(Cause) %>%
    summarise(n = n())
  
  ggplot(data, aes(x = Cause, y = FactValueNumeric)) +
    
    geom_boxplot(
      fill = color,
      alpha = 0.25,
      color = color,
      outlier.alpha = 0.4,
      outlier.size = 1.5,
      width = 0.55
    ) +
    
    geom_jitter(
      width = 0.18,
      alpha = 0.18,
      size = 1,
      color = color
    ) +
    
    stat_summary(
      fun = median,
      geom = "crossbar",
      width = 0.55,
      color = med_color,
      linewidth = 0.9
    ) +
    
    scale_y_log10(
      labels = function(x) ifelse(x >= 1000,
                                  paste0(round(x/1000), "K"),
                                  round(x))
    ) +
    
    geom_text(
      data = counts,
      aes(x = Cause, y = min(data$FactValueNumeric)*0.6,
          label = paste0("n=", n)),
      inherit.aes = FALSE,
      size = 2.5,
      color = TEXT_C,
      alpha = 0.55
    ) +
    
    labs(
      title = title,
      y = "Decessi (scala logaritmica)",
      x = NULL
    ) +
    
    theme_dark_custom +
    theme(
      axis.text.x = element_text(
        angle = 35,
        hjust = 1,
        size = 8.5
      ),
      plot.title = element_text(color = color)
    )
}

# ── Creazione pannelli ──────────────────────────────────────────────────────
p1 <- create_boxplot(
  d1,
  C1,
  MED_C1,
  "< 1 Mese  —  Distribuzione dei decessi per causa (0–27 giorni)"
)

p2 <- create_boxplot(
  d2,
  C2,
  MED_C2,
  "< 1 Anno  —  Distribuzione dei decessi per causa (1–11 mesi)"
)

# ── Layout verticale ────────────────────────────────────────────────────────
final_plot <- p1 / p2 +
  plot_annotation(
    title = "Box Plot — Variabilità dei Decessi per Causa tra Paesi e Anni",
    theme = theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        color = TEXT_C
      ),
      plot.background = element_rect(fill = BG)
    )
  )

# ── Salvataggio ─────────────────────────────────────────────────────────────
ggsave(
  "analysis_02_boxplot_cause.png",
  final_plot,
  width = 16,
  height = 14,
  dpi = 180,
  bg = BG
)

cat("✓ Grafico salvato: analysis_02_boxplot_cause.png\n")