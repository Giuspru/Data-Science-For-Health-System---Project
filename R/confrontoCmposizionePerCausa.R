# ── Librerie ────────────────────────────────────────────────────────────────
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)

# ── Caricamento dati ─────────────────────────────────────────────────────────
df1 <- read_csv("../datasetsUtilizzabiliPerAnalisi/deathsByCauseLess1Month.csv")
df2 <- read_csv("../datasetsUtilizzabiliPerAnalisi/deathsByCauseLess1Year.csv")

# ── Funzione aggregazione ────────────────────────────────────────────────────
cause_totals <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Cause) %>%
    summarise(Total = sum(FactValueNumeric, na.rm = TRUE)) %>%
    arrange(desc(Total))
}

agg1 <- cause_totals(df1)
agg2 <- cause_totals(df2)

# ── Etichette brevi ──────────────────────────────────────────────────────────
SHORT <- c(
  "Acute lower respiratory infections"                    = "ALRI",
  "Birth asphyxia and birth trauma"                       = "Asphyxia / Trauma",
  "Congenital anomalies"                                  = "Congenital Anomalies",
  "Diarrhoeal diseases"                                   = "Diarrhoeal Diseases",
  "HIV/AIDS"                                              = "HIV/AIDS",
  "Injuries"                                              = "Injuries",
  "Malaria"                                               = "Malaria",
  "Measles"                                               = "Measles",
  "Meningitis/encephalitis"                               = "Meningitis / Enceph.",
  "Prematurity"                                           = "Prematurity",
  "Sepsis and other infectious conditions of the newborn"= "Sepsis Neonato",
  "Tetanus"                                               = "Tetanus",
  "Tuberculosis"                                          = "Tuberculosis"
)

# ── Palette colori ───────────────────────────────────────────────────────────
PALETTE <- c(
  "#E05C5C", "#4A90D9", "#FFD166", "#06D6A0", "#F4A261",
  "#A78BFA", "#38BDF8", "#FB923C", "#34D399", "#F472B6",
  "#FACC15", "#94A3B8", "#C084FC"
)

cause_order <- agg1$Cause
color_map <- setNames(PALETTE[seq_along(cause_order)], cause_order)

BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
TEXT_C   <- "#C8D8E8"
C1       <- "#E05C5C"
C2       <- "#4A90D9"

# ── Funzione per creare il donut ─────────────────────────────────────────────
create_donut <- function(agg, accent, title, subtitle) {
  total <- sum(agg$Total)
  agg <- agg %>%
    mutate(
      Perc = Total / total * 100,
      Label = ifelse(Perc >= 3, paste0(round(Perc,1), "%"), ""),
      Short = SHORT[Cause],
      Explode = ifelse(Perc >= 15, 0.04, 0.01)
    )
  
  ggplot(agg, aes(x=2, y=Total, fill=Cause)) +
    geom_bar(stat="identity", color=BG, width=1) +
    coord_polar(theta="y", start=pi*140/180) +
    geom_text(aes(label=Label), position = position_stack(vjust = 0.5),
              size = 3.5, color = ifelse(agg$Perc >=5, "white", TEXT_C), fontface="bold") +
    scale_fill_manual(values=color_map) +
    xlim(0.5, 2.5) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = BG, color=BG),
      panel.background = element_rect(fill = PANEL_BG, color=PANEL_BG),
      legend.position = "none",
      plot.title = element_text(color=accent, face="bold", hjust=0.5, size=12),
      plot.subtitle = element_text(color=TEXT_C, hjust=0.5, size=8.5, alpha=0.6)
    ) +
    ggtitle(title, subtitle = subtitle) +
    annotate("text", x=0, y=0.1, label=paste0(round(total/1e6,1),"M"),
             color=accent, fontface="bold", size=6) +
    annotate("text", x=0, y=-0.14, label="decessi totali\n2000–2021",
             color=TEXT_C, alpha=0.65, size=3)
}

# ── Creazione grafici ───────────────────────────────────────────────────────
p1 <- create_donut(agg1, C1, "< 1 Mese  —  Composizione per Causa", "Neonati 0–27 giorni")
p2 <- create_donut(agg2, C2, "< 1 Anno  —  Composizione per Causa", "Bambini 1–11 mesi")

# ── Legenda condivisa ───────────────────────────────────────────────────────
legend_labels <- paste0(SHORT[cause_order], "  (",
                        round(agg1$Total/ sum(agg1$Total)*100,1), "% | ",
                        round(agg2$Total/ sum(agg2$Total)*100,1), "%)")

legend_df <- data.frame(Cause = cause_order, Label = legend_labels)

legend_grob <- ggplot(legend_df, aes(x=1, y=Cause, fill=Cause)) +
  geom_tile() +
  scale_fill_manual(values=color_map, guide="none") +
  geom_text(aes(label=Label), hjust=0, color=TEXT_C, size=3) +
  theme_void() +
  theme(plot.background = element_rect(fill = BG, color=BG))

# ── Composizione finale ─────────────────────────────────────────────────────
grid.arrange(p1, p2, ncol=2, bottom=legend_grob)


# ── Salvataggio grafico ──────────────────────────────────────────────────────
library(gridExtra)

# Imposta dimensioni e sfondo simili a Python (18x9 pollici, dpi ~180)
png(
  filename = "analysis_10_pie_composizione.png",
  width = 18*180, height = 9*180, res = 180, units = "px",
  bg = BG
)

# Layout con i due donut e legenda
grid.arrange(p1, p2, ncol=2, bottom=legend_grob)

dev.off()
cat("✓ Grafico salvato: analysis_10_pie_composizione.png\n")