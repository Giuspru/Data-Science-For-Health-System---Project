# ── Librerie ────────────────────────────────────────────────────────────────
library(tidyverse)
library(patchwork)
library(scales)

# ── Lettura dati ─────────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

# ── Funzione Top10 ──────────────────────────────────────────────────────────
top10 <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Location, ParentLocation) %>%
    summarise(Total = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Total)) %>%
    slice_head(n = 10)
}

t1 <- top10(df1)
t2 <- top10(df2)

# ── Colori per macro-regione WHO ────────────────────────────────────────────
REGION_COLORS <- c(
  "Africa"                = "#F4A261",
  "South-East Asia"       = "#E76F51",
  "Eastern Mediterranean" = "#A8DADC",
  "Western Pacific"       = "#457B9D",
  "Americas"              = "#8ECAE6",
  "Europe"                = "#95D5B2"
)

BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"
MED_C1   <- "#FFD166"
MED_C2   <- "#06D6A0"

# ── Tema scuro personalizzato ───────────────────────────────────────────────
theme_dark_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = GRID_C),
    panel.grid.minor = element_blank(),
    axis.text  = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    legend.background = element_rect(fill = BG),
    legend.key = element_rect(fill = BG),
    legend.text = element_text(color = TEXT_C),
    legend.title = element_text(color = TEXT_C)
  )

# ── Funzione per creare pannello ────────────────────────────────────────────
create_plot <- function(data, accent, title, subtitle) {
  
  data_sorted <- data %>%
    arrange(Total) %>%
    mutate(Location = factor(Location, levels = Location))
  
  ggplot(data_sorted,
         aes(x = Total / 1e6,
             y = Location,
             fill = ParentLocation)) +
    
    geom_col(height = 0.62) +
    
    geom_text(
      aes(label = sprintf("%.2f M", Total / 1e6)),
      hjust = -0.1,
      size = 3,
      color = TEXT_C,
      fontface = "bold"
    ) +
    
    scale_fill_manual(values = REGION_COLORS) +
    
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Decessi totali (milioni, 2000–2021)",
      y = NULL,
      fill = "Regione WHO"
    ) +
    
    theme_dark_custom +
    theme(
      plot.title = element_text(color = accent, face = "bold"),
      plot.subtitle = element_text(color = TEXT_C, size = 9),
      axis.text.y = element_text(size = 10)
    )
}

# ── Creazione pannelli ──────────────────────────────────────────────────────
p1 <- create_plot(
  t1,
  MED_C1,
  "< 1 Mese  |  Top 10 Paesi per Mortalità Totale",
  "Neonati 0–27 giorni"
)

p2 <- create_plot(
  t2,
  MED_C2,
  "< 1 Anno  |  Top 10 Paesi per Mortalità Totale",
  "Bambini 1–11 mesi"
)

# ── Layout affiancato ───────────────────────────────────────────────────────
final_plot <- p1 + p2 +
  plot_annotation(
    title = "Top 10 Paesi — Maggiore Mortalità Infantile Totale (2000–2021)",
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
  "analysis_03_top10_countries.png",
  final_plot,
  width = 18,
  height = 7,
  dpi = 180,
  bg = BG
)

cat("✓ Grafico salvato: analysis_03_top10_countries.png\n")