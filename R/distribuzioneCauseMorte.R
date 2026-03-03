# ── Librerie ────────────────────────────────────────────────────────────────
library(tidyverse)
library(scales)
library(patchwork)

# ── Lettura dati ─────────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

# ── Aggregazione ─────────────────────────────────────────────────────────────
agg1 <- df1 %>%
  group_by(Cause) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Value)

agg2 <- df2 %>%
  group_by(Cause) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Value)

# ── Palette ──────────────────────────────────────────────────────────────────
COLOR_DS1  <- "#E05C5C"
COLOR_DS2  <- "#4A90D9"
BG_COLOR   <- "#0F1923"
GRID_COLOR <- "#1E2E3D"
TEXT_COLOR <- "#E8EEF4"

# ── Funzione per creare un pannello ─────────────────────────────────────────
create_plot <- function(data, color, title, subtitle) {
  
  max_val <- max(data$Value)
  
  ggplot(data, aes(x = Value / 1e6,
                   y = reorder(Cause, Value))) +
    
    geom_col(fill = color,
             alpha = 0.85) +
    
    geom_text(aes(label = ifelse(Value >= 1e6,
                                 paste0(round(Value/1e6,2),"M"),
                                 paste0(round(Value/1e3,0),"K"))),
              hjust = -0.1,
              color = TEXT_COLOR,
              size = 3,
              fontface = "bold") +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Decessi totali (milioni, 2000–2021)",
      y = NULL
    ) +
    
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = BG_COLOR, color = NA),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = GRID_COLOR),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = TEXT_COLOR),
      axis.title.x = element_text(color = TEXT_COLOR),
      plot.title = element_text(color = color, face = "bold", size = 13),
      plot.subtitle = element_text(color = TEXT_COLOR, size = 9),
      axis.text.y = element_text(size = 9)
    )
}

# ── Creazione grafici ────────────────────────────────────────────────────────
p1 <- create_plot(
  agg1,
  COLOR_DS1,
  "< 1 Mese (0–27 giorni)",
  "Mortalità Neonatale"
)

p2 <- create_plot(
  agg2,
  COLOR_DS2,
  "< 1 Anno (1–11 mesi)",
  "Mortalità Post-Neonatale"
)

# ── Unione pannelli ──────────────────────────────────────────────────────────
final_plot <- p1 + p2 +
  plot_annotation(
    title = "Distribuzione delle Cause di Morte nei Primi 5 Anni di Vita",
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        color = TEXT_COLOR
      ),
      plot.background = element_rect(fill = BG_COLOR, color = NA)
    )
  )

# ── Salvataggio ──────────────────────────────────────────────────────────────
ggsave(
  "analysis_01_cause_distribution.png",
  final_plot,
  width = 18,
  height = 8,
  dpi = 180,
  bg = BG_COLOR
)

print("✓ Grafico salvato: analysis_01_cause_distribution.png")