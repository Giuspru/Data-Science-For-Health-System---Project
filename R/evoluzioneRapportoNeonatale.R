library(tidyverse)
library(ggplot2)
library(scales)

# ── Caricamento e aggregazione ────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

yearly_total <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Period) %>%
    summarise(FactValueNumeric = sum(FactValueNumeric, na.rm = TRUE)) %>%
    arrange(Period)
}

t1 <- yearly_total(df1)
t2 <- yearly_total(df2)

total <- t1$FactValueNumeric + t2$FactValueNumeric
years <- t1$Period

share      <- t1$FactValueNumeric / total * 100
share_post <- t2$FactValueNumeric / total * 100

# ── Stile ─────────────────────────────────────────────────────────────────────
BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"
C1       <- "#E05C5C"
C2       <- "#4A90D9"
ANNOT_C  <- "#FFD166"

# Preparazione data frame per ggplot
df_plot <- tibble(
  Year = years,
  Neonatal = share,
  PostNeonatal = share_post
)

# Trend lineare
fit <- lm(Neonatal ~ Year, data = df_plot)
df_plot <- df_plot %>%
  mutate(Trend = predict(fit))

# ── Valori per annotazioni e badge ────────────────────────────────────────────
first_val  <- df_plot$Neonatal[1]
last_val   <- df_plot$Neonatal[nrow(df_plot)]
first_year <- df_plot$Year[1]
last_year  <- df_plot$Year[nrow(df_plot)]
delta      <- last_val - first_val
sign       <- ifelse(delta > 0, "+", "")

# ── Grafico ───────────────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(x = Year)) +
  # Aree riempite
  geom_ribbon(aes(ymin = 0, ymax = Neonatal), fill = C1, alpha = 0.2) +
  geom_ribbon(aes(ymin = Neonatal, ymax = 100), fill = C2, alpha = 0.12) +
  # Linee principali
  geom_line(aes(y = Neonatal, color = "Neonatale"), size = 1.4) +
  geom_point(aes(y = Neonatal, color = "Neonatale"), size = 3) +
  geom_line(aes(y = PostNeonatal, color = "Post-Neonatale"), size = 1.4, linetype = "dashed") +
  geom_point(aes(y = PostNeonatal, color = "Post-Neonatale"), size = 3, shape = 15) +
  # Linea 50%
  geom_hline(yintercept = 50, color = ANNOT_C, linetype = "dotted", size = 0.7, alpha = 0.7) +
  # Trend lineare tratteggiato
  geom_line(aes(y = Trend), color = C1, linetype = "dashed", alpha = 0.4, size = 0.6) +
  # Annotazioni primo e ultimo anno
  annotate("text", x = first_year, y = first_val + 1.8, 
           label = paste0(round(first_val, 1), "%"), color = C1, fontface = "bold", size = 3.5) +
  annotate("text", x = last_year, y = last_val + 1.8, 
           label = paste0(round(last_val, 1), "%"), color = C1, fontface = "bold", size = 3.5) +
  # Badge variazione
  annotate("text", x = mean(df_plot$Year), y = 10,
           label = paste0("Variazione ", first_year, " → ", last_year, ": ", sign, round(delta, 1), " p.p."),
           color = ANNOT_C, fontface = "bold", size = 4, hjust = 0.5) +
  # Assi e tema
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(20, 80)) +
  scale_x_continuous(breaks = df_plot$Year) +
  scale_color_manual(values = c("Neonatale" = C1, "Post-Neonatale" = C2)) +
  labs(
    y = "Quota % sul totale combinato",
    color = "",
    title = "La Quota Neonatale è Aumentata o Diminuita dal 2000 al 2021?",
    subtitle = "Quota = DS1 / (DS1 + DS2)  |  p.p. = punti percentuali  |  Solo sesso totale (BTSX)  |  Aggregato su tutti i paesi  |  Fonte: WHO Child Causes of Death 2000–2021"
  ) +
  theme_minimal(base_family = "DejaVu Sans") +
  theme(
    plot.background = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major = element_line(color = GRID_C, size = 0.8),
    panel.grid.minor = element_line(color = GRID_C, size = 0.3, alpha = 0.3),
    axis.text = element_text(color = TEXT_C, size = 9),
    axis.title = element_text(color = TEXT_C, size = 10),
    plot.title = element_text(color = TEXT_C, face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = TEXT_C, size = 7.5, hjust = 0.5),
    legend.position = "top",
    legend.background = element_rect(fill = PANEL_BG, color = NA),
    legend.text = element_text(color = TEXT_C)
  )

# ── Salvataggio e visualizzazione ─────────────────────────────────────────────
ggsave("analysis_14_quota_neonatale.png", p, width = 14, height = 6, dpi = 180)
print("✓  Grafico salvato: analysis_14_quota_neonatale.png")
print(p)