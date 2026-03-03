library(tidyverse)
library(scales)

# ── Lettura dati ─────────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

# ── Aggregazione annuale ─────────────────────────────────────────────────────
yearly_total <- function(df) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Period) %>%
    summarise(Total = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop") %>%
    arrange(Period)
}

t1 <- yearly_total(df1)
t2 <- yearly_total(df2)
years <- t1$Period

# ── Metriche derivate ─────────────────────────────────────────────────────────
reduction_pct <- function(series) {
  (series[1] - series[length(series)]) / series[1] * 100
}

avg_annual_change <- function(series) {
  mean(diff(series) / series[-length(series)] * 100)
}

r1 <- reduction_pct(t1$Total)
r2 <- reduction_pct(t2$Total)
ac1 <- avg_annual_change(t1$Total)
ac2 <- avg_annual_change(t2$Total)

# ── Palette colori ───────────────────────────────────────────────────────────
BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"
C1       <- "#E05C5C"  # neonatale
C2       <- "#4A90D9"  # post-neonatale

# ── Trend lineare (regressione semplice) ─────────────────────────────────────
linear_fit <- function(years, values) {
  mod <- lm(values ~ years)
  predict(mod, newdata = data.frame(years = years))
}

trend_t1 <- linear_fit(years, t1$Total / 1e6)
trend_t2 <- linear_fit(years, t2$Total / 1e6)

# ── Dataset combinato per ggplot ─────────────────────────────────────────────
plot_data <- tibble(
  Year = rep(years, 2),
  Total = c(t1$Total / 1e6, t2$Total / 1e6),
  Group = factor(rep(c("< 1 Mese (neonatale)", "1–11 Mesi (post-neonatale)"), each = length(years)))
)

trend_data <- tibble(
  Year = rep(years, 2),
  Trend = c(trend_t1, trend_t2),
  Group = factor(rep(c("< 1 Mese (neonatale)", "1–11 Mesi (post-neonatale)"), each = length(years)))
)

# ── Costruzione grafico ─────────────────────────────────────────────────────
p <- ggplot(plot_data, aes(x = Year, y = Total, color = Group)) +
  
  # Aree sotto curva
  geom_area(data = filter(plot_data, Group == "< 1 Mese (neonatale)"),
            aes(fill = Group), alpha = 0.12, show.legend = FALSE) +
  geom_area(data = filter(plot_data, Group == "1–11 Mesi (post-neonatale)"),
            aes(fill = Group), alpha = 0.12, show.legend = FALSE) +
  
  # Linee principali e marker
  geom_line(size = 1.8) +
  geom_point(aes(shape = Group), size = 4, stroke = 0.5) +
  
  # Linee regressione
  geom_line(data = trend_data, aes(x = Year, y = Trend, color = Group),
            linetype = "dashed", size = 1, alpha = 0.45, inherit.aes = FALSE) +
  
  # Annotazioni valori iniziali/finali
  geom_text(data = tibble(
    Year = c(years[1], years[length(years)], years[1], years[length(years)]),
    Total = c(t1$Total[1]/1e6, t1$Total[length(years)]/1e6,
              t2$Total[1]/1e6, t2$Total[length(years)]/1e6),
    Label = sprintf("%.2fM", c(t1$Total[1]/1e6, t1$Total[length(years)]/1e6,
                               t2$Total[1]/1e6, t2$Total[length(years)]/1e6)),
    Group = factor(c("< 1 Mese (neonatale)", "< 1 Mese (neonatale)",
                     "1–11 Mesi (post-neonatale)", "1–11 Mesi (post-neonatale)"))
  ), aes(label = Label), nudge_y = c(0.06, 0.06, -0.14, -0.14),
  color = c(C1,C1,C2,C2), fontface = "bold") +
  
  # Badge riduzione percentuale
  annotate("label", x = 2005, y = 0.88,
           label = sprintf("▼ %.1f%% dal 2000 al 2021\n(media %.1f%%/anno)", r1, ac1),
           fill = PANEL_BG, color = C1, fontface = "bold", size = 3.2) +
  annotate("label", x = 2005, y = 0.74,
           label = sprintf("▼ %.1f%% dal 2000 al 2021\n(media %.1f%%/anno)", r2, ac2),
           fill = PANEL_BG, color = C2, fontface = "bold", size = 3.2) +
  
  # Scale
  scale_color_manual(values = c(C1, C2)) +
  scale_fill_manual(values = c(C1, C2)) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(labels = function(x) paste0(sprintf("%.1f", x), "M")) +
  
  labs(
    title = "Trend Globale della Mortalità Infantile per Fascia d'Età (2000–2021)",
    y = "Decessi totali (milioni)", x = NULL,
    color = NULL, shape = NULL, fill = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major = element_line(color = GRID_C),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    legend.position = "top",
    legend.text = element_text(color = TEXT_C),
    plot.title = element_text(color = TEXT_C, face = "bold", hjust = 0)
  )

# ── Salvataggio grafico ─────────────────────────────────────────────────────
ggsave("analysis_05_trend_globale.png", p, width = 16, height = 7, dpi = 180, bg = BG)

cat("✓ Grafico salvato: analysis_05_trend_globale.png\n")