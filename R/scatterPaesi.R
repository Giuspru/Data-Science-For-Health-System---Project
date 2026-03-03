# ── Librerie ────────────────────────────────────────────────────────────────
library(tidyverse)
library(scales)
library(ggrepel)

# ── Caricamento dati ─────────────────────────────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

# ── Aggregazione per sesso BTSX ──────────────────────────────────────────────
c1 <- df1 %>%
  filter(Sex == "SEX_BTSX") %>%
  group_by(Location, ParentLocation) %>%
  summarise(DS1 = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop")

c2 <- df2 %>%
  filter(Sex == "SEX_BTSX") %>%
  group_by(Location, ParentLocation) %>%
  summarise(DS2 = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop")

# ── Unione dati ──────────────────────────────────────────────────────────────
merged <- inner_join(c1, c2, by = c("Location", "ParentLocation"))

# ── Parametri grafico ────────────────────────────────────────────────────────
REGION_COLORS <- c(
  "Africa" = "#F4A261",
  "Americas" = "#38BDF8",
  "Eastern Mediterranean" = "#A78BFA",
  "Europe" = "#34D399",
  "South-East Asia" = "#E05C5C",
  "Western Pacific" = "#FFD166"
)

BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"
DIAG_C   <- "#06D6A0"
ANNOT_C  <- "#FFD166"

# ── Preparazione dati ────────────────────────────────────────────────────────
merged <- merged %>%
  mutate(color = REGION_COLORS[ParentLocation],
         color = ifelse(is.na(color), "#AAAAAA", color),
         size = 5)

lim_max <- max(merged$DS1, merged$DS2) * 1.6
lim_min <- min(merged$DS1, merged$DS2) * 0.5

# ── Filtra paesi da etichettare ───────────────────────────────────────────────
highlight <- c("Nigeria", "India", "Pakistan", "Afghanistan")
merged_highlight <- merged %>% filter(Location %in% highlight)

# ── Plot ────────────────────────────────────────────────────────────────────
p <- ggplot(merged, aes(x = DS1 + 1, y = DS2 + 1)) +
  geom_point(aes(color = ParentLocation), size = 3, alpha = 0.65) +
  scale_color_manual(values = REGION_COLORS, name = "Regione WHO") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = DIAG_C, size = 1.2) +
  geom_text_repel(
    data = merged_highlight,
    aes(label = Location, color = ParentLocation),
    size = 4, fontface = "bold",
    box.padding = 0.6, point.padding = 0.5,
    nudge_x = c(0.3, -0.3, 0.3, -0.3),
    nudge_y = c(0.3, 0.3, -0.3, -0.3)
  ) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  coord_cartesian(xlim = c(lim_min, lim_max),
                  ylim = c(lim_min, lim_max)) +
  theme_minimal(base_family = "DejaVu Sans") +
  theme(
    plot.background = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major = element_line(color = GRID_C),
    panel.grid.minor = element_line(color = GRID_C),
    axis.text = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    legend.background = element_rect(fill = PANEL_BG),
    legend.key = element_rect(fill = PANEL_BG),
    legend.text = element_text(color = TEXT_C),
    legend.title = element_text(color = TEXT_C)
  ) +
  labs(
    x = "Decessi totali DS1 (< 1 mese, scala log)",
    y = "Decessi totali DS2 (1–59 mesi, scala log)",
    title = "Correlazione per Paese — DS1 (< 1 mese) vs DS2 (1–59 mesi)"
  )

# ── Salvataggio ────────────────────────────────────────────────────────────
ggsave("analysis_13_scatter_paesi.png", p, width = 14, height = 10,
       dpi = 180, bg = BG)
print("✓ Grafico salvato: analysis_13_scatter_paesi.png")

p