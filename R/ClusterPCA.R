library(tidyverse)
library(ggplot2)
library(grid)

# ── Caricamento e costruzione feature matrix ────────────────────────────────
df1 <- read_csv("deathsByCauseLess1Month.csv")
df2 <- read_csv("deathsByCauseLess1Year.csv")

country_cause_pivot <- function(df, prefix) {
  df %>%
    filter(Sex == "SEX_BTSX") %>%
    group_by(Location, ParentLocation, Cause) %>%
    summarise(FactValueNumeric = sum(FactValueNumeric, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Cause, values_from = FactValueNumeric, values_fill = 0) %>%
    rename_with(~ paste0(prefix, "_", .x), -c(Location, ParentLocation)) %>%
    column_to_rownames(var = "Location")
}

c1 <- country_cause_pivot(df1, "DS1")
c2 <- country_cause_pivot(df2, "DS2")

# ── Aggiungo ParentLocation come colonna ─────────────────────────────────────
c1$ParentLocation <- df1 %>%
  filter(Sex == "SEX_BTSX") %>%
  distinct(Location, ParentLocation) %>%
  pull(ParentLocation)

c2$ParentLocation <- df2 %>%
  filter(Sex == "SEX_BTSX") %>%
  distinct(Location, ParentLocation) %>%
  pull(ParentLocation)

# ── Intersezione dei paesi e unione ──────────────────────────────────────────
common_countries <- intersect(rownames(c1), rownames(c2))
c1_sub <- c1[common_countries, ]
c2_sub <- c2[common_countries, ]
feat_raw <- cbind(c1_sub, c2_sub)

regions   <- feat_raw$ParentLocation
X_raw     <- as.matrix(feat_raw %>% select(-ParentLocation))
countries <- rownames(feat_raw)

# ── Normalizzazione per riga ────────────────────────────────────────────────
X_norm <- X_raw / rowSums(X_raw + 1e-9)

# ── K-Means fissando k = 8 ────────────────────────────────────────────────
set.seed(42)
K_OPT <- 8  # forziamo 8 cluster
km <- kmeans(X_norm, centers = K_OPT, nstart = 15)
labels <- km$cluster

# ── PCA manuale (2 componenti) ─────────────────────────────────────────────
X_c <- scale(X_norm, center = TRUE, scale = FALSE)
cov_mat <- cov(X_c)
eig <- eigen(cov_mat)
PC <- eig$vectors[,1:2]
X_2d <- X_c %*% PC
var_explained <- eig$values[1:2] / sum(eig$values) * 100

df_plot <- as_tibble(X_2d)
colnames(df_plot) <- c("PC1", "PC2")
df_plot <- df_plot %>%
  mutate(Country = countries,
         Cluster = factor(labels))

# ── Stile ───────────────────────────────────────────────────────────────────
CLUSTER_COLORS <- c("#E05C5C", "#4A90D9", "#FFD166", "#06D6A0",
                    "#A78BFA", "#F4A261", "#38BDF8", "#F472B6")
BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"

# ── Preparazione etichette top 3 paesi per cluster ─────────────────────────
top_labels <- df_plot %>%
  group_by(Cluster) %>%
  mutate(Volume = rowSums(X_raw[match(Country, countries), ])) %>%
  slice_max(Volume, n = 3) %>%
  ungroup() %>%
  mutate(Color = CLUSTER_COLORS[as.numeric(Cluster)])

# ── Grafico ────────────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Cluster), size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, color = GRID_C, size = 0.8) +
  geom_vline(xintercept = 0, color = GRID_C, size = 0.8) +
  geom_text(data = top_labels,
            aes(x = PC1, y = PC2, label = Country),
            color = top_labels$Color,
            hjust = 0, nudge_x = 0.02,
            size = 3, fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = CLUSTER_COLORS[1:K_OPT]) +
  labs(
    x = paste0("PC1  (", round(var_explained[1],1), "% varianza spiegata)"),
    y = paste0("PC2  (", round(var_explained[2],1), "% varianza spiegata)"),
    title = paste0("Cluster Analysis Congiunta — Proiezione PCA  (", K_OPT, " cluster, ", nrow(df_plot), " paesi)"),
    color = "Cluster"
  ) +
  theme_minimal(base_family = "DejaVu Sans") +
  theme(
    plot.background = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major = element_line(color = GRID_C, size = 0.5),
    panel.grid.minor = element_line(color = GRID_C, size = 0.25),
    axis.text = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    plot.title = element_text(color = TEXT_C, face = "bold", size = 14, hjust = 0.5),
    legend.background = element_rect(fill = PANEL_BG),
    legend.text = element_text(color = TEXT_C),
    legend.title = element_text(color = TEXT_C)
  )

# ── Salvataggio e visualizzazione ─────────────────────────────────────────────
ggsave("analysis_8_cluster_pca.png", p, width = 13, height = 9, dpi = 180)
print(paste0("✓  Grafico salvato: analysis_8_cluster_pca.png  (k=", K_OPT, ")"))
print(p)