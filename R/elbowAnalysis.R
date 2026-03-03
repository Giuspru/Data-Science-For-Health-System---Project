library(tidyverse)
library(ggplot2)

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

# ── Intersezione dei paesi e unione ──────────────────────────────────────────
common_countries <- intersect(rownames(c1), rownames(c2))
feat_raw <- cbind(c1[common_countries, ], c2[common_countries, ])

# ── Solo colonne numeriche per K-means ──────────────────────────────────────
X_raw <- as.matrix(feat_raw %>% select(where(is.numeric)))

# Normalizzazione per riga
X_norm <- X_raw / rowSums(X_raw + 1e-9)

# ── Funzione K-Means custom per calcolare inerzia ──────────────────────────
kmeans_inertia <- function(X, k, n_init=15, max_iter=300, seed=42) {
  set.seed(seed)
  best_inertia <- Inf
  for(i in 1:n_init) {
    centers <- X[sample(1:nrow(X), k), , drop = FALSE]
    for(j in 1:max_iter) {
      dists <- as.matrix(dist(rbind(X, centers)))[1:nrow(X), (nrow(X)+1):(nrow(X)+k)]
      labels <- apply(dists, 1, which.min)
      new_centers <- matrix(NA, nrow=k, ncol=ncol(X))
      for(cl in 1:k) {
        if(any(labels == cl)) {
          new_centers[cl, ] <- colMeans(X[labels == cl, , drop=FALSE])
        } else {
          new_centers[cl, ] <- centers[cl, ]
        }
      }
      if(all(abs(centers - new_centers) < 1e-8)) break
      centers <- new_centers
    }
    inertia <- sum(sapply(1:k, function(cl) sum((X[labels==cl,,drop=FALSE] - centers[cl,])^2)))
    if(inertia < best_inertia) best_inertia <- inertia
  }
  return(best_inertia)
}

# ── Calcolo inerzie per K variabili ────────────────────────────────────────
K_RANGE <- 2:8
inertias <- sapply(K_RANGE, function(k) kmeans_inertia(X_norm, k))

# ── k ottimale tramite secondo differenziale (gomito) ─────────────────────
diffs2 <- diff(diff(inertias))
K_OPT <- K_RANGE[which.max(abs(diffs2)) + 2]

# ── Stile ───────────────────────────────────────────────────────────────────
BG       <- "#0D1B2A"
PANEL_BG <- "#0F2034"
GRID_C   <- "#1A3148"
TEXT_C   <- "#C8D8E8"
C_LINE   <- "#4A90D9"
C_OPT    <- "#E05C5C"

# ── Preparo dataframe per ggplot ──────────────────────────────────────────
df_plot <- tibble(
  K = K_RANGE,
  Inertia = inertias
)

# ── Grafico gomito ─────────────────────────────────────────────────────────
p <- ggplot(df_plot, aes(x = K, y = Inertia)) +
  geom_line(color = C_LINE, size = 1.5) +
  geom_point(size = 3, color = C_LINE) +
  geom_point(data = df_plot %>% filter(K == K_OPT),
             aes(x = K, y = Inertia),
             color = C_OPT, size = 6, stroke = 1.5, shape = 21, fill = C_OPT) +
  geom_vline(xintercept = K_OPT, linetype="dashed", color=C_OPT, alpha=0.65, size=1) +
  geom_text(aes(label = round(Inertia, 3)), vjust = -0.5, color = TEXT_C, size = 3.5) +
  annotate("text", x = K_OPT + 0.5, y = inertias[K_OPT-1]*1.06,
           label = paste0("k = ", K_OPT, "\n(gomito massimo)"),
           color = C_OPT, fontface="bold", hjust = 0) +
  labs(
    x = "Numero di cluster  k",
    y = "Inerzia  (WCSS)",
    title = "Metodo del Gomito — Scelta del Numero Ottimale di Cluster"
  ) +
  theme_minimal(base_family = "DejaVu Sans") +
  theme(
    plot.background = element_rect(fill = BG, color = NA),
    panel.background = element_rect(fill = PANEL_BG, color = NA),
    panel.grid.major.y = element_line(color = GRID_C, size = 0.8),
    panel.grid.major.x = element_line(color = GRID_C, size = 0.4),
    axis.text = element_text(color = TEXT_C),
    axis.title = element_text(color = TEXT_C),
    plot.title = element_text(color = TEXT_C, face="bold", hjust=0.5),
    legend.position = "none"
  )

# ── Salvataggio e visualizzazione ─────────────────────────────────────────────
ggsave("analysis_15_elbow.png", p, width = 10, height = 6, dpi = 180)
print(paste0("✓  Grafico salvato: analysis_15_elbow.png  (k ottimale = ", K_OPT, ")"))
print(p)