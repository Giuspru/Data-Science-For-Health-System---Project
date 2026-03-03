library(tidyverse)
library(gridExtra)
library(grid)

# ── 1. CARICAMENTO DATI ─────────────────────────────────────────────────────
df <- read_csv("../datasetsUtilizzabiliPerAnalisi/deathsByCauseLess1Month.csv")

# ── 2. FUNZIONI AUSILIARIE ──────────────────────────────────────────────────

assign_ranks <- function(all_data) {
  # rank medio per ties (equivalente alla funzione Python)
  rank(all_data, ties.method = "average")
}

chi2_cdf_approx <- function(x, df_chi) {
  if (x <= 0) return(0)
  
  k <- df_chi
  z <- ((x / k)^(1/3) - (1 - 2/(9*k))) / sqrt(2/(9*k))
  
  # CDF normale standard
  pnorm(z)
}

kruskal_wallis_test <- function(groups_dict, alpha = 0.05) {
  
  k <- length(groups_dict)
  labels <- names(groups_dict)
  group_data <- lapply(groups_dict, as.numeric)
  
  all_data <- unlist(group_data)
  N <- length(all_data)
  
  all_ranks <- assign_ranks(all_data)
  
  rank_sums <- c()
  ns <- c()
  
  idx <- 1
  for (g in group_data) {
    ni <- length(g)
    ri <- all_ranks[idx:(idx + ni - 1)]
    rank_sums <- c(rank_sums, sum(ri))
    ns <- c(ns, ni)
    idx <- idx + ni
  }
  
  H <- (12 / (N * (N + 1))) *
    sum((rank_sums^2) / ns) -
    3 * (N + 1)
  
  # correzione ties
  counts <- table(all_data)
  tie_correction <- sum(counts^3 - counts)
  C <- 1 - tie_correction / (N^3 - N)
  H_corr <- ifelse(C != 0, H / C, H)
  
  df_chi <- k - 1
  p_value <- 1 - chi2_cdf_approx(H_corr, df_chi)
  reject <- p_value < alpha
  
  group_stats <- list()
  for (i in seq_along(labels)) {
    gdata <- group_data[[i]]
    group_stats[[labels[i]]] <- list(
      n         = length(gdata),
      mediana   = median(gdata),
      media     = mean(gdata),
      std       = sd(gdata),
      q1        = quantile(gdata, 0.25),
      q3        = quantile(gdata, 0.75),
      rank_sum  = rank_sums[i],
      rank_mean = rank_sums[i] / ns[i]
    )
  }
  
  list(
    H = H,
    H_corr = H_corr,
    df = df_chi,
    p_value = p_value,
    C_ties = C,
    N = N,
    k = k,
    alpha = alpha,
    rifiuta_H0 = reject,
    group_stats = group_stats
  )
}

# ── 3. PREPARAZIONE GRUPPI ──────────────────────────────────────────────────
groups <- df %>%
  group_split(ParentLocation) %>%
  setNames(unique(df$ParentLocation)) %>%
  lapply(function(x) na.omit(x$FactValueNumeric))

res <- kruskal_wallis_test(groups, alpha = 0.05)

# ── 4. STAMPA RISULTATI ─────────────────────────────────────────────────────
cat("\n", strrep("=",70), "\n")
cat("TEST DI KRUSKAL-WALLIS — Decessi infantili < 1 mese per Regione OMS\n")
cat(strrep("=",70), "\n\n")

cat(sprintf("  Numerosità totale (N):        %d\n", res$N))
cat(sprintf("  Numero di gruppi (k):         %d\n", res$k))
cat(sprintf("  Gradi di libertà (k-1):       %d\n", res$df))
cat(sprintf("  Statistica H (grezza):        %.4f\n", res$H))
cat(sprintf("  Fattore correzione ties (C):  %.6f\n", res$C_ties))
cat(sprintf("  Statistica H (corretta):      %.4f\n", res$H_corr))
cat(sprintf("  p-value (approssimato):       %.2e\n", res$p_value))
cat(sprintf("  Livello α:                    %.2f\n", res$alpha))
cat(sprintf("  Rifiuta H₀:                   %s\n\n",
            ifelse(res$rifiuta_H0, "SÌ ✗", "NO ✓")))

cat("  H₀: le distribuzioni sono uguali in tutte le regioni\n")
cat("  H₁: almeno una regione è diversa\n")

cat("\n", strrep("-",70), "\n")
cat(sprintf("  %-25s %6s %10s %10s %12s\n",
            "Regione","n","Mediana","Media","Rango medio"))
cat(strrep("-",70), "\n")

sorted_regions <- names(sort(
  sapply(res$group_stats, function(x) x$rank_mean),
  decreasing = TRUE
))

for (region in sorted_regions) {
  s <- res$group_stats[[region]]
  cat(sprintf("  %-25s %6d %10.1f %10.1f %12.1f\n",
              region, s$n, s$mediana, s$media, s$rank_mean))
}
cat(strrep("=",70), "\n")

# ── 5. VISUALIZZAZIONE ──────────────────────────────────────────────────────

palette <- c(
  "Africa"                = "#e05c5c",
  "Americas"              = "#e09a5c",
  "Eastern Mediterranean" = "#e0d45c",
  "Europe"                = "#5ce08a",
  "South-East Asia"       = "#5cb8e0",
  "Western Pacific"       = "#a05ce0"
)

# Ordine per mediana
region_order <- names(sort(
  sapply(res$group_stats, function(x) x$mediana),
  decreasing = TRUE
))

plot_df <- map_dfr(region_order, function(region) {
  tibble(
    region = region,
    value  = log1p(groups[[region]])
  )
})

# Boxplot
p1 <- ggplot(plot_df, aes(x = value, y = factor(region, levels = region_order),
                          fill = region)) +
  geom_boxplot(alpha = 0.75, outlier.size = 1) +
  scale_fill_manual(values = palette) +
  labs(title = "Distribuzione decessi per regione OMS (scala log)",
       x = "log(1 + Decessi)", y = "") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="#0f1117", color=NA),
    panel.background = element_rect(fill="#1a1d27", color=NA),
    text = element_text(color="#cccccc"),
    axis.text = element_text(color="#aaaaaa"),
    legend.position = "none"
  )

# Rango medio
rank_df <- tibble(
  region = names(res$group_stats),
  rank_mean = sapply(res$group_stats, function(x) x$rank_mean)
)

p2 <- ggplot(rank_df, aes(x = rank_mean,
                          y = reorder(region, rank_mean),
                          fill = region)) +
  geom_col(alpha=0.8) +
  scale_fill_manual(values = palette) +
  labs(title = "Rango medio per regione",
       x = "Rango medio", y = "") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill="#0f1117", color=NA),
    panel.background = element_rect(fill="#1a1d27", color=NA),
    text = element_text(color="#cccccc"),
    axis.text = element_text(color="#aaaaaa"),
    legend.position = "none"
  )

# Box risultati test
result_text <- paste0(
  "TEST DI KRUSKAL-WALLIS\n",
  "────────────────────────────\n",
  sprintf("N totale        %10d\n", res$N),
  sprintf("Gruppi (k)      %10d\n", res$k),
  sprintf("Gradi libertà   %10d\n", res$df),
  sprintf("H (grezzo)      %10.4f\n", res$H),
  sprintf("Correz. ties C  %10.6f\n", res$C_ties),
  sprintf("H (corretto)    %10.4f\n", res$H_corr),
  sprintf("p-value         %10.2e\n", res$p_value),
  sprintf("α               %10.2f\n", res$alpha)
)

p3 <- ggplot() +
  annotate("text", x=0, y=1, label=result_text,
           hjust=0, vjust=1, family="mono",
           size=4, color="#cccccc") +
  theme_void() +
  theme(
    plot.background = element_rect(fill="#1a1d27", color=NA)
  )

final_plot <- grid.arrange(p1, p2, p3,
                           layout_matrix = rbind(c(1,2),
                                                 c(1,3)))

ggsave("kruskal_wallis.png",
       final_plot,
       width=18, height=12, dpi=150,
       bg="#0f1117")

cat("\nGrafico salvato: kruskal_wallis.png\n")