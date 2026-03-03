library(tidyverse)
library(gridExtra)

# ── 1. CARICAMENTO DATI ─────────────────────────────────────────────────────
df <- read_csv("deathsByCauseLess1Month.csv")

regions <- unique(df$ParentLocation)

# ── 2. FUNZIONI KS IMPLEMENTATE DA ZERO ────────────────────────────────────

# CDF normale (usa pnorm invece di erf)
normal_cdf <- function(x, mu, sigma) {
  pnorm(x, mean = mu, sd = sigma)
}

# Valore critico (Miller 1956)
ks_critical_value <- function(n, alpha = 0.05) {
  c_alpha <- c("0.10" = 1.2238,
               "0.05" = 1.3581,
               "0.01" = 1.6276)
  c <- c_alpha[as.character(alpha)]
  if (is.na(c)) c <- 1.3581
  as.numeric(c) / sqrt(n)
}

# KS test manuale stile Lilliefors
ks_test_normality <- function(data, alpha = 0.05) {
  
  x <- sort(data)
  n <- length(x)
  
  mu <- mean(x)
  sigma <- sd(x)
  
  ecdf_vals <- (1:n) / n
  tcdf_vals <- normal_cdf(x, mu, sigma)
  
  D <- max(
    pmax(abs(ecdf_vals - tcdf_vals),
         abs((ecdf_vals - 1/n) - tcdf_vals))
  )
  
  D_crit <- ks_critical_value(n, alpha)
  reject <- D > D_crit
  
  list(
    D = D,
    D_critico = D_crit,
    n = n,
    media = mu,
    std = sigma,
    rifiuta_H0 = reject,
    alpha = alpha
  )
}

# ── 3. APPLICAZIONE PER REGIONE ────────────────────────────────────────────
results <- list()

for (region in regions) {
  data <- df %>%
    filter(ParentLocation == region) %>%
    pull(FactValueNumeric) %>%
    na.omit()
  
  results[[region]] <- ks_test_normality(data)
}

# ── 4. STAMPA TABELLA RISULTATI ────────────────────────────────────────────
cat("\n", strrep("=",75), "\n")
cat("TEST DI KOLMOGOROV-SMIRNOV — Normalità per Regione OMS (α = 0.05)\n")
cat(strrep("=",75), "\n")
cat(sprintf("%-25s %6s %9s %9s %12s\n",
            "Regione","n","D","D_crit","Rifiuta H₀"))
cat(strrep("-",75), "\n")

for (region in names(results)) {
  r <- results[[region]]
  esito <- ifelse(r$rifiuta_H0, "✗  SÌ", "✓  NO")
  cat(sprintf("%-25s %6d %9.5f %9.5f %12s\n",
              region, r$n, r$D, r$D_critico, esito))
}

cat(strrep("=",75), "\n")
cat("H₀: i dati seguono una distribuzione normale\n")
cat("Se D > D_critico → si rifiuta H₀ → distribuzione NON normale\n\n")

# ── 5. VISUALIZZAZIONE ─────────────────────────────────────────────────────

palette <- c(
  "Africa"                = "#e05c5c",
  "Americas"              = "#e09a5c",
  "Eastern Mediterranean" = "#e0d45c",
  "Europe"                = "#5ce08a",
  "South-East Asia"       = "#5cb8e0",
  "Western Pacific"       = "#a05ce0"
)

plot_list <- list()

for (region in names(results)) {
  
  r <- results[[region]]
  
  data <- df %>%
    filter(ParentLocation == region) %>%
    pull(FactValueNumeric) %>%
    na.omit() %>%
    sort()
  
  n <- r$n
  mu <- r$media
  sigma <- r$std
  color <- palette[region]
  if (is.na(color)) color <- "#ffffff"
  
  ecdf_y <- (1:n) / n
  tcdf_at_data <- normal_cdf(data, mu, sigma)
  
  diffs <- abs(ecdf_y - tcdf_at_data)
  idx_max <- which.max(diffs)
  
  x_d <- data[idx_max]
  y1  <- ecdf_y[idx_max]
  y2  <- tcdf_at_data[idx_max]
  
  x_range <- seq(min(data), max(data), length.out = 500)
  tcdf_y  <- normal_cdf(x_range, mu, sigma)
  
  df_plot <- tibble(x = data, ecdf = ecdf_y)
  
  p <- ggplot() +
    geom_step(data = df_plot,
              aes(x = x, y = ecdf),
              color = color, linewidth = 1) +
    geom_line(aes(x = x_range, y = tcdf_y),
              color = "white", linetype = "dashed", linewidth = 0.8) +
    geom_segment(aes(x = x_d, xend = x_d,
                     y = min(y1,y2), yend = max(y1,y2)),
                 color = "#ff4d4d", linewidth = 1.2) +
    geom_point(aes(x = x_d, y = y1),
               color = "#ff4d4d", size = 2) +
    geom_point(aes(x = x_d, y = y2),
               color = "#ff4d4d", size = 2) +
    labs(title = region,
         x = "Decessi",
         y = "Probabilità cumulata") +
    theme_minimal(base_family = "DejaVu Sans") +
    theme(
      plot.background = element_rect(fill = "#0f1117", color = NA),
      panel.background = element_rect(fill = "#1a1d27", color = NA),
      panel.grid = element_blank(),
      axis.text = element_text(color = "#aaaaaa", size = 7),
      axis.title = element_text(color = "#aaaaaa", size = 8),
      plot.title = element_text(color = color,
                                face = "bold",
                                size = 10,
                                hjust = 0.5)
    )
  
  plot_list[[region]] <- p
}

final_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 3
)

ggsave("ks_test_normalita.png",
       final_plot,
       width = 18,
       height = 14,
       dpi = 150,
       bg = "#0f1117")

cat("Grafico salvato.\n")