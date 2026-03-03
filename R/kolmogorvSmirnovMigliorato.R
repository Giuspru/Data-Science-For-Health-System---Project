library(tidyverse)
library(gridExtra)
library(grid)

# ── 1. DATI ────────────────────────────────────────────────────────────────
df <- read_csv("deathsByCauseLess1Month.csv")

palette <- c(
  "Africa"                = "#e05c5c",
  "Americas"              = "#e09a5c",
  "Eastern Mediterranean" = "#e0d45c",
  "Europe"                = "#5ce08a",
  "South-East Asia"       = "#5cb8e0",
  "Western Pacific"       = "#a05ce0"
)

# ── 2. FUNZIONI ─────────────────────────────────────────────────────────────

normal_cdf <- function(x, mu, sigma) {
  pnorm(x, mean = mu, sd = sigma)
}

ks_test_normality <- function(data, alpha = 0.05) {
  
  x <- sort(data)
  n <- length(x)
  
  mu <- mean(x)
  sigma <- sd(x)
  
  ecdf_vals <- (1:n) / n
  tcdf_vals <- normal_cdf(x, mu, sigma)
  
  diffs <- pmax(abs(ecdf_vals - tcdf_vals),
                abs((ecdf_vals - 1/n) - tcdf_vals))
  
  D <- max(diffs)
  D_crit <- 1.3581 / sqrt(n)
  
  list(
    D = D,
    D_critico = D_crit,
    n = n,
    media = mu,
    std = sigma,
    rifiuta_H0 = D > D_crit,
    ecdf = ecdf_vals,
    tcdf = tcdf_vals,
    x_sorted = x,
    diffs = diffs
  )
}

# ── 3. CALCOLO PER REGIONE ─────────────────────────────────────────────────

regions <- unique(df$ParentLocation)
results <- list()

for (region in regions) {
  data <- df %>%
    filter(ParentLocation == region) %>%
    pull(FactValueNumeric) %>%
    na.omit()
  
  results[[region]] <- ks_test_normality(data)
}

# ── 4. COSTRUZIONE GRAFICI ─────────────────────────────────────────────────

plot_list <- list()

for (region in regions) {
  
  r <- results[[region]]
  color <- palette[region]
  if (is.na(color)) color <- "white"
  
  x_s   <- r$x_sorted
  mu    <- r$media
  sigma <- r$std
  
  x_log  <- log10(x_s + 1)
  x_fine <- seq(min(x_log), max(x_log), length.out = 600)
  x_orig <- 10^x_fine - 1
  
  ecdf_y <- r$ecdf
  tcdf_y <- normal_cdf(x_orig, mu, sigma)
  
  # Punto di massima distanza
  tcdf_at_data <- normal_cdf(x_s, mu, sigma)
  diffs        <- abs(ecdf_y - tcdf_at_data)
  idx_max      <- which.max(diffs)
  
  xd <- x_log[idx_max]
  y1 <- ecdf_y[idx_max]
  y2 <- tcdf_at_data[idx_max]
  
  df_step <- tibble(x = x_log, y = ecdf_y)
  df_line <- tibble(x = x_fine, y = tcdf_y)
  
  p <- ggplot() +
    
    # Banda ±Dcrit
    geom_ribbon(data = df_line,
                aes(x = x,
                    ymin = pmax(y - r$D_critico, 0),
                    ymax = pmin(y + r$D_critico, 1)),
                fill = "white",
                alpha = 0.18) +
    
    # CDF teorica
    geom_line(data = df_line,
              aes(x = x, y = y),
              color = "white",
              linewidth = 1.2,
              linetype = "dashed") +
    
    # ECDF
    geom_step(data = df_step,
              aes(x = x, y = y),
              color = color,
              linewidth = 1.3) +
    
    # Freccia D
    annotate("segment",
             x = xd, xend = xd,
             y = y1, yend = y2,
             colour = "#ff4d4d",
             linewidth = 1.2,
             arrow = arrow(ends = "both", length = unit(0.15, "cm"))) +
    
    annotate("text",
             x = xd + (max(x_log)-min(x_log))*0.05,
             y = (y1+y2)/2,
             label = paste0("D = ", round(r$D,4)),
             color = "#ff4d4d",
             size = 3,
             fontface = "bold") +
    
    labs(
      title = region,
      x = "Decessi (scala log10)",
      y = "Probabilità cumulata"
    ) +
    
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill="#0f1117", color=NA),
      panel.background = element_rect(fill="#1a1d27", color=NA),
      panel.grid       = element_blank(),
      axis.text        = element_text(color="#aaaaaa", size=7),
      axis.title       = element_text(color="#aaaaaa", size=8),
      plot.title       = element_text(color=color,
                                      face="bold",
                                      size=10,
                                      hjust=0.5)
    ) +
    coord_cartesian(ylim=c(-0.02,1.05))
  
  plot_list[[region]] <- p
}

# ── 5. LAYOUT 2×3 ──────────────────────────────────────────────────────────

final_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 3
)

ggsave("ks_ecdf_vs_cdf.png",
       final_plot,
       width = 18,
       height = 10,
       dpi = 150,
       bg = "#0f1117")

cat("Grafico salvato: ks_ecdf_vs_cdf.png\n")