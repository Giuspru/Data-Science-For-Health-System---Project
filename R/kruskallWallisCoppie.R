library(tidyverse)
library(gridExtra)
library(ggplot2)

# ── 1. DATI ────────────────────────────────────────────────────────────────
df <- read_csv("deathsByCauseLess1Month.csv")
regions <- unique(df$ParentLocation)

groups <- lapply(regions, function(r) {
  df %>% filter(ParentLocation == r) %>% pull(FactValueNumeric) %>% na.omit()
})
names(groups) <- regions

# ── 2. FUNZIONI ─────────────────────────────────────────────────────────────

chi2_approx <- function(H, df_chi) {
  # Wilson-Hilferty 1931
  z <- ((H/df_chi)^(1/3) - (1 - 2/(9*df_chi))) / sqrt(2/(9*df_chi))
  1 - pnorm(z)
}

kw_two_groups <- function(x, y) {
  nx <- length(x); ny <- length(y)
  N <- nx + ny
  combined <- c(x, y)
  
  # Ranghi con gestione ties
  order <- order(combined)
  ranks <- numeric(N)
  i <- 1
  while (i <= N) {
    j <- i
    while (j < N && combined[order[j]] == combined[order[j+1]]) {
      j <- j + 1
    }
    mr <- (i + j)/2
    ranks[order[i:j]] <- mr
    i <- j + 1
  }
  
  R1 <- sum(ranks[1:nx])
  R2 <- sum(ranks[(nx+1):N])
  
  H <- (12/(N*(N+1))) * (R1^2/nx + R2^2/ny) - 3*(N+1)
  
  # Correzione per ties
  tbl <- table(combined)
  tie_sum <- sum(tbl^3 - tbl)
  C <- 1 - tie_sum/(N^3 - N)
  H_corr <- ifelse(C != 0, H/C, H)
  
  p <- chi2_approx(H_corr, df_chi=1)
  p <- min(max(p, 0), 1)
  
  list(
    H_grezzo   = round(H,4),
    H_corretto = round(H_corr,4),
    C_ties     = round(C,6),
    nx = nx,
    ny = ny,
    p_value = p,
    sig = p < 0.05
  )
}

# ── 3. TUTTI I CONFRONTI A COPPIE ────────────────────────────────────────────
region_pairs <- combn(regions, 2, simplify = FALSE)
results <- list()

for (pair in region_pairs) {
  r1 <- pair[1]; r2 <- pair[2]
  res <- kw_two_groups(groups[[r1]], groups[[r2]])
  res$regione_1 <- r1
  res$regione_2 <- r2
  results <- append(results, list(res))
}

# Ordinamento per p-value
results <- results[order(sapply(results, function(x) x$p_value))]

# ── 4. STAMPA CONSOLE ───────────────────────────────────────────────────────
cat("\n", strrep("=",85), "\n")
cat("KRUSKAL-WALLIS A COPPIE — Decessi infantili < 1 mese (α = 0.05)\n")
cat(sprintf("%-46s %8s %8s %12s %6s\n",
            "Coppia", "H_corr", "C_ties", "p-value", "Sig."))
cat(strrep("-",85), "\n")
for (r in results) {
  pair_name <- paste0(substr(r$regione_1,1,20), "  vs  ", substr(r$regione_2,1,20))
  sym <- ifelse(r$sig, "✓", "✗")
  cat(sprintf("%-46s %8.2f %8.4f %12.4e %6s\n",
              pair_name, r$H_corretto, r$C_ties, r$p_value, sym))
}
cat(strrep("=",85), "\n")
n_sig <- sum(sapply(results, function(x) x$sig))
cat(sprintf("\nConfronti significativi (p < 0.05): %d/%d\n", n_sig, length(results)))

# ── 5. MATRICE p-value ──────────────────────────────────────────────────────
n <- length(regions)
mat <- matrix(NA, n, n)
rownames(mat) <- colnames(mat) <- regions

for (r in results) {
  i <- match(r$regione_1, regions)
  j <- match(r$regione_2, regions)
  mat[i,j] <- r$p_value
  mat[j,i] <- r$p_value
}

# ── 6. HEATMAP −log10(p-value) ─────────────────────────────────────────────
log_mat <- -log10(pmax(mat, 1e-300))
log_mat[is.na(log_mat)] <- NA

thresh <- -log10(0.05)  # ≈1.301
short <- substr(regions, 1, 12)

heatmap_df <- expand.grid(X=1:n, Y=1:n)
heatmap_df$logpv <- as.vector(log_mat)
heatmap_df$text <- ""
heatmap_df$text[heatmap_df$X == heatmap_df$Y] <- "—"
for (i in 1:n) {
  for (j in 1:n) {
    if (i==j || is.na(mat[i,j])) next
    heatmap_df$text[heatmap_df$X==j & heatmap_df$Y==i] <- 
      paste0(sprintf("%.2e", mat[i,j]), "\n", ifelse(mat[i,j]<0.05,"✓","✗"))
  }
}

ggplot(heatmap_df, aes(x=X, y=Y, fill=logpv)) +
  geom_tile(color="#333344") +
  geom_text(aes(label=text), size=3, color=ifelse(heatmap_df$logpv>thresh,"white","#333333")) +
  scale_fill_gradientn(colors=rev(RColorBrewer::brewer.pal(11,"RdYlGn")),
                       limits=c(0,max(log_mat,na.rm=TRUE)),
                       na.value="#0f1117") +
  scale_x_continuous(breaks=1:n, labels=short) +
  scale_y_continuous(breaks=1:n, labels=short) +
  coord_fixed() +
  labs(title="Kruskal-Wallis a coppie — Matrice p-value\nDecessi infantili < 1 mese per Regione OMS",
       fill=expression(-log[10](p))) +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill="#0f1117", color=NA),
    panel.background = element_rect(fill="#1a1d27", color=NA),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(angle=30, hjust=1, color="#cccccc", size=9),
    axis.text.y      = element_text(color="#cccccc", size=9),
    axis.title       = element_text(color="#aaaaaa"),
    plot.title       = element_text(color="white", face="bold", size=12),
    legend.background=element_rect(fill="#1a1d27"),
    legend.text      = element_text(color="#aaaaaa"),
    legend.title     = element_text(color="#aaaaaa")
  )

ggsave("kw_pairwise_heatmap.png", width=10, height=8, dpi=150, bg="#0f1117")
cat("\nGrafico salvato: kw_pairwise_heatmap.png\n")