# ── Librerie ────────────────────────────────────────────────────────────────
library(readr)
library(dplyr)

# ── Caricamento dati ─────────────────────────────────────────────────────────
df_less_1_month <- read_csv("deathsByCauseLess1Month.csv")
df_less_1_year  <- read_csv("deathsByCauseLess1Year.csv")

# ── Funzione per statistiche descrittive ─────────────────────────────────────
descriptive_stats <- function(df, dataset_name) {
  
  stats <- data.frame(
    Dataset = dataset_name,
    Mean    = mean(df$Value, na.rm = TRUE),
    Median  = median(df$Value, na.rm = TRUE),
    `Std Dev` = sd(df$Value, na.rm = TRUE),
    Min     = min(df$Value, na.rm = TRUE),
    Max     = max(df$Value, na.rm = TRUE)
  )
  
  return(stats)
}

# ── Calcolo statistiche ──────────────────────────────────────────────────────
stats_month <- descriptive_stats(df_less_1_month, "Deaths < 1 Month")
stats_year  <- descriptive_stats(df_less_1_year,  "Deaths < 1 Year")

# ── Dataframe comparativo ────────────────────────────────────────────────────
comparison_df <- bind_rows(stats_month, stats_year)

print(head(comparison_df))