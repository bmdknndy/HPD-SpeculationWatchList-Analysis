library(tidyverse)
library(data.table)
library(fixest)
library(broom)

sunab_ <- fixest:::sunab

df <- fread("data/processed/df.csv") %>%
  mutate(
    bbl       = as.character(bbl),
    year      = as.integer(year),
    sale_year = as.integer(sale_year)
  ) %>%
  filter(!is.na(bbl))

NEVER <- 10000L

df <- df %>%
  mutate(
    SWL_year      = if_else(isSWL == 1, sale_year, NEVER),
    mySWLn_year25 = if_else(isBot25nta == 1, sale_year, NEVER)
  )


summary(select(df, SWL_year, mySWLn_year25, HPDv_all, evicts))

evict_swl <- feglm(
  evicts ~ sunab(SWL_year, 
                 year, 
                 ref.p = -1,
                 bin.rel = list("-2" = -5:-2, "4" = 4:5)) | bbl + year,
  data = df, family = "poisson", vcov = ~bbl
)

evict_nta25 <- feglm(
  evicts ~ sunab(mySWLn_year25, year, ref.p = -1, bin.rel = list("-2" = -5:-2, "4" = 4:5)) | bbl + year,
  data = df,
  family = "poisson",
  vcov = ~ bbl
)

hpd_swl <- feglm(
  HPDv_all ~ sunab(SWL_year, year, ref.p = -1, bin.rel = list("-2" = -5:-2, "4" = 4:5)) | bbl + year,
  data = df,
  family = "poisson",
  vcov = ~ bbl
)

hpd_nta25 <- feglm(
  HPDv_all ~ sunab(mySWLn_year25, year, ref.p = -1, bin.rel = list("-2" = -5:-2, "4" = 4:5)) | bbl + year,
  data = df,
  family = "poisson",
  vcov = ~ bbl
)


# --- save model objects (so your QMD can read them) ---
saveRDS(evict_swl,   "exports/models/evict_swl.rds")
saveRDS(evict_nta25, "exports/models/evict_nta25.rds")
saveRDS(hpd_swl,     "exports/models/hpd_swl.rds")
saveRDS(hpd_nta25,   "exports/models/hpd_nta25.rds")

# --- save tidy coefficient tables (optional but useful) ---
evict_tidy <- bind_rows(
  tidy(evict_swl, conf.int = TRUE)   %>% mutate(outcome = "evicts",   spec_def = "official_swl"),
  tidy(evict_nta25, conf.int = TRUE) %>% mutate(outcome = "evicts",   spec_def = "nta_bot25")
)

hpd_tidy <- bind_rows(
  tidy(hpd_swl, conf.int = TRUE)     %>% mutate(outcome = "HPDv_all", spec_def = "official_swl"),
  tidy(hpd_nta25, conf.int = TRUE)   %>% mutate(outcome = "HPDv_all", spec_def = "nta_bot25")
)

all_results_tidy <- bind_rows(evict_tidy, hpd_tidy)
write_csv(all_results_tidy, "exports/model_results_tidy.csv")

