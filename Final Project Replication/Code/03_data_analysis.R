library(tidyverse)
library(fixest)
library(gender)
library(lmtest)
library(car)


data_path <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/Combined/All_Journals_Merged_Resolved.csv"
plots_dir <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/plots"
dir.create(plots_dir, showWarnings = FALSE)

journals  <- c("AER", "ECTA", "JPE", "QJE", "Restud")
post_year <- 2013L
col_aer   <- "#2C5F8A"
col_ctrl  <- "#A0A0A0"

run_did <- function(data, outcome, rhs = "did_2yr") {
  feols(as.formula(paste0(outcome, " ~ ", rhs, " | journal + year")),
        data = data, cluster = ~journal)
}

# load data

df_raw <- read_csv(data_path, show_col_types = FALSE)

df_all <- df_raw %>%
  mutate(
    year         = as.integer(`Publication Year.x`),
    journal      = Journal,
    authors      = Authors_Resolved,
    affiliations = Affiliations
  ) %>%
  filter(
    between(year, 2000, 2022),
    journal %in% journals,
    !is.na(authors)
  ) %>%
  select(year, journal, authors, affiliations)

cat("Full sample (2000-2022):", nrow(df_all), "\n")

# institutional classification
exclude_pattern <- regex(
  "Pennsylvania State|Pennsylvania Commonwealth|British Columbia",
  ignore_case = TRUE
)

top10 <- c(
  "Massachusetts Institute of Technology",
  "Harvard University",
  "Princeton University",
  "University of Chicago",
  "Stanford University",
  "Yale University",
  "University of California Berkeley",
  "Northwestern University",
  "Columbia University",
  "University of Pennsylvania"
)

top15 <- c(
  top10,
  "New York University",
  "University of Michigan",
  "University of California Los Angeles",
  "London School",
  "University of California San Diego"
)

is_elite <- function(affil_string, elite_list) {
  if (is.na(affil_string)) return(NA_integer_)
  institutions <- str_split(affil_string, ";")[[1]] %>% str_trim()
  institutions <- institutions[!str_detect(institutions, exclude_pattern)]
  if (length(institutions) == 0) return(0L)
  as.integer(any(str_detect(
    institutions,
    regex(paste(elite_list, collapse = "|"), ignore_case = TRUE)
  )))
}

df_all <- df_all %>%
  mutate(
    elite10           = map_int(affiliations, is_elite, elite_list = top10),
    elite15           = map_int(affiliations, is_elite, elite_list = top15),
    first_affil       = map_chr(affiliations, ~ {
      if (is.na(.x)) NA_character_
      else str_trim(str_split(.x, ";")[[1]][1])
    }),
    elite10_firstonly = map_int(first_affil, ~ {
      if (is.na(.x)) return(NA_integer_)
      cleaned <- str_trim(str_split(.x, ";")[[1]])
      cleaned <- cleaned[!str_detect(cleaned, exclude_pattern)]
      if (length(cleaned) == 0) return(0L)
      as.integer(any(str_detect(
        cleaned,
        regex(paste(top10, collapse = "|"), ignore_case = TRUE)
      )))
    })
  )

# validation: check for pure false positives only
pure_false <- df_all %>%
  filter(
    str_detect(affiliations,
               regex("Pennsylvania State|Pennsylvania Commonwealth|British Columbia",
                     ignore_case = TRUE)),
    elite10 == 1
  ) %>%
  filter(
    !str_detect(affiliations,
                regex(paste(top10, collapse = "|"), ignore_case = TRUE))
  )

cat(sprintf("Pure false positives (no real elite co-occurring): %d\n",
            nrow(pure_false)))

if (nrow(pure_false) == 0) {
  cat("Validation passed: all elite10=1 codings are legitimate\n")
} else {
  warning("Pure false positives remain — inspect pure_false object")
  print(pure_false %>% select(affiliations))
}

# author count

df_all <- df_all %>%
  mutate(n_authors = str_count(authors, ";") + 1)

# gender coding

df_all <- df_all %>%
  mutate(
    row_id       = row_number(),
    first_author = str_trim(map_chr(str_split(authors, ";"), 1)),
    first_name   = case_when(
      str_detect(first_author, ",") ~
        word(str_trim(str_extract(first_author, "(?<=,).*")), 1),
      TRUE ~ word(first_author, 1)
    )
  )

gender_lookup <- gender(unique(na.omit(df_all$first_name)),
                        method = "ssa") %>%
  select(name, proportion_female) %>%
  rename(first_name = name)

df_all <- df_all %>%
  left_join(gender_lookup, by = "first_name") %>%
  mutate(
    female_first = case_when(
      proportion_female >= 0.80 ~ 1L,
      proportion_female <= 0.20 ~ 0L,
      TRUE                      ~ NA_integer_
    )
  )

all_authors_long <- df_all %>%
  select(row_id, authors) %>%
  separate_rows(authors, sep = ";") %>%
  mutate(
    author = str_trim(authors),
    fname  = case_when(
      str_detect(author, ",") ~
        word(str_trim(str_extract(author, "(?<=,).*")), 1),
      TRUE ~ word(author, 1)
    )
  ) %>%
  left_join(gender_lookup, by = c("fname" = "first_name")) %>%
  mutate(is_female = if_else(proportion_female >= 0.80, 1L, 0L,
                             missing = NA_integer_))

author_gender_summary <- all_authors_long %>%
  group_by(row_id) %>%
  summarise(
    any_female   = as.integer(any(is_female == 1, na.rm = TRUE)),
    female_share = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

df_all <- df_all %>%
  left_join(author_gender_summary, by = "row_id")

cat(sprintf("Gender match coverage: %.1f%%\n",
            100 * mean(!is.na(df_all$female_first))))

# citations within next 5 years of publication

year_cit_cols <- names(df_raw)[grepl("^[0-9]{4}\\.0$", names(df_raw))]
cat("Year citation columns found:", length(year_cit_cols), "\n")

df_cit_raw <- df_raw %>%
  mutate(year = as.integer(`Publication Year.x`)) %>%
  filter(
    between(year, 2000, 2022),
    Journal %in% journals,
    !is.na(Authors_Resolved)
  )

cit_matrix            <- df_cit_raw %>%
  select(all_of(year_cit_cols)) %>%
  as.matrix()
cit_matrix[is.na(cit_matrix)] <- 0
pub_years_all         <- df_cit_raw$year

df_all$cit_5yr <- sapply(seq_len(nrow(cit_matrix)), function(i) {
  yr   <- pub_years_all[i]
  cols <- paste0(yr:(yr + 4), ".0")
  cols <- cols[cols %in% year_cit_cols]
  sum(cit_matrix[i, cols])
})

cat("Citation merge check:", sum(!is.na(df_all$cit_5yr)),
    "of", nrow(df_all), "\n")

# did variables + additional analysis

df_all <- df_all %>%
  mutate(
    treat    = as.integer(journal == "AER"),
    post_3yr = as.integer(year >= 2014),
    post_2yr = as.integer(year >= 2013),
    post_1yr = as.integer(year >= 2012),
    post_0yr = as.integer(year >= 2011),
    did_3yr  = treat * post_3yr,
    did_2yr  = treat * post_2yr,
    did_1yr  = treat * post_1yr,
    did_0yr  = treat * post_0yr,
    rel_year = year - post_year
  )

df      <- df_all %>% filter(between(year, 2008, 2018))
df_full <- df_all %>% filter(between(year, 2000, 2022))
df_nocv <- df_all %>% filter(between(year, 2000, 2019))

cat("\nMain sample (2008-2018):", nrow(df), "\n")
print(table(df$journal))

# summary stats

cat("\n===== TABLE 1: SUMMARY STATISTICS =====\n")

sumstats <- df %>%
  group_by(journal) %>%
  summarise(
    N           = n(),
    Elite10     = round(mean(elite10,      na.rm = TRUE), 3),
    Elite15     = round(mean(elite15,      na.rm = TRUE), 3),
    Fem_First   = round(mean(female_first, na.rm = TRUE), 3),
    Any_Female  = round(mean(any_female,   na.rm = TRUE), 3),
    Fem_Share   = round(mean(female_share, na.rm = TRUE), 3),
    Avg_Authors = round(mean(n_authors,    na.rm = TRUE), 2),
    Cit5_Mean   = round(mean(cit_5yr,      na.rm = TRUE), 1),
    .groups = "drop"
  )

all_row <- df %>%
  summarise(
    journal     = "All",
    N           = n(),
    Elite10     = round(mean(elite10,      na.rm = TRUE), 3),
    Elite15     = round(mean(elite15,      na.rm = TRUE), 3),
    Fem_First   = round(mean(female_first, na.rm = TRUE), 3),
    Any_Female  = round(mean(any_female,   na.rm = TRUE), 3),
    Fem_Share   = round(mean(female_share, na.rm = TRUE), 3),
    Avg_Authors = round(mean(n_authors,    na.rm = TRUE), 2),
    Cit5_Mean   = round(mean(cit_5yr,      na.rm = TRUE), 1)
  )

bind_rows(sumstats, all_row) %>% print()

# 2x2 DID
cat("\n===== RAW 2x2 DiD (elite10) =====\n")

table2 <- df %>%
  mutate(
    group  = if_else(treat == 1, "AER", "Controls"),
    period = if_else(post_2yr == 1, "Post", "Pre")
  ) %>%
  group_by(group, period) %>%
  summarise(
    elite10_mean = mean(elite10, na.rm = TRUE),
    n            = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from  = period,
              values_from = c(elite10_mean, n))

print(table2)

# diagnostics

cat("\n===== DIAGNOSTICS =====\n")

m_lm <- lm(elite10 ~ did_2yr + factor(journal) + factor(year),
           data = df)

cat("\nBreusch-Pagan:\n");       print(bptest(m_lm))
cat("\nDurbin-Watson:\n");       print(dwtest(m_lm))
cat("\nRESET test:\n");          print(resettest(m_lm))
cat("\nVIF (first 3 terms):\n"); print(vif(m_lm)[1:3])
cat(sprintf("\nLPM fitted value range: [%.3f, %.3f]\n",
            min(fitted(m_lm)), max(fitted(m_lm))))

# parallel trends

cat("\n===== PARALLEL TRENDS: PRE-PERIOD COEFFICIENTS =====\n")

df_pre <- df %>%
  filter(year < 2013) %>%
  mutate(rel_year_f = relevel(factor(rel_year), ref = "-1"))

m_pretrend <- feols(
  elite10 ~ i(rel_year_f, treat, ref = "-1") | journal + year,
  data = df_pre, cluster = ~journal
)
print(coeftable(m_pretrend))

# main regressions
cat("\n===== TABLE 3: INSTITUTIONAL DiD =====\n")

m_inst_10    <- run_did(df, "elite10")
m_inst_nauth <- feols(elite10 ~ did_2yr + n_authors | journal + year,
                      data = df, cluster = ~journal)
m_inst_15    <- run_did(df, "elite15")

etable(m_inst_10, m_inst_nauth, m_inst_15,
       headers = c("Top-10", "Top-10 + Team Size", "Top-15"),
       digits  = 4)

# test gender mechanism
cat("\n===== TABLE 4: GENDER DiDs (E&P Mechanism Test) =====\n")

m_fem_first <- run_did(df, "female_first")
m_any_fem   <- run_did(df, "any_female")
m_fem_share <- run_did(df, "female_share")

etable(m_fem_first, m_any_fem, m_fem_share,
       headers  = c("Female First", "Any Female", "Female Share"),
       digits   = 4,
       se.below = TRUE)

cat("\n===== INSTITUTION x GENDER INTERACTION =====\n")

df_int <- df %>% filter(!is.na(female_first), !is.na(elite10))

m_int <- feols(
  elite10 ~ did_2yr + female_first + did_2yr:female_first | journal + year,
  data = df_int, cluster = ~journal
)

etable(m_int, digits = 4, se.below = TRUE)

# permutation inference
cat("\n===== PERMUTATION / PLACEBO REASSIGNMENT =====\n")

perm_results <- map_df(journals, function(j) {
  df_perm <- df %>%
    mutate(
      treat_perm = as.integer(journal == j),
      did_perm   = treat_perm * post_2yr
    )
  m_perm <- feols(elite10 ~ did_perm | journal + year,
                  data = df_perm, cluster = ~journal)
  tibble(
    treated_journal = j,
    beta            = coef(m_perm)["did_perm"],
    se              = se(m_perm)["did_perm"]
  )
})

perm_results %>%
  arrange(desc(beta)) %>%
  mutate(rank = row_number()) %>%
  print()

real_beta     <- perm_results %>%
  filter(treated_journal == "AER") %>% pull(beta)
placebo_betas <- perm_results %>%
  filter(treated_journal != "AER") %>% pull(beta)

cat(sprintf("\nOne-sided permutation p-value: %.3f\n",
            mean(placebo_betas >= real_beta)))

# lag structure robustness check

cat("\n===== APPENDIX C1: ALTERNATIVE LAG STRUCTURES =====\n")

map_df(c("did_3yr", "did_2yr", "did_1yr", "did_0yr"), function(dv) {
  m <- feols(as.formula(paste("elite10 ~", dv, "| journal + year")),
             data = df, cluster = ~journal)
  tibble(
    spec    = dv,
    beta    = coef(m)[dv],
    se      = se(m)[dv],
    p_clust = pvalue(m)[dv]
  )
}) %>% print()

# sample windows check (robustness)

cat("\n===== APPENDIX C2: ALTERNATIVE SAMPLE WINDOWS =====\n")

list(
  "Main (2008-2018)"     = df,
  "Full (2000-2022)"     = df_full,
  "No COVID (2000-2019)" = df_nocv
) %>%
  imap_dfr(function(d, nm) {
    m <- feols(elite10 ~ did_2yr | journal + year,
               data = d, cluster = ~journal)
    tibble(
      sample  = nm,
      beta    = coef(m)["did_2yr"],
      se      = se(m)["did_2yr"],
      p_clust = pvalue(m)["did_2yr"]
    )
  }) %>% print()

# alternative elite robustness check

cat("\n===== APPENDIX C3: ALTERNATIVE INSTITUTIONAL DEFINITIONS =====\n")

map_df(c("elite10", "elite10_firstonly", "elite15"), function(dv) {
  m <- feols(as.formula(paste(dv, "~ did_2yr | journal + year")),
             data = df, cluster = ~journal)
  tibble(
    definition = dv,
    beta       = coef(m)["did_2yr"],
    se         = se(m)["did_2yr"],
    p_clust    = pvalue(m)["did_2yr"]
  )
}) %>% print()

# LOOV robustness check
cat("\n===== APPENDIX C4: LEAVE-ONE-OUT =====\n")

map_df(c("ECTA", "JPE", "QJE", "Restud"), function(j) {
  m <- feols(elite10 ~ did_2yr | journal + year,
             data    = filter(df, journal != j),
             cluster = ~journal)
  tibble(
    dropped = j,
    beta    = coef(m)["did_2yr"],
    se      = se(m)["did_2yr"],
    p_clust = pvalue(m)["did_2yr"]
  )
}) %>% print()

# gender outcomes
cat("\n===== APPENDIX C5: GENDER DiD ESTIMATES =====\n")

map_df(c("female_first", "any_female", "female_share"), function(dv) {
  m <- feols(as.formula(paste(dv, "~ did_2yr | journal + year")),
             data = df, cluster = ~journal)
  tibble(
    outcome = dv,
    beta    = coef(m)["did_2yr"],
    se      = se(m)["did_2yr"],
    p_clust = pvalue(m)["did_2yr"]
  )
}) %>% print()

# citation analysis

cat("\n===== APPENDIX D: CITATION ANALYSIS =====\n")

m_cit_elite <- feols(cit_5yr ~ elite10  | journal + year,
                     data = df, cluster = ~journal)
m_cit_ols   <- feols(cit_5yr ~ did_2yr  | journal + year,
                     data = df, cluster = ~journal)
m_cit_pois  <- fepois(cit_5yr ~ did_2yr | journal + year,
                      data = df, cluster = ~journal)

etable(m_cit_elite, m_cit_ols, m_cit_pois,
       headers = c("Elite Premium", "DiD OLS", "DiD Poisson"),
       digits  = 4)

# gender match diagnostic

cat("\n===== APPENDIX A: GENDER MATCH COVERAGE =====\n")

df %>%
  mutate(matched = !is.na(female_first)) %>%
  group_by(journal, post_2yr) %>%
  summarise(
    n       = n(),
    matched = sum(matched),
    pct     = round(matched / n * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(period = if_else(post_2yr == 1, "Post", "Pre")) %>%
  select(journal, period, n, matched, pct) %>%
  print()

m_match <- feols(
  as.integer(!is.na(female_first)) ~ did_2yr | journal + year,
  data = df, cluster = ~journal
)
cat("\nDoes match rate vary with treatment? (should be ~0):\n")
print(coeftable(m_match))

# figures

cat("\n===== GENERATING FIGURES =====\n")

theme_paper <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# Figure 1: Elite share over time
fig1_data <- df %>%
  filter(!is.na(elite10)) %>%
  group_by(journal, year) %>%
  summarise(elite_share = mean(elite10), .groups = "drop")

p1 <- ggplot(fig1_data,
             aes(x        = year,
                 y        = elite_share,
                 group    = journal,
                 colour   = journal == "AER",
                 linetype = journal == "AER")) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2013,
             linetype   = "dashed",
             colour     = "grey40") +
  annotate("rect",
           xmin  = 2008, xmax = 2018,
           ymin  = -Inf, ymax = Inf,
           alpha = 0.04, fill = "steelblue") +
  scale_colour_manual(
    values = c("TRUE" = col_aer,  "FALSE" = col_ctrl),
    labels = c("TRUE" = "AER",    "FALSE" = "Control journals")
  ) +
  scale_linetype_manual(
    values = c("TRUE" = "solid",  "FALSE" = "dashed"),
    labels = c("TRUE" = "AER",    "FALSE" = "Control journals")
  ) +
  labs(x        = "Year",
       y        = "Share affiliated with top-10 department",
       colour   = NULL,
       linetype = NULL) +
  theme_paper

ggsave(file.path(plots_dir, "fig1_elite_share_over_time.png"),
       p1, width = 7, height = 4, dpi = 300)
cat("Saved fig1\n")

# Figure 2: Event study
df_es <- df %>%
  filter(!is.na(elite10)) %>%
  mutate(rel_year_f = relevel(factor(rel_year), ref = "-1"))

m_es <- feols(
  elite10 ~ i(rel_year_f, treat, ref = "-1") | journal + year,
  data = df_es, cluster = ~journal
)

es_df <- coeftable(m_es) %>%
  as_tibble(rownames = "term") %>%
  filter(str_detect(term, "rel_year_f")) %>%
  mutate(
    rel = as.integer(str_extract(term, "-?\\d+")),
    lo  = Estimate - 1.96 * `Std. Error`,
    hi  = Estimate + 1.96 * `Std. Error`
  ) %>%
  add_row(rel = -1, Estimate = 0, `Std. Error` = 0, lo = 0, hi = 0)

p2 <- ggplot(es_df, aes(x = rel, y = Estimate)) +
  geom_hline(yintercept = 0,
             linetype   = "dashed",
             colour     = "grey50") +
  geom_vline(xintercept = 0,
             linetype   = "dashed",
             colour     = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              alpha = 0.15, fill = "steelblue") +
  geom_line(colour = col_aer) +
  geom_point(colour = col_aer, size = 2) +
  labs(x = "Years relative to post-treatment threshold (2013 = 0)",
       y = "AER coefficient on elite10") +
  theme_paper

ggsave(file.path(plots_dir, "fig2_event_study.png"),
       p2, width = 7, height = 4, dpi = 300)
cat("Saved fig2\n")

# Figure 3: Pre/post elite share by journal
fig3_data <- df %>%
  filter(!is.na(elite10)) %>%
  mutate(period = if_else(post_2yr == 1,
                          "Post (2013-2018)",
                          "Pre (2008-2012)")) %>%
  group_by(journal, period) %>%
  summarise(elite_share = mean(elite10), .groups = "drop") %>%
  mutate(journal = reorder(journal, elite_share))

p3 <- ggplot(fig3_data,
             aes(x = journal, y = elite_share, fill = period)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(
    values = c("Pre (2008-2012)"  = "grey70",
               "Post (2013-2018)" = col_aer)
  ) +
  labs(x = NULL, y = "Elite-10 share", fill = NULL) +
  theme_paper

ggsave(file.path(plots_dir, "fig3_elite_prepost_by_journal.png"),
       p3, width = 7, height = 4, dpi = 300)
cat("Saved fig3\n")

# Figure 4: Elite citation premium pre/post by journal
fig4_data <- df %>%
  filter(!is.na(elite10)) %>%
  mutate(
    elite_label = if_else(elite10 == 1, "Elite-affiliated", "Non-elite"),
    period      = if_else(post_2yr == 1,
                          "Post (2013-2018)",
                          "Pre (2008-2012)")
  ) %>%
  group_by(journal, elite_label, period) %>%
  summarise(mean_cit = mean(cit_5yr, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(fig4_data,
             aes(x      = period,
                 y      = mean_cit,
                 colour = elite_label,
                 group  = elite_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~journal, nrow = 1) +
  scale_colour_manual(
    values = c("Elite-affiliated" = col_aer,
               "Non-elite"        = col_ctrl)
  ) +
  labs(x = NULL, y = "Mean 5-year citations", colour = NULL) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(plots_dir, "fig4_elite_citation_premium.png"),
       p4, width = 11, height = 5, dpi = 300)
cat("Saved fig4\n")

# DONE
cat("\nAll done. Figures saved to:", plots_dir, "\n")
