library(readxl)
library(dplyr)
library(readr)

journals   <- c("AER", "ECTA", "JPE", "QJE", "Restud")
keep_types <- c("Article", "Article; Early Access")
base_path  <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/Raw"

# Citation Report
citation_out <- file.path(base_path, "Stacked Citation Reports")
dir.create(citation_out, showWarnings = FALSE)
read_citation <- function(path) read_excel(path, skip = 10, col_types = "text")

for (j in journals) {
  files <- list.files(base_path, pattern = paste0("^", j, " - Citation Report \\d+\\.xlsx$"), full.names = TRUE)
  if (length(files) == 0) { message("No citation reports found for: ", j); next }
  stacked <- lapply(files, read_citation) |> bind_rows()
  write_csv(stacked, file.path(citation_out, paste0(j, "_Citation_Report_Stacked.csv")))
  message("Saved: ", j, "_Citation_Report_Stacked.csv (", nrow(stacked), " rows)")
}

# Full Report
full_out <- file.path(base_path, "Stacked Full Reports")
dir.create(full_out, showWarnings = FALSE)
read_full <- function(path) read_excel(path, col_types = "text") |> filter(`Document Type` %in% keep_types)

for (j in journals) {
  files <- list.files(base_path, pattern = paste0("^", j, " - Full Report \\d+\\.xls[x]?$"), full.names = TRUE)
  if (length(files) == 0) { message("No full reports found for: ", j); next }
  stacked <- lapply(files, read_full) |> bind_rows()
  write_csv(stacked, file.path(full_out, paste0(j, "_Full_Report_Stacked.csv")))
  message("Saved: ", j, "_Full_Report_Stacked.csv (", nrow(stacked), " rows)")
}

# Merge
merged_out <- file.path(base_path, "Merged")
dir.create(merged_out, showWarnings = FALSE)

drop_titles <- c("JPE Turnaround Times")

for (j in journals) {
  citation <- read_csv(file.path(citation_out, paste0(j, "_Citation_Report_Stacked.csv")),
                       show_col_types = FALSE) |> mutate(across(everything(), as.character))
  full     <- read_csv(file.path(full_out,     paste0(j, "_Full_Report_Stacked.csv")),
                       show_col_types = FALSE) |>
    mutate(across(everything(), as.character)) |>
    filter(!trimws(`Article Title`) %in% drop_titles)
  
# Step 1: DOI Match
  matched_doi <- inner_join(
    citation |> filter(!is.na(DOI)),
    full     |> filter(!is.na(DOI)),
    by = "DOI", relationship = "many-to-many"
  ) |> distinct(DOI, .keep_all = TRUE)
  
# Step 2: title match for full rows missing DOI
  full_no_doi  <- full     |> filter(is.na(DOI))
  citation_all <- citation |> mutate(Title_clean = trimws(Title))
  
  matched_title <- full_no_doi |>
    mutate(Title_clean = trimws(`Article Title`)) |>
    inner_join(citation_all, by = "Title_clean",
               suffix = c("", ".cit"),
               relationship = "many-to-many") |>
    distinct(Title_clean, .keep_all = TRUE) |>
    select(-Title_clean)
  
# Step 3: hardcoded fix for ECTA truncated title 
  matched_fix <- tibble()
  if (j == "ECTA") {
    fr <- full |> filter(trimws(`Article Title`) == "Understanding Preferences: Demand Types, and the Existence of Equilibrium With Indivisibilities")
    cr <- citation |> filter(trimws(Title) == "Understanding Preferences: Demand Types")
    if (nrow(fr) == 1 && nrow(cr) == 1) {
      matched_fix <- bind_cols(cr, fr |> select(-any_of(names(cr))))
      message("  ECTA fix: matched truncated title")
    }
  }
  
# Step 4: combine
  merged <- bind_rows(matched_doi, matched_title, matched_fix)
  
  unmatched_full <- full |>
    filter(is.na(DOI) | !DOI %in% matched_doi$DOI) |>
    filter(!trimws(`Article Title`) %in% trimws(matched_title$`Article Title`)) |>
    filter(!trimws(`Article Title`) %in% trimws(matched_fix$`Article Title`))
  
  message(j, " | DOI matched: ",   nrow(matched_doi),
          " | Title matched: ", nrow(matched_title),
          " | Fix matched: ",   nrow(matched_fix),
          " | Total: ",         nrow(merged),
          " | Still unmatched: ", nrow(unmatched_full))
  
  write_csv(merged, file.path(merged_out, paste0(j, "_Merged.csv")))
  message("Saved: ", j, "_Merged.csv (", nrow(merged), " rows)")
}

# Combine all journals
combined_out <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/Combined"
dir.create(combined_out, showWarnings = FALSE)

all_journals <- lapply(journals, function(j) {
  read_csv(file.path(merged_out, paste0(j, "_Merged.csv")),
           show_col_types = FALSE) |>
    mutate(across(everything(), as.character), Journal = j)
}) |> bind_rows()

write_csv(all_journals, file.path(combined_out, "All_Journals_Merged.csv"))
message("Saved: All_Journals_Merged.csv (", nrow(all_journals), " rows)")