# =============================================================================
# resolve_author_names_final.R
#
# Pipeline:
#   Pass 1–3: OpenAlex DOI match (Exact, Prefix, Stripped)
#   Pass 4–5: WoS internal cross-reference
#   Pass 6: Manual corrections
#   Pass 7: Final stubborn cleanup pass
#
# Outputs:
#   - All_Journals_Merged_Resolved.csv
#   - Resolved_Authors_Changes.csv
#   - STUBBORN_BEFORE_FINAL.csv
#   - STUBBORN_AFTER_FINAL.csv
# =============================================================================

library(stringr)
library(dplyr)
library(tidyr)
library(readr)

# file paths
WOS_FILE    <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/Combined/All_Journals_Merged.csv"
OA_FILE     <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/OpenAlex Data/OpenAlex Data.csv"
OUT_DIR <- "C:/Users/nida_/OneDrive/Documents/ECON 497/Final Project/Data/Combined"
OUT_FULL    <- file.path(OUT_DIR, "All_Journals_Merged_Resolved.csv")
OUT_CHANGES <- file.path(OUT_DIR, "Resolved_Authors_Changes.csv")
OUT_STUB_PRE  <- file.path(OUT_DIR, "STUBBORN_BEFORE_FINAL.csv")
OUT_STUB_POST <- file.path(OUT_DIR, "STUBBORN_AFTER_FINAL.csv")

# helper functions

parse_authors <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_trim(str_split(x, ";")[[1]])
}

get_lastname <- function(author) {
  parts <- str_split(author, ",")[[1]]
  if (length(parts) < 1) return("")
  str_trim(parts[1])
}

get_first <- function(author) {
  parts <- str_split(author, ",")[[1]]
  if (length(parts) < 2) return("")
  # If 3 parts, middle is usually the first-name component
  str_trim(parts[2])
}

is_initial_only <- function(first) {
  clean <- str_remove_all(first, "[\\.\\s\\-]")
  suffixes <- c("I", "II", "III", "IV", "V", "JR", "SR")
  str_detect(clean, "^[A-Z]{1,4}$") && !(clean %in% suffixes)
}

name_to_initials <- function(first) {
  parts <- str_split(first, "[\\s\\.\\-]+")[[1]]
  parts <- parts[nchar(parts) > 0]
  paste0(toupper(substr(parts, 1, 1)), collapse = "")
}

normalize <- function(s) {
  if (is.na(s) || length(s) == 0) return("")
  s <- iconv(s, to = "ASCII//TRANSLIT")
  s <- str_replace_all(s, "[\u2010-\u2015]", "-")
  s <- str_replace_all(s, "[^a-zA-Z0-9]", "")
  tolower(s)
}

normalize_lastname <- function(s) {
  full_norm <- normalize(s)
  stripped  <- str_remove(full_norm, "^(van|von|dela|de|del|der|den|la|le|di|du|el|al)+")
  list(norm = full_norm, stripped = stripped)
}

initials_compatible <- function(a, b) {
  a == b || startsWith(a, b) || startsWith(b, a)
}

extract_stubborn_failures <- function(df, original_col = "Authors.x", resolved_col = "Authors_Resolved") {
  suffixes <- c("I", "II", "III", "IV", "V", "JR", "SR")
  
  out <- df %>%
    mutate(
      orig = str_split(.data[[original_col]], ";"),
      res  = str_split(.data[[resolved_col]], ";")
    ) %>%
    unnest(c(orig, res)) %>%
    mutate(
      orig = str_trim(orig),
      res  = str_trim(res)
    ) %>%
    filter(
      !is.na(orig),
      orig != "",
      orig == res,
      # catches things like:
      # "Smith, J"
      # "Smith, J."
      # "Smith, JA"
      # "Smith, A.I."
      # "Smith, P.-A."
      str_detect(orig, ",\\s*[A-Z][A-Z\\.\\-\\s]{0,6}$")
    ) %>%
    filter(
      !sapply(str_split(orig, ",\\s*"), function(x) any(str_trim(x) %in% suffixes))
    ) %>%
    select(
      Author_Stubborn = orig,
      DOI,
      Title
    ) %>%
    distinct()
  
  out
}

apply_named_fixes_to_author_string <- function(name_str, fix_map) {
  if (is.na(name_str) || name_str == "") return(name_str)
  authors <- parse_authors(name_str)
  resolved <- sapply(authors, function(a) {
    if (a %in% names(fix_map)) fix_map[[a]] else a
  }, USE.NAMES = FALSE)
  paste(resolved, collapse = "; ")
}

# load data
cat("Loading data...\n")
wos <- read.csv(
  WOS_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

oa <- read.csv(
  OA_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Keep original authors for audit trail
wos$Authors_Original <- wos[["Authors.x"]]

# build open alex lookup
cat("Building OpenAlex lookup...\n")

oa$doi_clean <- tolower(str_trim(str_remove(oa$doi, "^https://doi\\.org/")))
doi_to_oa <- list()

for (i in seq_len(nrow(oa))) {
  doi <- oa$doi_clean[i]
  names_str <- oa[["authorships.author.display_name"]][i]
  
  if (is.na(doi) || doi == "" || is.na(names_str) || names_str == "") next
  
  entries <- list()
  
  for (name in str_trim(str_split(names_str, "\\|")[[1]])) {
    parts <- str_split(str_trim(name), "\\s+")[[1]]
    parts <- parts[nchar(parts) > 0]
    
    if (length(parts) < 2) next
    
    ln <- parts[length(parts)]
    fn <- paste(parts[-length(parts)], collapse = " ")
    ini <- name_to_initials(fn)
    nln <- normalize_lastname(ln)
    
    entries[[length(entries) + 1]] <- list(
      norm = nln$norm,
      stripped = nln$stripped,
      initials = ini,
      full = paste0(ln, ", ", fn)
    )
  }
  
  if (length(entries) > 0) {
    doi_to_oa[[doi]] <- entries
  }
}

# build WOS internal lookup
cat("Building WoS internal lookup...\n")

wos_lookup <- list()

for (val in na.omit(wos[["Authors.x"]])) {
  for (author in parse_authors(val)) {
    ln <- get_lastname(author)
    first <- get_first(author)
    
    if (nchar(first) == 0 || is_initial_only(first)) next
    
    ini <- name_to_initials(first)
    nln <- normalize_lastname(ln)
    
    keys <- c(
      paste0(nln$norm, "|||", ini),
      paste0(nln$stripped, "|||", ini)
    )
    
    for (k in keys) {
      if (!k %in% names(wos_lookup)) {
        wos_lookup[[k]] <- author
      }
    }
  }
}

# main resolution
resolve_one <- function(author, doi_clean) {
  parts <- str_split(author, ",")[[1]]
  ln <- str_trim(parts[1])
  
  first <- if (length(parts) == 3) {
    str_trim(parts[2])
  } else {
    get_first(author)
  }
  
  if (nchar(first) == 0 || !is_initial_only(first)) {
    return(list(name = author, method = "not_initial"))
  }
  
  ini <- name_to_initials(first)
  nln <- normalize_lastname(ln)
  
  # Pass 1–3: OpenAlex DOI lookup
  entries <- doi_to_oa[[doi_clean]]
  if (!is.null(entries)) {
    for (e in entries) {
      if (e$norm == nln$norm && initials_compatible(ini, e$initials)) {
        return(list(name = e$full, method = "oa_norm"))
      }
    }
    for (e in entries) {
      if (e$stripped == nln$stripped && initials_compatible(ini, e$initials)) {
        return(list(name = e$full, method = "oa_stripped"))
      }
    }
  }
  
  # Pass 4–5: WoS internal lookup
  keys <- c(
    paste0(nln$norm, "|||", ini),
    paste0(nln$stripped, "|||", ini)
  )
  
  for (k in keys) {
    if (k %in% names(wos_lookup)) {
      return(list(name = wos_lookup[[k]], method = "wos_internal"))
    }
  }
  
  return(list(name = author, method = "unresolved"))
}

resolve_author_string <- function(name_str, doi_clean) {
  if (is.na(name_str) || name_str == "") return(name_str)
  authors <- parse_authors(name_str)
  resolved <- lapply(authors, resolve_one, doi_clean = doi_clean)
  paste(sapply(resolved, `[[`, "name"), collapse = "; ")
}

cat("Resolving author names...\n")
wos$doi_clean <- tolower(str_trim(wos$DOI))
wos$Authors_Resolved <- mapply(
  resolve_author_string,
  wos[["Authors.x"]],
  wos$doi_clean,
  USE.NAMES = FALSE
)

# manual fixes
cat("Applying manual corrections...\n")

manual_fixes <- c(
  "Garrat, R"              = "Garratt, Rod",
  "Haung, J"               = "Huang, Jian",
  "Mitcell, MF"            = "Mitchell, Matthew F.",
  "Postlewatte, A"         = "Postlewaite, Andrew",
  "Lopez-Codova, JE"       = "Lopez-Cordova, J. Ernesto",
  "López-Códova, JE"       = "Lopez-Cordova, J. Ernesto",
  "Bala, V"                = "Bala, Venkatesh",
  "Van Huyck, J"           = "Van Huyck, John B.",
  "Kahn, JA"               = "Kahn, James A.",
  "Van Keilegom, I"        = "Van Keilegom, Ingrid",
  "Muller, HM"             = "Muller, Holger M.",
  "Telegdy, A"             = "Telegdy, Almos",
  "Van Order, R"           = "Van Order, Robert",
  "de Fontenay, CC"        = "de Fontenay, Catherine C.",
  "de la Grandville, O"    = "de La Grandville, Olivier",
  "van der Leij, MJ"       = "van der Leij, Marco J.",
  "Hong, CS"               = "Hong, Chew Soo",
  "Hong, A"                = "Hong, Han",
  "Serrat, A"              = "Serrat, Angel",
  "Rodriguez-Palmero, C"   = "Rodriguez-Palmero, Carlos",
  "Rodríguez-Palmero, C"   = "Rodriguez-Palmero, Carlos",
  "Andersen, TG"           = "Andersen, Torben G.",
  "den Haan, WJ"           = "den Haan, Wouter J.",
  "Li, G"                  = "Li, Guo",
  "Van Den Berg, GJ"       = "van den Berg, Gerard J.",
  "van den Berg, GJ"       = "van den Berg, Gerard J.",
  "Van den Berg, GJ"       = "van den Berg, Gerard J.",
  "Chari, V. V."           = "Chari, Varadarajan V.",
  "Gudmundsson, BR"        = "Gudmundsson, Bjorn R.",
  "La Manna, MMA"          = "La Manna, Manfredi M. A.",
  "Ben McCartney, W."      = "McCartney, W. Ben",
  "Belloni, A."            = "Belloni, Alexandre",
  "Chen, D."               = "Chen, Daniel L.",
  "Chernozhukov, V."       = "Chernozhukov, Victor",
  "Hansen, C."             = "Hansen, Christian",
  "Fernandez-Val, I."      = "Fernandez-Val, Ivan",
  "Darolles, S."           = "Darolles, Serge",
  "Florens, J. P."         = "Florens, Jean-Pierre",
  "Heckman, J. J."         = "Heckman, James J.",
  "Meghir, C."             = "Meghir, Costas",
  "Vytlacil, E."           = "Vytlacil, Edward",
  "von Stengel, B"         = "von Stengel, Bernhard",
  "Moulines, E"            = "Moulines, Eric",
  "Ekeland, I."            = "Ekeland, Ivar",
  "van den Elzen, A"       = "van den Elzen, Antoon",
  "Ghysels, E."            = "Ghysels, Eric",
  "Somanathan, E."         = "Somanathan, Eswaran",
  "Engel, EMRA"            = "Engel, Eduardo M. R. A.",
  "von Thadden, EL"        = "von Thadden, Ernst-Ludwig",
  "DeFraja, G"             = "De Fraja, Gianni",
  "de Garidel-Thoron, T"   = "de Garidel-Thoron, Thomas",
  "da Costa, CE"           = "da Costa, Carlos E.",
  "Reinikka, R"            = "Reinikka, Ritva",
  "Caroli, E"              = "Caroli, Eve",
  "Russell, J. R."         = "Russell, Jeffrey R.",
  "Rai, AS"                = "Rai, Ashok S.",
  "Moen, ER"               = "Moen, Espen R.",
  "Silva, JMCS"            = "Santos Silva, J. M. C.",
  "Bhaskar, V."            = "Bhaskar, Venkataraman",
  "Ignacio Conde-Ruiz, J." = "Conde-Ruiz, J. Ignacio",
  "Ishiguro, S"            = "Ishiguro, Shingo"
)

wos$Authors_Resolved <- sapply(
  wos$Authors_Resolved,
  apply_named_fixes_to_author_string,
  fix_map = manual_fixes,
  USE.NAMES = FALSE
)

# stubborn before final pass
cat("Generating stubborn list before final pass...\n")
true_failures_pre <- extract_stubborn_failures(wos)
write.csv(true_failures_pre, OUT_STUB_PRE, row.names = FALSE)

cat(sprintf("Stubborn count before final pass: %d\n", nrow(true_failures_pre)))

# Final Stubborn fixes
# Add anything left over here after reviewing STUBBORN_BEFORE_FINAL.csv.
# This is the safest place for the last few manual fills.

final_stubborn_fixes <- c(
  "Chiappori, P.-A." = "Chiappori, Pierre-Andre",
  "Libassi, CJ"      = "Libassi, Clifford J.",
  "Magnac, T."       = "Magnac, Thierry",
  "Khwaja, A.I."     = "Khwaja, Asim Ijaz"
)

wos$Authors_Resolved <- sapply(
  wos$Authors_Resolved,
  apply_named_fixes_to_author_string,
  fix_map = final_stubborn_fixes,
  USE.NAMES = FALSE
)

# Change log
cat("Creating change log...\n")

changes <- wos %>%
  mutate(
    Authors_Original = ifelse(is.na(Authors_Original), "", Authors_Original),
    Authors_Resolved = ifelse(is.na(Authors_Resolved), "", Authors_Resolved)
  ) %>%
  filter(
    Authors_Original != "",
    Authors_Resolved != "",
    Authors_Original != Authors_Resolved
  ) %>%
  select(
    DOI,
    Title,
    Authors_Original,
    Authors_Resolved
  )

write.csv(changes, OUT_CHANGES, row.names = FALSE)

# Stubborn after final pass
cat("Generating stubborn list after final pass...\n")
true_failures_post <- extract_stubborn_failures(wos)
write.csv(true_failures_post, OUT_STUB_POST, row.names = FALSE)

cat(sprintf("Final stubborn count after final pass: %d\n", nrow(true_failures_post)))

# FINAL COSMETIC CLEANUP:
# Fix particle surnames like:
#   "Wincoop, Eric van"   -> "van Wincoop, Eric"
#   "Tella, Rafael Di"    -> "Di Tella, Rafael"
#   "Croix, David de la"  -> "de la Croix, David"
# Also transliterate accents to ASCII for consistency.

fix_particle_name <- function(author) {
  if (is.na(author) || author == "") return(author)
  
  author <- str_trim(author)
  
  # Case 1: single trailing particle
  # "Wincoop, Eric van" -> "van Wincoop, Eric"
  m1 <- str_match(author, "^([^,]+),\\s+(.+?)\\s+(van|von|de|di|du|den|der|del|la|le)$")
  if (!all(is.na(m1))) {
    last_core <- str_trim(m1[2])
    first     <- str_trim(m1[3])
    particle  <- str_trim(m1[4])
    return(paste0(particle, " ", last_core, ", ", first))
  }
  
  # Case 2: two-word trailing particles
  # "Croix, David de la" -> "de la Croix, David"
  # "Berg, Gerard van den" -> "van den Berg, Gerard"
  m2 <- str_match(author, "^([^,]+),\\s+(.+?)\\s+(de\\s+la|van\\s+den|van\\s+der)$")
  if (!all(is.na(m2))) {
    last_core <- str_trim(m2[2])
    first     <- str_trim(m2[3])
    particle  <- str_trim(m2[4])
    return(paste0(particle, " ", last_core, ", ", first))
  }
  
  # Case 3: optional extra common cases
  # "Grandville, Olivier de la" -> "de la Grandville, Olivier"
  # "Fontenay, Catherine C. de" -> "de Fontenay, Catherine C."
  m3 <- str_match(author, "^([^,]+),\\s+(.+?)\\s+(de\\s+los|de\\s+las|de\\s+le|de)$")
  if (!all(is.na(m3))) {
    last_core <- str_trim(m3[2])
    first     <- str_trim(m3[3])
    particle  <- str_trim(m3[4])
    return(paste0(particle, " ", last_core, ", ", first))
  }
  
  author
}

fix_particle_string <- function(name_str) {
  if (is.na(name_str) || name_str == "") return(name_str)
  authors <- parse_authors(name_str)
  authors <- sapply(authors, fix_particle_name, USE.NAMES = FALSE)
  paste(authors, collapse = "; ")
}

# Apply cosmetic particle cleanup
wos$Authors_Resolved <- sapply(
  wos$Authors_Resolved,
  fix_particle_string,
  USE.NAMES = FALSE
)

# normalize accents to ASCII for cleaner consistency
# Comment this out if you want to preserve accents.
ascii_clean <- function(x) {
  if (is.na(x) || x == "") return(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_replace_all(x, "[\u2010-\u2015]", "-")
  x
}

wos$Authors_Resolved <- sapply(wos$Authors_Resolved, ascii_clean, USE.NAMES = FALSE)

# Save the final dataset
cat("Saving final resolved dataset...\n")
write.csv(wos, OUT_FULL, row.names = FALSE)

cat("\nDone.\n")
cat(sprintf("Resolved dataset: %s\n", OUT_FULL))
cat(sprintf("Change log: %s\n", OUT_CHANGES))
cat(sprintf("Stubborn before final pass: %s\n", OUT_STUB_PRE))
cat(sprintf("Stubborn after final pass: %s\n", OUT_STUB_POST))