# ====================================================================================================================
### Preparing municipal mapping with postal codes data
# ====================================================================================================================
### Packages
check_packages <- function(pkg){

  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg) > 0) {
    install.packages(new_pkg, dependencies = TRUE)
  }
}

# Needed packages
packages <- c("tidyverse", "lubridate", "readxl")
check_packages(packages)
lapply(X = packages, FUN = library, character.only = TRUE)
rm(check_packages, packages)
# ====================================================================================================================
### Read in data
plz_to_muni <-
  list.files(path = "data-raw/BFS/PLZ_to_Muni", pattern = "xlsx?$", full.names = T) %>%
  grep(pattern = ".*_\\d{4}_\\d{2}_\\d{2}.xlsx$", value = T) %>%
  map(~readxl::read_excel(path = .x, sheet = "PLZ4 -> GDE - NPA4 -> COM", skip = 10, col_names = T) %>%
        mutate(date = as.Date(x = str_extract(string = .x, pattern = "\\d{4}_\\d{2}_\\d{2}"), format = "%Y_%m_%d"))) %>%
  map(setNames, nm = c("plz", "share_in_muni", "commune_id", "canton_abb", "commune_name", "date")) %>%
  reduce(rbind) %>%
  mutate_at(.vars = vars(plz, commune_id), .funs = as.integer) %>%
  mutate(
    year = as.integer(lubridate::year(date))
  ) %>%
  select(date, year, plz, share_in_muni, commune_id, commune_name, canton_abb) %>%
  arrange(plz, share_in_muni)

usethis::use_data(
  plz_to_muni,
  overwrite = T
)
