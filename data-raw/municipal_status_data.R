# ====================================================================================================================
### Preparing municipal merger data
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
commune_mutations <-
  read_excel(path = "data-raw/BFS/Communes_mutées.xlsx", sheet = "Données", skip = 1L) %>%
  setNames(nm = c("mutation_id", "from_canton_abb", "from_district", "from_commune_id", "from_commune_name", "to_canton_abb", "to_district", "to_commune_id", "to_commune_name", "date")) %>%
  mutate_at(.vars = vars(mutation_id, from_district, from_commune_id, to_commune_id, to_district), .funs = as.integer) %>%
  mutate(
    date = lubridate::date(date),
    year = as.integer(lubridate::year(date))
  ) %>%
  select(date, year, mutation_id, from_canton_abb, from_district, from_commune_id, from_commune_name, to_canton_abb, to_district, to_commune_id, to_commune_name) %>%
  arrange(date, from_commune_id, to_commune_id)

### Problem cases
problem_cases <-
  read_excel(path = "data-raw/BFS/problem_cases.xlsx", sheet = "Sheet1") %>%
  setNames(nm = c("mutation_id", "from_canton_abb", "from_district", "from_commune_id", "from_commune_name", "to_canton_abb", "to_district", "to_commune_id", "to_commune_name", "date")) %>%
  mutate_at(.vars = vars(mutation_id, from_district, from_commune_id, to_commune_id, to_district), .funs = as.integer) %>%
  mutate(
    date = lubridate::date(date),
    year = as.integer(lubridate::year(date))
  ) %>%
  select(date, year, mutation_id, from_canton_abb, from_district, from_commune_id, from_commune_name, to_canton_abb, to_district, to_commune_id, to_commune_name) %>%
  arrange(date, from_commune_id, to_commune_id)

commune_mutations <-
  rbind(
    commune_mutations,
    problem_cases
  )

official_municipality_lists <-
  list.files(path = "data-raw/BFS/Muncipality_Status", pattern = "xlsx?$", full.names = T) %>%
  grep(pattern = ".*_\\d{4}_\\d{2}_\\d{2}.xlsx$", value = T) %>%
  map(~readxl::read_excel(path = .x) %>%
        mutate(date = as.Date(x = str_extract(string = .x, pattern = "\\d{4}_\\d{2}_\\d{2}"), format = "%Y_%m_%d"))) %>%
  map(setNames, nm = c("history_number", "canton_abb", "district_id", "district_name", "commune_id", "commune_name", "registration_date", "date")) %>%
  reduce(rbind) %>%
  mutate(year = as.integer(lubridate::year(date))) %>%
  arrange(year, commune_id) %>%
  select(date, year, commune_id, commune_name, district_id, district_name, canton_abb)


usethis::use_data(
  commune_mutations,
  official_municipality_lists,
  overwrite = T
)
