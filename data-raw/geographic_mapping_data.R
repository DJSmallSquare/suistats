# ====================================================================================================================
### Preparing geographic mapping data
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
geographic_mapping <-
  list.files(path = "data-raw/BFS/Geographic_Groups", pattern = "xlsx?$", full.names = T) %>%
  grep(pattern = ".*_\\d{4}_\\d{2}_\\d{2}.xlsx$", value = T) %>%
  map(~readxl::read_xlsx(path = .x, sheet = "Données", skip = 1, col_names = T) %>%
        mutate(date = as.Date(x = str_extract(string = .x, pattern = "\\d{4}_\\d{2}_\\d{2}"), format = "%Y_%m_%d"))) %>%
  map(setNames, nm = c("commune_id", "commune_name", "canton_id", "canton_abb", "district_id", "district_name", "large_regions", "metro_area", "agglomeration_2000", "urban_area", "ms_region", "employ_area", "ms_region2", "mountain_region", "date")) %>%
  reduce(rbind) %>%
  filter(!is.na(commune_id)) %>%
  mutate_at(.vars = vars(commune_id, canton_id, district_id, large_regions:mountain_region), .funs = as.integer) %>%
  mutate(year =as.integer(lubridate::year(date))) %>%
  arrange(year, date, commune_id) %>%
  select(date, year, commune_id, commune_name, canton_id, canton_abb, district_id, district_name, large_regions, metro_area, agglomeration_2000, urban_area, ms_region, employ_area, mountain_region)

usethis::use_data(
  geographic_mapping,
  overwrite = T
)
