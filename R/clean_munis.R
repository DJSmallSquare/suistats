#' Clean the municipality list
#'
#' @param df Provide a data frame you want to clean
#' @param vars_to_clean Define the variable name or vector of variable names of variables you need to aggregate
#' @param muni_status Use the output from `mm::which_muni_status` here or define the status by giving a "yyyy-mm-dd" date
#' @param muni_var Define the variable name containing the municipal ID's
#' @param time_var Define the variable name of the time dimension (if left empty, a cross-section is assumed)
#'
#' @return A data frame containing the clean municipality list
#' @export
#'
#' @importFrom dplyr filter left_join distinct between pull select mutate group_by ungroup summarise_at vars if_else "%>%"
#' @importFrom rlang sym syms
#'
#' @examples
#' muni_status <- mm::which_muni_status(df = vacancy_stock, target_year = 2018)
#' mm::clean_munis(df = vacancy_stock, muni_status = muni_status, muni_var = "commune_id", time_var = "year", vars_to_clean = c("vacant", "stock"))
clean_munis <- function(df, muni_status, muni_var, time_var = NULL, vars_to_clean) {

  # Error handling
  if (any(!c(muni_var, time_var, vars_to_clean) %in% names(df))) {
    stop("Please check that variables exist in your data.")
  }

  target_year <- substr(x = muni_status, start = 1, stop = 4) %>% as.integer()
  group_vars <- c(muni_var, time_var)

  ### Clean municipality ids for a given year status
  df_tmp <- df
  for(i in 1:5) {

    df_tmp <-
      df_tmp %>% filter(!!rlang::sym(time_var) <= target_year) %>%
      dplyr::left_join(
        mm::commune_mutations %>%
          dplyr::filter(date <= muni_status, from_commune_id != to_commune_id) %>%
          dplyr::distinct(year, from_commune_id, to_commune_id) %>%
          dplyr::filter(dplyr::between(x = year, left = df %>% dplyr::pull(year) %>% min(), right = df %>% dplyr::pull(year) %>% max())) %>% dplyr::select(-year),
        by = c("commune_id" = "from_commune_id")
      ) %>%
      dplyr::mutate(
        commune_id = dplyr::if_else(!is.na(to_commune_id), to_commune_id, commune_id)
      ) %>% dplyr::group_by(!!!rlang::syms(group_vars)) %>% dplyr::summarise_at(.vars = dplyr::vars(!!!rlang::syms(vars_to_clean)), .funs = sum, na.rm = T) %>% dplyr::ungroup()

  }

  return(df_tmp)

}
