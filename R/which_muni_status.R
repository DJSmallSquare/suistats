#' Check which municipality list to use for a given target year
#'
#' @param df Define the data frame you wish to clean that contains at least a municipality id column (optional is an annual time dimension column)
#' @param target_year Define the target year which determines the final list of municipalities
#' @param muni_var Provide the variable name that identifies the municipal ID's
#' @param time_var Provide the variable name for the time dimesion (if left empty, a cross-section is assumed)
#'
#' @return A string indicating which municipality list definition to use, for a given target year
#' @export
#'
#' @importFrom dplyr filter distinct pull "%>%"
#' @importFrom rlang sym
#'
#' @examples
#' mm::which_muni_status(df = vacancy_stock, target_year = 2018)
which_muni_status <- function(df, target_year, muni_var, time_var = NULL) {

  if (any(!c(muni_var, time_var) %in% names(df))) {
    stop("Please check that municipal variable and/or the time variable exist in your data.")
  }

  # Make sure the year is an integer
  target_year <- as.integer(target_year)

  if (is.null(time_var)) {
    ### Check that the target year fully intersects with official list
    matching_lists <-
      lapply(X = mm::official_municipality_lists %>% dplyr::filter(year == target_year) %>% dplyr::distinct(date) %>% dplyr::pull(date),
             FUN = function(x) {
               all.equal(
                 df %>% dplyr::pull(!!sym(muni_var)),
                 mm::official_municipality_lists %>% dplyr::filter(date == x) %>% dplyr::pull(commune_id),
                 tolerance = 0
               )
             }
      ) %>% unlist() %>% as.logical()

    # Error handling
    if (all(is.na(matching_lists))) {
      stop("Please check that the target year and the reference year underlying the cross-section co-incide.")
    }

  } else {
    ### Check that the target year fully intersects with official list
    matching_lists <-
      lapply(X = mm::official_municipality_lists %>% dplyr::filter(year == target_year) %>% dplyr::distinct(date) %>% dplyr::pull(date),
             FUN = function(x) {
               all.equal(
                 df %>% dplyr::filter(!!sym(time_var) == target_year) %>% dplyr::pull(!!sym(muni_var)),
                 mm::official_municipality_lists %>% dplyr::filter(date == x) %>% dplyr::pull(commune_id),
                 tolerance = 0
               )
             }
      ) %>% unlist() %>% as.logical()

    # Error handling
    if (all(is.na(matching_lists))) {
      stop("Please check that the target year exists in data you provided.")
    }

  }

  if (length(which(x = matching_lists)) > 1) {
    print(paste0("Data based on the municipality status as of ", target_year, ".01.01"))
    target_municipality_status <- paste0(target_year, "-01-01")
  } else if (which(x = matching_lists) == 1) {
    print(paste0("Data based on the municipality status as of ", target_year, ".01.01"))
    target_municipality_status <- paste0(target_year, "-01-01")
  } else if (which(x = matching_lists) == 2) {
    print(paste0("Data based on the municipality status as of ", target_year, ".12.31"))
    target_municipality_status <- paste0(target_year, "-12-31")
  }

  return(target_municipality_status)

}
