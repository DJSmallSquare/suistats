#' Performs a final check of cleaning quality and overwrites the original data frame
#'
#' @param df Provide the target data frame to be over-written with the cleaned version
#' @param clean_df Provide the clean data frame from `mm::clean_munis`
#' @param muni_status Use the output from `mm::which_muni_status` here
#' @param muni_var Define the variable name containing the municipal ID's
#' @param time_var Define the variable name of the time dimension (if left empty, a cross-section is assumed)
#'
#' @return The overwritten clean data frame
#' @export
#'
#' @importFrom dplyr distinct pull filter tibble "%>%"
#' @importFrom purrr reduce
#'
#' @examples
#' muni_status <- mm::which_muni_status(df = vacancy_stock, target_year = 2018)
#' df_clean <- mm::clean_munis(df = vacancy_stock, muni_status = muni_status, muni_var = "commune_id", time_var = "year", vars_to_clean = c("vacant", "stock"))
#' mm::clean_quality(df = vacancy_stock, clean_df = clean_df, muni_status = muni_status, muni_var = "commune_id", time_var = "year")
clean_quality <- function(df, clean_df, muni_status, muni_var, time_var = NULL) {

  target_year <- substr(x = muni_status, start = 1, stop = 4) %>% as.integer()

  if (is.null(time_var)) {
    ### Check that quality of matching
    matching_quality_check <-
      complete_list = all.equal(
        clean_df %>% dplyr::pull(!!rlang::sym(muni_var)),
        mm::official_municipality_lists %>% dplyr::filter(date == muni_status) %>% dplyr::pull(commune_id)
      )

  } else {
    ### Check that quality of matching
    matching_quality_check <-
      lapply(X = df %>% dplyr::filter(!!rlang::sym(time_var) <= target_year) %>% dplyr::distinct(!!rlang::sym(time_var)) %>% dplyr::pull(),
             FUN = function(x) {

               check <- dplyr::tibble(
                 year = as.integer(x),
                 complete_list = all.equal(
                   clean_df %>% dplyr::filter(!!rlang::sym(time_var) == x) %>% dplyr::pull(!!rlang::sym(muni_var)),
                   mm::official_municipality_lists %>% dplyr::filter(date == muni_status) %>% dplyr::pull(commune_id)
                 )
               )

             }
      ) %>% purrr::reduce(rbind)

  }

  final_matching_check <- all(matching_quality_check %>% dplyr::pull(complete_list))
  if (final_matching_check) {
    print("You're ready to be the next Da Vinci.")
    df <- clean_df
  } else {
    stop("Please check the data again.")
  }

  return(df)

}
