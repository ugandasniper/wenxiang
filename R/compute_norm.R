#' @title Compute Norm Statistics for Scored Likert Scales
#' @description FUNCTION_DESCRIPTION
#' @param data a dataframe with score of each dimensions
#' @param list_mapping a list indicating the structure of scale
#' @param n_sigma to exclude cases less or greater than n_sigma of distribution, Default: 3
#' @param exclude_ids to manually specify the row number of cases to be excluded for norm computation, otherwise exclude cases by `n_sigma` principle.
#' @return mean, standard deviation, sample size and the rownumber of cases excluded for each dimension
#' @details DETAILS
#' @examples
#'
#' @seealso
#'  \code{\link[tibble]{rownames}}
#'  \code{\link[dplyr]{between}}
#' @rdname compute_norm
#' @export
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr between
compute_norm <-
  function(data,
           list_mapping,
           n_sigma = 3,
           exclude_ids)
  {
    stop(n_sigma <= 0, "`n_sigma` should be greater than 0")

    data <- data %>% tibble::rownames_to_column()
    norm_output <- data.frame()

    if (missing(exclude_ids)) {
      ## exclude
      exclude_ids <- list()
      dimensions_to_drop_outliers <-
        get_all_dimensions_from_mapping_list(list_mapping)
      for (dim in dimensions_to_drop_outliers) {
        M <- data[[dim]] %>% mean(na.rm = TRUE)
        SD <- data[[dim]] %>% sd(na.rm = TRUE)
        .exclude_ids <-
          data[["rowname"]][!dplyr::between(data[[dim]], M - n_sigma * SD, M + n_sigma * SD)]
        exclude_ids[[dim]] <- unique(.exclude_ids)
      }
    }

    data <- data %>% filter(!rowname %in% unlist(exclude_ids))

    ## compute norm
    for (dim in exclude_dimensions) {
      .norm_output <- data.frame(
        list(
          dimension = dim,
          M = data[[dim]] %>% mean(na.rm = TRUE),
          SD = data[[dim]] %>% sd(na.rm = TRUE),
          n = data %>% drop_na(!!dim) %>% pull(dim) %>% length(),
          case_excluded = exclude_ids[[dim]] %>% str_c(collapse = ",")
        )
      )
      norm_output %<>% bind_rows(.norm_output)
    }

    return(norm_output)
  }
