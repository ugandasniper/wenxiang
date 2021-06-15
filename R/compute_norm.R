#' @title Compute Norm Statistics for Scored Likert Scales
#' @description FUNCTION_DESCRIPTION
#' @param data a dataframe with score of each dimensions
#' @param list_mapping a list indicating the structure of scale
#' @param n_sigma to exclude cases less or greater than n_sigma of distribution, Default: 3
#' @param exclude_ids to manually specify the row number of cases to be excluded for norm computation, otherwise exclude cases by `n_sigma` principle.
#' @return mean, standard deviation, sample size and the rownumber of cases excluded for each dimension
#' @details DETAILS
#' @examples
#' dim_cols = c("dimension-1", "dimension-2", "dimension-3")
#' data <- scoreQuestionnaire(
#'   data = questionnaire_mixed,
#'   df_mapping = mapping_mixed,
#'   dim_cols = dim_cols,
#'   item_col = '序号',
#'   item_type_col = '题型',
#'   key_answer_col = '正确答案'
#' )
#' list_mapping <- get_dimension_mapping_from_df(mapping_mixed, dim_cols = dim_cols)
#' norm_output <- compute_norm(data, list_mapping, n_sigma=3)
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
    if (n_sigma <= 0) stop("`n_sigma` should be greater than 0")

    data <- data %>% tibble::rownames_to_column()
    norm_output <- data.frame()
    .all_dims <- get_all_dimensions_from_mapping_list(list_mapping)

    if (missing(exclude_ids)) {
      ## exclude
      exclude_ids <- list()
      dimensions_to_drop_outliers <- .all_dims
      for (dim in dimensions_to_drop_outliers) {
        M <- data[[dim]] %>% mean(na.rm = TRUE)
        SD <- data[[dim]] %>% stats::sd(na.rm = TRUE)
        .exclude_ids <-
          data[["rowname"]][!dplyr::between(data[[dim]], M - n_sigma * SD, M + n_sigma * SD)]
        exclude_ids[[dim]] <- unique(.exclude_ids)
      }
    }
    data <- data %>% dplyr::filter(!rowname %in% unlist(exclude_ids))

    ## compute norm
    for (dim in .all_dims) {
      .norm_output <- data.frame(
        list(
          dimension = dim,
          M = data[[dim]] %>% mean(na.rm = TRUE),
          SD = data[[dim]] %>% stats::sd(na.rm = TRUE),
          n = data %>% tidyr::drop_na(!!dim) %>% dplyr::pull(dim) %>% length(),
          case_excluded = exclude_ids[[dim]] %>% stringr::str_c(collapse = ",")
        )
      )
      norm_output <- norm_output %>% dplyr::bind_rows(.norm_output)
    }

    return(norm_output)
  }
