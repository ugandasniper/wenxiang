scoreQuestionnaire <-
  function(data,
           df_mapping,
           dim_cols = c("scale", 'dimension-1', 'dimension-2', 'dimension-3'),
           item_col,
           NP_col,
           how = base::mean) {

  }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param df_mapping PARAM_DESCRIPTION
#' @param dim_cols PARAM_DESCRIPTION, Default: c("dimension-1", "dimension-2")
#' @param item_col PARAM_DESCRIPTION, Default: '序号'
#' @param how PARAM_DESCRIPTION, Default: mean
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' data(test_data, test_mapping)
#' df_questionnarie <- test_data; df_mapping <- test_mapping
#' df_questionnarie <-
#' scoreItem(
#'   df_questionnarie,
#'   df_mapping,
#'   dim_cols = c("scale", 'dimension-1', 'dimension-2'),
#'   item_col = 'id'
#' )
#' @seealso
#'  \code{\link[purrr]{partial}},\code{\link[purrr]{map}}
#' @rdname scoreItem
#' @export
#' @importFrom purrr partial map
scoreItem <-
  function(data,
           df_mapping,
           dim_cols = c("dimension-1", "dimension-2"),
           item_col = "序号",
           how = base::mean) {
    # [default] fill `df_mapping`
    if (any(is.na(df_mapping[dim_cols]))) {
      df_mapping <- fill_mapping(df_mapping)
    }
    # [default] `dim_cols`
    # if (missing(dim_cols)) {
    #   dim_cols <- c("dimension-1", "dimension-2")
    # }
    # determine the function for scoring items
    if (identical(how, base::rowSums) | identical(how, base::rowMeans)) {
      compute_func <-  purrr::partial(how, na.rm = TRUE)
    } else if (is.function(how)) {
      compute_func <- how
    }

    dim_deepest <- rev(dim_cols)[1]
    Dim2Items <- df_mapping[[item_col]] %>%
      split(df_mapping[[dim_deepest]]) %>%
      purrr::map(as.character)
    for (.dimension in names(Dim2Items)) {
      ## TODO: stopifnot
      .items <- Dim2Items[[.dimension]]
      data[[.dimension]] <- data[.items] %>% compute_func()
    }
    return(data)
  }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param list_mapping PARAM_DESCRIPTION
#' @param how PARAM_DESCRIPTION, Default: mean
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' data(test_data, test_mapping)
#' df_questionnarie <- test_data; df_mapping <- test_mapping
#' dim_cols <- c("scale", 'dimension-1', 'dimension-2')
#' df_questionnarie <- scoreItem(df_questionnarie,
#'                               df_mapping,
#'                               dim_cols = dim_cols,
#'                               item_col = 'id')
#' list_mapping <- get_dimension_mapping_from_df(df_mapping, dim_cols = dim_cols)
#' df_questionnarie <- scoreDimension(df_questionnarie,
#'                                    list_mapping)
#' @seealso
#'  \code{\link[purrr]{vec_depth}}, \code{\link[purrr]{partial}}, \code{\link[purrr]{map_if}}
#'  \code{\link[rlist]{list.ungroup}}
#' @export
#' @importFrom purrr vec_depth partial map_depth
#' @importFrom rlist list.ungroup

scoreDimension <- function(data, list_mapping, how = base::mean)
{
  dim_depth <- purrr::vec_depth(list_mapping)

  if (identical(how, sum)) {
    compute_func <-  purrr::partial(rowSums, na.rm = TRUE)
  } else if (identical(how, mean)) {
    compute_func <- purrr::partial(rowMeans, na.rm = TRUE)
  } else if (is.function(how)) {
    compute_func <- how
  }

  .depth_seq <- rev((1:dim_depth)[-1])
  for (.depth in .depth_seq) {
    if (.depth == dim_depth) {
      mapping_list_at_depth <- list_mapping %>% rlist::list.ungroup()
    } else {
      mapping_list_at_depth <-
        list_mapping %>% purrr::map_depth(.depth = .depth - 1, names)
    }

    for (d in names(mapping_list_at_depth)) {
      data[d] <- data[mapping_list_at_depth[[d]]] %>% compute_func()
    }
  }
  return(data)
}


