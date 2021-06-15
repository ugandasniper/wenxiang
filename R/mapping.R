#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_mapping PARAM_DESCRIPTION
#' @param dim_cols PARAM_DESCRIPTION, Default: c("scale", "dimension-1", "dimension-2", "dimension-3")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' data(test_mapping)
#' list_mapping <- get_dimension_mapping_from_df(
#'   test_mapping,
#'   dim_cols = c('scale', 'dimension-1', 'dimension-2')
#' )
#' str(list_mapping)
#' @seealso
#'  \code{\link[rlist]{list.parse}}, \code{\link[rlist]{list.group}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{modify}}
#' @rdname get_dimension_mapping_from_df
#' @export
#' @importFrom rlist list.parse list.group
#' @importFrom purrr map modify_depth
get_dimension_mapping_from_df <-
  function(df_mapping,
           dim_cols = c('scale', 'dimension-1', 'dimension-2', 'dimension-3')) {
    dim_depth <- length(dim_cols)
    dim_deepest <- dim_cols[dim_depth]

    ## TODO: if `dim_cols` not in `df_mapping`s column names?
    dim_cols <- colnames(df_mapping) %>% intersect(dim_cols)


    if (any(is.na(df_mapping[dim_cols]))) {
      df_mapping <- fill_mapping(df_mapping)
    }

    .dim_list <-
      rlist::list.parse(df_mapping[dim_cols] %>% unique())
    args_call <-
      purrr::map(dim_cols[1:dim_depth - 1], ~ call("get", .))
    .dim_list <-
      do.call(rlist::list.group, c(.data = quote(.dim_list), args_call))
    .dim_list <- .dim_list %>%
      purrr::modify_depth(.depth = dim_depth, ~ .[[dim_deepest]]) %>%
      purrr::modify_depth(.depth = dim_depth - 1, purrr::flatten_chr)
    return(.dim_list)
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list_mapping PARAM_DESCRIPTION
#' @param only_subdimensions PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{vec_depth}}
#' @rdname get_all_dimensions_from_mapping_list
#' @export
#' @importFrom purrr vec_depth
get_all_dimensions_from_mapping_list <-
  function(list_mapping,
           only_subdimensions = FALSE)
    {
    output <- c()
    mapping_depth <- purrr::vec_depth(list_mapping)
    for (.depth in seq(1, mapping_depth - 1) %>% rev) {
      if (.depth == mapping_depth - 1) {
        dims <- list_mapping %>% modify_depth(.depth - 1, ~ .) %>% unlist(use.names = FALSE)
        output <- c(output, dims)
      }
      if (only_subdimensions & .depth == 1) {
        next
      }
      dims <- list_mapping %>% modify_depth(.depth - 1, names) %>% unlist(use.names = FALSE)
      output <- c(output, dims)
    }
    return(unique(output))
  }



#' @title fill empty mapping dataframe
#' @description fill empties in a mapping dataframe with the higher level dimensions
#' @param df_mapping a dataframe indicating the dimension structure
#' @param dim_cols column names in `df_mapping` indicating the hierarchy of dimensions
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' data(test_mapping)
#' list_mapping <- fill_mapping(test_mapping)
#' print(list_mapping)
#' @rdname fill_mapping
#' @export
#' @importFrom data.table transpose
#' @importFrom tidyr fill
#' @importFrom tidyselect everything
fill_mapping <- function(df_mapping, dim_cols) {
  df_mapping[dim_cols] <- df_mapping[dim_cols] %>%
    data.table::transpose(keep.names = "rn") %>%
    tidyr::fill(tidyselect::everything(), .direction = "down") %>%
    data.table::transpose(make.names = "rn")
  return(df_mapping)
}
