% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_norm.R
\name{compute_norm}
\alias{compute_norm}
\title{Compute Norm Statistics for Scored Likert Scales}
\usage{
compute_norm(data, list_mapping, n_sigma = 3, exclude_ids)
}
\arguments{
\item{data}{a dataframe with score of each dimensions}

\item{list_mapping}{a list indicating the structure of scale}

\item{n_sigma}{to exclude cases less or greater than n_sigma of distribution, Default: 3}

\item{exclude_ids}{to manually specify the row number of cases to be excluded for norm computation, otherwise exclude cases by \code{n_sigma} principle.}
}
\value{
mean, standard deviation, sample size and the rownumber of cases excluded for each dimension
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
dim_cols = c("dimension-1", "dimension-2", "dimension-3")
data <- scoreQuestionnaire(
  data = questionnaire_mixed,
  df_mapping = mapping_mixed,
  dim_cols = dim_cols,
  item_col = '序号',
  item_type_col = '题型',
  key_answer_col = '正确答案'
)
list_mapping <- get_dimension_mapping_from_df(mapping_mixed, dim_cols = dim_cols)
norm_output <- compute_norm(data, list_mapping, n_sigma=3)

}
\seealso{
\code{\link[tibble]{rownames}}
\code{\link[dplyr]{between}}
}
