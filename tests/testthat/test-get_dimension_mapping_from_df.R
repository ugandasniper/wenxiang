test_that("`get_dimension_mapping_from_df` works", {
  mapping_expected  <-
    list(
      情绪管理能力 = list(
        情绪管理能力 = c("自我情绪识别", "他人情绪识别", "情绪调节能力")
        ),
      `ERC（儿童情绪检核表）` = list(
        情绪调节 = c("情绪调节"),
        情绪管理 = c("情绪管理")
      )
    )

  data(test_mapping)
  mapping_tested <- get_dimension_mapping_from_df(
    test_mapping,
    dim_cols = c(
      'scale',
      'dimension-1',
      'dimension-2'
    )
  )
})
