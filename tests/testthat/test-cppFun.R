test_that("converts geographical coordinates works", {
  convertCoordinates(39.90105, 116.42079,from = 'WGS-84',to = 'GCJ-02')
})
