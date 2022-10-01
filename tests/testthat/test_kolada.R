outlist=KoladaAPI()
da=matrix(outlist[[1]],ncol=4)
a=colnames(da)=names(outlist[[1]])


test_that("Checking colnames", {
  expect_equal(a[2], c("municipality"))
  expect_equal(a[1], c("kpi"))
  expect_equal(a[3], c("period"))
  expect_equal(a[4], c("value"))
})

test_that("Checking variable type", {
  expect_equal(typeof(da), "list")
  expect_equal(typeof(a), "character")
})


test_that("function should be called without parameter", {
  expect_error(KoladaAPI(1))
  expect_error(KoladaAPI("Stockholm"))
})


test_that("Checking correctness of dimension of output", {
  expect_length(da, 4)
  expect_length(a, 4)
  
})
