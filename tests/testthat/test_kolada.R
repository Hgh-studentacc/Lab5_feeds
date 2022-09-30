library(httr)
library(shiny)
library(readxl)
library(ggplot2)
url1="https://raw.githubusercontent.com/muhis097/Lab5/main/data/List_kommun.xlsx"
url2="https://raw.githubusercontent.com/muhis097/Lab5/main/data/factors.xlsx"
GET(url1, write_disk(tfile1 <- tempfile(fileext = ".xlsx")))
GET(url2, write_disk(tfiles <- tempfile(fileext = ".xlsx")))
List_kommun = read_excel(tfile1,col_types = "text")
List_factors = read_excel(tfiles,col_types = "text",col_names=FALSE)
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


