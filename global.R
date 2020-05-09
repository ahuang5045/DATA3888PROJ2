library(shiny)
library(bomrang)
library(tidyverse)
library(forecast)
library(plotly)
library(DescTools)
library(glmnet)
library(RCurl)
library(DT)
library(shinyWidgets)

theme_set(theme_bw(base_size = 18))

modelf = readRDS("final_model.rds")
para = read.csv("finalpara.csv")
cv_eval = readRDS("CV_EVAL_classifers.rds")
reg = readRDS("reg.rds")

#need reg exported here
pam = sort(unique(reg$nzero))
para = para %>% arrange(desc(abs(parameter.values)))

parsedat = function(url) {
  test_dat = read.csv(text=getURL(url), header=T)
  test_dat = test_dat[-1]
  write.csv(test_dat, "extractedfull.csv")
  tdat = test_dat[which(colnames(test_dat) %in% para$X)]
  write.csv(tdat, "extractedtdat.csv")
}

parsefull = function() {
  test_dat = read.csv("extractedfull.csv", header=T)
  test_dat = test_dat[-1]
  return (test_dat)
}

F1 <- function(x) {
  TN <- x[2,2]
  FP <- x[1,2]
  TP <- x[1,1]
  FN <- x[2,1]
  2*TP/(2*TP+FP+FN)
}

readdat = function() {
  a= read.csv("extractedtdat.csv")
  a = a[-1]
  return (a)}

#got to use all 5469 variables!

xdat_all = read.csv("Full_data(allgenes).csv")
xdat = readRDS("xdat.rds")
y = readRDS("y.rds")
