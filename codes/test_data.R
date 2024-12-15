
## load test data
library(readr)
#path <- "C:/study/R/time_series_analysis(4-2)/final_project"
#setwd(path)

python.new <- read_csv("QueryResultsPy.csv")
R.new <- read_csv("QueryResultsR.csv")

library(tidyverse)
library(lubridate)
python.new
R.new

## data wrangling
python.new$Month <- ymd(paste0(python.new$...2, python.new$...3, "01"))
python.new$Python <- python.new$d
test.py <- python.new %>% select(Month, Python)

R.new$Month <- ymd(paste0(R.new$...2, R.new$...3, "01"))
R.new$R <- R.new$d
test.R <- R.new %>% select(Month, R)

## change class to ts
test.py <- test.py$Python %>% ts(start=c(2008, 8), end=c(2024, 11), frequency=12)

test.R <- test.R[-1,]
test.R <- test.R$R %>% ts(start=c(2008, 11), end=c(2024, 11), frequency=12)

## finally, select only the future values (24-03 ~ 24-11)
test.py <- test.py %>% window(start=c(2024, 3), end=c(2024, 11))
test.R <- test.R %>% window(start=c(2024, 3), end=c(2024, 11))



