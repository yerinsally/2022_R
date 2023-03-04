install.packages("readr")
library(readr)

exam <- read_csv("exam.csv",
                 col_names = T,
                 col_types = cols("c", "f", "f", "i", "i", "i"),
                 na = "na",
                 locale = locale('ko', encoding = 'euc-kr')) # 오류해결 조건 추가

# 데이터 탐색 : glimpse, str, summary
install.packages("dplyr")
library(dplyr)
glimpse(exam)
str(exam)
summary(exam)
