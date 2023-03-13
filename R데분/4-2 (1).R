# imdb 실습
library(readr)
movie <- read_csv("imdb.csv", col_names = T)

library(dplyr)
glimpse(movie)
movie$Certificate <- as.factor(movie$Certificate)
movie$Genre <- as.factor(movie$Genre)
movie$Released_Year <- as.factor(movie$Released_Year)

install.packages("stringr")
library(stringr)
movie$Runtime <- str_replace_all(string = movie$Runtime,
                                 pattern = " min",
                                 replacement = "")
movie$Runtime <- as.integer(movie$Runtime)

# 데이터 탐색적 분석
summary(movie)
library(descr)
freq(movie$Certificate)

library(ggplot2)
qplot(data = movie, Released_Year, fill = Certificate)

library(psych)
describe(movie$Meta_score)
describe(movie$IMDB_Rating)
# 분산계수 = 표준편차/평균-> MS = 12.38/77.97, IMDB = 0.28/7.95
12.38 / 77.97
0.28 / 7.95

hist(movie$Meta_score, breaks = seq(0, 100, 1))
hist(movie$IMDB_Rating, breaks = seq(0, 10, 0.1))

table(movie$Released_Year == 2019 & movie$Meta_score > 95)
table(is.na(movie$Gross))
