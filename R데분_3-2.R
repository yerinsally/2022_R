# 빈도수 구하기
table(exam$address)

install.packages("descr")
library(descr)
freq(exam$address)

install.packages("ggplot2")
library(ggplot2)
qplot(data = exam, address, fill = gender)
qplot(data = exam, class, fill = gender)

# 기술통계량 구하기
table(is.na(exam$math))
mean(exam$math, na.rm = T) # 결측치 제거 조건

install.packages("psych")
library(psych)
describe(exam$math)
describe(exam$english)
describe(exam$history)
hist(exam$english, breaks = seq(0, 100, by = 5))
hist(exam$history, breaks = seq(0, 100, by = 10))
