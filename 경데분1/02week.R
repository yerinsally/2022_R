# 2week : 데이터 탐색 및 분석기초
library(readr)
exam <- read_csv("exam.csv", col_names = T,
                 col_types = cols("c", "f", "f", "i", "i", "i"),
                 na = "na",
                 locale = locale('ko', encoding = 'euc-kr'))

# 데이터 형태 파악하기
library(dplyr)
glimpse(exam)
summary(exam)

# 빈도 파악하기
table(exam$address)
library(descr)
freq(exam$address)
library(ggplot2)
qplot(data = exam, address, fill = gender)

# 기술통계량 구하기
mean(exam$math) # NA 제거해야함
table(is.na(exam$math)) # freq도 가능, NA 1개
mean(exam$math, na.rm = T) # 60.379
var(exam$math, na.rm = T) # 478.815
library(psych)
describe(exam) # trimmed, mad, skew, kurtosis

# 히스토그램
hist(exam$english, breaks = seq(0,100,10))

# 비교 & 논리 연산자
table(exam$address == "원효로") # 5명
table(exam$gender != "Female") # 14명
library(descr)
freq(exam$address == "원효로")
freq(exam$gender != "Female")
table(exam$math <= 50) # 13명
table(exam$english <= 50 & exam$history >= 80) # 6명
table(exam$math >= 90 | exam$history >= 90) # 13명
table(exam$address %in% c("효창동","청파동","서계동")) # 14명


# 실습하기 : weather.csv

# 문제1
library(readr)
weather <- read_csv("weather.csv",
                    locale = locale('ko', encoding = 'euc-kr'))

# 문제2
library(dplyr)
glimpse(weather)

# 문제3
summary(weather)
# A : 일강수량, 평균현지기압

# 문제4
var(weather$일강수량, na.rm = T)
# A : 448.562

weather$요일 <- weekdays(weather$일시) # 척도 = Date
weather$요일구분 <- as.factor(weather$요일구분)

# 문제5
library(psych)
weather_descr <- describe(weather)
# A : 왜도/첨도가 가장 작은 변수는 최고기온/합계일조시간

# 문제6
library(descr)
freq(weather$요일)

# 문제7
hist(weather$평균기온, breaks = seq(-20,50,1))

# 문제8
table(weather$최고기온 > 30 & weather$평균상대습도 > 80)
# A : 4일

# 문제9
table(weather$최저기온 < -10 | weather$합계일조시간 < 1)
# A : 75일

weather$요일 <- factor(weather$요일, levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
library(ggplot2)
qplot(data = weather, 요일, fill = 요일구분)

# 문제10
weather$요일구분 <- factor(weather$요일구분, levels = c("휴일","평일"))
qplot(data = weather, 요일구분, fill = 요일)

# 문제11
library(dplyr)
weather <- weather %>% rename(평균기압 = 평균현지기압)
weather <- weather %>% rename(평균습도 = 평균상대습도)

# 문제12
weather$일강수량 <- ifelse(is.na(weather$일강수량), 0, weather$일강수량)
mean(weather$평균기압, na.rm = T)
weather$평균기압 <- ifelse(is.na(weather$평균기압), 1006.26, weather$평균기압)
table(is.na(weather$일강수량))
table(is.na(weather$평균기압)) # 확인
