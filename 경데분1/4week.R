# 4week 데이터 전처리 (2) : weather.csv
# R데분에 없는 내용임

weather_new <- weather

# 문제1
library(dplyr)
weather_new <- weather_new %>% rename(최소습도 = 최소상대습도, 일조시간 = 합계일조시간, 일사량 = 합계일사량)

# 문제2
weather_new_2 <- weather_new %>% filter(요일구분 == "평일")
round(mean(weather_new_2$최대풍속), digits = 2)
# A : 4.82

# 문제3
weather_new_3 <- weather_new %>% 
  filter(평균습도 >= quantile(weather_new$평균습도, probs = c(0.95)))
mean(weather_new_3$평균습도)
# A :93.532

# 문제4
weather_new_4 <- weather_new %>% 
  select(contains("기온") | contains("요일"))

# 문제5
weather_new_5 <- weather_new_4 %>% filter(요일구분 == "휴일")
table(weather_new_5$요일)
# A : 화요일
weather_new_4 %>% 
  group_by(요일, 요일구분) %>% 
  summarise(n()) # 다른 방법

# 문제6
weather_new <- weather_new %>% 
  mutate(불쾌지수 = 1.8*평균기온-0.55*(1-평균습도/100)*(1.8*평균기온-26)+32)

# 문제7
weather_new <- weather_new %>% 
  mutate(grade = case_when(불쾌지수 <= 67~"very good", 불쾌지수 <= 73~"good", 불쾌지수 <= 77~"not bad", 불쾌지수 > 77~"bad"))

# 문제8
weather_new$grade <- factor(weather_new$grade, levels = c("very good","good", "not bad","bad"))
table(weather_new$grade) # 출력순서 확인

# 문제9
weather_new %>% 
  group_by(grade) %>% 
  summarise(n(), mean(일사량))
# A : very good(=253,13.4) / good(=49,16.7) / not bad(=43,15.1) /bad(=21,12.5)

# 참고
n_distinct(weather_new$요일)
