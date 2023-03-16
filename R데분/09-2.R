# 두 변수 막대그래프
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_sum = mean(sum))
ggplot(df_mpg, aes(drv, mean_sum)) + geom_bar(stat = "identity")
ggplot(df_mpg, aes(reorder(drv, -mean_sum), mean_sum)) + geom_bar(stat = "identity")
ggplot(df_mpg, aes(reorder(drv, -mean_sum), mean_sum, fill = drv)) + geom_bar(stat = "identity")

# 빈도 막대그래프
ggplot(mpg, aes(class)) + geom_bar()
ggplot(mpg, aes(class, fill = class)) + geom_bar()
ggplot(mpg, aes(class, fill = class)) + geom_bar() + xlim(c("compact", "midsize", "suv")) # 세가지 등급 제약
ggplot(mpg, aes(class, fill = class)) + geom_bar() + coord_flip() # 가로
ggplot(mpg, aes(class, fill = class)) + geom_bar() + coord_polar() # 거미줄

# 차량 등급에 따른 빈도 막대그래프
ggplot(mpg, aes(class, fill = fuel)) + geom_bar()
ggplot(mpg, aes(class, fill = fuel)) + geom_bar(position = "dodge") # 옆으로 쌓기
ggplot(mpg, aes(class, fill = fuel)) + geom_bar(position = "fill") # 크기 동일

# 실습문제
mpg_suv <- mpg %>% 
  group_by(manufacturer) %>%
  filter(class == "suv") %>%
  summarise(mean_city = mean(city)) %>% 
  arrange(-mean_city) %>% 
  head(5)
ggplot(mpg_suv, aes(reorder(manufacturer, mean_city), mean_city, fill = manufacturer)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "회사별 suv 도심연비 평균 비교", x = "제조사", y = "도심연비 평균")

# 히스토그램
ggplot(mpg, aes(highway)) + geom_histogram(binwidth = 1, fill = "yellow", colour = "red") + labs(title = "고속도로 연비 히스토그램", x = "고속도로 연비", y = "빈도")

