# 11-1. USArrests 지도 시각화

# preprocess
crime <- USArrests
library(tibble)
crime <- rownames_to_column(crime, var = "state")
crime$state <- tolower(crime$state)

# state 데이터 활용하기
install.packages("maps")
library(maps)
library(ggplot2)
states_map <- map_data("state")

# 강력범죄 단계구분도 실습
install.packages("mapproj") # map_id 명령어
install.packages("ggiraphExtra") # ggChoropleth 함수
library(mapproj)
library(ggiraphExtra)
ggChoropleth(data = crime, aes(fill = Murder, map_id = state), map = states_map, interactive = T)


# 11-2. interactive 그래프

# interactive 산점도와 막대그래프 그리기
library(readr)
mpg <- read_csv("mpg.csv")
install.packages("plotly")
library(plotly)
library(ggplot2)
p1 <- ggplot(data = mpg, aes(displ, highway, col = drv)) + geom_point()
ggplotly(p1) # interactive
p2 <- ggplot(mpg, aes(class, fill = class)) + geom_bar() + coord_flip()
ggplotly(p2)
p3 <- ggplot(mpg, aes(class, fill = fuel)) + geom_bar(position = "dodge")
ggplotly(p3)

# diamoonds 데이터 이용
head(diamonds)
str(diamonds)
p4 <- ggplot(diamonds, aes(cut, fill = clarity)) + geom_bar(position = "dodge")
ggplotly(p4)
p5 <- ggplot(diamonds, aes(cut, fill = color)) + geom_bar(position = "dodge")
ggplotly(p5)

# interactive 시계열 그래프 그리기
# economics 데이터
install.packages("dygraphs")
library(dygraphs)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
dygraph(eco) %>% dyRangeSelector()
# corona19 데이터
corona19 <- read_csv("corona19.csv")
eco_a <- xts(corona19$total_cases, order.by = corona19$date)
eco_b <- xts(corona19$total_vaccinations/100, order.by = corona19$date) # 100으로 나눠서 스케일 동일하게
eco_c <- cbind(eco_a, eco_b)
colnames(eco_c) <- c("total_cases", "total_vaccinations") # 변수명 바꾸기
dygraph(eco_c) %>% dyRangeSelector()
