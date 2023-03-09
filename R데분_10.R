# 선 그래프
library(dplyr)
library(ggplot2)
economics <- ggplot2::economics
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics, aes(date, unemploy)) + geom_line() + geom_point()
ggplot(economics, aes(date, unemploy)) + geom_line(color = "red") + geom_point(color = "darkred")

# 상자 그래프
library(readr)
mpg <- read_csv("mpg.csv")
mpg$sum <- mpg$city + mpg$highway
ggplot(mpg, aes(drv, highway, fill = drv)) + geom_boxplot()
ggplot(mpg, aes(drv, highway, fill = drv)) + geom_boxplot(outlier.colour = "red")
ggplot(mpg, aes(drv, highway, fill = drv)) + geom_boxplot(outlier.colour = "red") + stat_summary(fun = "mean", geom = "point")

# 실습
corona19 <- read.csv("corona19.csv", stringsAsFactors = F)
glimpse(corona19)
corona19$date <- as.Date(corona19$date)
# 산점도
ggplot(corona19, aes(new_tests, new_cases)) + geom_point() 
ggplot(corona19, aes(new_tests, new_cases)) + geom_point() + xlim(10000, 60000) + ylim(0, 3000) + geom_smooth()
# 막대 그래프
ggplot(corona19, aes(date, new_cases, fill = new_cases)) + geom_bar(stat = "identity")
# 선 그래프
ggplot(corona19, aes(date, new_cases)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, new_deaths)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, total_deaths)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, positive.rate)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, reproduction.rate)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, total_vaccinations)) + geom_line(color = "red") + geom_point(color = "blue")
ggplot(corona19, aes(date, people_fully_vaccinated)) + geom_line(color = "red") + geom_point(color = "blue")

# corona19_new : 그래프 같이 나타내기
corona19_new <- read.csv("corona19_new.csv", stringsAsFactors = F)
corona19_new$date <- as.Date(corona19_new$date)
ggplot(corona19_new, aes(date, number, color = type)) + geom_line() + geom_point() 
