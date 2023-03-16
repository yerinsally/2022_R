# 9. 데이터 시각화

# preprocess
library(readr)
mpg <- read_csv("mpg.csv")
mpg$sum <- mpg$city + mpg$highway

# 산점도
library(ggplot2)
ggplot(mpg, aes(displ, highway)) + geom_point()
ggplot(mpg, aes(displ, highway)) + geom_point() + xlim(3,6) + ylim(20,30)
ggplot(mpg, aes(displ, highway, color = drv)) + geom_point(aes(shape = drv, size = fuel))

# mpg 실습문제
ggplot(mpg, aes(city, highway)) + geom_point()
ggplot(mpg, aes(city, highway)) + geom_point() + xlim(0,30) + ylim(0,40)
ggplot(mpg, aes(city, highway, color = cyl)) + geom_point() + xlim(0,30) + ylim(0,40)
ggplot(mpg, aes(city, highway, color = cyl)) + geom_point(aes(shape = drv)) + xlim(0,30) + ylim(0,40)
ggplot(mpg, aes(city, highway, color = cyl)) + geom_point(aes(shape = drv)) + xlim(0,30) + ylim(0,40) + geom_smooth()

# midwest 실습문제
midwest <- read_csv("midwest.csv")
ggplot(midwest, aes(poptotal, popasian)) + geom_point()
ggplot(midwest, aes(poptotal, popasian)) + geom_point() + xlim(0,350000) + ylim(0,5000)
ggplot(midwest, aes(poptotal, popasian, color = state)) + geom_point(aes(shape = state)) + xlim(0,350000) + ylim(0,5000)
ggplot(midwest, aes(poptotal, popasian, color = state)) + geom_point(aes(shape = state)) + xlim(0,350000) + ylim(0,5000) + geom_smooth()
