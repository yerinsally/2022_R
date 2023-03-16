# 변수명 바꾸기
library(dplyr)
movie <- movie %>% rename(Title = Series_Title,
                          Year = Released_Year)
movie$var1 <- movie$Year
movie$var1 <- NULL

# 변수의 측정값 바꾸기
movie$Running <- movie$Runtime
movie$Running <- ifelse(movie$Running > 200, "Long", "Not Long")
table(movie$Running == "Long")

table(is.na(movie$Gross))
pairs(movie[15:16]) # 우상향
mean(movie$Gross / movie$No_of_Votes, na.rm = T)
movie$Gross <- ifelse(is.na(movie$Gross), 217.1, movie$Gross)
table(movie$Gross == 217.1) # 확인작업
