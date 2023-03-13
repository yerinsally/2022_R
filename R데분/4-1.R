# 비교 연산자
library(descr)
freq(exam$address == "원효로")
table(exam$address == "원효로")
freq(exam$gender != "Female")
table(exam$gender != "Female")
freq(exam$math == 50)
freq(exam$math != 50)
freq(exam$math >= 50)
freq(exam$math < 50)

# 논리 연산자
freq(exam$english <= 50 & exam$history >= 80)
freq(exam$math >= 90 | exam$history >= 90)
freq(exam$address == "효창동" | exam$address == "청파동" |exam$address == "서계동" )
freq(exam$address %in% c("효창동", "청파동", "서계동"))
