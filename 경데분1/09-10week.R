# 9. t-검정

library(dplyr)
ttest <- read.csv("ttest.csv")

# 준비단계
# 척도변경
glimpse(ttest) 
ttest$priority <- as.factor(ttest$priority)
ttest$shipping <- as.factor(ttest$shipping)
ttest$customer <- as.factor(ttest$customer)
ttest$category <- as.factor(ttest$category)
ttest$container <- as.factor(ttest$container)
# 빈도수 확인
library(descr)
freq(ttest$priority)
freq(ttest$shipping)
freq(ttest$customer)
freq(ttest$category)
freq(ttest$container)
# 변수 위치 조정
ttest <- ttest %>% relocate(where(is.factor)) 
ttest <- ttest %>% relocate(name, .after = margin)
# 이상치 검토 및 빈도수 확인
library(psych)
descr <- describe(ttest[c(6:10)])
descr <- descr %>% mutate(UL = mean+2*sd) # 상한
descr <- descr %>% mutate(LL = mean-2*sd) # 상한
table(ttest$quantity > 54.53)
table(ttest$sales > 8945.98) # 320개
table(ttest$price > 670.1) # 107개
table(ttest$cost > 47.37) # 611개
table(ttest$margin > 0.78) # 365개
# 새 데이터프레임 생성
ttest_new <- ttest %>% filter(sales <= 8945.98, price <= 670.1, 
                              cost <= 47.37, margin <= 0.78)

# STEP1 : 가설수립
# H0 : Home Office 판매금액 평균과 Consumer 판매금액 평균은 동일하다.
# Ha : Home Office 판매금액 평균과 Consumer 판매금액 평균은 동일하지 않다.

# STEP2 : 집단별 데이터프레임
ttest_HO <- ttest_new %>% filter(customer == "Home Office")
ttest_CS <- ttest_new %>% filter(customer == "Consumer")

# STEP3 : 정규성 조건 검토
summary(ttest_HO$sales)
summary(ttest_CS$sales)
hist(ttest_HO$sales, breaks = seq(0,9000,50))
hist(ttest_CS$sales, breaks = seq(0,9000,50))
shapiro.test(ttest_HO$sales) # 유의 -> 만족X
shapiro.test(ttest_CS$sales) # 유의 -> 만족X
# p-value가 유의하지 않아야 정규성 조건 만족
ttest_HO <- ttest_HO %>% mutate(lnsales = log(sales))
ttest_CS <- ttest_CS %>% mutate(lnsales = log(sales))
summary(ttest_HO$lnsales)
hist(ttest_HO$lnsales, breaks = seq(0,10,0.1))
hist(ttest_CS$lnsales, breaks = seq(0,10,0.1))

# STEP4 : 등분산성 조건 검토
var.test(ttest_HO$lnsales, ttest_CS$lnsales) # 유의X -> 만족
# p-value가 유의하지 않아야 등분산성 조건 만족

# STEP5 : 독립표본 t-검정/가설검정
t.test(ttest_HO$lnsales, ttest_CS$lnsales,
       alternative = "two.sided", var.equal = T)
# p-value = 0.2601 : 유의X -> 귀무가설 채택
# 두 모평균은 동일하다.


# 추가 실습
# STEP2 : 집단별 데이터프레임
ttest_TN <- ttest_new %>% filter(category == "Technology")
ttest_FN <- ttest_new %>% filter(category == "Furniture")

# STEP3 : 정규성 조건 검토
hist(ttest_TN$sales, breaks = seq(0,9000,50))
hist(ttest_FN$sales, breaks = seq(0,9000,50))
shapiro.test(ttest_TN$sales) # 유의 -> 만족X
shapiro.test(ttest_FN$sales) # 유의 -> 만족X
ttest_TN <- ttest_TN %>% mutate(lnsales = log(sales))
ttest_FN <- ttest_FN %>% mutate(lnsales = log(sales))
hist(ttest_TN$lnsales, breaks = seq(0,10,0.1))
hist(ttest_FN$lnsales, breaks = seq(0,10,0.1))

# STEP4 : 등분산성 조건 검토
var.test(ttest_TN$lnsales, ttest_FN$lnsales) # 유의 -> 만족X

# STEP5 : 이분산 독립표본 t-검정/가설검정
t.test(ttest_TN$lnsales, ttest_FN$lnsales,
       alternative = "two.sided", var.equal = F)
# p-value : 유의 -> 대립가설 채택
# 두 모평균은 동일하지 않다.



# 10. 대응표본 t-검정

pttest <- read_csv("pttest.csv", locale = locale("ko", encoding = "euc-kr"))

# STEP1 : 가설수립
# H0 : 주말배송 주문평균과 새벽배송 주문평균은 차이가 없다.
# Ha : 주말배송 주문평균이 새벽배송 주문평균보다 크다.

# STEP2 : 차이변수 만들기
pttest <- pttest %>% mutate(d = morning - weekend)

# STEP3 : 정규성 조건 검토
summary(pttest$d)
hist(pttest$d, breaks = seq(-15,8,1))
shapiro.test(pttest$d) # 유의 -> 만족X

# STEP4 : 대응표본 t-검정
t.test(pttest$morning, pttest$weekend, 
       alternative = "two.sided", paired = T)
# p-value 유의 -> 대립가설 채택
# 주말배송 주문평균이 새벽배송 주문평균보다 크다.

