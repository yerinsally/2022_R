# 11. One-way ANOVA

# 실습 1

# STEP1 : 전처리 및 가설수립
anova1 <- ttest
anova1 %>% group_by(priority) %>% summarise(n(), mean(price))
# 이상치 검토 및 제거
descr2 <- describe(anova1$price) 
descr2 <- descr2 %>% mutate(UL = mean+2*sd)
descr2 <- descr2 %>% mutate(LL = mean-2*sd)
table(anova1$price > descr2$UL) # 107개
anova1_new <- anova1 %>% filter(price <= descr2$UL)
# 새로운 변수 만들기
library(forcats)
anova1_new <- anova1_new %>% 
  mutate(prior = fct_collapse(priority, "High" = c("Critical", "High")))
anova1_new %>% group_by(prior) %>% summarise(mean(price), na.rm = T)
# H0 : 네 집단의 price 모평균은 같다.
# Ha : 적어도 한 집단의 price 모평균은 다른 집단과 다르다.

# STEP2 : 서브 데이터프레임
anova1_H <- anova1_new %>% filter(prior == "High")
anova1_L <- anova1_new %>% filter(prior == "Low")
anova1_M <- anova1_new %>% filter(prior == "Medium")
anova1_N <- anova1_new %>% filter(prior == "Not Specified")

# STEP3 : 정규성 검토
summary(anova1_H$price)
hist(anova1_H$price, breaks = seq(0,600,10))
hist(anova1_L$price, breaks = seq(0,600,10))
hist(anova1_M$price, breaks = seq(0,600,10))
hist(anova1_N$price, breaks = seq(0,600,10))
shapiro.test(anova1_H$price)
shapiro.test(anova1_L$price)
shapiro.test(anova1_M$price)
shapiro.test(anova1_N$price)
# 모두 유의 -> 만족X, but 정규성 가정

# STEP4 : 등분산성 검토
library(car)
leveneTest(price~prior, data = anova1_new)
# p-value = 0.1307 : 유의X -> 만족

# STEP5 : ANOVA 검정
anova1_result <- aov(price~prior, data = anova1_new)
summary(anova1_result)
# p-value = 0.133 : 유의X -> 귀무가설 채택
# 네 집단의 price 모평균은 같다.


# 실습2
# 사후분석도 진행

# STEP1 : 전처리 및 가설수립
anova2 <- pttest
anova2 %>% group_by(payment) %>% summarise(mean(expense, na.rm = T))
# 이상치 검토 및 제거
descr3 <- describe(anova2$expense)
descr3 <- descr3 %>% mutate(UL = mean+2*sd)
descr3 <- descr3 %>% mutate(LL = mean-2*sd)
table(anova2$expense > descr3$UL) # 5개
anova2_new <- anova2 %>% filter(expense <= descr3$UL)
table(is.na(anova2_new$expense)) # NA도 제거됨
anova2_new %>% group_by(payment) %>% summarise(mean(expense))
# H0 : 세 집단 간에 expense 모평균은 모두 동일하다.
# Ha : 적어도 한 집단의 expense 모평균은 다른 집단과 다르다.

# STEP2 : 서브 데이터프레임
anova2_S <- anova2_new %>% filter(payment == "간편결제")
anova2_A <- anova2_new %>% filter(payment == "계좌이체")
anova2_C <- anova2_new %>% filter(payment == "신용카드")

# STEP3 : 정규성 검토
summary(anova2_S$expense)
hist(anova2_S$expense, breaks = seq(0,2000,40))
hist(anova2_A$expense, breaks = seq(0,2000,40))
hist(anova2_C$expense, breaks = seq(0,2000,40))
shapiro.test(anova2_S$expense)
shapiro.test(anova2_A$expense)
shapiro.test(anova2_C$expense)
# 모두 유의 -> 만족X, but 정규성 가정

# STEP4 : 등분산성 검토
leveneTest(expense~payment, data = anova2_new)
# p-value = 0.02866 > 0.01 : 유의X -> 만족

# STEP5 : ANOVA 검정
anova2_result <- aov(expense~payment, data = anova2_new)
summary(anova2_result)
# p-value : 유의 -> 대립가설 채택
# 등분산조건 만족하니까 duncan.test 사후분석

# STEP6 : 사후분석
library(agricolae)
duncan.test(anova2_result, "payment", console = T)
# 같은 집단(a,b)의 표본평균은 같다고 해석
# 표본평균 높은 순 : 신용카드 expense 모평균(a) > 계좌이체 expense 모평균(b) = 간편결제 expense 모평균(b)

# 추가작업 : alpha = 0.05
# STEP4에서 p-value < 0.05, 유의 -> 등분산 만족X
# STEP5 : 이분산 ANOVA 검정
oneway.test(expense~payment, data = anova2_new)
# p-value : 유의 -> 대립가설 채택
# 등분산조건 만족하지 않으니까 dunn.test 사후분석
# STEP6 : 사후분석
library(dunn.test)
dunn.test(anova2_new$expense, anova2_new$payment, method = "bonferroni")
# p-value = 1 > alpha(유의 X), 계좌이체 모평균 = 간편결제 모평균
# p-value = 0 < alpha(유의) : 신용카드 모평균 > 간편결제 모평균(-)
# p-value = 0 < alpha(유의) : 신용카드 모평균 > 계좌이체 모평균(-)
# 등분산 가정 사후분석 결과와 동일
