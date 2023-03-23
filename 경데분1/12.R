# 12. Two-way ANOVA

# STEP1 : 전처리 및 가설수립
two_anova <- pttest
# 이상치 검토 및 제거
descr4 <- describe(two_anova$expense)
descr4 <- descr4 %>% mutate(UL = mean+3*sd)
descr4 <- descr4 %>% mutate(LL = mean-3*sd)
table(two_anova$expense > descr4$UL) # 이상치 5개
two_anova_new <- two_anova %>% filter(expense <= descr4$UL)
# H0 : 두 독립변수 간에 상호작용효과가 없다.
# Ha : 두 독립변수 간에 상호작용효과가 있다.

# STEP2 : 서브 데이터프레임
two_anova_male <- two_anova_new %>% filter(gender == "Male")
two_anova_female <- two_anova_new %>% filter(gender == "Female")

# STEP3 : 정규성 검토
summary(two_anova_male$expense)
hist(two_anova_male$expense, breaks = seq(0,2000,40))
hist(two_anova_female$expense, breaks = seq(0,2000,40))
shapiro.test(two_anova_male$expense)
shapiro.test(two_anova_female$expense)
# 모두 유의 -> 만족X, but 정규성 가정

# STEP4 : 등분산성 검토
library(car)
leveneTest(expense~gender, data = two_anova_new)
# p-value = 0.001238 : 유의 -> 만족X

# STEP5 : 이분산 ANOVA 검정
oneway.test(expense~gender, data = two_anova_new)
# p-value 유의 -> 대립가설 채택
# gender에 따라 집단을 구분했을때, 종속변수 expense의 모평균은 차이가 있다.

# STEP6 : 사후분석(생략)

# STEP7 : Two-way ANOVA 및 그래프
# (1) Two-way ANOVA
two_anova_result <- aov(expense~gender*os, data = two_anova_new)
summary(two_anova_result) # p-value = 0.000372 : 유의
# 전제조건 만족(STEP5) & 상호작용변수(IV1*IV2 유의) -> 대립가설 채택
# (2) 그래프 그리기
two_anova_new$gender <- factor(two_anova_new$gender, 
                               levels = c("Male", "Female"))
# One-way ANOVA에서 표본평균이 작은 순서로 집단을 먼저 출력하도록 함.
library(HH)
interaction2wt(expense~gender*os, data = two_anova_new)
# 기존 관계 강화됨

# STEP8 : 추가 분석
# 집단을 네 개로 세분화 one-way ANOVA : MA, Mi, FA, Fi
two_anova_new <- two_anova_new %>% 
  mutate(genderos = ifelse(gender=="Male"&os=="Android","MA", 
                           ifelse(gender=="Male"&os=="iOS","Mi", 
                                  ifelse(gender=="Female"&os=="Android","FA","Fi"))))
table(two_anova_new$genderos)
# H0 : 네 개 집단 간의 expense 모평균은 동일하다.
# Ha : 적어도 한 집단의 expense 모평균은 다른 집단과 다르다.
# (1) 서브 데이터프레임 만들기
two_anova_MA <- two_anova_new %>% filter(genderos == "MA")
two_anova_Mi <- two_anova_new %>% filter(genderos == "Mi")
two_anova_FA <- two_anova_new %>% filter(genderos == "FA")
two_anova_Fi <- two_anova_new %>% filter(genderos == "Fi")
# (2) 정규성 검토
shapiro.test(two_anova_MA$expense)
shapiro.test(two_anova_Mi$expense)
shapiro.test(two_anova_FA$expense)
shapiro.test(two_anova_Fi$expense) # 모두 유의 -> 정규성 만족X
# (3) 등분산성 검토
leveneTest(expense~genderos, data = two_anova_new) # 유의 -> 등분산 만족X
# (4) 이분산 one-way ANOVA
oneway.test(expense~genderos, data = two_anova_new) # 유의 -> 대립가설 채택 
# (5) 사후 분석 : dunn.test
library(dunn.test)
dunn.test(two_anova_new$expense, two_anova_new$genderos, method = "bonferroni")
# 모평균 FA < Fi : p-value(0.0006) < 0.05(two-way), t-통계량 (-)
# 모평균 FA > MA, 모평균 FA > Mi, 모평균 Fi > MA, 모평균 Fi > Mi, 모평균 MA = Mi
# 모평균 : Fi > FA > MA = Mi

