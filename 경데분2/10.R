# 10. 로짓회귀분석

library(readr)
library(dplyr)
library(psych)

# 데이터 검토 및 전처리
employ <- read_csv("employment.csv")
summary(employ)
table(is.na(employ))
table(employ$result)
table(employ$ranking)
table(employ$self)
table(employ$intern)
descr <- describe(employ[3:6])
descr <- descr %>% mutate(UL = mean+3*sd, LL = mean-3*sd) # 이상치X

# 문제1 : DV, IV간 상관관계 파악 & 가설수립
corr.test(employ[2:6], method = "pearson", 
          alpha = 0.05, use = "pairwise.complete.obs")
# 피어슨 상관계수 모두 양수 & 2p-value 모두 0(유의)
# H1 : GPA가 높을수록 채용될 가능성(확률)이 높다.
# H2 : test가 높을수록 채용될 가능성(확률)이 높다.
# H3 : ranking이 높을수록 채용될 가능성(확률)이 높다.
# H4 : self가 높을수록 채용될 가능성(확률)이 높다.

# 문제2 : 로짓회귀식 수립 & 이상치 확인
logit1 <- glm(result~GPA+test+ranking+self, data = employ, 
              family = binomial())
library(car)
outlierTest(logit1)
# 해석) 66행의 2p-value = 0.012245 < 0.05(alpha) 유의, 66행을 이상치로 판정

# 문제3 : 이상치 제거 후 로짓회귀식 재수립 & 가설검정
employ <- employ %>% filter(id != 66)
logit1 <- glm(result~GPA+test+ranking+self, data = employ, 
              family = binomial())
summary(logit1)
# b1~b4 모두 유의(p-value)한 양수(estimate)로 추정, H1~H4 채택
# Null deviance = logit1의 -2LL

# 문제4 : 모형적합도 확인
library(ResourceSelection)
hoslem.test(logit1$y, logit1$fitted.values)
# y(실제값), fitted.values(Yihat을 통해 도출된 확률값)
# 해석) p-value(0.4808 > 0.05) 유의하지 않아 모형 적합함
library(DescTools)
PseudoR2(logit1, which = c("CoxSnell", "Nagelkerke"))
# 해석) 0~1 사이의 값, 적절함

# 문제5 : intern 추가에 따른 가설수립 & 로짓회귀식 수립
corr.test(employ[2:7], method = "pearson", 
          alpha = 0.05, use = "pairwise.complete.obs")
# 피어슨 상관계수 모두 양수 & 2p-value 모두 0(유의)
# H5 : intern 경험 있을수록 채용될 가능성(확률)이 높다.
logit2 <- glm(result~GPA+test+ranking+self+intern, data = employ, 
              family = binomial()) # intern 추가

# 문제6 : 이상치 제거 후 로짓회귀식 재수립
library(car)
outlierTest(logit2)
# 해석) 242행의 2p-value = 0.011967 < 0.05(alpha) 유의, id = 243을 이상치로 판정
employ <- employ %>% filter(id != 243)
logit1 <- glm(result~GPA+test+ranking+self, data = employ, 
              family = binomial())
logit2 <- glm(result~GPA+test+ranking+self+intern, data = employ, 
              family = binomial())

# 문제 7 : intern 추가 타당성 검토 & 가설검정
summary(logit1) # Residual deviance(-2LL) = 663.97
summary(logit2) # Residual deviance(-2LL) = 651.22(감소함)
# -2LL 작을수록 설명력 좋음, 변화량 유의미한지 검토하기
difference <- logit1$deviance - logit2$deviance # 카이제곱값
dof <- logit1$df.residual - logit2$df.residual # 자유도
1 - pchisq(difference, dof) 
# 카이제곱값=difference, 자유도=dof : pchisq = 0~카이제곱값까지 누적된 넓이
# 전체 넓이(=1)에서 빼야 바깥쪽 넓이 도출됨
# p-value(0.0003557586) < 0.05 유의 -> logit2의 설명력/모형적합도가 더 좋음(=intern을 추가하는 것이 의미있음)
summary(logit2)
# b1~b5 모두 유의한 양수로 추정, H1~H5 모두 채택

# 문제8 : 모형적합도 확인
library(ResourceSelection)
hoslem.test(logit2$y, logit2$fitted.values)
# y(실제값), fitted.values(Yihat을 통해 도출된 확률값)
# 해석) p-value(0.001052 < 0.05) 유의하므로 모형 적합X
library(DescTools)
PseudoR2(logit1, which = c("CoxSnell", "Nagelkerke"))
# 해석) 0~1 사이의 값, 적절함

# 문제9 : hit ratio 구하기
prediction <- predict(logit2, newdata = employ)
# logit value(Yihat) 도출하여 prediction이라는 열벡터 생성
prediction <- ifelse(prediction < 0, 0, 1)
# logit value를 기준값 0을 기준으로 0과 1로 예측치로 전환
library(caret)
employ$result <- as.factor(employ$result)
prediction <- as.factor(prediction)
confusionMatrix(employ$result, prediction)
# data(employ$result) & reference(prediction) 모두 level이 동일한 범주형 척도여야함
# hit ratio = Accuracy = 0.8407

# 문제10 : 예측하기
C1001 <- data.frame(id=1001, GPA=3.58, test=110, ranking=3, self=9, intern=1)
str(C1001)
str(employ)
# 주의 : 변수명과 척도가 employ와 일치해야함 -> 일치
predict(logit2, C1001)
# logit value = -0.2503677(음수) -> 0(채용 안됨)으로 예측

