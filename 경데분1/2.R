# 다중회귀식 추정결과를 통한 가설검정
summary(lm1)
# 회귀계수 추정치(estimate = bj)의 부호 확인 : b3 < 0로 도출
# 현재 도출된 p-value = 2p-value, 단측검정이므로 2p-value와 0.1(=2*alpha) 비교
# R-squared = 0.7612, Adjusted R-squared = 0.7611 : 설명력, 정확도 높음
# 전체 가설에 대한 대립가설(인과관계 X) : F-통계량, p-value로 설명

# H1 채택 : beta1 > 0 , 0.079 < 0.1 (유의)
# H2 채택 : beta2 > 0 , 2.53e-12 < 0.1 (유의)
# H3 채택 : beta3 < 0 , 2e-16 < 0.1 (유의)
# H4 채택 : beta4 > 0 , 0.120 > 0.1 (유의 X)
# H5 채택 : beta5 > 0 , 2e-16 < 0.1 (유의)
# 추정된 다중회귀식 (예측 시 활용)
# Yi(hat) = 44.630 + 1.125x1i + 4.130x2i - 1.608x3i + 1.030x5i

# 모형적합도(AIC) 제고를 위한 다중회귀식 추정 방법
lm_f <- step(lm1, direction = "forward") # IV 하나씩 추가
lm_b <- step(lm1, direction = "backward") # 다 추가하고 IV 하나씩 제거
lm_s <- step(lm1, direction = "both") # 혼합방식
summary(lm_f)
summary(lm_b)
summary(lm_s)
# AIC = 94349.55로 f/b/s 세 방식 다 동일
# 이때 모든 IV가 포함된 다중회귀식이 모형적합도가 가장 높음. --> 어케 알아
# 인과관계 목적 X


# 다중공산성 확인
library(car)
vif(lm1)
# VIF > 5.3 : temp & atemp IVs 다중공산성 일으킴

# atemp 제거
# VIF가 가장 크고, temp로 일정부분 대체 가능
lm2 <- lm(total~temp+humidity+windspeed+difference, data = bicycle)
vif(lm2)
summary(lm2) # 여전히 H4 기각(0.96 > 0.1)

# temp & atemp 모두 제거
lm3 <- lm(total~humidity+windspeed+difference, data = bicycle)
vif(lm3)
summary(lm3)
# H4의 2p-value = 0.041 < 0.1 (유의)
# H4의 회귀계수 추정치(b4) 부호 반대 -> 유의하더라도 채택X

# IV의 상대적 중요도
# 표준화 회귀계수 추정치의 절대값 크기 비교
library(lm.beta)
lm.beta(lm1)
# 유의하지 않은 b4에 대해서는 표준화 회귀계수 추정치 무시
# 절대값 : difference > atemp > humidity > temp
lm.beta(lm3)
# H4는 기각하지만, b4는 유의(0.041<0.1)하므로 중요도 파악에 포함
# 절대값 : difference > humidity > windspeed

