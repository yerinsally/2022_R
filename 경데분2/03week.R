# 새로운 IV 추가 및 타당성 검토 --> lm3에???

# 1. working 변수
# 형태상 범주형 but 내용상 더미변수(이대로 추가)
lm4 <- lm(total~humidity+windspeed+difference+working, data = bicycle)
# 수정 R2 증가?
summary(lm3) # R-squared: 0.6947, Adjusted R-squared: 0.6946
summary(lm4) # R-squared: 0.7432,	Adjusted R-squared: 0.7431
# R2 증가량 통계적으로 유의?
anova(lm3, lm4)
# F-통계량 = 2024.7, p-value = 2.2e-16 < alpha -> 통계적으로 유의
# 따라서, lm3보다 lm4의 설명력이 더 좋음. working 추가하는 것이 바람직함

# lm4에 대한 추정결과
summary(lm4)
# working 출력순서 : No(0), Yes(1) -> 앞에 출력되는 구분이 reference
# Yi(hat) = 209.867 - 1.625x3i - 0.212x4i + 1.166x5i - 80.953dv1i
# dv1i = 0 or 1 : 0(working = No)일때 Yi(hat)이 더 큼
# 따라서, 근무하지 않을때 대여횟수가 더 증가한다
# 참고 : R에서 자동적으로 숫자 1부터 부여
str(bicycle$working) # 1~2
str(bicycle$season) # 1~4

# 2. season 변수
lm5 <- lm(total~humidity+windspeed+difference+working+season, data = bicycle)
# season과 관련해서 3(=4-1)개의 더미변수 생성
# 수정 R2 증가?
summary(lm4) # R-squared: 0.7432, Adjusted R-squared: 0.7431
summary(lm5) # R-squared: 0.778, Adjusted R-squared: 0.7778
# R2 증가량 통계적으로 유의?
anova(lm4, lm5)
# F-통계량 = 559.18, p-value = 2.2e-16 < alpha -> 통계적으로 유의
# 따라서, lm4보다 lm5의 설명력이 더 좋음. season 추가하는 것이 바람직함

# lm5에 대한 추정결과
summary(lm5)
# season 출력순서 : No(0), Yes(1) -> 앞에 출력되는 구분이 reference
# 기존 독립변수 회귀계수 추정치 : 2p-value와 0.1 비교(단측검정)
# 네 개 더미변수 회귀계수 추정치 : 2p-value와 0.05 비교(양측검정, 가설수립X)
# b4(0.373) > 0.1이므로 유의하지 않음
# Yi(hat) = 172.680 - 1.809x3i + 1.136x5i - 78.929dv1i + 68.971dv2i + 81.502dv3i + 36.710dv4i
# spring : 0, 0, 0 / summer : 1, 0, 0 / fall : 0, 1, 0 / winter : 0, 0, 1
# spring : Yi(hat) = 172.680 - 1.809x3i + 1.136x5i - 78.929dv1i + 0
# summer : Yi(hat) = 172.680 - 1.809x3i + 1.136x5i - 78.929dv1i + 68.971 (reference에 비해 종속변수값 증가)

# 다중공산성 확인
library(car)
vif(lm5) #  GVIF^(1/(2*Df)) < 2이면 다중공산성은 문제없음


# 새로운 독립변수 atemp 추가
lm6 <- lm(total~humidity+windspeed+difference+working+season+atemp, data = bicycle)
summary(lm6) # R-squared: 0.81, Adjusted R-squared: 0.8098
anova(lm5, lm6) #  F-통계량 = 1804.2, p-value = 2.2e-16 < 0
# lm5에 비해 수정 R2는 증가, R2 변화량 통계적으로 유의
# 따라서 lm6의 설명력이 더 좋음

# BUT, dv3i(seasonfall) 회귀계수 추정치의 부호가 반대 = -18.250
vif(lm6) # 다중공산성은 X
library(lm.beta)
lm.beta(lm6) # 상대적 중요도 : atemp > difference > ...
# 새로 추가된 atemp의 중요도가 2위, dv2i~dv4i의 중요도는 매우 떨어짐
# 따라서, 중요도가 떨어지는 독립변수는 회귀계수 추정치가 다른 중요도가 높은 IVs에 영향을 받아서 왜곡될 수 있음

# lm5에 기반하여 종속변수(예측치)를 예측
lm5 <- lm(total~humidity+windspeed+difference+working+season, data = bicycle)
# working : Yes, season : summer, temp : 32.2, atemp : 35.4, humidity : 65.7, windspeed : 6.5, difference : 120
# Yi(hat) = 172.680 - 1.809x3i + 1.136x5i - 78.929dv1i + 68.971dv2i + 81.502dv3i + 36.710dv4i
172.680 - 1.809*65.7 + 1.136*120 - 78.929*1 + 68.971*1 + 81.502*0 + 36.710*0
# 종속변수 예측치는 180.191

