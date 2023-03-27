# 조절효과 확인하기

# Model1 : lm3 = IVs와 DV로만 구성
# Model2 : lm4 = Model1에 MV(working)를 추가해서 구성
# Model3 : lm7 = Model2에 상호작용변수(humidity*working)를 추가해서 구성
# lm4와 lm7 비교해서 lm7의 설명력이 더 좋은지 확인해야함
# working 척도 변경
str(bicycle$working) # 범주형
bicycle$working <- as.numeric(bicycle$working)

# Model3 = lm7
library(dplyr) # inter : 상호작용변수 만들기
bicycle <- bicycle %>% mutate(inter = humidity*working)
lm7 <- lm(total~humidity+windspeed+difference+working+inter, data = bicycle)

# lm4 / lm7 비교
summary(lm4) # R-squared: 0.7432, Adjusted R-squared: 0.7431
summary(lm7) # R-squared: 0.7554, Adjusted R-squared: 0.7553
anova(lm4, lm7) # F-통계량 = 532.49, p-value = 2.2e-16 < 0
# 수정 R2 증가, 변화량 통계적으로 유의
# 따라서, lm4보다 lm7의 설명력이 더 좋음 -> lm7 채택

# 1. Model1 = lm3 : H3에서 humidity는 total에 (-) 인과관계 설정, b3 = -5.061e로 예상대로 음수로 유의함
# 기존 인과관계(-)가 강화 = b7 < 0 / 기존 인과관계(-)가 약화 = b7 > 0
# 2. 상호작용변수의 조절효과 가설 수립 : H6 = humidity와 total에 대한 음의 인과관계에 working의 조절효과가 존재한다. (양측검정)
# 3. 상호작용변수 부호 확인 : b7 = 2.044e > 0로 기존 인과관계 약화
# 따라서, 습도가 높아지면 대여횟수가 감소하지만, 일하는 시기(working = Yes)에는 감소하는 정도가 완화된다.
# ex. 습도가 20%에서 일을 안하면(상호작용변수값 = 20) 대여횟수가 100회인데, 일을 하면(상호작용변수값 = 40) 대여횟수가 110회(감소하는 정도 완화)

