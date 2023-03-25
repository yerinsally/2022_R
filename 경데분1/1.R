# 다중회귀분석

# 상관분석 : dbl 변수들만
library(psych)
corr.test(bicycle[ ,c(7:14)], method = "pearson", 
          alpha = 0.05, use = "pairwise.complete.obs")
# 상관계수 / p-value 도출됨
# temp & atemp : 가장 강한 양의 상관관계(0.99)
# 모든 변수들 간의 상관계수 추정치가 유의함 : 2p-value < 0.05

# 가설 수립
# IV에서 casual, registered 제외 (total에 포함되니까)
# IV : temp, atemp, humidity, windspeed, difference, DV : total
# H1 : temp -> total (+)
# H2 : atemp -> total (+)
# H3 : humidity -> total (-)
# H4 : windspeed -> total (+)
# H5 : difference -> total (+)

# 다중회귀식 수립
# Yi(hat) = a + b1x1i + b2x2i + b3x3i + b4x4i + b5x5i
lm1 <- lm(total~temp+atemp+humidity+windspeed+difference, data = bicycle)

# 전제조건 확인
plot(lm1)
# Residuals vs Fitted : 빨간 선이 하얀 점선을 따라 일직선이면 선형성 조건 만족
# Normal Q-Q : 점들이 대각선 위에 존재하면 정규성 조건 만족
# Scale-Location : 빨간 선이 일직선이면 등분산성 & 선형성 조건 만족
# Residuals vs Leverage : 빨간선 수평이 일직선이면 등분산성 조건 만족, leverage는 사례가 다른 사례로부터 떨어진 정도로 0에 가까울수록 바람직함, Cook's distance는 0.5 or 1을 넘으면 해당 사례가 회귀계수 추정치에 지나치게 많은 영향을 미침
# 6720, 6721, 6722, 8915, 8917, 8918 행 제거하면 선형성,정규성,등분산성 개선
library(dplyr) # case 직접 찾아서 제거!
bicycle <- bicycle %>% filter(case != 6728, case != 6729, case != 6730, case != 9004, case != 9006, case != 9007) # 행 번호 찾아서 case 삭제
lm1 <- lm(total~temp+atemp+humidity+windspeed+difference, data = bicycle)
plot(lm1) # lm1 업데이트

# 정규성 조건 확인
ks.test(bicycle$total, pnorm, 
        mean = mean(bicycle$total), sd = sd(bicycle$total))
# Kolmogorov-Smirnov test : n이 클때 정규성 검토
# 2p-value = 0 < 0.05(alpha) = 유의하므로 정규성 조건 만족 X
shapiro.test(bicycle$total)
# Shapiro test : n이 작을때 정규성 검토
hist(bicycle$total, breaks = seq(0, 1000, 10)) # 정규분포 모양 X
# hist(log(bicycle$total), breaks = seq(0, 10, 0.1))
# -> log : 밑이 e(무리수)인 자연로그

# 독립성 조건 확인 : 오차의 자기상관
library(car)
durbinWatsonTest(lm1)
# Positive Autocorrelation (0.915 < 2) & rho != 0 (p-value 유의)
# 오차는 서로 양의 자기상관 존재(p-value = 0 or rho != 0)

# 따라서, 해당 데이터는 회귀분석을 실행하기에는 한계점 존재

