# 14-1. 다항모집단 적합성 검정

telecom <- read_csv("telecom.csv")
table(telecom$past) # 과거 다항모집단
table(telecom$current) # 현재 다항모집단

# 첫번째 가설 검정
# 과거 시점에 3사의 시장점유율이 동일한지 여부
chisq.test(c(120, 100, 80)) # 대립가설 채택 (X-squared = 카이제곱 값)

# 두번째 가설 검정
# 현재 시점에 3사의 시장점유율이 동일한지 여부
chisq.test(c(149, 85, 66)) # 대립가설 채택

# 세번째 가설 검정
# 과거 시장점유율과 현재 시장점유율이 모두 동일한지 여부
chisq.test(c(149, 85, 66), p = c(0.4, 1/3, 4/15)) # p = 과거 비율, 대립가설 채택(변화O)

# 세번째 가설검정에 대한 사후분석
# (1) SKT
prop_SKT <- matrix(c(120, 180, 149, 151), nrow = 2, byrow = T)
rownames(prop_SKT) <- c("past", "current")
colnames(prop_SKT) <- c("SKT", "not SKT")
prop_SKT
prop.test(prop_SKT, alternative = "two.sided", correct = T) # 양측검정, 정규성
# p-value 유의 -> 대립가설 채택
# SKT의 시장점유율은 과거와 현재 비교시 변화 있음(높아짐)

# (2) KT
prop_KT <- matrix(c(100, 200, 85, 215), nrow = 2, byrow = T)
rownames(prop_KT) <- c("past", "current")
colnames(prop_KT) <- c("KT", "not KT")
prop_KT
prop.test(prop_KT, alternative = "two.sided", correct = T) # 양측검정, 정규성
# p-value 유의X -> 귀무가설 채택
# KT의 시장점유율은 과거와 현재 비교시 변화 없음

# (3) LG U+
prop_LGU <- matrix(c(80, 220, 66, 234), nrow = 2, byrow = T)
rownames(prop_LGU) <- c("past", "current")
colnames(prop_LGU) <- c("LGU", "not LGU")
prop_LGU
prop.test(prop_LGU, alternative = "two.sided", correct = T) # 양측검정, 정규성
# p-value 유의X -> 귀무가설 채택
# LGU+의 시장점유율은 과거와 현재 비교시 변화 없음


# 14-2. 독립성 검정

tennis <- read_csv("tennis.csv")
str(tennis)

# 데이터 전처리
names(tennis) <- tolower(names(tennis))
tennis$surface <- tolower(tennis$surface)
tennis$result <- tolower(tennis$result)
tennis$surface <- as.factor(tennis$surface)
# 범주 단위 압축
table(tennis$surface)
library(forcats)
tennis$surface <- fct_collapse(tennis$surface, "clay" = c("clay", "clay (i)"))
tennis$surface <- fct_collapse(tennis$surface, "hard" = c("hard", "hard (i)"))
# 결측치 제거
table(is.na(tennis$surface)) # 1개
tennis_new <- tennis %>% filter(!is.na(surface))
# Nadal 데이터만 추출
table(tennis_new$player)
tennis_nadal <- tennis_new %>% filter(player == "Rafael Nadal")
# 범주 단위 압축
table(tennis_nadal$result)
tennis_nadal$result <- fct_collapse(tennis_nadal$result, "l" = c("l", "lr", "lw"))
tennis_nadal$result <- fct_collapse(tennis_nadal$result, "w" = c("w", "wr", "ww"))

# 독립성 검정
# H0 : 코트 유형은 경기결과에 영향을 미치지 않는다.
# Ha : 코트 유형은 경기결과에 영향을 미친다.
xtabs(~surface + result, tennis_nadal) # cross table
prop.table(xtabs(~surface + result, tennis_nadal), margin = 1)
chisq.test(xtabs(~surface + result, tennis_nadal)) # 유의 -> 대립가설 채택
# surface와 result는 독립적이지 않음.
# surface가 result에 영향을 미침 : clay court에서 승률이 더 높음.

