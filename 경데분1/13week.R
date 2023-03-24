# 13. 비율비교와 독립성 검정

proptest <- read_csv("proptest.csv")

# H0 : 20대와 30대 장바구니 구매전환률은 모비율 차이가 없다.
# Ha : 20대와 30대 장바구니 구매전환률은 모비율 차이가 있다.

# 1. 빈도수 교차표
table(proptest$customer == 20 & proptest$trans == "Yes") # 48명
table(proptest$customer == 20 & proptest$trans == "No") # 49명
table(proptest$customer == 30 & proptest$trans == "Yes") # 30명
table(proptest$customer == 30 & proptest$trans == "No") # 73명
prop <- matrix(c(48,49,30,73), nrow = 2, ncol = 2, byrow = T)
rownames(prop) <- c(20,30)
colnames(prop) <- c("Yes","No")

# 2. 비율에 대한 교차표
prop.table(prop, margin = 1)

# 3. 가설 검정
# (1) p20_bar - p30_bar 정규분포 수렴 조건
97 * 0.495 # n20*p20_bar > 5
97 * 0.505 # n20*(1-p20_bar) > 5
103 * 0.291 # n30*p20_bar > 5
103 * 0.709 # n30*(1-p20_bar) > 5
# (2) 양측 검정 : 정규성 전제
prop.test(prop, alternative = "two.sided", correct = T)
prop.test(prop, alternative = "two.sided", correct = F) # Z-통계량(차이X)
# p-value = 0.005 : 유의 -> 대립가설 채택
# 최종 결론 : p20(0.495) > p30(0.291) 모비율 차이가 있다.

