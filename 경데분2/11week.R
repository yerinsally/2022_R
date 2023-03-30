# 11. 군집분석

HRA <- read_csv("attrition_CA.csv")
glimpse(HRA)
HRA$accident <- as.factor(HRA$accident)
HRA$promotion <- as.factor(HRA$promotion)
HRA$department <- as.factor(HRA$department)
HRA$salary <- as.factor(HRA$salary)
HRA$left <- as.factor(HRA$left)

# 11-1. 계층적 군집분석 실습

# STEP1 : 데이터프레임 생성
HRA_hr <- HRA %>% filter(department == "hr")
table(is.na(HRA_hr))

# STEP2 : 더미변수 생성
str(HRA_hr)
# 범주형 척도 + 범주 개수가 3개 이상 -> 더미변수로 변경 = salary, department(필요X)
table(HRA_hr$salary) # 더미변수 2개 생성하기
HRA_hr <- HRA_hr %>% mutate(low = ifelse(salary == "low", 1, 0),
                            medium = ifelse(salary == "medium", 1, 0))
# high = 0 0 / low = 1 0 / medium = 0 1
HRA_hr$id <- NULL
HRA_hr$department <- NULL
HRA_hr$salary <- NULL

# STEP3 : 사례간의 거리 측정
library(cluster)
distance_hr <- daisy(HRA_hr, metric = "gower") # 객체

# STEP4 : 계층적 군집분석 실시
HRA_CA_hr <- hclust(distance_hr, method = "ward.D2") # 리스트
plot(HRA_CA_hr, col = "darkgreen", main = "HRA") # 덴드로그램

# STEP5 : 최적의 군집개수(k) 구하기
# NbClust 함수로 유클리디안 거리 적용하려면 모든 변수는 계량형 척도여야함
str(HRA_hr)
HRA_hr$left <- as.numeric(HRA_hr$left)
HRA_hr$accident <- as.numeric(HRA_hr$accident)
HRA_hr$promotion <- as.numeric(HRA_hr$promotion)
library(NbClust)
set.seed(123) # 결과 고정
HRA_hr_NC1 <- NbClust(HRA_hr, distance = "euclidean", 
                      min.nc = 5, max.nc = 15, method = "average") # k = 9

# STEP6 : k에 기반한 군집분석 실시
HRA_hr_HCA <- cutree(HRA_CA_hr, 9) # 열 벡터
table(HRA_hr_HCA)
result_hr <- aggregate(HRA_hr, by = list(cluster = HRA_hr_HCA), mean)
# cluster별(9개) 특징 확인 가능
HRA_hr <- HRA_hr %>% mutate(cluster1 = HRA_hr_HCA)
# 각 어느 군집인지 파악하기 위해 기존 df에 cluster 부착



# 11-2. 비계층적 군집분석(K-평균) 실습

# STEP1 : 데이터프레임 생성
# (형태 & 내용상) 계량형 척도로 측정된 변수만 추출
glimpse(HRA_hr)
HRA_hr_k <- HRA_hr[2:6]
table(is.na(HRA_hr_k))

# STEP2 : 변수 측정값 표준화
HRA_hr_k <- scale(HRA_hr_k)

# STEP3 : 최적의 군집개수(k) 구하기
library(NbClust)
set.seed(125) # 결과 고정
HRA_hr_NC2 <- NbClust(HRA_hr_k, distance = "euclidean", 
                      min.nc = 5, max.nc = 15, method = "kmeans") # k = 5

# STEP4 : K-means 실행
set.seed(124)
HRA_hr_KCA <- kmeans(HRA_hr_k, centers = 5, nstart = 25)
# centers에는 위에서 구한 최적의 k값 입력
HRA_hr_KCA$size # 각 군집별 사례 빈도
result_hr2 <- aggregate(HRA_hr, by = list(cluster = HRA_hr_KCA$cluster), mean)
# cluster별(5개) 특징 확인 가능
HRA_hr <- HRA_hr %>% mutate(cluster2 = HRA_hr_KCA$cluster)
# 각 어느 군집인지 파악하기 위해 기존 df에 cluster 부착
HRA_hr %>% 
  group_by(cluster1, cluster2) %>% 
  summarise(n()) %>% 
  print(n = 100) # 두 개의 군집 비교
