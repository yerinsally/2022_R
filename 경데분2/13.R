# 13. KNN

cancer <- read_csv("cancer.csv")

# STEP1 : 데이터프레임 준비 및 전처리
str(cancer)
summary(cancer)
# class 변수(= diagnosis) : 범주형 척도
cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL
table(cancer$diagnosis)
# 새로운 데이터프레임 생성
cancer_z <- as.data.frame(scale(cancer[2:31])) # 표준화
cancer_z$diagnosis <- cancer$diagnosis # diagnosis(DV) 변수 붙여넣기

# STEP2 : train / test 데이터셋 구성
# 2개의 대상을 cancer_z 행의 개수만큼 복원 추출하여 7:3 비율로 객체 형성
set.seed(123)
ind <- sample(2, nrow(cancer_z), replace = T, prob = c(0.7, 0.3))
# [행, 열] : 행의 번호가 (ind = 1/2)인 벡터 추출
cancer_train <- cancer_z[ind == 1, ] # ind = 1 -> train
cancer_test <- cancer_z[ind == 2, ] # ind = 2 -> test
# diagnosis 측정값 비율 비교
table(cancer_z$diagnosis == "M") / nrow(cancer_z)
table(cancer_train$diagnosis == "M") / nrow(cancer_train)
table(cancer_test$diagnosis == "M") / nrow(cancer_test)

# STEP3 : 최적의 k값 도출
library(caret)
grid1 <- expand.grid(k = 3:10) # 근접한 이웃개수 후보군 설정
control <- trainControl(method="repeatedcv", number=10, repeats=5) # 학습방법 세팅
set.seed(135)
knn.train <- train(diagnosis~., data = cancer_train, 
                   method = "knn", trControl = control, tuneGrid = grid1)
knn.train # 최적의 k는 8이 됨 : A&K가 가장 크다.
varImp(knn.train, scale = F) # IV 중요도

# STEP4 : 성능 평가
pred.test <- predict(knn.train, newdata = cancer_test)
pred.test # 예측결과
confusionMatrix(pred.test, cancer_test$diagnosis) # 예측값 vs. 실제값
# train A : 0.9663978 & K : 0.9271068
# test A : 0.9756 & K : 0.9467 (오히려 증가, 과대적합X)

# STEP5 : KNN 모델 성능 개선
library(kknn)
set.seed(777)
kknn.train <- train.kknn(diagnosis~., data = cancer_train, 
                         kmax = 25, distance = 2, 
                         kernel = c("rectangular", "triangular", "Epanechnikov",
                                    "biweight", "triweight", "cosine", "inversion",
                                    "Gaussian", "rank", "optimal"))
kknn.train
# Minimal misclassification: 0.02716049, Best kernel: triangular, Best k: 20
1 - 0.02716049
# train Accuarcy = 0.9728 : 기존 0.9663보다 소폭 증가
kknn.pred.test <- predict(kknn.train, newdata = cancer_test) # test에 적용
kknn.pred.test
confusionMatrix(kknn.pred.test, cancer_test$diagnosis)
# test A : 0.9756 & K : 0.9467 -> knn과 성능 동일
# 실제 성능비교는 test 데이터셋으로) 기존 knn.train과 비교해서 kknn.train의 성능 동일

# STEP6 : 예측
# 새로운 열개 사례로 예측
cancer_pred <- read_csv("cancer_pred.csv")
cancer_pred$id <- NULL
cancer_pred <- as.data.frame(scale(cancer_pred)) # 표준화
# knn & kknn 모델로 예측
pred.test_knn <- predict(knn.train, newdata = cancer_pred)
pred.test_kknn <- predict(kknn.train, newdata = cancer_pred)
pred.test_knn # 예측결과
pred.test_kknn
# 예측결과 동일 : B M B B B M B M B B

