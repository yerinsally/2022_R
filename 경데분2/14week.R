# 14. SVM

# STEP1 : 데이터프레임 작성 및 전처리
cancer <- read_csv("cancer.csv")
cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL
cancer_SVM <- as.data.frame(scale(cancer[2:31])) # 방법1
cancer_SVM <- as.data.frame(scale(cancer[-1])) # 방법2 : 첫번째 열 제거
cancer_SVM$diagnosis <- cancer$diagnosis

# STEP2 : 두 개의 데이터셋 구성
set.seed(222)
ind_SVM <- sample(2, nrow(cancer_SVM), replace = T, prob = c(0.7,0.3))
cancer_SVM_train <- cancer_SVM[ind_SVM == 1, ]
cancer_SVM_test <- cancer_SVM[ind_SVM == 2, ]
table(cancer_SVM$diagnosis == "M") / nrow(cancer_SVM)
table(cancer_SVM_train$diagnosis == "M") / nrow(cancer_SVM_train)
table(cancer_SVM_test$diagnosis == "M") / nrow(cancer_SVM_test)
# 비율 차이가 많이 나면 set.seed() 다시 설정 후 실행

# (1) linear kernel trick
# STEP3 : train 데이터프레임을 이용한 훈련
set.seed(333)
library(e1071)
linear.svm <- tune.svm(diagnosis~., data = cancer_SVM_train, kernel = "linear", 
                       cost = c(0.1,0.25,0.5,0.75,1,2,3,4,5,7,10))
summary(linear.svm) # 최적의 cost = 0.1, Accuarcy = 0.98
1 - 0.01981707
linear.svm$best.model # sv는 29개

# STEP4 : test 데이터를 이용한 성능 평가
linear.test <- predict(linear.svm$best.model, newdata = cancer_SVM_test)
library(caret)
confusionMatrix(linear.test, cancer_SVM_test$diagnosis)
# Accuracy : 0.9578, Kappa : 0.9044, Sensitivity : 0.9730, Specificity : 0.9273, Pos Pred Value(precision) : 0.9643
# 과대적합 살짝 있음(0.98-0.9578)

# (2) polynomial kernel trick
# STEP3 : train 데이터프레임을 이용한 훈련
set.seed(444)
poly.svm <- tune.svm(diagnosis~., data = cancer_SVM_train, kernel = "polynomial",
                     degree = c(2:5), gamma = seq(0.3, 3, by=0.5), 
                     coef0 = seq(0.5, 3, by=0.5), cost = c(0.1,0.25,0.5,1,2))
summary(poly.svm)
# degree = 2, gamma = 0.3, coef0 = 1, cost = 0.1 / Accuracy = 0.9752439
1 - 0.0247561
poly.svm$best.model # sv는 61개

# STEP4 : test 데이터를 이용한 성능 평가
poly.test <- predict(poly.svm$best.model, newdata = cancer_SVM_test)
confusionMatrix(poly.test, cancer_SVM_test$diagnosis)
# Accuracy : 0.9639, Kappa : 0.9177,  Sensitivity : 0.9820, Specificity : 0.9273, Pos Pred Value(precision) : 0.9646
# 과대적합 없이 적절함 / polynomial kernel trick이 linear kernel trick보다 우수한 성과를 도출함(교수님 강의안과 다른 결과).

# (3) rbf kernel trick
# STEP3 : train 데이터프레임을 이용한 훈련
set.seed(555)
rbf.svm <- tune.svm(diagnosis~., data = cancer_SVM_train, kernel = "radial", 
                    gamma = seq(0.01, 2, by=0.1), cost = seq(0.01, 2, by=0.1))
summary(rbf.svm) # gamma = 0.01, cost = 0.81 / Accuracy = 0.9726829
1 - 0.02731707
rbf.svm$best.model # sv는 95개

# STEP4 : test 데이터를 이용한 성능 평가
rbf.test <- predict(rbf.svm$best.model, newdata = cancer_SVM_test)
confusionMatrix(rbf.test, cancer_SVM_test$diagnosis)
# Accuracy : 0.9639, Kappa : 0.9169,  Sensitivity : 0.9910, Specificity : 0.9091, Pos Pred Value(precision) : 0.9565
# 과대적합 없이 적절함. rbf와 polynomial kernel trick 성능은 동일함.

# (4) sigmoid kernel trick
# STEP3 : train 데이터프레임을 이용한 훈련
set.seed(666)
sigmoid.svm <- tune.svm(diagnosis~., data = cancer_SVM_train, kernel = "sigmoid", 
                        gamma = seq(0.01, 2, by=0.1), cost = seq(0.01, 2, by=0.1))
summary(sigmoid.svm) # gamma = 0.01, cost = 1.71 / Accuracy = 0.970122
1 - 0.02987805
sigmoid.svm$best.model # sv는 83개


# STEP5 : linear kernel trick에 기반한 성능 개선
# 성능이 가장 우수한 모델로 사용, 교수님 강의안 일단 똑같이 진행
set.seed(777)
linear.svm <- tune.svm(diagnosis~., data = cancer_SVM_train, 
                       kernel = "linear", cost = seq(0.01,2,by=0.05))
summary(linear.svm) # cost = 0.96, Accuracy = 0.985122 / 성능 개선됨
1 - 0.01487805 
linear.svm$best.model # sv는 29개
linear.test <- predict(linear.svm$best.model, newdata = cancer_SVM_test)
confusionMatrix(linear.test, cancer_SVM_test$diagnosis)
# Accuracy : 0.9578, Kappa : 0.9044, Sensitivity : 0.9730, Specificity : 0.9273, Pos Pred Value : 0.9643
# 원래 성능지표 = Accuracy : 0.9578, Kappa : 0.9044, Sensitivity : 0.9730, Specificity : 0.9273, Pos Pred Value(precision) : 0.9643

# STEP6 : 예측
cancer_pred <- read_csv("cancer_pred.csv")
cancer_pred$id <- NULL
cancer_pred <- as.data.frame(scale(cancer_pred))
svm.predict <- predict(linear.svm$best.model, newdata = cancer_pred)
svm.predict
# SVM 예측결과 : B M B M B M B M B B
# KNN 예측결과 : B M B B B M B M B B

