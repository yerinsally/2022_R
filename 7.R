# preprocessing
library(readr)
exam <- read_csv("exam.csv", col_names = T,
                 locale = locale('ko', encoding = 'euc-kr'))
library(dplyr)
exam <- exam %>% relocate(address, .before = gender)
exam <- exam %>% relocate(test, .after = total)
exam <- exam %>% relocate(grade, .after = average)
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)
exam$grade <- as.factor(exam$grade)
exam$test <- as.factor(exam$test)


# 새로운 변수 id 추가
exam <- exam %>% mutate(id = c(1:30)) %>% relocate(id)

# left_join 함수 : exam_science.csv
library(readr)
exam_science <- read_csv("exam_science.csv")
exam <- left_join(exam, exam_science, by = "id")

# bind_rows 함수 : exam_add.csv
exam_add <- read_csv("exam_add.csv", locale = locale('ko', encoding = 'euc-kr'))
# 변수명과 척도 일치시키기
library(dplyr)
glimpse(exam)
glimpse(exam_add)
exam_add$gender <- as.factor(exam_add$gender)
exam_add$class <- as.factor(exam_add$class)
exam_add$test <- as.factor(exam_add$test)
exam <- bind_rows(exam, exam_add)
# exam에서 NA 변경하기
exam$average <- ifelse(is.na(exam$average), exam$total/3, exam$average)
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))
# 중복된 id 확인 및 제거
n_distinct(exam$id)
exam %>% group_by(id) %>% summarise(count = n()) %>% arrange(-count) # 같은 학생임
exam <- exam %>% distinct(id, .keep_all = T)

# 이상치(outlier) 처리
exam <- exam %>% relocate(science, .after = english)
table(exam[5:8] > 100)
table(exam$math > 100)
table(exam$history > 100)
table(exam$english > 100)
table(exam$science > 100) # 이상치 확인
exam$science <- ifelse(exam$science > 100, NA, exam$science)

# 정해진 범위 내 이상치 확인
library(psych)
descr <- describe(exam[5:8])
# 상한값 구하기
descr$mean + 2.57583*descr$sd
# 하한값 구하기
descr$mean - 2.57583*descr$sd
# min, max값이 상한, 하한값 내에 있는지 판정 -> 이상치 확인
descr <- descr %>% mutate(low = mean - 2.57583*sd, upper = mean + 2.57583*sd)
