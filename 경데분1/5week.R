# 5week 데이터 전처리 (3) : exam.csv

# 데이터프레임 정비
exam <- exam %>% relocate(address, .before = gender)
exam <- exam %>% relocate(test, .after = total)
exam <- exam %>% relocate(grade, .after = average)
exam <- exam %>% mutate(id = c(1:30)) %>% relocate(id)

# join 함수
exam_science <- read_csv("exam_science.csv")
exam <- left_join(exam, exam_science, by = "id")

# bind_rows 함수
exam_add <- read_csv("exam_add.csv",
                     locale = locale('ko', encoding = 'euc-kr'))
glimpse(exam)
glimpse(exam_add)
exam_add$gender <- as.factor(exam_add$gender)
exam_add$class <- as.factor(exam_add$class)
exam <- bind_rows(exam, exam_add)
exam <- exam %>% relocate(science, .after = english)
# 중복된 id 확인 및 처리
exam %>% group_by(id) %>% summarise(count = n()) %>% arrange(-count)
exam <- exam %>% distinct(id, .keep_all = T)

# 이상치 처리
table(exam$math > 100)
table(exam$history > 100)
table(exam$english > 100)
table(exam$science > 100) # 이상치 존재
exam$science <- ifelse(exam$science > 100, NA, exam$science)

# 정규분포 가정하, 이상치 처리
library(psych)
exam_descr <- describe(exam[5:8])
exam_descr <- exam_descr %>% mutate(upper = mean + 2.57583*sd) # 상한값
exam_descr <- exam_descr %>% mutate(lower = mean - 2.57583*sd) # 하한값
# [5:8]의 max/min과 비교하여 이상치 확인
