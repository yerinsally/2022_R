library(readr)
exam <- read_csv("exam.csv", col_names = T, locale = locale('ko', encoding = 'euc-kr'))
exam$math <- as.numeric(exam$math)

# filter 함수 실습
library(dplyr)
exam_c1 <- exam %>% filter(class == 1)
exam_male <- exam %>% filter(gender == "Male")
mean(exam_male$english, na.rm = T)
# 문제1~3
exam_123 <- exam %>% filter(class %in% c(1,2,3))
mean(exam_123$math)
exam_n4 <- exam %>% filter(class != 4) %>% filter(math >= 90 | history >= 95)
quantile(exam$english, probs = c(0.9))
exam %>% filter(english >= quantile(exam$english, probs = c(0.9)))

# select 함수 실습
exam %>% select(class, english, math)
exam %>% select(-address)
exam %>% select(contains("add"))
# 문제4
exam %>% filter(class == 1) %>% select(gender, math)

# arrange 함수 실습
exam %>% arrange(math)
exam %>% arrange(-math)
exam %>% arrange(class, -math) %>% print(n=Inf)

# mutate 함수 실습
exam <- exam %>% mutate(total = math + english + history, average = (math + english + history)/3)
exam <- exam %>% mutate(test = ifelse(total >= 180, "pass", "fail"))
table(exam$test) # freq 함수 가능
exam <- exam %>% mutate(grade = ifelse(average < 60, "fail", ifelse(average < 75, "middle", ifelse(average < 90, "good", "excellent")))) # 4개 집단 = ifelse 3개

# case when 사용 : NA 고려해야함
exam$test <- NULL
exam$grade <- NULL
exam <- exam %>% mutate(test = case_when(total < 180~"fail", total >= 180~"pass"))
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))
exam$test <- ifelse(is.na(exam$total), NA, exam$test) # 15번째 값 NA : TRUE~ 조건 X

# relocate 함수 실습
exam <- exam %>% relocate(total, .before = test)
exam <- exam %>% relocate(average, .after = test)
glimpse(exam)
exam <- exam %>% relocate(where(is.character)) # 문자형 척도 변수 맨 앞으로 이동
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)
exam$test <- as.factor(exam$test)
exam$grade <- as.factor(exam$grade)
exam <- exam %>% relocate(where(is.factor), .before = where(is.character)) # 범주형 척도 변수를 문자형 척도 변수 앞으로 이동

# group_by와 summarise 함수 실습
exam %>% group_by(class) %>% summarise(count = n(), mean_math = mean(math, na.rm = T), sd_math = sd(math, na.rm = T)) # 변수명 지정
exam_new <- exam %>% group_by(class, gender) %>%  summarise(count = n(), mean_history = mean(history))

# 참고
exam_new %>% mutate(perc = count/sum(count)) # 반별로 sum 적용됨
exam_new %>% mutate(perc = count/sum(exam_new$count)) # 전체 sum

# exam 파일 다시 csv로 저장
write.csv(exam, "exam.csv", fileEncoding = 'cp949')
