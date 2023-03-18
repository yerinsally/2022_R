# 3week : 데이터 전처리 (1) : exam.csv

# filter 함수
exam_c1 <- exam %>% filter(exam$class == 1)
exam_male <- exam %>% filter(gender == "Male")
mean(exam_male$english) # 59.571점
# 문제1
exam_123 <- exam %>% filter(class %in% c(1,2,3))
mean(exam_123$math) # 56.05점
# 문제2
exam_n4 <- exam %>% 
  filter(class != 4) %>% 
  filter(math >= 90 | history >= 95)
# 문제3
exam %>% filter(english >= quantile(exam$english, probs = c(0.9)))

# select 함수 : 원하는 변수(열) 추출
exam %>% select(class, math, english)
exam %>% select(-address)
# 문제4
exam %>% filter(class == 1) %>% select(gender, math)

# arrange 함수
exam %>% arrange(math)
exam %>% arrange(class, -math) %>%  print(n=Inf)

# mutate 함수
exam <- exam %>% mutate(total = math+english+history,
                        average = (math+english+history)/3)
# ifelse
exam <- exam %>% mutate(test = ifelse(total >= 180, "pass", "fail"))
library(descr)
freq(exam$test) # 합격자 빈도수 확인
# case_when
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))

# relocate 함수
exam <- exam %>% relocate(total, .before = test)
exam <- exam %>% relocate(average, .after = test)
exam <- exam %>% relocate(where(is.character))
exam <- exam %>% relocate(where(is.factor), .before = where(is.character))

# group_by와 summarise 함수
exam %>% 
  group_by(class) %>% 
  summarise(count = n(), mean_math = mean(math, na.rm = T), sd_math = sd(math, na.rm = T)) # NA 결측치 제거!
exam_new <- exam %>% 
  group_by(class, gender) %>% 
  summarise(count = n(), mean_hist = mean(history))

# 참고 : sum 함수
exam_new %>% mutate(perc = count / sum(count)) # 반별 sum
exam_new %>% mutate(perc = count / sum(exam_new$count)) # 전체 sum
