# 13-1. 단어 빈도 비교하기

# 비교 분석 준비
library(dplyr)
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>% as_tibble() %>% mutate(president = "moon")
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>% as_tibble() %>% mutate(president = "park")
# 두 연설문 통합
bind_speeches <- bind_rows(moon, park) %>% relocate(president, .before = value)

# 데이터 전처리
# value 변수 내 특수문자 및 연속된 공백 제거
library(stringr)
speeches <- bind_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "), 
         value = str_squish(value))

# 명사 기준 토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>% 
  unnest_tokens(input = value, output = word, token = extractNoun)

# 하위 집단(대통령)별 단어 빈도 구하기
frequency <- speeches %>% count(president, word) %>% filter(str_count(word) > 1)

# 자주 사용된 단어 추출
top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  print(n = Inf)

# 빈도수 상위 10개 막대그래프 만들기
library(ggplot2)
ggplot(top10, aes(reorder(word, n), n, fill = president)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free_y")

# 특정 단어(국민) 제외하고 막대그래프 만들기
top10_2 <- frequency %>% 
  filter(word != "국민") %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F) %>% 
  print(n = Inf)
ggplot(top10_2, aes(reorder(word, n), n, fill = president)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free_y")

# 그래프별로 빈도수 정렬 구분
ggplot(top10_2, aes(reorder_within(word, n, president), n, fill = president)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free_y")
# X축 항목의 명칭 조정
ggplot(top10_2, aes(reorder_within(word, n, president), n, fill = president)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL)
