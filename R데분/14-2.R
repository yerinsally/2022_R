# 14-2.

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(stringr)

# (1) 로그오즈비 구하기

# 로그오즈비 변수 만들기
frequency_wide <- frequency_wide %>% mutate(log_odds_ratio = log(odds_ratio))

# top10 데이터 만들기
top10_log <- frequency_wide %>% 
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

# 막대그래프 그리기
ggplot(top10_log, aes(reorder(word, log_odds_ratio), log_odds_ratio, fill = president)) + geom_bar(stat = "identity") + coord_flip() + labs(x = NULL)

# (2) TF-IDF 구하기

# 데이터 준비
library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")
speeches <- raw_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "), 
         value = str_squish(value))
library(KoNLP)
speeches <- speeches %>% 
  unnest_tokens(input = value, output = word, token = extractNoun)
frequency_four <- speeches %>% 
  count(president, word) %>% 
  filter(str_count(word) > 1)

# 연설문별로 TF/IDF/TF-IDF 계산
frequency_four <- frequency_four %>% 
  bind_tf_idf(term = word, document = president, n = n) %>% 
  arrange(-tf-idf)
frequency_four %>% filter(president == "노무현")
frequency_four %>% filter(president == "이명박")

# 막대그래프 그리기
top10_tf <- frequency_four %>% 
  group_by(president) %>% 
  slice_max(tf_idf, n = 10, with_ties = F)
str(top10_tf)
top10_tf$president <- factor(top10_tf$president, 
                             levels = c("문재인","박근혜","이명박","노무현"))
ggplot(top10_tf, aes(reorder_within(word, tf_idf, president), 
                     tf_idf, fill = president)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  facet_wrap(~president, scales = "free") + 
  scale_x_reordered() + 
  labs(x = NULL)
