# 15. 감정분석

library(dplyr)
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")

# 이모티콘 살펴보기
library(stringr)
dic %>% filter(!str_detect(word, "[가-힣]")) %>% arrange(word)
# 단어 분류
dic %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos", 
                            ifelse(polarity <= -1, "neg", "neu"))) %>% 
  count(sentiment)

# 문장의 감정점수 구하기
# STEP1 : 단어 기준 토큰화하기
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.", 
                          "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
library(tidytext)
df <- df %>% unnest_tokens(input = sentence, output = word, 
                           token = "words", drop = F)

# STEP2 : 단어에 감정점수 부여하기
df <- left_join(df, dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# STEP3 : 문장별로 감정점수 합산하기
score_df <- df %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))

# 댓글 감정 분석하기

# 전처리
raw_news_comment <- read_csv("news_comment_parasite.csv")
# 고유 번호 변수 만들기 & html 특수문자 제거하기
install.packages("textclean")
library(textclean)
news_comment <- raw_news_comment %>% 
  mutate(id = row_number(), 
         reply = str_squish(replace_html(reply))) %>% 
  relocate(id, .before = reg_time)

# 단어 기준 토큰화 및 감정점수 부여하기
word_comment <- news_comment %>% 
  unnest_tokens(input = reply, output = word, 
                token = "words", drop = F)
word_comment <- left_join(word_comment, dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 감정 분류 및 단어 빈도별 막대그래프 그리기
word_comment <- word_comment %>% 
  mutate(sentiment = ifelse(polarity == 2, "pos", 
                            ifelse(polarity == -2, "neg", "neu")))
# 빈도수 상위 10개 단어 데이터프레임
top10_sentiment <- word_comment %>% 
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n,n=10)
library(ggplot2)
ggplot(top10_sentiment, aes(reorder_within(word, n, sentiment), 
                            n, fill = sentiment)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_wrap(~sentiment, scales = "free") + 
  scale_x_reordered() + 
  labs(x = NULL) + 
  geom_text(aes(label = n))

# 댓글별 감정점수 구하고 내용 살펴보기
score_comment <- word_comment %>% 
  group_by(id, reply) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup()
score_comment %>% arrange(-score) %>% head(10)
score_comment %>% arrange(score) %>% head(10)
score_comment %>% count(score)
score_comment <- score_comment %>% 
  mutate(sentiment = ifelse(score >= 1, "pos", 
                            ifelse(score <= -1, "neg", "neu")))
score_comment %>% count(sentiment)



