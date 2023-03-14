# 12-1. 텍스트 마이닝(1)

# 텍스트 전처리
library(dplyr)
library(stringr)
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
head(raw_moon)
moon <- raw_moon %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble()
moon

# 토큰화하기
install.packages("tidytext")
library(tidytext)
library(dplyr)
word_space <- moon %>% unnest_tokens(input = value,
                                     output = word, 
                                     token = "words")

# 단어 빈도 분석하기
word_space <- word_space %>%
  count(word, sort = T) %>% 
  filter(str_count(word) > 1) # 두 글자 이상만 남기기
library(ggplot2)
top20 <- word_space %>% head(20) # 빈도수 상위 20위
ggplot(top20, aes(reorder(word, -n), n, fill = word)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "문재인 출마 연설문 단어 빈도") + 
  theme(title = element_text(size = 12)) # 막대그래프

# 워드 클라우드 만들기
install.packages("ggwordcloud")
library(ggwordcloud)
ggplot(word_space, aes(label = word, size = n)) + 
  geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), range = c(3, 30)) # 기본 설정만
ggplot(word_space, aes(label = word, 
                       size = n,
                       col = n)) + 
  geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), 
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal() # 색 설정
# 글자체 바꾸기
install.packages("showtext")
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
ggplot(top20, aes(reorder(word, -n), n, fill = word)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "문재인 출마 연설문 단어 빈도") + 
  theme(title = element_text(size = 12),
        text = element_text(family = "nanumgothic")) # 막대그래프 글자체 설정
ggplot(word_space, aes(label = word, 
                       size = n,
                       col = n)) + 
  geom_text_wordcloud(seed = 1234,
                      family = "nanumgothic") + 
  scale_radius(limits = c(3, NA), 
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal() # 워드 클라우드 글자체 설정
