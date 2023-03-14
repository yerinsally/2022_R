# 12-2. 텍스트 마이닝(2)

# 형태소 분석 준비
library(KoNLP)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggwordcloud)
library(tidytext)

# unnest_tokens 예시
library(tidytext)
text <- tibble(value = c("대한민국은 민주공화국이다.", "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))
text %>% unnest_tokens(input = value, output = word, token = extractNoun)

# unnest_tokens 실행
word_noun <- moon %>% unnest_tokens(input = value, output = word, token = extractNoun)

# 명사 빈도 분석하기
word_noun <- word_noun %>% 
  count(word, sort = T) %>% 
  filter(str_count(word) > 1) # 두 글자 이상만
top20 <- word_noun %>% head(20)

# 막대 그래프 만들기
font_add_google(name = "Black Han Sans", family = "BHS") # 글자체 설정
showtext_auto()
ggplot(top20, aes(reorder(word, -n), n, fill = word)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(title = "문재인 출마 연설문 명사 빈도") + 
  theme(title = element_text(size = 12),
        text = element_text(family = "BHS"))

# 워드 클라우드 만들기
ggplot(word_noun, aes(label = word, 
                       size = n,
                       col = n)) + 
  geom_text_wordcloud(seed = 1234,
                      family = "BHS") + 
  scale_radius(limits = c(2, NA), 
               range = c(3, 15)) +
  scale_color_gradient(low = "darkgreen",
                       high = "darkred") +
  theme_minimal()

# 특정 단어가 사용된 문장 살펴보기
# 문장 기준 토큰화
sentences_moon <- raw_moon %>%
  str_squish() %>% 
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence, 
                token = "sentences") 
# 빈도수 가장 많은 단어가 포함된 문장 확인
sentences_moon %>% filter(str_detect(sentence, "국민"))
sentences_moon %>% filter(str_detect(sentence, "일자리")) %>% print.data.frame(right = F)
