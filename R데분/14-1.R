# 14-1. 오즈비 구하기 : 상대적으로 중요한 단어 비교하기

# 막대그래프 그리기
top10_odds <- top10_odds %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))
ggplot(top10_odds, aes(reorder_within(word, n, president), n, fill = president)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free_y") + 
  scale_x_reordered() + 
  labs(x = NULL)
ggplot(top10_odds, aes(reorder_within(word, n, president), n, fill = president)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  facet_wrap(~president, scales = "free") + 
  scale_x_reordered() + 
  labs(x = NULL) # 상대적인 빈도 조정

# 주요 단어가 포함된 문장 살펴보기
speeches_sentence <- bind_speeches %>% as_tibble() %>% 
  unnest_tokens(input = value, output = sentence, token = "sentences")
speeches_sentence %>% 
  filter(president == "moon" & str_detect(sentence, "복지국가"))
speeches_sentence %>% 
  filter(president == "park" & str_detect(sentence, "행복"))
# print.data.frame(right = F)로 모든 문장 보기

# 중요도가 비슷한 단어 살펴보기
frequency_wide %>% 
  arrange(abs(1-odds_ratio)) %>% 
  head(10)
frequency_wide %>% 
  filter(moon >= 5 & park >= 5) %>% 
  arrange(abs(1-odds_ratio)) %>% 
  head(10)
