# 13-2. 오즈비 구하기

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(stringr)

# long form -> wide form 변경
df_long <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10) %>% 
  filter(word %in% c("국민", "우리", "정치", "행복"))
df_wide <- df_long %>% 
  pivot_wider(names_from = president, values_from = n, values_fill = list(n = 0))
frequency_wide <- frequency %>%
  pivot_wider(names_from = president, values_from = n, values_fill = list(n = 0))

# 오즈비 구하기
frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon+1)/(sum(moon+1))), 
         ratio_park = ((park+1)/(sum(park+1)))) # 연설문별 단어 비중 구하기
frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park) # 오즈비
# moon > park
frequency_wide %>% arrange(-odds_ratio)
# park > moon
frequency_wide %>% arrange(odds_ratio)
# 비슷
frequency_wide %>% arrange(abs(1-odds_ratio))

# top10 데이터 만들기
top10_odds <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10) %>% 
  arrange(-odds_ratio)
