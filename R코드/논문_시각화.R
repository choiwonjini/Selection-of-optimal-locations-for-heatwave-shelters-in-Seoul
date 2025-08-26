# 취약성 지수 시각화 ####
library(dplyr)
library(ggplot2)
library(gridExtra)
setwd("C:\\Users\\32217778\\Documents\\.공부\\BDA 공모전")

vul = read.csv("폭염취약성지수.csv")
vul = vul %>%
  arrange(desc(폭염취약성지수))

top6_dong <- vul %>%
  slice_max(order_by = 폭염취약성지수, n = 6)

bot6_dong <- vul %>%
  slice_min(order_by = 폭염취약성지수, n = 6)

# 상위 6개 그래프 생성 (가로 막대)
plot_top <- ggplot(top6_dong, 
                   aes(x = reorder(행정동명, 폭염취약성지수), y = 폭염취약성지수)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = round(폭염취약성지수, 3)), 
            hjust = 1.2, color = "white", size = 4) +  # 막대 안에 지수 표시
  labs(title = "Top 6",
       x = "", y = "폭염 취약성 지수") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 1))

# 하위 6개 그래프 생성 (가로 막대)
plot_bottom <- ggplot(bot6_dong, 
                      aes(x = reorder(행정동명, 폭염취약성지수), y = 폭염취약성지수)) +
  geom_bar(stat = "identity", fill = "yellowgreen") +
  geom_text(aes(label = round(폭염취약성지수, 3)), 
            hjust = -0.1, color = "black", size = 4) +  # 막대 안에 지수 표시
  labs(title = "Bottom 6",
       x = "", y = "폭염 취약성 지수") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 1))

# 두 그래프를 한 화면에 배치
grid.arrange(plot_top, plot_bottom, ncol = 2)

# 노인 대비 쉼터 개수 시각화 ####
cnt = read.csv("dataframe.csv")
cnt = cnt[, c(1,2,19)]
head(cnt)

top6_dong_cnt <- cnt %>%
  slice_max(order_by = 무더위쉼터개수, n = 6)
top6_dong_cnt$행정동명 = c("Gangil-dong",
                       "Shinwol 5-dong",
                       "Jongam-dong",
                       "Amsa 1-dong",
                       "Dunchon 2-dong",
                       "Guro 2-dong")
# 상위 6개 그래프 생성 (가로 막대)
ggplot(top6_dong_cnt, 
       aes(x = reorder(행정동명, 무더위쉼터개수), y = 무더위쉼터개수)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(무더위쉼터개수, 5)), 
            hjust = 1.2, color = "white", size = 4) +  # 막대 안에 지수 표시
  labs(title = "Top 6",
       x = "", y = "Number of Cooling Center") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 1))

bot6_dong_cnt <- cnt %>%
  slice_min(order_by = 무더위쉼터개수, n = 6)
cnt[cnt$무더위쉼터개수 == 0,]
'
개포1동, 청담동, 둔촌1동, 대학동, 보라매동, 반포본동, 가락1동
위 7개 행정동은 무더위 쉼터가 존재하지 않음
'

# 우선순위지수 시각화 ####
pri = read.csv("우선순위지수.csv")

# 상위와 하위 6개 행정동 필터링
top_bottom_df <- pri %>%
  arrange(desc(우선순위지수)) %>%
  slice(c(1:6, (n() - 5):n()))

# 막대그래프 생성
ggplot(top_bottom_df, aes(x = reorder(행정동명, -우선순위지수), 
                          y = 우선순위지수, 
                          fill = 우선순위지수)) +
  geom_bar(stat = "identity") +
  labs(title = "우선순위지수 상위 및 하위 6개 행정동", x = "", y = "") +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal()
