# 지수는 높지만 노인대비쉼터수비율은 낮은 곳 선별 (우선순위지수 생성)
library(dplyr)

setwd("C:\\Users\\32217778\\Desktop\\논문")
vul = read.csv("폭염취약성지수.csv")
cnt = read.csv("dataframe.csv")
cnt = cnt[, c(2,21)]

str(vul)
str(cnt)

# 데이터프레임 병합
df_merged <- merge(vul, cnt, by = "행정동명")
head(df_merged)
sum(is.na(df_merged)) # 결측치 없음

library(scales)  # rescale() 함수 사용

df_merged <- df_merged %>%
  mutate(
    # 취약성 지수 표준화
    scaled_취약성지수 = rescale(폭염취약성지수, to = c(0, 1)),
    
    # 쉼터 개수를 역비례 표준화 (적을수록 높은 값)
    scaled_역비례_쉼터개수 = rescale(-노인대비쉼터수, to = c(0, 1))
  )


# 분산 계산
var_취약지수 = var(df_merged$scaled_취약성지수)
var_쉼터수 = var(df_merged$scaled_역비례_쉼터개수)

# 가중치 계산
w1 <- var_취약지수 / (var_취약지수 + var_쉼터수);w1
w2 <- var_쉼터수 / (var_취약지수 + var_쉼터수);w2

# 두 값을 더해 우선순위 지표 생성
df_merged$우선순위지수 = df_merged$scaled_취약성지수*w1 + df_merged$scaled_역비례_쉼터개수*w2

# 우선순위가 높은 순으로 정렬
final_df <- df_merged %>%
  arrange(desc(우선순위지수))

head(final_df)
str(final_df)
View(final_df)
final_df[c(1:7, 397:403),]

#write.csv(final_df, "우선순위지수.csv", row.names = F)