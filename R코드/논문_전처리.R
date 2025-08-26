setwd("C:\\Users\\32217778\\Downloads")
lonely = read.csv("독거노인 인구수.csv", header = T)
head(lonely)
str(lonely)
lonely = lonely[, 2:3]
colnames(lonely) = c('행정동명', '독거노인_인구수')
lonely = subset(lonely, lonely$행정동 != "소계")
nrow(lonely)


data = read.csv("dataframe.csv")
nrow(data)
head(data)

library(dplyr)
join_df = left_join(lonely, data, by = "행정동명")
right_df = right_join(lonely, data, by = "행정동명")
sum(is.na(join_df))
sum(is.na(right_df))
write.csv(join_df, "left_join.csv")
write.csv(right_df, "right_df.csv")



only_in_lonely <- anti_join(data, lonely, by = "행정동명")
only_in_data <- anti_join(lonely, data, by = "행정동명")
only_in_lonely$행정동명
only_in_data$행정동명

##

basic = read.csv("기초생활수급자.csv", header = F)
basic = basic[, 2:3]
colnames(basic) = c("행정동명", "기초생활수급자_수")
head(basic)
nrow(basic)
only_in_basic <- anti_join(data, basic, by = "행정동명")
only_in_basic$행정동명
only_in_data2 <- anti_join(basic, data, by = "행정동명")
only_in_data2$행정동명


# df에서 중복된 행정동명 확인
duplicated_df <- df %>%
  group_by(행정동명) %>%
  filter(n() > 1)

# 수급자 데이터에서 중복된 행정동명 확인
duplicated_basic <- basic %>%
  group_by(행정동명) %>%
  filter(n() > 1)

# 행정동명 기준으로 수급자수를 합쳐서 중복된 행 합치기
lonely <- lonely %>%
  group_by(행정동명) %>%                      # 행정동명을 기준으로 그룹화
  summarize(독거노인_인구수 = sum(독거노인_인구수, na.rm = TRUE), .groups = "drop") # 수급자수를 합산

duplicated_lonely_combined = lonely_combined %>%
  group_by(행정동명) %>%
  filter(n() > 1)

# 기타 평균내기
avg_other = mean(duplicated_basic$`기초생활수급자 수`)
# 기타 제거
basic_no_other = subset(basic, basic$행정동명 != "기타")


#### 조인
# 기존 데이터에 독거노인 데이터 조인
pcadata = read.csv("PCAdataframe_csv.csv")
pca_lonely = left_join(pcadata, lonely, by="행정동명")
pca_lonely$독거노인_인구수

# 결측치 제외 평균
avg = mean(pca_lonely$독거노인_인구수, na.rm = T)

pca_lonely$독거노인_인구수 = ifelse(is.na(pca_lonely$독거노인_인구수), 
                             avg, pca_lonely$독거노인_인구수)


# 조인 데이터에 기초수급자 조인
final_join = left_join(pca_lonely, basic_no_other, by="행정동명")
nrow(final_join)
sum(is.na(final_join$`기초생활수급자 수`))

# 결측치를 기타 평균값으로
final_join$`기초생활수급자 수` = ifelse(is.na(final_join$`기초생활수급자 수`), 
                                avg_other, final_join$`기초생활수급자 수`)

ncol(final_join)

#write.csv(final_join, "pcadf.csv")


# 항동 1행 결측치 존재
sum(is.na(final_join))
na_rows <- final_join[rowSums(is.na(final_join)) > 0, ]


str(final_join)
