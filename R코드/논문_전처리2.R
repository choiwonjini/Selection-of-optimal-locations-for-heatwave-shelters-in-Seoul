setwd("C:\\Users\\32217778\\Documents\\.공부\\논문")
#
# 소방 ####
df = read.csv("df.csv") # 이미 인구당 소방인력 데이터 있음
head(df)
fire = read.csv("소방.csv")
head(fire)
nrow(df)
colnames(fire) = fire[1,]
fire = fire[-c(1,2), c(2,3,6)]
colnames(fire) = c("자치구", "인구", "소방공무원")
str(fire)
fire$인구 = as.numeric(fire$인구)
fire$소방공무원 = as.numeric(fire$소방공무원)

fire$인구당소방인력 = fire$소방공무원/fire$인구

df_merged <- merge(df, fire, by = "자치구")
head(df_merged)

df_merged = df_merged[, -16]
nrow(df_merged)
colnames(df_merged)[ncol(df_merged)] = "인구당소방인력"
#인구 수정
fire1 = fire[, c(1,3)]



# 보건 ####
med = read.csv("보건소.csv")
head(med)
med = med[-c(1,2,3,4), c(2,3)]
colnames(med) = c("자치구", "보건소인원")
str(med)
med$보건소인원 = as.numeric(med$보건소인원)
gu_merge = merge(fire, med, by = "자치구")
gu_merge$인구당보건인력 = gu_merge$보건소인원/gu_merge$인구

which(is.na(df_merged), arr.ind = TRUE)
df_merged[118,] # 항동 결측치인데 이유가 bullshit 생활인구였음.
df_merged = df_merged[, -3] # 구라 생활인구 삭제
head(df_merged)
head(gu_merged)
gu_merged = gu_merge[, -c(2,3,4)]
final_df = merge(df_merged, gu_merged, by = "자치구")
final_df = final_df[, -c(16, 18)]
head(final_df)
str(final_df)
nrow(final_df)
ncol(final_df)
#write.csv(final_df, "dfdf.csv", row.names = F)




# 생활인구 ####
pop = read.csv("생활인구_202408.csv", row.names = NULL)
head(pop)
colnames(pop) = colnames(pop)[2:33]
pop = pop[, -33]
pop = pop[, c(1,2,3,4)]

# 일평균 생활인구
library(dplyr)
result <- pop %>%
  group_by(행정동코드, 기준일ID) %>%                # 행정동코드와 기준일별로 그룹화
  summarise(일별_총생활인구수 = sum(총생활인구수)) %>%  # 하루 동안의 생활인구수 합산
  group_by(행정동코드) %>%                        # 행정동별로 그룹화
  summarise(일평균생활인구수 = mean(일별_총생활인구수)) # 일평균 계산
result = data.frame(result)
head(result)

temp = read.csv("기온.csv") # 행정동코드 데이터
temp = temp[, c(3, 4, 5)]
colnames(temp) = c("자치구", "행정동코드", "행정동")
str(temp)
head(temp)

dong_merge = merge(result, temp, by = "행정동코드")
colnames(dong_merge)[c(2,4)] = c("일평균생활인구수", "행정동명")
dong_merge = dong_merge[, -c(1, 3)]

# real_final = merge(final_df, dong_merge, by = "행정동명") # 갑자기 200행대로 줄어듦
nrow(final_df)
nrow(dong_merge)
nrow(real_final)
head(final_df)
head(dong_merge)
head(real_final)

aa = as.factor(dong_merge$행정동명)
aa # 제대로 행정동 있는데?

sum(is.na(real_final))
a = which(is.na(real_final), arr.ind = TRUE)
a = data.frame(a)
real_final[a$row,] # domg_merge에 00제1동 이렇게 "제"가 붙어버림

# 숫자 붙은 제 제거
library(stringr)
dong_names = dong_merge$행정동명
dong_names_cleaned <- ifelse(str_detect(dong_names, "제\\d"), 
                             str_replace(dong_names, "제", ""), 
                             dong_names)
dong_merge$행정동명 = dong_names_cleaned

real_final = merge(final_df, dong_merge, by = "행정동명")
nrow(real_final)
sum(is.na(real_final)) # 성공





# 등록인구 ####
adpop = read.csv("등록인구.csv")
head(adpop)
str(adpop)
colnames(adpop) = adpop[1,]
adpop = adpop[-c(1,2), c(3,5,15)]
colnames(adpop) = c("행정동명", "등록인구수", "노인인구수")
adpop = subset(adpop, 행정동명 != "소계")
adpop$등록인구수 = as.numeric(adpop$등록인구수)
adpop$노인인구수 = as.numeric(adpop$노인인구수)
adpop$노인인구비율 = adpop$노인인구수/adpop$등록인구수
head(adpop)
nrow(adpop)

real_real_final = merge(real_final, adpop, by = "행정동명")
str(real_real_final)
nrow(real_real_final)
sum(is.na(real_real_final)) #결측치 없음
head(real_real_final)

# 저소득노인비율 다시 ####
old = read.csv("독거노인.csv")
head(old)
old = old[-c(1:5), c(3,10)]
colnames(old) = c("행정동명", "저소득노인인구")
old = subset(old, 행정동명 != "소계")
final1 = merge(final, old, by = "행정동명")
head(final1)
final1 = final1[, -10]
str(final1)
final1$저소득노인인구 = as.numeric(final1$저소득노인인구)
sum(is.na(final1$저소득노인인구))
nrow(final1)
q = data.frame(which(is.na(final1), arr.ind = TRUE))
final1[q$row,] # 없으면 0이 아니라 결측치로 되어있음 -> 0으로 변경
final1$저소득노인인구 = ifelse(is.na(final1$저소득노인인구), 0, final1$저소득노인인구)
final1$저소득노인비율 = final1$저소득노인인구 / final1$노인인구수
head(final1)



# 장애인 ####
dis = read.csv("장애인.csv")
head(dis)
str(dis)
dis = dis[-c(1:4), c(3,4)]
colnames(dis) = c("행정동명", "장애인구수")
dis = subset(dis, 행정동명 != "소계")
dis = subset(dis, 행정동명 != "기타")
nrow(dis)

final2 = merge(final1, dis, by = "행정동명")
nrow(final2) # 신사동 개많음

# 중복 확인
duplicated_rows <- final2[duplicated(final2[c("자치구", "행정동명")]), ]
duplicated_rows
nrow(duplicated_rows)

final2 <- final2 %>%
  distinct(자치구, 행정동명, .keep_all = TRUE)

str(final2)
final2$장애인구수 = as.numeric(final2$장애인구수)



# 65세이상 인구당 무더위 쉼터 설치 수, 기타 통계량 ####
data = final2
data$노인대비쉼터수 = data$무더위쉼터.개수/data$노인인구수
head(data)
nrow(data)
data$장애인구비율 = data$장애인구수/data$등록인구수
data$독거노인비율 = data$독거노인_인구수/data$등록인구수
data$기초생활수급자비율 = data$기초생활수급자.수/data$등록인구수

# 인구밀도 ####
den = read.csv("인구밀도.csv")
head(den)
str(den)
den$X2023.2 = as.numeric(den$X2023.2)
den = den[-c(1,2), c(3,5)]
colnames(den) = c("행정동명", "면적(km)")
den = subset(den, 행정동명 != "소계")
data = merge(data, den, by = "행정동명")
data$`면적(km)` = as.numeric(data$`면적(km)`)
data$인구밀도 = data$등록인구수/data$`면적(km)`
head(data)
sum(is.na(data)) # 결측치없음



# 도시화면적비율 ####
city = read.csv("도시화면적.csv")
head(city)
city = city[-c(1,2,3), c(2,4,ncol(city))]
head(city)
city = subset(city, city$시가화면적별.2. == "시가화면적비율 (%)")
city = city[, -2]
colnames(city) = c("자치구", "도시화면적비율")
city$도시화면적비율 = city$도시화면적비율/100

data = merge(data, city, by = "자치구")
head(data)
str(data)
nrow(data)


# 녹지 ####
green = read.csv("녹지.csv")
head(green)
str(green)
green = green[-c(1:3), c(2,6,8,16)]
colnames(green) = c("자치구", "시설녹지", "일반녹지", "하천변조경")
green$시설녹지 = as.numeric(green$시설녹지)
green$일반녹지 = as.numeric(green$일반녹지)
green$하천변조경 = as.numeric(green$하천변조경)

#결측치 0으로 대체
green$시설녹지 = ifelse(is.na(green$시설녹지), 0, green$시설녹지)
green$일반녹지 = ifelse(is.na(green$일반녹지), 0, green$일반녹지)
green$하천변조경 = ifelse(is.na(green$하천변조경), 0, green$하천변조경)

sum(is.na(green))
green$녹지 = green$시설녹지 + green$일반녹지
green = green[, c(1,4,5)]
colnames(green) = c("자치구", "하천변조경(km)", "녹지면적(km)")
green$`녹지면적(km)` = green$`녹지면적(km)` / 1000000
green$`하천변조경(km)` = green$`하천변조경(km)` / 1000000
green = green[-c(nrow(green), nrow(green) - 1),]

# 자치구별 행정동 개수
district_counts <- data %>%
  group_by(자치구) %>%
  summarise(행정동_개수 = n())

# green 데이터프레임과 병합하여 행정동별 녹지면적 계산
green <- left_join(green, district_counts, by = "자치구")
green$`하천변조경(km)` = green$`하천변조경(km)` / green$행정동_개수
green$`녹지면적(km)` = green$`녹지면적(km)` / green$행정동_개수
green = green[, -4]
# data와 조인
data = merge(data, green, by = "자치구")
head(data)
nrow(data)
data$하천변조경비율 = data$`하천변조경(km)` / data$`면적(km)`
data$녹지면적비율 = data$`녹지면적(km)` / data$`면적(km)`

# 최종 데이터 마무리 정리 ####
data = data[, -c(18, 27, 30, 31)]
str(data)
data = data %>%
  select(자치구,
         행정동명,
         자외선지수.평균.,
         노인체감온도.평균.,
         폭염시간합계.5.9월.,
         온열질환.환자수,
         온열질환.추정사망자,
         노인인구수,
         노인인구비율,
         독거노인_인구수,
         독거노인비율,
         기초생활수급자.수,
         기초생활수급자비율,
         저소득노인인구,
         저소득노인비율,
         장애인구수,
         장애인구비율,
         인구밀도,
         무더위쉼터.개수,
         노인대비쉼터수,
         인구당보건인력,
         인구당소방인력,
         도시화면적비율,
         하천변조경비율,
         녹지면적비율,
         재정자립도,
         RGDP,
         등록인구수,
         일평균생활인구수
  )
data = data[, -c(28:29)]
data = data %>%
  rename(
    평균자외선지수 = 자외선지수.평균.,
    평균자외선지수 = 자외선지수.평균.,
    평균노인체감온도 = 노인체감온도.평균.,
    폭염시간합계 = 폭염시간합계.5.9월.,
    온열질환환자수 = 온열질환.환자수,
    온열질환추정사망자 = 온열질환.추정사망자,
    독거노인수 = 독거노인_인구수,
    무더위쉼터개수 = 무더위쉼터.개수,
    기초생활수급자수 = 기초생활수급자.수
    )
#write.csv(data, "dataframe.csv", row.names = F)
# 노후주택 데이터 추가 ####
df = read.csv("dataframe.csv")
home = read.csv("주택.csv")
str(home)
head(home)
head(df)
home = home[-c(1:2), c(2,3,7)]
home$X2023 = as.numeric(home$X2023)
home$X2023.4 = as.numeric(home$X2023.4)
home$단독연립다세대 = home$X2023.4 + home$X2023
colnames(home) = c("자치구", "단독연립대세대주택")
home = home[, c(1,4)]
head(home)

df = merge(df, home, by="자치구")
head(df)
#write.csv(df, "dataframe.csv", row.names = F)


# 둔촌1동, 반포본동 인구 관련 칼럼들은 극단적인 이상치임 -> 두 행 제거 ####
df = read.csv("dataframe.csv")
df = df[df$행정동명 != "둔촌1동",] #반포본동도 제거거

#write.csv(df, "dataframe.csv", row.names = F)
# 소방 다시 ####
fire = read.csv("소방.csv")
head(fire)
nrow(df)
colnames(fire) = fire[1,]
fire = fire[-c(1,2), c(2,3,6)]
colnames(fire) = c("자치구", "인구", "소방공무원")
str(fire)
fire$인구 = as.numeric(fire$인구)
fire$소방공무원 = as.numeric(fire$소방공무원)
fire$인구당소방인력 = fire$소방공무원/fire$인구

fire = fire[, c(1,4)]
df = df[, -22]
df=merge(df, fire, by="자치구")
