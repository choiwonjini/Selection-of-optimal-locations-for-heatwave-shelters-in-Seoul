setwd("C:\\Users\\32217778\\Documents\\.공부\\논문")
# 기초작업 ####
temp_df = df[, -c(9,10,11,12,13,14,15,20,21)]
duplicated_rows <- temp_df[temp_df$행정동명 %in% temp_df$행정동명[duplicated(temp_df$행정동명)], ];duplicated_rows
temp_df <- temp_df %>%
  mutate(행정동명 = case_when(
    자치구 == "관악구" & 행정동명 == "신사동" ~ "신사동(관악)",
    자치구 == "강남구" & 행정동명 == "신사동" ~ "신사동(강남)",
    TRUE ~ 행정동명  # 나머지 행은 변경하지 않음
  ))

temp_df = df[, -c(7,8)] #노인도 그냥 다시 ㄱㄱ


nosinsa = data.frame(temp_df[, -c(7,8,9,10,11,12,13,18,19)])
#write.csv(nosinsa, "nosinsa.csv", row.names = F)


# 등록인구, 노인, 노인비율 ####
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
View(adpop)

row.names(adpop) <- NULL

# 중복행 찾기
duplicated_rows <- adpop[adpop$행정동명 %in% adpop$행정동명[duplicated(adpop$행정동명)], ];duplicated_rows

adpop[330,1] = "신사동(강남)"
adpop[359,1] = "신사동(관악)"
head(adpop)

# 독거노인, 저소득노인 ####

lonelow = read.csv("독거노인_저소득노인.csv")
head(lonelow)
str(lonelow)
lonelow = lonelow[-c(1:5), -c(1,5,6,7,8,9,11,12,13,14,15)]
colnames(lonelow) = c("자치구", "행정동명", "독거노인수", "저소득노인수")
lonelow = subset(lonelow, 행정동명 != "소계")

duplicated_rows <- lonelow[lonelow$행정동명 %in% lonelow$행정동명[duplicated(lonelow$행정동명)], ]
duplicated_rows
lonelow <- lonelow %>%
  mutate(행정동명 = case_when(
    자치구 == "관악구" & 행정동명 == "신사동" ~ "신사동(관악)",
    자치구 == "강남구" & 행정동명 == "신사동" ~ "신사동(강남)",
    TRUE ~ 행정동명  # 나머지 행은 변경하지 않음
  ))
lonelow = lonelow[, -1] # 자치구 제거 (조인할때 헷갈림 방지)
head(lonelow)
# 비율은 최종 df에서 계산 ㄱㄱ

# 장애인구 ####
dis = read.csv("장애인.csv")
head(dis)
str(dis)
dis = dis[-c(1:4), c(2,3,4)]
colnames(dis) = c("자치구", "행정동명", "장애인구수")
dis = subset(dis, 행정동명 != "소계")
dis = subset(dis, 행정동명 != "기타")
nrow(dis)

duplicated_rows <- dis[dis$행정동명 %in% dis$행정동명[duplicated(dis$행정동명)], ];duplicated_rows

dis <- dis %>%
  mutate(행정동명 = case_when(
    자치구 == "관악구" & 행정동명 == "신사동" ~ "신사동(관악)",
    자치구 == "강남구" & 행정동명 == "신사동" ~ "신사동(강남)",
    TRUE ~ 행정동명  # 나머지 행은 변경하지 않음
  ))
dis = dis[, -1]
head(dis)

# 인구밀도 ####
den = read.csv("인구밀도.csv")
head(den)
str(den)
den$X2023.2 = as.numeric(den$X2023.2)
den = den[-c(1,2), c(2,3,5)]
colnames(den) = c("자치구", "행정동명", "면적(km)")
den = subset(den, 행정동명 != "소계")

duplicated_rows <- den[den$행정동명 %in% den$행정동명[duplicated(den$행정동명)], ];duplicated_rows
den <- den %>%
  mutate(행정동명 = case_when(
    자치구 == "관악구" & 행정동명 == "신사동" ~ "신사동(관악)",
    자치구 == "강남구" & 행정동명 == "신사동" ~ "신사동(강남)",
    TRUE ~ 행정동명  # 나머지 행은 변경하지 않음
  ))
den = den[, -1]
head(den)
'
최종 df 합치고 인구밀도 계산
data$`면적(km)` = as.numeric(data$`면적(km)`)
data$인구밀도 = data$등록인구수/data$`면적(km)`
head(data)
sum(is.na(data)) # 결측치없음'

# 
# 하천,녹지면적 - 구 ####
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
district_counts <- df %>%
  group_by(자치구) %>%
  summarise(행정동_개수 = n())

# green 데이터프레임과 병합하여 행정동별 녹지면적 계산
green <- left_join(green, district_counts, by = "자치구")
green$`하천변조경(km)` = green$`하천변조경(km)` / green$행정동_개수
green$`녹지면적(km)` = green$`녹지면적(km)` / green$행정동_개수
green = green[, -4]

"# data와 조인
data = merge(data, green, by = "자치구")
head(data)
nrow(data)
data$하천변조경비율 = data$`하천변조경(km)` / data$`면적(km)`
data$녹지면적비율 = data$`녹지면적(km)` / data$`면적(km)`"

# 노후주택수 - 구 ####
home = read.csv("주택.csv")
str(home)
head(home)
home = home[-c(1:2), c(2,3,7)]
home$X2023 = as.numeric(home$X2023)
home$X2023.4 = as.numeric(home$X2023.4)
home$단독연립다세대 = home$X2023.4 + home$X2023
home = home[, c(1,4)]
colnames(home) = c("자치구", "단독연립다세대주택")
head(home)


# 기초생활수급자 ####
gicho = read.csv("기초생활수급자.csv")
head(gicho)

gicho = gicho[-c(1:3), c(1,2,3,4)]
gicho$X2022 = as.numeric(gicho$X2022)
gicho$X2022.1 = as.numeric(gicho$X2022.1)
gicho$기초생활수급자수 = gicho$X2022 + gicho$X2022.1
gicho=gicho[,c(1,2,5)]
colnames(gicho) = c("자치구", "행정동명", "기초생활수급자수")
gicho=subset(gicho, (행정동명 != "소계") & (행정동명 != "기타"))


duplicated_rows <- gicho[gicho$행정동명 %in% gicho$행정동명[duplicated(gicho$행정동명)], ];duplicated_rows
gicho <- gicho %>%
  mutate(행정동명 = case_when(
    자치구 == "관악구" & 행정동명 == "신사동" ~ "신사동(관악)",
    자치구 == "강남구" & 행정동명 == "신사동" ~ "신사동(강남)",
    TRUE ~ 행정동명  # 나머지 행은 변경하지 않음
  ))
gicho = gicho[, -1]
head(gicho)

# 데이터 최종 병합 ####

# df, adpop(등록인구, 노인, 노인비율)
#str(df)
#df = df[, -c(7,8,9,10,11,12,13,14,15,20,21)]
str(nosinsa)
#da = merge(df, adpop, by = "행정동명")
da = merge(nosinsa, adpop, by = "행정동명")
head(da)
str(dal)

# da, lonelow(독거노인, 저소득노인) 
dal = merge(da, lonelow, by = "행정동명")
head(dal)
str(dal)

# dadal# dal, dis(장애인구)
dald = merge(dal, dis, by = "행정동명")
head(dald)
str(dald)

# dald, den(면적)(인구밀도 계산할거임)
daldd = merge(dald, den, by = "행정동명")
head(daldd)
str(daldd)

# daldd, green(하천, 녹지)
dalddg = merge(daldd, green, by = "자치구")
head(dalddg)
str(dalddg)

# dalddg, home(주택)
dalddgh = merge(dalddg, home, by = "자치구")
head(dalddgh)
dalddgh = dalddgh[, -13] # 주택 쓸데없는 변수 삭제

# dalddgh, gicho(기초생활수급자)
dalddghg = merge(dalddgh, gicho, by = "행정동명")
head(dalddghg)

# 중복 행 없음
duplicated_rows <- dalddg[dalddg$행정동명 %in% dalddg$행정동명[duplicated(dalddg$행정동명)], ];duplicated_rows

# 데이터 최종 계산 ####
head(dalddghg)
final = data.frame(dalddghg)
str(final)

# 숫자형으로 변환
final$독거노인수 = as.numeric(final$독거노인수)
final$저소득노인수 = as.numeric(final$저소득노인수)
final$장애인구수 = as.numeric(final$장애인구수)
final$면적.km. = as.numeric(final$면적.km.)

# 독거노인 비율
final$독거노인비율 = final$독거노인수 / final$등록인구수

# 저소득노인 비율
final$저소득노인비율 = final$저소득노인수 / final$등록인구수

# 장애인구 비율
final$장애인구비율 = final$장애인구수 / final$등록인구수

# 인구밀도
final$인구밀도 = final$등록인구수 / final$면적.km.

# 하천변조경 비율
final$하천변조경비율 = final$하천변조경.km. / final$면적.km.

# 녹지 비율
final$녹지비율 = final$녹지면적.km. / final$면적.km.

# 기초생활수급자 비율
final$기초생활수급자비율 = final$기초생활수급자수 / final$등록인구수

head(final)
View(final)

# 데이터 최종 정리 ####
final = final %>%
  select(자치구,
         행정동명,
         평균자외선지수,
         평균노인체감온도 ,
         폭염시간합계 ,
         온열질환환자수,
         등록인구수,
         노인인구수,
         노인인구비율,
         독거노인수 ,
         독거노인비율 ,
         기초생활수급자수 ,
         기초생활수급자비율,
         저소득노인수,
         저소득노인비율,
         장애인구수,
         장애인구비율,
         인구밀도,
         단독연립다세대주택,
         무더위쉼터개수 ,
         노인대비쉼터수,
         인구당보건인력,
         인구당소방인력,
         도시화면적비율,
         하천변조경비율,
         녹지비율 ,
         재정자립도,
         RGDP,
         면적.km.,
         하천변조경.km.,
         녹지면적.km.
  )

# 명동, 소공동, 잠실7동 저소득노인 결측치
final[rowSums(is.na(final)) > 0, ]
final$저소득노인수 = ifelse(is.na(final$저소득노인수), 
                      mean(final$저소득노인수, na.rm = T), final$저소득노인수)
final$저소득노인비율 = ifelse(is.na(final$저소득노인비율), 
                       mean(final$저소득노인비율, na.rm = T), final$저소득노인비율)
sum(is.na(final))
head(final)
#write.csv(final, "dataframe.csv", row.names = F)
