setwd("C:\\Users\\32217778\\Documents\\.공부\\BDA 공모전")
df = read.csv("dataframe.csv")
View(df)
var(df$평균자외선지수)
var(df$평균노인체감온도)

var(df$온열질환환자수)
var(df$재정자립도)
lapply(df, var)


climate_exposure <- c('평균자외선지수', '평균노인체감온도', '폭염시간합계', '온열질환환자수')
sensitivity <- c('노인인구비율', '독거노인비율', '기초생활수급자비율', '저소득노인비율', '장애인구비율', '인구밀도', '단독연립다세대주택')
adaptive_capacity <- c('노인대비쉼터수', '인구당보건인력', '인구당소방인력', '도시화면적비율', '하천변조경비율', '녹지비율', '재정자립도', 'RGDP')

# Min-Max 정규화 함수
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 데이터프레임에서 지정된 열들을 Min-Max 표준화
normalize_columns <- function(df, variables) {
  df[, variables] <- lapply(df[, variables], min_max_normalize)
  return(df)
}

# 데이터 표준화
df <- normalize_columns(df, climate_exposure)
df <- normalize_columns(df, sensitivity)
df <- normalize_columns(df, adaptive_capacity)
x=df[,c(climate_exposure,sensitivity, adaptive_capacity)]
lapply(x, var)
view(x)

cor = cor(x)
View(cor)

# 예시: PCA 로딩 확인
pca_exposure <- prcomp(df[, climate_exposure], center = TRUE, scale. = TRUE)
print(pca_exposure$rotation)  # Exposure 그룹의 로딩 벡터
summary(pca_exposure)

pca_adaptive <- prcomp(df[, adaptive_capacity], center = TRUE, scale. = TRUE)
print(pca_adaptive$rotation)  # Adaptive Capacity 그룹의 로딩 벡터
summary(pca_adaptive)





