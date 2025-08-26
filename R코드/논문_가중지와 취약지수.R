setwd("C:\\Users\\32217778\\Documents\\.공부\\논문")
df = read.csv("dataframe.csv")

# 데이터 준비
climate_exposure <- c('평균자외선지수', '평균노인체감온도', '폭염시간합계', '온열질환환자수')
sensitivity <- c('노인인구비율', '독거노인비율', '기초생활수급자비율', '저소득노인비율', 
                 '장애인구비율', '인구밀도', '단독연립다세대주택')
adaptive_capacity <- c('노인대비쉼터수', '인구당보건인력', '인구당소방인력', '도시화면적비율',
                       '하천변조경비율', '녹지비율', '재정자립도', 'RGDP')

# Min-Max 표준화 함수
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

# PCA 수행 후 PC1이 설명하는 분산 비율 추출 함수
calculate_group_variance <- function(df, variables) {
  pca_result <- prcomp(df[, variables], center = TRUE, scale. = FALSE) # 이미 표준화되어 있으므로 scale = FALSE
  explained_variance_ratio <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
  return(explained_variance_ratio[1])  # 첫 번째 주성분의 분산 비율
}

#
pca_result = prcomp(df[, climate_exposure], center = TRUE, scale. = FALSE)
pca_result$sdev^2 # 주성분의 고유값
# 다변량 4-2 15쪽 참고해서 pc1=a1x1+a2x2+,,, 쓰기
pca_result



# 각 그룹의 첫 번째 주성분이 설명하는 분산 비율 계산
exposure_variance <- calculate_group_variance(df, climate_exposure)
sensitivity_variance <- calculate_group_variance(df, sensitivity)
adaptive_capacity_variance <- calculate_group_variance(df, adaptive_capacity)

# 총 설명 분산 비율
total_variance <- exposure_variance + sensitivity_variance + adaptive_capacity_variance

# 가중치 계산
exposure_weight <- exposure_variance / total_variance
sensitivity_weight <- sensitivity_variance / total_variance
adaptive_capacity_weight <- adaptive_capacity_variance / total_variance

# 결과 출력
cat("노출 가중치:", exposure_weight, "\n")
cat("민감도 가중치:", sensitivity_weight, "\n")
cat("적응 능력 가중치:", adaptive_capacity_weight, "\n")
  

# PCA 수행 함수 (변수별 계수 계산)
calculate_pca_components <- function(df, variables) {
  pca_result <- prcomp(df[, variables], center = TRUE, scale. = FALSE)
  return(pca_result$rotation[, 1])  # 첫 번째 주성분 계수
}

# 각 그룹에 대한 세부 가중치 계산 함수
calculate_group_weights <- function(df, group_variables, group_weight) {
  # PCA 계수 계산(첫 번째 주성분 계수)
  pca_coefficients <- calculate_pca_components(df, group_variables)
  
  # 변수별 세부 가중치 계산
  total_abs_coefficients <- sum(abs(pca_coefficients))  # 절대값 합산
  variable_weights <- abs(pca_coefficients) / total_abs_coefficients
  
  # 그룹의 가중치를 각 변수에 분배
  weighted_variable_weights <- group_weight * variable_weights
  return(weighted_variable_weights)
}


# 각 그룹별 세부 가중치 계산
climate_exposure_weights <- calculate_group_weights(df, climate_exposure, exposure_weight)
sensitivity_weights <- calculate_group_weights(df, sensitivity, sensitivity_weight)
adaptive_capacity_weights <- calculate_group_weights(df, adaptive_capacity, adaptive_capacity_weight)

# 결과 출력
cat("climate_exposure 변수들의 세부 가중치:", climate_exposure_weights, "\n")
cat("sensitivity 변수들의 세부 가중치:", sensitivity_weights, "\n")
cat("adaptive_capacity 변수들의 세부 가중치:", adaptive_capacity_weights, "\n")


# 세부 가중치를 기반으로 취약성 지수 계산
df$폭염취약성지수 <- (
  (df$평균자외선지수 * climate_exposure_weights[1]) +
    (df$평균노인체감온도 * climate_exposure_weights[2]) +
    (df$폭염시간합계 * climate_exposure_weights[3]) +
    (df$온열질환환자수 * climate_exposure_weights[4]) +
    (df$노인인구비율 * sensitivity_weights[1]) +
    (df$독거노인비율 * sensitivity_weights[2]) +
    (df$기초생활수급자비율 * sensitivity_weights[3]) +
    (df$저소득노인비율 * sensitivity_weights[4]) +
    (df$장애인구비율 * sensitivity_weights[5]) +
    (df$인구밀도 * sensitivity_weights[6]) +
    (df$단독연립다세대주택 * sensitivity_weights[7]) -
    (df$노인대비쉼터수 * adaptive_capacity_weights[1]) -
    (df$인구당보건인력 * adaptive_capacity_weights[2]) -
    (df$인구당소방인력 * adaptive_capacity_weights[3]) -
    (df$도시화면적비율 * adaptive_capacity_weights[4]) -
    (df$하천변조경비율 * adaptive_capacity_weights[5]) -
    (df$녹지비율 * adaptive_capacity_weights[6]) -
    (df$재정자립도 * adaptive_capacity_weights[7]) -
    (df$RGDP * adaptive_capacity_weights[8])
)

# 결과 출력
print(df$폭염취약성지수)


# 결과 데이터프레임 생성
result_df <- df[, c('자치구', '행정동명', '폭염취약성지수')]
print(result_df)
str(df)
# 폭염 취약성 지수를 내림차순으로 정렬
df_sorted <- df[order(df$폭염취약성지수, decreasing = TRUE), c(1,2,32)]

# 정렬된 데이터 출력
head(df_sorted)  # 상위 6개 출력

#write.csv(df_sorted, "폭염취약성지수.csv", row.names = F)
