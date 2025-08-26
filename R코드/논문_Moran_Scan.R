# 데이터 전처리 ####
# 위경도 데이터 병합
library(sf)
geo_data <- st_read("hangjeongdong_서울특별시.geojson")
dong_names <- sub("서울특별시 .*구 ", "", geo_data$adm_nm)
geo_data[, 2] = dong_names

## 행정동 이름 및 중심점 추출
result <- data.frame(
  행정동 = geo_data$adm_nm, # 행정동 이름 열
  위도 = st_coordinates(st_centroid(geo_data))[, 2], # 중심점의 위도
  경도 = st_coordinates(st_centroid(geo_data))[, 1]  # 중심점의 경도
)

colnames(result)[1] = "행정동명"

setwd("C:\\Users\\32217778\\Desktop\\논문")
df = read.csv("dataframe.csv")
merged_df = merge(df, result, by="행정동명")

pri = read.csv("우선순위지수.csv")
include_pri = merge(merged_df, pri, by='행정동명')
head(include_pri)
#write.csv(include_pri, "totaldata.csv", row.names = F)

###########################
# Moran's I
###########################.
# Moran's I: 공간 자기상관을 판단하는 지표
# H0: 공간적 자기상관이 없다.
df = read.csv("totaldata.csv")

dists <- as.matrix(dist(cbind(df$위도.x, df$경도.x)))

# 가중치를 거리의 역수로(멀수록 작은 가중치 주려고)
dists.inv <- 1/dists
diag(dists.inv) <- 0

dists.inv[1:5, 1:5]

# 분석변수를 우선순위지수로 하고,
# 행정동별 위경도를 기반으로 한 거리의 역수를 가중치로 한다.
library(ape)
Moran.I <- Moran.I(x=df$우선순위지수, weight=dists.inv)

#Moran's I
Moran.I$observed

#p-value of Moran's I
Moran.I$p.value

# 모란 통계량 = 0.181 -> 약한 양의 상관관계가 있다.
# p-value = 0 -> 자기상관이 없다는 H0 기각
# 서울시 행정동별 우선순위지수 분포는 약한 양의 공간적 자기상관이 있다.

###########################
# 스캔 통계량
###########################.
# 폭염에 취약한 지역들이 공간적으로 군집되어 있는지, 아니면 무작위로 분포하는지를 확인

'
스캔 통계는 특정 공간 단위에서 사건 수가 인구에 비해 비정상적으로
높은 지역을 탐지하는 기법.
'
df = read.csv("totaldata.csv")
str(df)
head(df)

scandf = data.frame(위도 = df$위도.x,
                    경도 = df$경도.x,
                    max = 100,
                    vul = df$scaled_취약성지수 * 100)
head(scandf)

library(smerc)
coords <- with(scandf, cbind(위도, 경도))
out <- scan.test(
  coords = coords, cases = floor(scandf$vul),
  pop = scandf$max, nsim = 49,
  longlat = TRUE, alpha = 0.2
)

result = summary(out)

# sum(scandf$pri) = sum(out$total_cases)

# 군집 중심 좌표를 계산하여 데이터프레임으로 변환
cluster_centers <- do.call(rbind, lapply(seq_along(out$clusters), function(i) {
  cluster <- out$clusters[[i]]
  indices <- cluster$locids  # 군집에 포함된 데이터 포인트 인덱스
  cluster_points <- coords[indices, ]  # 군집에 포함된 좌표
  center <- colMeans(cluster_points)  # 중심 좌표 계산 (평균)
  
  # 결과 저장
  data.frame(
    cluster_id = i,
    center_lat = center[1],  # 위도 중심
    center_lon = center[2]   # 경도 중심
  )
}))

# 결과 확인
print(cluster_centers)

result_clusters = cbind(result, cluster_centers)

library(dplyr)
rownames(result_clusters) = result_clusters$cluster_id
result_clusters = result_clusters[, -8]
result_clusters
