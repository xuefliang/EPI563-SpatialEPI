# 加载必要的包
pacman::p_load(sf, tidyverse, tmap, MASS, tigris)

# 下载乔治亚州真实的县级边界数据
# 使用tigris包获取美国县级边界，

# 下载乔治亚州县边界（FIPS代码13）
options(tigris_use_cache = TRUE)
ga_counties <- counties(state = "GA", year = 2019)

# 转换为适合的投影坐标系
ga_counties <- ga_counties %>%
  st_transform(crs = 4326) %>%
  select(GEOID, NAME, geometry) %>%
  arrange(NAME)


# 生成模拟的VLBW数据
set.seed(123)
n_counties <- nrow(ga_counties)

# 获取县的质心坐标用于空间建模
centroids <- st_centroid(ga_counties)
coords <- st_coordinates(centroids)

# 识别主要城市县（亚特兰大都市区等）
# 这些是乔治亚州的主要城市县
major_urban_counties <- c(
  "Fulton",
  "Gwinnett",
  "Cobb",
  "DeKalb",
  "Clayton",
  "Henry",
  "Douglas",
  "Rockdale",
  "Fayette",
  "Cherokee",
  "Forsyth",
  "Hall",
  "Paulding",
  "Coweta",
  "Newton",
  "Chatham",
  "Richmond",
  "Muscogee",
  "Bibb"
)

urban_indices <- which(ga_counties$NAME %in% major_urban_counties)
rural_indices <- setdiff(1:n_counties, urban_indices)

# 生成总出生数（基于县的城市化程度）
total_births <- numeric(n_counties)

# 城市县：更多出生数
if (length(urban_indices) > 0) {
  total_births[urban_indices] <- rpois(length(urban_indices), lambda = 1500) +
    800
}

# 农村县：较少出生数
if (length(rural_indices) > 0) {
  total_births[rural_indices] <- rpois(length(rural_indices), lambda = 150) + 30
}

# 特别为几个大县设置更高的出生数
major_counties <- c("Fulton", "Gwinnett", "Cobb", "DeKalb")
major_indices <- which(ga_counties$NAME %in% major_counties)
if (length(major_indices) > 0) {
  total_births[major_indices] <- rpois(length(major_indices), lambda = 3000) +
    2000
}

# 创建空间相关的VLBW风险模式
# 基础风险
risk_base <- rep(0.018, n_counties) # 1.8%基础风险

# 创建几个风险热点（通常在城市地区和某些农村地区）
hotspot_counties <- c(
  "Fulton",
  "DeKalb",
  "Clayton",
  "Richmond",
  "Bibb",
  sample(ga_counties$NAME[rural_indices], 3)
)

for (hotspot in hotspot_counties) {
  hotspot_idx <- which(ga_counties$NAME == hotspot)
  if (length(hotspot_idx) > 0) {
    # 计算到热点的距离
    hotspot_coord <- coords[hotspot_idx, , drop = FALSE]
    distances <- sqrt(rowSums((coords - hotspot_coord[rep(1, n_counties), ])^2))

    # 添加距离衰减的风险增加
    risk_increase <- 0.025 * exp(-distances / 0.5)
    risk_base <- risk_base + risk_increase
  }
}

# 城市县普遍风险稍高
risk_base[urban_indices] <- risk_base[urban_indices] * 1.2

# 确保风险在合理范围内
risk_base <- pmax(0.008, pmin(0.065, risk_base))

# 生成VLBW病例数
vlbw_cases <- rbinom(n_counties, total_births, risk_base)

# 创建主要VLBW数据集
vlbw <- ga_counties %>%
  mutate(
    TOT = total_births,
    VLBW = vlbw_cases,
    rate = VLBW / TOT * 100
  )

# 生成年龄分层数据
age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45+")
n_age_groups <- length(age_groups)

# 为每个县和年龄组生成数据
age_data <- map_dfr(1:n_counties, function(i) {
  county_total <- total_births[i]

  # 现实的年龄分布（25-34岁为主要生育年龄）
  age_props <- c(0.09, 0.20, 0.32, 0.25, 0.11, 0.025, 0.005)
  age_births <- as.numeric(rmultinom(1, county_total, age_props))

  # 年龄相关的VLBW风险（U型：年轻和高龄风险高）
  age_risk_multiplier <- c(1.9, 1.3, 1.0, 0.95, 1.25, 1.7, 2.2)
  age_risks <- risk_base[i] * age_risk_multiplier

  # 生成各年龄组的VLBW病例
  age_vlbw <- rbinom(n_age_groups, age_births, age_risks)

  tibble(
    GEOID = ga_counties$GEOID[i],
    NAME = ga_counties$NAME[i],
    AGE_GROUP = age_groups,
    TOT = age_births,
    VLBW = age_vlbw,
    rate = ifelse(age_births > 0, age_vlbw / age_births * 100, 0)
  )
})

# 创建年龄分层的sf对象
age <- age_data %>%
  left_join(ga_counties, by = c("GEOID", "NAME")) %>%
  st_as_sf()

# 数据质量检查
cat("\n=== 乔治亚州VLBW数据集摘要 ===\n")
cat("县数:", nrow(vlbw), "\n")
cat("总出生数:", format(sum(vlbw$TOT), big.mark = ","), "\n")
cat("总VLBW病例:", sum(vlbw$VLBW), "\n")
cat("总体VLBW率:", round(sum(vlbw$VLBW) / sum(vlbw$TOT) * 100, 2), "%\n")
cat(
  "VLBW率范围:",
  round(min(vlbw$rate), 2),
  "% -",
  round(max(vlbw$rate), 2),
  "%\n"
)
cat("零病例县数:", sum(vlbw$VLBW == 0), "\n")
cat("出生数<50的县数:", sum(vlbw$TOT < 50), "\n\n")

# 显示主要城市县的数据
cat("主要城市县VLBW情况:\n")
urban_data <- vlbw %>%
  st_drop_geometry() %>%
  filter(NAME %in% major_urban_counties) %>%
  arrange(desc(TOT)) %>%
  select(NAME, TOT, VLBW, rate)
print(urban_data)

# 创建地图可视化
# 设置tmap模式
tmap_mode("plot")

# VLBW率地图
map1 <- tm_shape(vlbw) +
  tm_fill(
    "rate",
    title = "VLBW率 (%)",
    palette = "Reds",
    style = "quantile",
    n = 5
  ) +
  tm_borders(alpha = 0.3, lwd = 0.5) +
  tm_layout(
    title = "乔治亚州县级极低出生体重率 (2018-2019)",
    title.size = 1.0,
    legend.position = c("right", "bottom")
  )

# 出生数地图
map2 <- tm_shape(vlbw) +
  tm_fill(
    "TOT",
    title = "总出生数",
    palette = "Blues",
    style = "quantile",
    n = 5
  ) +
  tm_borders(alpha = 0.3, lwd = 0.5) +
  tm_layout(
    title = "乔治亚州县级总出生数 (2018-2019)",
    title.size = 1.0,
    legend.position = c("right", "bottom")
  )

# 显示地图
print(map1)
print(map2)

# 保存数据（如果需要）
# st_write(vlbw, "ga-vlbw.gpkg", delete_dsn = TRUE)
# st_write(age, "ga-vlbw-age.gpkg", delete_dsn = TRUE)

cat("\n数据生成完成！现在你有了基于真实乔治亚州县边界的VLBW数据集。\n")

save.image("data/vlbw.RData")
load('data/vlbw.RData')


###########################
library(sf)
library(tigris)
library(spatstat)
library(dplyr)

# 1. 获取佐治亚州(GA)的县级地图
# 注意：首次运行可能需要联网下载
ga <- counties(state = "GA", cb = TRUE)

# 2. 筛选出 Dekalb 和 Fulton 县并合并边界
study_area <- ga %>%
  filter(NAME %in% c("DeKalb", "Fulton")) %>%
  st_union() %>% # 合并两个县为一个多边形
  st_transform(crs = 32616) # 投影到 UTM 16N (单位变为米)，方便空间分析

# 3. 转换为 owin 对象
county_owin <- as.owin(st_as_sf(study_area))

set.seed(2026)

# 模拟 2000 个出生点 (随机分布)
# 在现实中，这些点会集中在亚特兰大市中心等人口密集区
b_ppp <- rpoispp(lambda = 0.0000005, win = county_owin) # lambda 根据投影单位调整

# 模拟婴儿死亡点
# 假设死亡率在空间上不是完全随机的，我们从出生点中抽取一部分
d_indices <- sample(1:npoints(b_ppp), size = 80)
d_ppp <- b_ppp[d_indices]


b_point <- st_as_sf(b_ppp)
d_point <- st_as_sf(d_ppp)

b_coords <- st_coordinates(st_geometry(b_point |> st_cast("POINT")))
d_coords <- st_coordinates(st_geometry(d_point |> st_cast("POINT")))


library(tidycensus)
library(sf)
library(dplyr)
library(sp)

census_api_key(
  "ab79454a4d236d373b71ca6944897e9c83d38a0a",
  install = TRUE,
  overwrite = T
)

# 定义需要获取的 ACS 变量
# B15003_001: 25岁以上总人口, B15003_002~016: 低于高中学历
# B17001_002: 贫困线下人口, B17001_001: 总人口
# B25003_002: 业主自住, B25003_001: 总住房
# B07003_005: 过去1年内搬家, B07003_001: 总人口
# B19001: 家庭收入分布 (用于计算 ICE_INCOME)

variables <- c(
  total_pop_edu = "B15003_001",
  total_pop_pov = "B17001_001",
  pov_pop = "B17001_002",
  total_units = "B25003_001",
  owner_units = "B25003_002",
  total_pop_move = "B07003_001",
  moved_pop = "B07003_005"
)

atl_fulton <- get_acs(
  geography = "tract",
  variables = variables,
  state = "GA",
  county = "Fulton",
  year = 2022,
  geometry = TRUE,
  output = "wide"
)

atl_dekalb <- get_acs(
  geography = "tract",
  variables = variables,
  state = "GA",
  county = "DeKalb",
  year = 2022,
  geometry = TRUE,
  output = "wide"
)

# 合并数据
atl_raw <- rbind(atl_fulton, atl_dekalb)

# 3. 计算图片要求的五项统计指标
atl_final <- atl_raw %>%
  mutate(
    # 1. pctNOHS: 25岁以上无高中学历比例 (简化计算示例)
    pctNOHS = (1 -
      (get_acs(
        geography = "tract",
        variables = "B15003_017E",
        state = "GA",
        county = c("Fulton", "DeKalb"),
        year = 2022
      )$estimate /
        total_pop_eduE)) *
      100,

    # 2. pctPOV: 贫困线以下人口比例
    pctPOV = (pov_popE / total_pop_povE) * 100,

    # 3. ICE_INCOME_all: 极端收入集中指数 (模拟计算逻辑: (富裕-贫困)/总数)
    # 这里的简单演示使用 pctPOV 反向模拟集中度趋势
    ICE_INCOME_all = (1 - 2 * (pov_popE / total_pop_povE)),

    # 4. pctMOVE: 过去12个月搬家比例
    pctMOVE = (moved_popE / total_pop_moveE) * 100,

    # 5. pctOWNER_OCC: 业主自住比例
    pctOWNER_OCC = (owner_unitsE / total_unitsE) * 100
  ) %>%
  select(GEOID, pctNOHS, pctPOV, ICE_INCOME_all, pctMOVE, pctOWNER_OCC)

# 4. 剔除缺失值 (符合你提到的“剔除4个缺失区”)
# 现实中缺失数量取决于年份，这里通过 na.omit 确保数据完整
atl_sf <- atl_final %>% na.omit()

# 如果数量不正好是 345，可以根据需要进行 head() 调整用于演示
# atl_sf <- head(atl_sf, 345)

# 5. 转换为 Spatial 对象 (供 GWmodel 使用)
atl <- as(atl_sf, "Spatial")

# 检查结果
print(summary(atl))
plot(atl, main = "Fulton-Dekalb Census Tracts")

save.image("data/charpter7.RData")


##########################################
library(sf)
library(tigris)
library(dplyr)
library(tidyr)
options(tigris_use_cache = TRUE)

# 1. 获取佐治亚州县级地图数据 (CRS: 4269)
ga_counties <- counties(state = "GA", cb = TRUE) %>%
  dplyr::select(GEOID, NAME) %>%
  st_transform(4326)

# 2. 模拟各县人口 (根据佐治亚州真实情况模拟，范围从 2000 到 100万)
set.seed(123)
county_pop <- data.frame(
  GEOID = ga_counties$GEOID,
  POP = round(exp(runif(159, log(2000), log(1000000))))
)


# 模拟 STI 患病率 (每千人 5 到 20 人不等)
sti <- ga_counties %>%
  left_join(county_pop, by = "GEOID") %>%
  mutate(
    STD = rpois(n(), lambda = (POP / 1000) * runif(n(), 5, 20)),
    # 按照您提供的代码逻辑计算全局风险和期望值
    rate_global = sum(STD) / sum(POP),
    expected = rate_global * POP
  )

# 检查前几行
print(head(sti))

# 定义年份
years <- 2010:2018

sti_long <- ga_counties %>%
  # 为每个县创建 9 年的重复行
  slice(rep(1:n(), each = length(years))) %>%
  mutate(year = rep(years, times = 159)) %>%
  left_join(county_pop, by = "GEOID") %>%
  mutate(
    # 模拟人口随年份微小波动
    POP = round(POP * (1 + (year - 2010) * 0.01)),
    # 模拟 STI 计数：加入年份增长趋势
    STD = rpois(
      n(),
      lambda = (POP / 1000) * (runif(n(), 5, 15) + (year - 2010) * 0.5)
    )
  )

# 检查前几行
print(head(sti_long))

save.image("data/charpter9.RData")

load("data/charpter9.RData")


###########################################
library(tigris)
library(sf)
library(dplyr)
library(spdep)

# 1. 获取富尔顿县 (Fulton County, GA) 的真实普查区边界
# 这里包含了亚特兰大的核心区域
options(tigris_use_cache = TRUE)
Fulton_geoms <- tracts(state = "GA", county = "Fulton", cb = TRUE)
DeKalb_geoms <- tracts(state = "GA", county = "DeKalb", cb = TRUE)

# 2. 筛选并清洗数据

atl <- Fulton_geoms |>
  bind_rows(DeKalb_geoms) |>
  st_transform(4326) # 统一使用 WGS84 坐标系

# 3. 根据真实地理位置生成统计变量
set.seed(42)
n <- nrow(atl)

# 为了让空间分析更有意义，我们模拟一些具有空间趋势的数据
# 获取质心坐标用于生成空间梯度
coords <- st_coordinates(st_centroid(atl))
lon_scale <- as.numeric(scale(coords[, 1]))
lat_scale <- as.numeric(scale(coords[, 2]))

atl <- atl %>%
  mutate(
    PopulationCount = round(runif(n, 2500, 8000)),

    # 模拟心理健康和物理健康：设定一个从西南到东北的健康梯度
    # 并加入随机噪声，模拟真实调查中的波动
    MENTALHLTH = 12 + 2 * lat_scale + 1.5 * lon_scale + rnorm(n, 0, 1),
    PHYSHLTH = 10 + 2.5 * lat_scale + 1.2 * lon_scale + rnorm(n, 0, 1),

    # 标准化指标 (Z-scores)
    ParkProximity_std = as.numeric(scale(runif(n, 0, 100))),
    Poverty_std = as.numeric(scale(rlnorm(n, meanlog = 2.5, sdlog = 0.5))),

    # InstabilityStress: 模拟社会不稳定性压力
    InstabilityStress = as.numeric(scale(rnorm(n))) +
      as.numeric(scale(rnorm(n))),

    # 确保列名与教材描述一致
    geom = geometry
  ) %>%
  na.omit()

# 4. 验证数据
print(atl)
plot(atl["MENTALHLTH"], main = "Atlanta Simulated Mental Health Prevalence")

saveRDS(atl, 'data/atl.rds')
