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
