library(sf)
library(tidyverse)
library(tidycensus)
library(censusapi)
library(tidyUSDA)
library(flextable)
library(terra)
library(tigris)
library(tmap)

census_api_key("ab79454a4d236d373b71ca6944897e9c83d38a0a", install = TRUE)

all_vars <- load_variables(year = 2018, dataset = 'acs5', cache = T)
head(all_vars)

a <- all_vars %>%
  filter(stringr::str_detect(name, 'B27001')) %>% # this limits to rows for the B27001 table
  filter(stringr::str_detect(label, 'No health insurance')) # this limits to rows with this text

myVars <- c('B27001_001', a$name)


# First, request the data from ACS
insure_tidy <- get_acs(
  geography = 'county',
  variables = myVars,
  year = 2018,
  survey = 'acs5'
) %>%
  select(-moe)

# Look at the resulting object
head(insure_tidy)


# Now I pull out the denominator
insure_denom <- insure_tidy %>%
  filter(variable == 'B27001_001') %>%
  rename(TOTPOP = estimate) %>%
  select(-variable)

# Look at the resulting object
head(insure_denom)

# Now I sum up all the variables for the numerator
insure_num <- insure_tidy %>%
  filter(variable != 'B27001_001') %>%
  group_by(GEOID) %>%
  summarise(no_insure = sum(estimate))

head(insure_num)

# Finally, merge the numerator and denominator in order to calculate prevalence
uninsured <- insure_denom %>%
  left_join(insure_num, by = 'GEOID') %>%
  mutate(uninsured = no_insure / TOTPOP) %>%
  select(GEOID, uninsured)

# Take a look at the resulting object
head(uninsured)

insure_tidy2 <- get_acs(
  geography = 'county',
  variables = myVars,
  year = 2018,
  geometry = TRUE, # added geometry = T
  survey = 'acs5'
)


options(tigris_use_cache = TRUE)
us <- counties(cb = TRUE, resolution = '5m', year = 2018, class = 'sf')
summary(us)
plot(st_geometry(us))

us <- us %>%
  filter(!(STATEFP %in% c('02', '15', '66', '60', '78', '72', '69'))) %>%
  select(GEOID, STATEFP, COUNTYFP, NAME) %>%
  st_transform(5070)

plot(st_geometry(us))


#############
# 生成包含美国所有县的death数据集
set.seed(123)
# 美国各州的FIPS代码和州名
states_info <- data.frame(
  state_fips = sprintf("%02d", 1:56),
  state_name = c(
    "AL",
    "AK",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",
    "DE",
    "FL",
    "GA",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY",
    "DC",
    "AS",
    "GU",
    "MP",
    "PR",
    "VI"
  ),
  # 每个州大概的县数量
  county_count = c(
    67,
    29,
    15,
    75,
    58,
    64,
    8,
    3,
    67,
    159,
    5,
    44,
    102,
    92,
    99,
    105,
    120,
    64,
    16,
    24,
    14,
    83,
    87,
    82,
    115,
    56,
    93,
    17,
    10,
    21,
    33,
    62,
    100,
    53,
    88,
    77,
    36,
    67,
    5,
    46,
    66,
    95,
    254,
    29,
    14,
    134,
    39,
    55,
    72,
    23,
    1,
    5,
    1,
    4,
    78,
    3
  )
)
# 生成所有县的数据
all_counties <- data.frame()
for (i in 1:nrow(states_info)) {
  state_fips <- states_info$state_fips[i]
  state_name <- states_info$state_name[i]
  n_counties <- states_info$county_count[i]

  # 生成该州的县数据
  county_nums <- seq(1, n_counties * 2, 2) # 奇数县代码

  # 生成县名
  county_names <- paste0(
    sample(
      c(
        "Washington",
        "Jefferson",
        "Franklin",
        "Jackson",
        "Lincoln",
        "Madison",
        "Clay",
        "Monroe",
        "Union",
        "Greene",
        "Wayne",
        "Hamilton",
        "Marion",
        "Adams",
        "Wilson",
        "Johnson",
        "Smith",
        "Brown",
        "Jones",
        "Miller",
        "Davis",
        "Garcia",
        "Rodriguez",
        "Wilson",
        "Martinez",
        "Anderson",
        "Taylor",
        "Thomas",
        "Hernandez",
        "Moore",
        "Martin",
        "Jackson",
        "Thompson",
        "White",
        "Lopez",
        "Lee",
        "Gonzalez",
        "Harris",
        "Clark",
        "Lewis",
        "Robinson",
        "Walker",
        "Perez",
        "Hall",
        "Young",
        "Allen",
        "Sanchez",
        "Wright",
        "King",
        "Scott",
        "Green",
        "Baker",
        "Adams",
        "Nelson",
        "Hill",
        "Ramirez",
        "Campbell",
        "Mitchell",
        "Roberts",
        "Carter",
        "Phillips",
        "Evans",
        "Turner",
        "Torres",
        "Parker",
        "Collins",
        "Edwards",
        "Stewart",
        "Flores",
        "Morris",
        "Nguyen",
        "Murphy",
        "Rivera",
        "Cook",
        "Rogers",
        "Morgan",
        "Peterson",
        "Cooper",
        "Reed",
        "Bailey",
        "Bell",
        "Gomez",
        "Kelly",
        "Howard",
        "Ward",
        "Cox",
        "Diaz",
        "Richardson",
        "Wood",
        "Watson",
        "Brooks",
        "Bennett",
        "Gray",
        "James",
        "Reyes",
        "Cruz",
        "Hughes",
        "Price",
        "Myers",
        "Long",
        "Foster",
        "Sanders",
        "Ross",
        "Morales",
        "Powell",
        "Sullivan",
        "Russell",
        "Ortiz",
        "Jenkins",
        "Gutierrez",
        "Perry",
        "Butler",
        "Barnes",
        "Fisher",
        "Henderson",
        "Coleman",
        "Simmons",
        "Patterson",
        "Jordan",
        "Reynolds",
        "Hamilton",
        "Graham",
        "Kim",
        "Gonzales",
        "Alexander",
        "Ramos",
        "Wallace",
        "Griffin",
        "West",
        "Cole",
        "Hayes",
        "Chavez",
        "Gibson",
        "Bryant",
        "Ellis",
        "Stevens",
        "Murray",
        "Ford",
        "Marshall",
        "Owens",
        "McDonald",
        "Harrison",
        "Ruiz",
        "Kennedy",
        "Wells",
        "Alvarez",
        "Woods",
        "Mendoza",
        "Castillo",
        "Olson",
        "Webb",
        "Washington",
        "Tucker",
        "Freeman",
        "Burns",
        "Henry",
        "Vasquez",
        "Snyder",
        "Simpson",
        "Crawford",
        "Jimenez",
        "Porter",
        "Mason",
        "Shaw",
        "Gordon",
        "Wagner",
        "Hunter",
        "Romero",
        "Hicks",
        "Dixon",
        "Hunt",
        "Palmer",
        "Robertson",
        "Black",
        "Holmes",
        "Stone",
        "Meyer",
        "Boyd",
        "Mills",
        "Warren",
        "Fox",
        "Rose",
        "Rice",
        "Moreno",
        "Schmidt",
        "Patel",
        "Ferguson",
        "Nichols",
        "Herrera",
        "Medina",
        "Ryan",
        "Fernandez",
        "Weaver",
        "Daniels",
        "Stephens",
        "Gardner",
        "Payne",
        "Kelley",
        "Dunn",
        "Pierce",
        "Arnold",
        "Tran",
        "Spencer",
        "Peters",
        "Hawkins",
        "Grant",
        "Hansen",
        "Castro",
        "Hoffman",
        "Hart",
        "Elliott",
        "Cunningham",
        "Knight",
        "Bradley"
      ),
      n_counties,
      replace = TRUE
    ),
    " County, ",
    state_name
  )

  # 生成人口数据（根据州的大小调整）
  if (state_name %in% c("CA", "TX", "FL", "NY", "PA")) {
    # 大州，人口范围更大
    population <- round(rlnorm(n_counties, meanlog = log(50000), sdlog = 1))
  } else if (state_name %in% c("WY", "VT", "AK", "ND", "SD")) {
    # 小州，人口较少
    population <- round(rlnorm(n_counties, meanlog = log(15000), sdlog = 0.8))
  } else {
    # 中等州
    population <- round(rlnorm(n_counties, meanlog = log(30000), sdlog = 0.9))
  }

  # 确保人口数据合理
  population <- pmax(population, 1000) # 最小1000人
  population <- pmin(population, 2000000) # 最大200万人

  # 生成死亡数据（死亡率在6-18‰之间，考虑人口老龄化等因素）
  death_rate <- runif(n_counties, 0.006, 0.018)
  deaths <- round(population * death_rate)

  # 计算粗死亡率（每10万人）
  crude_rate <- round((deaths / population) * 100000, 4)

  # 创建该州的数据框
  state_data <- data.frame(
    FIPS = paste0(state_fips, sprintf("%03d", county_nums[1:n_counties])),
    County = county_names,
    Deaths = deaths,
    Population = population,
    crude = crude_rate
  )

  all_counties <- rbind(all_counties, state_data)
}
# 将生成的数据赋值给death
death <- all_counties
# 确保前几行匹配您的示例数据
death[1:6, ] <- data.frame(
  FIPS = c("01001", "01003", "01005", "01007", "01009", "01011"),
  County = c(
    "Autauga County, AL",
    "Baldwin County, AL",
    "Barbour County, AL",
    "Bibb County, AL",
    "Blount County, AL",
    "Bullock County, AL"
  ),
  Deaths = c(536, 2357, 312, 276, 689, 112),
  Population = c(55601, 218022, 24881, 22400, 57840, 10138),
  crude = c(964.0114, 1081.0836, 1253.9689, 1232.1429, 1191.2172, 1104.7544)
)
############

us2 <- us %>%
  left_join(death, by = c('GEOID' = 'FIPS')) %>%
  left_join(uninsured, by = 'GEOID')

t1 <- tm_shape(us2) +
  tm_fill(
    'crude',
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.bu_pu"
    ),
    fill.legend = tm_legend(title = 'Rate per 100,000 py')
  ) +
  tm_borders(fill_alpha = 0.2) +
  tm_credits('Source: CDC Wonder', position = c('RIGHT', 'BOTTOM')) +
  tm_title('All-cause mortality rate, 2016-2018') +
  tm_layout(bg.color = 'grey85')
t2 <- tm_shape(us2) +
  tm_fill(
    'uninsured',
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "brewer.yl_or_rd"
    ),
    fill.legend = tm_legend(title = '% Uninsured', format = function(x) {
      paste0(formatC(x * 100, digits = 1, format = "f"), "%")
    })
  ) +
  tm_borders(fill_alpha = 0.2) +
  tm_credits(
    'Source: American Community Survey',
    position = c('RIGHT', 'BOTTOM')
  ) +
  tm_title('Uninsured Prevalence, 2014-2018') +
  tm_layout(bg.color = 'grey85')
# 排列两个地图
tmap_arrange(t1, t2, ncol = 1)

save.image("data/charpter2.RData")
load("data/charpter2.RData")
