################################################################################
## 1. 문고 관련 
################################################################################

# 문고 관련 데이터 읽기
book <- read.csv("data/book.csv", stringsAsFactors = FALSE)

##------------------------------------------------------------------------------
## 과학분야 베스트셀러 현황 테이블 출력
##------------------------------------------------------------------------------
knitr::kable(books %>% 
               select(`순위`=popularity_rank, `제목`=title, `저자`=author, 
                      `출판사`=publisher, `페이지`=pages, `출판년도`=release_date),
             table.attr = "class=\"table table-striped-green table-width\"",
             format = "html",
             caption = "과학분야 베스트셀러 현황")


################################################################################
## 2. 서울시 인구통계
################################################################################

# 인구통계

## 면적 및 인구 현황 

# 인구, 인구밀도, 면적 데이터 불러오기
stat_population <- read.csv("data/stat_population.csv", stringsAsFactors = FALSE)

# 행정구역 면적 Top 1, Bottom 1 정보 계산
area_top <- stat_population$CTY_NM[which.max(stat_population$area)]
area_bottom <- stat_population$CTY_NM[which.min(stat_population$area)]
multi_area <- round(stat_population$area[which.max(stat_population$area)] /
                      stat_population$area[which.min(stat_population$area)], 0)

# 인구밀도 Top 1, Bottom 1 정보 계산
density_top <- stat_population$CTY_NM[which.max(stat_population$density_pop)]
density_bottom <- stat_population$CTY_NM[which.min(stat_population$density_pop)]
multi_density <- round(stat_population$density_pop[which.max(stat_population$density_pop)] /
                         stat_population$density_pop[which.min(stat_population$density_pop)], 0)


##------------------------------------------------------------------------------
## 구별 면적 및 인구통계 현황 테이블 출력
##------------------------------------------------------------------------------
knitr::kable(
  stat_population %>% select(-CTY_CD),
  table.attr = "class=\"table table-striped-green\"",
  format = "html",
  col.names = c("구 이름", "인구(명)", "면적(km^2^)", "인구밀도(명/km^2^)"),
  format.args = list(big.mark = ",", digit = 2, scientific = FALSE),
  caption = "구별 면적 및 인구통계 현황")
  
##------------------------------------------------------------------------------
## 성별 인구 현황 계산
##------------------------------------------------------------------------------
# 성별/연령대별 인구 데이터 불러오기
population_seoul <- read.csv("data/population_seoul.csv", stringsAsFactors = FALSE)

# 성별 인구 비율 계산
population_gender <- population_seoul |> 
  select(CTY_NM, gender, total_pop) |> 
  pivot_wider(names_from = gender, values_from = total_pop) |> 
  mutate(pct_female = female / (female + male) * 100,
         pct_male = male / (female + male) * 100) |> 
  arrange(desc(pct_female))

# 성별 인구 비율 Top 1, Bottom 1 정보 계산
female_top <- population_gender$CTY_NM[which.max(population_gender$pct_female)]
female_top_value <- population_gender$pct_female[which.max(population_gender$pct_female)]

female_bottom <- population_gender$CTY_NM[which.min(population_gender$pct_female)]
female_bottom_value <- population_gender$pct_female[which.min(population_gender$pct_female)]


##------------------------------------------------------------------------------
## 성별 인구 현황 테이블 출력
##------------------------------------------------------------------------------
knitr::kable(
  population_gender,
  table.attr = "class=\"table table-striped-green\"",
  format = "html",
  digits = 2,
  col.names = c("구 이름", "여성 인구", "남성 인구", "여성 인구 비율(%)", "남성 인구 비율(%)"),
  format.args = list(big.mark = ",", scientific = FALSE),
  caption = "구별 성별 인구 현황")

##------------------------------------------------------------------------------
## 서울특별시 구 단위의 성별 인구 구성비 시각화
##------------------------------------------------------------------------------  
fig_gender <- population_gender |> 
  select(CTY_NM, female, male, pct_female) |> 
  mutate(CTY_NM = fct_reorder(CTY_NM, pct_female)) |>
  pivot_longer(cols = -c(CTY_NM, pct_female), names_to = "gender", values_to = "population") |>
  mutate(gender = factor(gender, levels = c("male", "female"), labels = c("남성", "여성"))) |> 
  ggplot(aes(x = CTY_NM, y = population, fill = gender)) +
  geom_col(position = "fill") +
  coord_flip() +
  dpxReport::dpx_theme(base_family = "NanumSquare") +
  labs(title = "성별 인구 비율", x = "지역", y = "인구 비율",
       fill = "성별") 

fig_gender


## 연령별 인구 현황 

### 연령별 인구 현황 

##------------------------------------------------------------------------------
## 연령별 인구 현황 계산
##------------------------------------------------------------------------------
# 연령별 인구현황 계산
population_age <- population_seoul |> 
  mutate(age_10 = age_0_9 + age_10_19) |>
  mutate(age_60 = age_60_69 + age_70_79 + age_80_89 + age_90_99 + age_100_over) |>
  select(CTY_NM, age_10, age_20_29, age_30_39, age_40_49, age_50_59, age_60) |> 
  group_by(CTY_NM) |>
  summarise_all(sum) |> 
  arrange(desc(age_60))

# 연령별 인구수 Top 1, Bottom 1 정보 계산
age_top <- population_age$CTY_NM[which.max(population_age$age_60)]
age_top_value <- population_age$age_60[which.max(population_age$age_60)]

age_bottom <- population_age$CTY_NM[which.min(population_age$age_60)]
age_bottom_value <- population_age$age_60[which.min(population_age$age_60)]


##------------------------------------------------------------------------------
## 연령별 인구 현황 계산
##------------------------------------------------------------------------------
knitr::kable(
  population_age,
  table.attr = "class=\"table table-striped-green\"",
  format = "html",
  digits = 2,
  col.names = c("구 이름", "10대이하", "20대", "30대", "40대", "50대", "60대이상"),
  format.args = list(big.mark = ",", scientific = FALSE),
  caption = "구별 연령별 인구 현황")

  
  
### 연령별 인구 구성비 현황
  
##------------------------------------------------------------------------------
## 연령별 인구 현황 계산
##------------------------------------------------------------------------------
# 연령별 인구 비율 계산
population_age_ratio <- data.frame(CTY_NM = population_age[, "CTY_NM"],
                                   population_age[, -1] / apply(population_age[, -1], 1, sum) * 100) |> 
  arrange(desc(age_60))

# 연령별 인구 비율 Top 1, Bottom 1 정보 계산
age_top_ratio <- population_age_ratio$CTY_NM[which.max(population_age_ratio$age_60)]
age_top_ratio_value <- population_age_ratio$age_60[which.max(population_age_ratio$age_60)]

age_bottom_ratio <- population_age_ratio$CTY_NM[which.min(population_age_ratio$age_60)]
age_bottom_ratio_value <- population_age_ratio$age_60[which.min(population_age_ratio$age_60)]


##------------------------------------------------------------------------------
## 구별 성별 인구 구성비 현황 테이블 출력
##------------------------------------------------------------------------------
knitr::kable(
  population_age_ratio,
  table.attr = "class=\"table table-striped-green\"",
  format = "html",
  digits = 2,
  col.names = c("구 이름", "10대이하(%)", "20대(%)", "30대(%)", "40대(%)", "50대(%)", "60대이상(%)"),
  format.args = list(big.mark = ",", scientific = FALSE),
  caption = "구별 성별 인구 구성비 현황")


##------------------------------------------------------------------------------
## 서울특별시 구 단위의 연령대별 인구 구성비 시각화
##------------------------------------------------------------------------------    
fig_age <- population_age_ratio |> 
  select(CTY_NM, age_10, age_20_29, age_30_39, age_40_49, age_50_59, age_60) |> 
  mutate(CTY_NM = fct_reorder(CTY_NM, age_60)) |>
  pivot_longer(cols = -CTY_NM, names_to = "age", values_to = "population") |> 
  mutate(age = factor(age, 
                      levels = c("age_60", "age_50_59", "age_40_49", "age_30_39", "age_20_29", "age_10"), 
                      labels = c("60대+", "50대", "40대", "30대", "20대", "10대-"))) |>   
  ggplot(aes(x = CTY_NM, y = population, fill = age)) +
  geom_col(position = "fill") +
  coord_flip() +
  guides(fill = guide_legend(ncol = 6)) +
  dpxReport::dpx_theme(base_family = "NanumSquare") +
  labs(title = "연령대별 인구 비율", x = "지역", y = "인구 비율",
       fill = "연령대") 

fig_age


##------------------------------------------------------------------------------
## 인구관련 뉴스 분석 워드클라우드 시각화
##------------------------------------------------------------------------------   
library(webshot)
library(htmlwidgets)
library(showtext)

showtext_auto()

noun_population <- readr::read_csv("data/noun_population.csv")

min_freq <- 3
remove_n <- 3

noun_seoul <- noun_population |>
  filter(mega %in% "seoul") |>
  filter(n >= min_freq) |>
  filter(row_number() > remove_n) |>
  select(-mega) |>
  wordcloud2::wordcloud2(fontFamily = "NamumSquare")

saveWidget(noun_seoul, "tmp.html", selfcontained = FALSE)
webshot("tmp.html", "seoul.png", delay = 5, vwidth = 1200, vheight = 900)
