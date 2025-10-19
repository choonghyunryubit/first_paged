library(tidyverse)
library(tidytext)

client_id <- "t9iL0Yqma3NV8lTJow87"
client_secret <- "QQ3gvfN8CE"

seoul_list <- search_naver(
  "서울시 인구", client_id = client_id, client_secret = client_secret, 
  max_record = 1000, sort = "sim", do_done = TRUE
)

kangwon_list <- search_naver(
  "강원도 인구", client_id = client_id, client_secret = client_secret, 
  max_record = 1000, sort = "sim", do_done = TRUE
)

min_freq <- 5
remove_n <- 5
background <- "#FFFFFF"

seoul_list |>
  filter(nchar(description_text) > 0) |>
  unnest_tokens(noun, description_text, bitNLP::morpho_mecab, type = "noun") |>
  group_by(noun) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  filter(n >= min_freq) |>
  filter(row_number() > remove_n) |>
  wordcloud2::wordcloud2(backgroundColor = background,
                         fontFamily = "NamumSquare")

kangwon_list |>
  filter(nchar(description_text) > 0) |>
  unnest_tokens(noun, description_text, bitNLP::morpho_mecab, type = "noun") |>
  group_by(noun) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  filter(n >= min_freq) |>
  filter(row_number() > remove_n) |>
  wordcloud2::wordcloud2(backgroundColor = background,
                         fontFamily = "NamumSquare")

none_population <- rbind(data.frame(mega = "seoul", seoul_list |>
                                      filter(nchar(description_text) > 0) |>
                                      unnest_tokens(noun, description_text, bitNLP::morpho_mecab, type = "noun") |>
                                      group_by(noun) |>
                                      count() |>
                                      arrange(desc(n)) |>
                                      ungroup()),
                         data.frame(mega = "kangwon", kangwon_list |>
                                      filter(nchar(description_text) > 0) |>
                                      unnest_tokens(noun, description_text, bitNLP::morpho_mecab, type = "noun") |>
                                      group_by(noun) |>
                                      count() |>
                                      arrange(desc(n)) |>
                                      ungroup()))

write.csv(none_population, "none_population.csv", row.names = FALSE)


none_population |>
  filter(mega %in% "seoul") |>
  filter(n < max(n)) |> 
  filter(n >= min_freq) |>
  filter(row_number() > remove_n) |>
  select(-mega) |>
  wordcloud2::wordcloud2(backgroundColor = background,
                         fontFamily = "NamumSquare")

