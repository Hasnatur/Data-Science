install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")
install.packages("qdapRegex")
install.packages("hunspell")

library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(qdapRegex)
library(hunspell)

url <- "https://www.natgeotv.com/me/page/oceans-and-ice-are-absorbing-the-brunt-of-climate-change"
webpage <- read_html(url)

text_data <- webpage %>%
  html_nodes("body") %>%
  html_text()

clean_text <- text_data %>%
  rm_between("<", ">", extract = FALSE) %>%
  gsub("&[^;]+;", " ", .) %>%
  str_replace_all("[^[:alnum:][:space:]]", " ") %>%
  str_squish() %>%
  tolower()

tokens <- unlist(strsplit(clean_text, "\\s+"))

stop_words <- stopwords("en")
filtered_tokens <- tokens[!tokens %in% stop_words]

stemmed_tokens <- wordStem(filtered_tokens, language = "en")

contractions <- list(
  "don't" = "do not", "isn't" = "is not",
  "you're" = "you are", "can't" = "cannot"
)
expanded_tokens <- sapply(stemmed_tokens, function(word) {
  if (word %in% names(contractions)) contractions[[word]] else word
})

emojiless_tokens <- str_replace_all(expanded_tokens, "[\U0001F600-\U0001F64F]|[\U0001F300-\U0001F5FF]|[\U0001F680-\U0001F6FF]", "")

corrected_tokens <- sapply(emojiless_tokens, function(word) {
  suggestions <- hunspell_suggest(word)
  if (length(suggestions[[1]]) > 0) suggestions[[1]][1] else word
})

final_text <- paste(corrected_tokens, collapse = " ")

processed_data <- data.frame(
  Original_Text = text_data,
  Cleaned_Text = final_text
)

write.csv(processed_data, "processed_text_data.csv", row.names = FALSE)

cat("Text preprocessing completed and saved to 'processed_text_data.csv'")
