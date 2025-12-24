#List of required packages
packages <- c("tidyverse", "readr", "stringr", "janitor",
              "tidytext", "SnowballC", "wordcloud", "topicmodels")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#Read CSV File
csv_path <- "/Users/abulbasharsaurov/Desktop/Bangladesh.csv"

TEXT_COL <- "content"

raw <- read_csv(csv_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# If the specified TEXT_COL is not present, try to guess a plausible text column
if (!TEXT_COL %in% names(raw)) {
  candidates <- names(raw)[map_lgl(raw, ~ is.character(.x) || is.factor(.x))]
  TEXT_COL <- candidates[which.max(sapply(raw[candidates],
      function(x) mean(nchar(as.character(x)), na.rm = TRUE)))]
  message("Guessed text column: ", TEXT_COL)
}

df <- raw %>%
  mutate(doc_id = row_number(),
         text   = as.character(.data[[TEXT_COL]])) %>%
  select(doc_id, everything())

# Normalize encoding (helps for non-Latin scripts)
df$text <- iconv(df$text, from = "", to = "UTF-8")

# Basic checks
cat("Rows:", nrow(df), "\n")

cat("NAs in text:", sum(is.na(df$text)), "\n")

# Drop NA/blank and exact duplicate texts
df <- df %>%
  filter(!is.na(text)) %>%
  mutate(text = str_squish(text)) %>%
  filter(text != "") %>%
  distinct(text, .keep_all = TRUE)

# Show a sample
df %>% select(doc_id, !!TEXT_COL := text) %>% head(5)

#Clean Text
#Define Cleaning Function
clean_text <- function(x) {
  x %>%
    str_replace_all("https?://\\S+|www\\.[^\\s]+", " ") %>%  # Remove URLs
    str_replace_all("@\\w+|#\\w+", " ") %>%                 # Remove mentions/hashtags
    str_replace_all("[^\\p{L}\\p{N}\\s']", " ") %>%         # Keep letters, numbers, space
    str_to_lower() %>%                                      # Convert to lowercase
    str_squish()                                            # Remove extra spaces
}

#Apply Cleaning
df <- df %>% 
  mutate(text_clean = clean_text(text))

#Tokenize and Remove Stop Words
data(stop_words)

tokens <- df %>%
  unnest_tokens(word, text_clean, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) >= 3)  # Remove very short words (1-2 letters)

# Preview
head(tokens)

#Stemming or Lemmatization (Optional)
tokens <- tokens %>%
  mutate(stem = SnowballC::wordStem(word, language = "en"))

tokens %>% select(doc_id, word, stem) %>% head(10)

#Word Frequency
# Count frequency of full words
word_freq <- tokens %>%
  count(word, sort = TRUE)

# Preview top 20 words
head(word_freq, 20)

#Word Frequency
# Select top 20 words
top_n <- 20

p <- word_freq %>%
  slice_max(n, n = top_n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +   # Changed 'stem' to 'word'
  geom_col(fill = "steelblue") +               # Added color for better visibility
  coord_flip() +
  labs(title = paste("Top", top_n, "words (by frequency)"),
       x = "Word", y = "Count") +
  theme_minimal()

print(p)

# Save image
ggplot2::ggsave("top_words_bar.png", p, width = 7, height = 5, dpi = 300)

#Word Frequency
#Word cloud
# Prepare data for Word Cloud
wc_df <- word_freq %>% filter(n > 1)

# Save as PNG
png("wordcloud.png", width = 1200, height = 900, res = 150)
par(mar = c(1,1,1,1))

wordcloud(words = wc_df$word,          # Changed 'stem' to 'word'
          freq = wc_df$n, 
          max.words = 200, 
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2")) # Added colors for better visuals

dev.off()

# Display in RStudio plot viewer
wordcloud(words = wc_df$word, 
          freq = wc_df$n, 
          max.words = 200, 
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# Calculate TF-IDF
tfidf <- tokens %>%
  count(doc_id, word, sort = FALSE) %>%      # Changed 'stem' to 'word'
  bind_tf_idf(term = word, document = doc_id, n = n) %>%
  arrange(desc(tf_idf))

# Create Document-Term Matrix (DTM)
dtm_tfidf <- tfidf %>%
  cast_dtm(document = doc_id, term = word, value = tf_idf)

# Display DTM details
print(dtm_tfidf)

#Raw counts
dtm_counts <- tokens %>% count(doc_id, stem) %>% cast_dtm(doc_id, stem, n)

# Run LDA Topic Model
k <- 3
lda_model <- LDA(dtm_tfidf, k = k, control = list(seed = 1234))

# Extract Top Terms per Topic
topic_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(topic_terms)

#Save Outputs
write_csv(df,        "cleaned_text.csv")
write_csv(word_freq, "top_words.csv")
write_csv(tfidf,     "tfidf_by_doc.csv")

cat("All files saved successfully.\n")