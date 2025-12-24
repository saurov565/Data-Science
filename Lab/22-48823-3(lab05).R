#List of required packages
packages <- c("rvest", "stringr")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Define the URL of the page to scrape
url <- "https://www.thedailystar.net/sports"

# Read the HTML from the URL
webpage <- read_html(url)

# 3. Extract Headlines
# The Daily Star usually uses <h3> tags with the class "title" for article headers
headlines <- webpage %>%
  html_elements("h3.title") %>% 
  html_text2()

# 4. Extract Links
# We grab the 'href' attribute from the <a> tag inside the <h3>
links <- webpage %>%
  html_elements("h3.title > a") %>%
  html_attr("href")

# Ensure links are full URLs (The Daily Star often uses relative paths like /sports/cricket...)
full_links <- paste0("https://www.thedailystar.net", links)

# 5. Handle uneven lengths (Safety Step)
# Sometimes scraping finds more titles than links or vice versa. We trim to the smallest count.
min_len <- min(length(headlines), length(full_links))

# 6. Create the Data Frame
# "Year" and "Rating" are removed because they don't exist on a news homepage.
news_df <- data.frame(
  Rank = 1:min_len,
  Headline = headlines[1:min_len],
  Link = full_links[1:min_len]
)

# 7. Display the result
head(news_df)