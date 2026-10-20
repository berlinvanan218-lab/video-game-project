library(dplyr)

# Content-based features: genre, platform, publisher, popularity, era
build_features <- function(df) {
  top_pub <- df %>% count(Publisher, sort = TRUE) %>% slice_head(n = 30) %>% pull(Publisher)
  d <- df %>% mutate(Pub2 = ifelse(Publisher %in% top_pub, Publisher, "Other"))

  m <- cbind(
    model.matrix(~ Genre - 1, d) * 3,                       # genre matters most
    model.matrix(~ Platform - 1, d) * 1.5,
    model.matrix(~ Pub2 - 1, d) * 1,
    as.numeric(scale(log1p(d$Global_Sales))) * 0.5,         # popularity
    as.numeric(scale(d$Year)) * 0.3                         # era
  )
  m / sqrt(rowSums(m^2))                                    # unit rows -> dot product = cosine
}

# ids = optional vector of .id values (the sidebar filters) to restrict candidates
recommend_games <- function(df, feat, game, n = 10, ids = NULL) {
  idx <- which(tolower(df$Name) == tolower(game))
  if (length(idx) == 0) return(NULL)
  i <- idx[which.max(df$Global_Sales[idx])]

  out <- df %>% mutate(Similarity = round(as.numeric(feat %*% feat[i, ]), 3))
  out <- out[tolower(out$Name) != tolower(game), ]
  if (!is.null(ids)) out <- out[out$.id %in% ids, ]

  out %>%
    arrange(desc(Similarity), desc(Global_Sales)) %>%
    distinct(Name, .keep_all = TRUE) %>%
    slice_head(n = n) %>%
    select(Rank, Name, Platform, Genre, Publisher, Year, Global_Sales, Similarity)
}
