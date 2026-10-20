library(plumber)
library(dplyr)
library(readr)
library(tidyr)

# =====================================================================
# 1. LOAD, CLEAN, ENRICH
# =====================================================================
df <- read_csv("data/vgsales.csv", na = c("N/A", ""), show_col_types = FALSE) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Year), Year <= 2016, !is.na(Publisher)) %>%
  distinct() %>%
  mutate(
    .id      = row_number(),
    Decade   = paste0(floor(Year / 10) * 10, "s"),
    RankBand = cut(Rank, c(0, 10, 50, 100, 500, 1000, 5000, Inf),
                   labels = c("Top 10", "11-50", "51-100", "101-500",
                              "501-1000", "1001-5000", "5000+"))
  )

source("recommender.R")
feat <- build_features(df)

# ---- k-means segments (computed once on all games) ----
set.seed(42)
X  <- scale(log1p(as.matrix(df[c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")])))
km <- kmeans(X, centers = 4, nstart = 10)
cm <- df %>% mutate(cl = km$cluster) %>% group_by(cl) %>%
  summarise(g = mean(Global_Sales), na = sum(NA_Sales), eu = sum(EU_Sales),
            jp = sum(JP_Sales), ot = sum(Other_Sales), .groups = "drop") %>% arrange(cl)
sh   <- as.matrix(cm[c("na", "eu", "jp", "ot")]); sh <- sh / rowSums(sh)
lead <- c("North America-led", "Europe-led", "Japan-led", "Rest-of-world-led")[max.col(sh)]
tier <- c("niche", "modest", "strong", "blockbuster")[rank(cm$g, ties.method = "first")]
seg_labels <- paste(lead, tier)
df$Segment <- seg_labels[km$cluster]

# ---- regression model (predict sales) ----
top_pub <- df %>% count(Publisher, sort = TRUE) %>% slice_head(n = 30) %>% pull(Publisher)
df$Pub2 <- ifelse(df$Publisher %in% top_pub, df$Publisher, "Other")
model   <- lm(log1p(Global_Sales) ~ Genre + Platform + Year + Pub2, data = df)
model_s <- summary(model)

plat_median <- tapply(df$Global_Sales, df$Platform, median)

# =====================================================================
# 2. FILTER HELPERS  (every column can be a filter)
# =====================================================================
arg <- function(a, k) { v <- a[[k]]; if (is.null(v) || length(v) == 0) "" else as.character(v[1]) }
vals <- function(x) if (x == "") character(0) else strsplit(x, "|", fixed = TRUE)[[1]]
num  <- function(x, default) if (x == "") default else suppressWarnings(as.numeric(x))

flt <- function(req) {
  a <- req$argsQuery
  d <- df
  g <- vals(arg(a, "genre"));     if (length(g)) d <- d[d$Genre %in% g, ]
  p <- vals(arg(a, "platform"));  if (length(p)) d <- d[d$Platform %in% p, ]
  u <- vals(arg(a, "publisher")); if (length(u)) d <- d[d$Publisher %in% u, ]
  d <- d[d$Year >= num(arg(a, "year_from"), -Inf) & d$Year <= num(arg(a, "year_to"), Inf), ]
  d <- d[d$Rank >= num(arg(a, "rank_from"), -Inf) & d$Rank <= num(arg(a, "rank_to"), Inf), ]
  d <- d[d$Global_Sales >= num(arg(a, "min_sales"), -Inf) &
         d$Global_Sales <= num(arg(a, "max_sales"), Inf), ]
  q <- arg(a, "q")
  if (q != "") d <- d[grepl(tolower(q), tolower(d$Name), fixed = TRUE), ]
  d
}

# one aggregator used by every "group by" view
agg <- function(d, col) {
  d %>% group_by(.data[[col]]) %>%
    summarise(
      Global = round(sum(Global_Sales), 1),
      NA_Sales = round(sum(NA_Sales), 1), EU_Sales = round(sum(EU_Sales), 1),
      JP_Sales = round(sum(JP_Sales), 1), Other_Sales = round(sum(Other_Sales), 1),
      Games = n(), Avg = round(mean(Global_Sales), 2),
      Best = Name[which.max(Global_Sales)], .groups = "drop"
    ) %>%
    rename(Group = 1) %>%
    mutate(Share = round(100 * Global / sum(Global), 1))
}

GAME_COLS <- c("Rank", "Name", "Platform", "Year", "Genre", "Publisher",
               "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")

# =====================================================================
# 3. CORS
# =====================================================================
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Health check
#* @get /health
function() list(status = "ok")

# =====================================================================
# 4. META (values for the filter panel)
# =====================================================================
#* @get /meta
function() {
  list(
    genres     = sort(unique(df$Genre)),
    platforms  = sort(unique(df$Platform)),
    publishers = df %>% count(Publisher, sort = TRUE) %>% pull(Publisher),
    year_min = min(df$Year), year_max = max(df$Year),
    rank_min = min(df$Rank), rank_max = max(df$Rank),
    sales_max = max(df$Global_Sales)
  )
}

# =====================================================================
# 5. DESCRIPTIVE ANALYTICS (all respect the filters)
# =====================================================================
#* KPIs
#* @get /summary
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list(games = 0))
  yr <- d %>% group_by(Year) %>% summarise(s = sum(Global_Sales), .groups = "drop")
  list(
    games = nrow(d), total = round(sum(d$Global_Sales), 1),
    avg = round(mean(d$Global_Sales), 2), median = round(median(d$Global_Sales), 2),
    blockbusters = sum(d$Global_Sales >= 1),
    best = d$Name[which.max(d$Global_Sales)], best_sales = max(d$Global_Sales),
    peak_year = yr$Year[which.max(yr$s)],
    platforms = n_distinct(d$Platform), genres = n_distinct(d$Genre),
    publishers = n_distinct(d$Publisher)
  )
}

#* Group-by view: dim = Genre | Platform | Publisher | Year | Decade | RankBand
#* @get /by
function(req, dim = "Genre", top = 15, res) {
  if (!dim %in% c("Genre", "Platform", "Publisher", "Year", "Decade", "RankBand")) {
    res$status <- 400; return(list(error = "Unknown dim"))
  }
  d <- flt(req)
  if (!nrow(d)) return(list())
  out <- agg(d, dim)
  if (dim %in% c("Genre", "Platform", "Publisher"))
    out <- out %>% arrange(desc(Global)) %>% slice_head(n = as.integer(top))
  if (dim == "Year")
    out <- out %>% mutate(YoY = round(100 * (Global / lag(Global) - 1), 1))
  out
}

#* Regional totals
#* @get /regions
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list())
  data.frame(
    Region = c("North America", "Europe", "Japan", "Other regions"),
    Key = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"),
    Sales = round(c(sum(d$NA_Sales), sum(d$EU_Sales), sum(d$JP_Sales), sum(d$Other_Sales)), 1)
  ) %>% mutate(Share = round(100 * Sales / sum(Sales), 1))
}

#* Yearly trend per Genre / Platform / Publisher (top N series)
#* @get /trend
function(req, by = "Genre", top = 8, res) {
  if (!by %in% c("Genre", "Platform", "Publisher")) { res$status <- 400; return(list(error = "bad by")) }
  d <- flt(req)
  if (!nrow(d)) return(list())
  keep <- d %>% group_by(.data[[by]]) %>% summarise(s = sum(Global_Sales), .groups = "drop") %>%
    arrange(desc(s)) %>% slice_head(n = as.integer(top)) %>% pull(1)
  d %>% mutate(Series = .data[[by]]) %>% filter(Series %in% keep) %>%
    group_by(Year, Series) %>% summarise(Global = round(sum(Global_Sales), 2), .groups = "drop")
}

#* Genre x Platform heatmap (top 12 platforms)
#* @get /matrix
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list())
  keep <- d %>% count(Platform, wt = Global_Sales, sort = TRUE) %>% slice_head(n = 12) %>% pull(Platform)
  d %>% filter(Platform %in% keep) %>% group_by(Genre, Platform) %>%
    summarise(Global = round(sum(Global_Sales), 1), .groups = "drop")
}

#* Platform lifecycle: first, peak, last year
#* @get /lifecycle
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list())
  d %>% group_by(Platform, Year) %>% summarise(s = sum(Global_Sales), .groups = "drop") %>%
    group_by(Platform) %>%
    summarise(First = min(Year), Peak = Year[which.max(s)], Last = max(Year),
              Lifespan = max(Year) - min(Year) + 1, Total = round(sum(s), 1), .groups = "drop") %>%
    arrange(desc(Total)) %>% slice_head(n = 20)
}

# =====================================================================
# 6. GAME LISTS
# =====================================================================
#* Top games, sortable by any sales column
#* @get /top-games
function(req, n = 10, sort = "Global_Sales") {
  if (!sort %in% c("Global_Sales", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) sort <- "Global_Sales"
  flt(req) %>% arrange(desc(.data[[sort]])) %>% slice_head(n = as.integer(n)) %>% select(all_of(GAME_COLS))
}

#* Paginated, sortable game table
#* @get /games
function(req, page = 1, size = 15, sort = "Rank", dir = "asc") {
  d <- flt(req)
  if (!nrow(d)) return(list(total = 0, rows = list()))
  if (!sort %in% GAME_COLS) sort <- "Rank"
  d <- d[order(d[[sort]], decreasing = (dir == "desc")), ]
  p <- as.integer(page); s <- min(as.integer(size), 100)
  i <- seq_len(nrow(d))
  list(total = nrow(d), rows = d[i > (p - 1) * s & i <= p * s, GAME_COLS])
}

#* Sleeper hits: sold far above platform median but outside the top 300
#* @get /hidden-gems
function(req, n = 10) {
  flt(req) %>%
    mutate(Ratio = round(Global_Sales / as.numeric(plat_median[Platform]), 1)) %>%
    filter(Rank > 300) %>% arrange(desc(Ratio)) %>% slice_head(n = as.integer(n)) %>%
    select(Rank, Name, Platform, Genre, Year, Global_Sales, Ratio)
}

#* Autocomplete
#* @get /search
function(q = "") {
  if (nchar(q) < 2) return(list())
  hits <- df %>% filter(grepl(tolower(q), tolower(Name), fixed = TRUE)) %>%
    group_by(Name) %>% summarise(s = sum(Global_Sales), .groups = "drop") %>%
    arrange(desc(s)) %>% slice_head(n = 10)
  data.frame(Name = hits$Name)
}

# =====================================================================
# 7. STATISTICAL / ADVANCED ANALYTICS
# =====================================================================
#* Sales distribution (histogram bins)
#* @get /distribution
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list())
  br <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, 10, Inf)
  lb <- c("<0.1M", "0.1-0.25M", "0.25-0.5M", "0.5-1M", "1-2M", "2-5M", "5-10M", "10M+")
  d %>% mutate(Bin = cut(Global_Sales, br, labels = lb, include.lowest = TRUE)) %>%
    count(Bin, .drop = FALSE) %>% rename(Games = n)
}

#* Correlation matrix (long format)
#* @get /correlation
function(req) {
  d <- flt(req)
  if (nrow(d) < 3) return(list())
  cols <- c("Rank", "Year", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  m <- suppressWarnings(cor(as.data.frame(d)[cols])); m[is.na(m)] <- 0
  as.data.frame(as.table(m)) %>% rename(r = Freq) %>% mutate(r = round(r, 2))
}

#* k-means market segments
#* @get /clusters
function(req) {
  d <- flt(req)
  if (!nrow(d)) return(list())
  list(
    profile = d %>% group_by(Segment) %>%
      summarise(Games = n(), Total = round(sum(Global_Sales), 1), Avg = round(mean(Global_Sales), 2),
                NA_Sales = round(mean(NA_Sales), 2), EU_Sales = round(mean(EU_Sales), 2),
                JP_Sales = round(mean(JP_Sales), 2), Other_Sales = round(mean(Other_Sales), 2),
                .groups = "drop") %>% arrange(desc(Total)),
    points = d %>% slice_sample(n = min(500, nrow(d))) %>% select(Name, Segment, NA_Sales, EU_Sales)
  )
}

#* Regression model quality + genre/platform effects
#* @get /model-info
function() {
  co <- model_s$coefficients
  eff <- function(prefix) {
    k <- grep(paste0("^", prefix), rownames(co))
    data.frame(Term = sub(prefix, "", rownames(co)[k]),
               Effect = round((exp(co[k, 1]) - 1) * 100, 1))
  }
  list(
    r2 = round(model_s$r.squared, 3), adj_r2 = round(model_s$adj.r.squared, 3),
    rmse = round(sigma(model), 3), n = nobs(model),
    publishers = c(top_pub, "Other"),
    genre_effects = eff("Genre") %>% arrange(desc(Effect)),
    platform_effects = eff("Platform") %>% arrange(desc(Effect)) %>% slice_head(n = 12)
  )
}

#* Predict global sales for a hypothetical game
#* @get /predict
function(genre = "Action", platform = "PS4", publisher = "Other", year = 2015, res) {
  pub <- ifelse(publisher %in% top_pub, publisher, "Other")
  nd <- data.frame(Genre = genre, Platform = platform, Year = as.integer(year), Pub2 = pub)
  p <- tryCatch(predict(model, nd, interval = "prediction", level = 0.8), error = function(e) NULL)
  if (is.null(p)) { res$status <- 400; return(list(error = "Unknown genre or platform")) }
  list(
    predicted = round(max(expm1(p[1, "fit"]), 0), 2),
    low = round(max(expm1(p[1, "lwr"]), 0), 2),
    high = round(max(expm1(p[1, "upr"]), 0), 2)
  )
}

# =====================================================================
# 8. RECOMMENDER
# =====================================================================
#* Games similar to a title, restricted by the sidebar filters
#* @get /recommend
function(req, game = "", n = 8, res) {
  r <- recommend_games(df, feat, game, as.integer(n), flt(req)$.id)
  if (is.null(r)) { res$status <- 404; return(list(error = "Game not found")) }
  r
}
