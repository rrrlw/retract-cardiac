#####SETUP#####
library(tidyverse)
library(gridExtra)
library(lubridate)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(rvest)
library(xml2)
library(rjson)
library(magrittr)
library(trend)

#####UTILITY FUNCTIONS#####
year_split <- function(year_val) {
  vapply(X = year_val,
         FUN.VALUE = character(1),
         FUN = function(curr_year) {
           if (is.na(curr_year)) return(NA_character_)
           
           if (curr_year < 2000) {
             "pre-2000"
           } else if (curr_year < 2005) {
             "2000-2004"
           } else if (curr_year < 2010) {
             "2005-2009"
           } else if (curr_year < 2015) {
             "2010-2014"
           } else {
             "2015-2020"
           }
         })
}

test_year_trend <- function(date_col) {
  year_freq <- date_col %>%
    year() %>%
    table()
  
  df_freq <- data.frame(Year = names(year_freq) %>% as.integer(),
                        Freq = year_freq %>% as.integer()) %>%
    filter(Year >= 2000 & Year < 2020)
  
  mk.test(df_freq$Freq)
}

author_format <- function(auth) {
  parts <- auth %>%
    strsplit(split = " ", fixed = TRUE) %>%
    unlist()
  
  last_name <- parts[length(parts)]
  first_initial <- parts[1] %>%
    substr(1, 1)
  
  parts <- parts[-length(parts)] %>%
    substr(1, 1)
  
  # paste(last_name, paste(parts, collapse = ""))
  paste(last_name, first_initial)
}

author_list <- function(auth_col) {
  auth_col %>%
    strsplit(split = ";", fixed = TRUE) %>%
    unlist() %>%
    vapply(FUN.VALUE = character(1),
           FUN = author_format,
           USE.NAMES = FALSE)
}

remove_time <- function(val) {
  # remove everything after space
  vapply(X = val,
         FUN.VALUE = character(1),
         FUN = function(curr) {
           if (is.na(curr)) {
             return(NA_character_)
           }
           
           space_index <- as.numeric(regexpr(pattern = " ",
                                             text = curr,
                                             fixed = TRUE))[1]
           
           return(substr(curr, 1, space_index - 1))
         }) -> ans
  
  names(ans) <- NULL
  return(ans)
}

digits_only <- function(chr) {
  gsub(pattern = "[^0-9]",
       replacement = "",
       x = chr)
}

get_col_counts <- function(col, strip = "+", split = ";") {
  col %>%
    gsub(pattern = strip, replacement = "", fixed = TRUE) %>%
    strsplit(split = split, fixed = TRUE) %>%
    unlist() %>%
    table()
}

#####DATA ANALYSIS#####
# input
retract <- read_csv("retractions.csv",
                    col_types = "cccccffccfccccccfcfc")

# manually fill in missing value
retract[retract$`Record ID` == "24122", ]$OriginalPaperDate <- "3/6/2018 0:00"

# get overall medicine info (baseline)
med_retract <- retract %>%
  # select only medicine-related articles
  filter(grepl(pattern = "(HSC) Medicine",
               x = retract$Subject,
               fixed = TRUE)) %>%
  # convert date columns to Date class
  mutate(RetractionDate = as.Date(remove_time(RetractionDate),
                                  format = "%m/%d/%Y"),
         OriginalPaperDate = as.Date(remove_time(OriginalPaperDate),
                                     format = "%m/%d/%Y"),
         CatchTime = as.numeric(RetractionDate - OriginalPaperDate))

# get cardiology info
cardio_retract <- med_retract %>%
  # select only cardiology-related articles
  filter(grepl(pattern = "(HSC) Medicine - Cardiology",
               x = med_retract$Subject,
               fixed = TRUE))

#####TIME TRENDS#####
# test number of papers over time in medicine
test_year_trend(med_retract$OriginalPaperDate) %>%
  print()

# test number of papers over time in cardiology
test_year_trend(cardio_retract$OriginalPaperDate) %>%
  print()

#####NUMBERS: AUTHOR INFO#####
med_auth_freq <- med_retract %>%
  pull(Author) %>%
  author_list() %>%
  table() %>%
  sort(decreasing = TRUE)
card_auth_freq <- cardio_retract %>%
  pull(Author) %>%
  author_list() %>%
  table() %>%
  sort(decreasing = TRUE)

df_med_auth <- data.frame(Author = factor(names(med_auth_freq),
                                          levels = names(med_auth_freq)),
                          Freq = as.integer(med_auth_freq),
                          Arbitrary_X = seq_along(med_auth_freq))
df_card_auth<- data.frame(Author = factor(names(card_auth_freq),
                                          levels = names(card_auth_freq)),
                          Freq = as.integer(card_auth_freq),
                          Arbitrary_X = seq_along(card_auth_freq))

# proportion of medicine articles to which most retraction-profilic author contributed
med_auth_freq[1:1] %>%
  sum() %>%
  divide_by(sum(med_auth_freq)) %>%
  print()

# proportion of medicine authors who contributed to only 1 retraction
mean(med_auth_freq == 1) %>%
  print()

# proportion of cardiology articles to which most retraction-profilic author contributed
card_auth_freq[1:1] %>%
  sum() %>%
  divide_by(sum(card_auth_freq)) %>%
  print()

# proportion of cardiology authors who contributed to only 1 retraction
mean(card_auth_freq == 1) %>%
  print()

# plot med auth distribution
g1 <- ggplot(df_med_auth, aes(x = Arbitrary_X, y = Freq)) +
  geom_point(shape = 20, size = 0.5) +
  xlab("Authors (Medicine)") +
  ylab("# papers") +
  ggtitle("(A)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
g2 <- ggplot(df_med_auth, aes(x = Arbitrary_X, y = log10(Freq))) +
  geom_point(shape = 20, size = 0.5) +
  xlab("Authors (Medicine)") +
  ylab("log10(# papers)") +
  ggtitle("(B)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
g3 <- ggplot(df_card_auth, aes(x = Arbitrary_X, y = Freq)) +
  geom_point(shape = 20, size = 0.5) +
  xlab("Authors (Cardiology)") +
  ylab("# papers") +
  ggtitle("(C)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
g4 <- ggplot(df_card_auth, aes(x = Arbitrary_X, y = log10(Freq))) +
  geom_point(shape = 20, size = 0.5) +
  xlab("Authors (Cardiology)") +
  ylab("log10(# papers)") +
  ggtitle("(D)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
g <- arrangeGrob(g1, g2, g3, g4, ncol = 2)
ggsave("auth-freq.png", plot = g, width = 6, height = 4)

#####FIG: CATCH TIMES#####
# histograms of med_retract vs cardio_retract catch times
df_med <- data.frame(Group = "Med",
                     Time = med_retract$CatchTime,
                     PubYear = year(med_retract$OriginalPaperDate)) %>%
  mutate(Years = Time / 365.25)
df_card<- data.frame(Group = "Cardio",
                     Time = cardio_retract$CatchTime,
                     PubYear = year(cardio_retract$OriginalPaperDate)) %>%
  mutate(Years = Time / 365.25)
df_hist <- rbind(df_med, df_card)
ggplot(df_hist, aes(x = Years, linetype = Group)) +
  geom_density() +
  xlim(c(0, 40)) +
  xlab("Time to retraction (years)") +
  ylab("Density") +
  theme_bw()
ggsave("retract-time.png", width = 6, height = 2.25)

# proportion of medicine articles retracted within 1 year and 5 years, respectively
df_med %>%
  filter(Years <= 1) %>%
  nrow() %>%
  divide_by(nrow(df_med))

df_med %>%
  filter(Years <= 5) %>%
  nrow() %>%
  divide_by(nrow(df_med))

# proportion of cardiology articles retracted within 1 year and 5 years, respectively
df_card %>%
  filter(Years <= 1) %>%
  nrow() %>%
  divide_by(nrow(df_card))

df_card %>%
  filter(Years <= 5) %>%
  nrow() %>%
  divide_by(nrow(df_card))

# Kolmogorov-Smirnov test to see if retract time dists are significantly different
ks.test(df_med$Years, df_card$Years) %>%
  print()

# Mann-Kendall test to check if catch time trend is downward for medicine and cardiology, respectively
df_med %>%
  filter(PubYear >= 2000 & PubYear < 2020) %>%
  group_by(PubYear) %>%
  summarise(Median_CatchTime = median(Time)) %>%
  pull(Median_CatchTime) %>%
  mk.test() %>%
  print()

df_card %>%
  filter(PubYear >= 2000 & PubYear < 2020) %>%
  group_by(PubYear) %>%
  summarise(Median_CatchTime = median(Time)) %>%
  pull(Median_CatchTime) %>%
  mk.test() %>%
  print()

# ONLY RUN ONCE
# use google scholar + rvest to get num citations per article
# cardio_retract <- mutate(cardio_retract, NumCitedBy = character(1))
# for (i in seq_len(nrow(cardio_retract))) {
# for (i in 51:60) {
#   cardio_retract$NumCitedBy[i] <- gscholar_cites(cardio_retract$OriginalPaperDOI[i])
#   print(i)
#   Sys.sleep(15)
# }
# write_csv(x = cardio_retract, file = "cardio-retract.csv")

# make a choropleth of retractions by country (world map)?

#####FIG: RETRACT TIME SERIES#####
# time series of num pubs and num retracts each year
cardio_ts <- mutate(cardio_retract,
                    OrigYear = year(OriginalPaperDate),
                    RetractYear = year(RetractionDate)) %>%
  pivot_longer(cols = c(OrigYear, RetractYear),
               names_to = "Type")
ggplot(cardio_ts, aes(x = value, linetype = Type)) +
  geom_line(stat = "count") +
  xlab("Year") +
  ylab("Count") +
  scale_linetype_discrete(name = "Event",
                       labels = c("Pub Date", "Retract Date")) +
  theme_bw()
ggsave("num-timeseries.png", width = 6, height = 2.25)

#####FIG: DATA FLOWCHART#####
# flowchart of data processing (retraction watch + google scholar, etc.)
flowchart <- DiagrammeR::grViz("digraph{
  graph [layout = dot rankdir = LR]
  
  # global style
  node [shape = rectangle]
  
  # node info
  rw [label = 'Retraction Watch database\n (n = 22,740)']
  med [label = 'Medicine subject\n (n = 6,594)']
  cardio [label = 'Cardiology articles\n (n = 445)']
  gscholar [label = 'Google Scholar\ncitation info']
  final [label = 'Final dataset\n (n = 445)']
  
  # edge info
  rw -> med -> cardio -> final
  gscholar -> final
}")
flowchart
flowchart %>% export_svg %>% charToRaw %>% rsvg_png("dataflow.png")

#####FIG: CITES BEFORE RETRACT#####
# histogram of num citations prior to retraction (impact of bad science)
cardio_retract_cite <- read_csv("cardio-retract.csv")
ggplot(cardio_retract_cite, aes(x = NumCitedBy)) +
  geom_histogram(bins = 20, color = "black", fill = "white") +
  xlab("# retracted article citations") +
  ylab("# articles") +
  theme_bw()
ggsave("cites-before-retract.png", width = 6, height = 2.25)

# Mann-Kendall test for downward trend of retracted cardiology citations in last 20 years
cardio_retract_cite %>%
  mutate(PubYear = year(OriginalPaperDate)) %>%
  filter(PubYear >= 2000 & PubYear < 2020) %>%
  group_by(PubYear) %>%
  summarise(Median_CitedBy = median(NumCitedBy, na.rm = TRUE)) %>%
  pull(Median_CitedBy) %>%
  mk.test() %>%
  print()

#####FIG: TIME SERIES DECADE PUB#####
cardio_catch_ts <- cardio_retract_cite %>%
  mutate(PubSplit = year_split(year(OriginalPaperDate))) %>%
  select(PubSplit, CatchTime, NumCitedBy, Subject) %>%
  mutate(PubSplit = factor(PubSplit,
                           levels = c("pre-2000",
                                      "2000-2004",
                                      "2005-2009",
                                      "2010-2014",
                                      "2015-2020"))) %>%
  mutate(FieldGroup = "Cardiology")

med_catch_ts <- med_retract %>%
  mutate(PubSplit = year_split(year(OriginalPaperDate))) %>%
  filter(!is.na(PubSplit)) %>%
  select(PubSplit, CatchTime, Subject) %>%
  mutate(PubSplit = factor(PubSplit,
                           levels = c("pre-2000",
                                      "2000-2004",
                                      "2005-2009",
                                      "2010-2014",
                                      "2015-2020")),
         NumCitedBy = NA_integer_) %>%
  mutate(FieldGroup = "Medicine")

combo_catch <- rbind(cardio_catch_ts, med_catch_ts) %>%
  mutate(CatchTimeYrs = CatchTime / 365.25)

# boxplot catch time (every 5 years)
ggplot(combo_catch, aes(x = PubSplit, y = CatchTimeYrs, fill = FieldGroup)) +
  geom_boxplot() +
  xlab("Original Publication Date") +
  ylab("Time to Retraction (years)") +
  ylim(c(0, 40)) +
  scale_fill_manual(name = "Field", values = c("white", "grey60")) +
  theme_bw()
ggsave("catch-by-5year.png", width = 6, height = 2.5)

# boxplot cites before retract each 10-year span
g1 <- ggplot(cardio_catch_ts, aes(PubSplit, NumCitedBy)) +
  geom_boxplot() +
  xlab("Original Publication Date") +
  ylab("# citations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(A)")
# ggsave("cite-by-5year.png", plot = g1, width = 3, height = 2.5)
g2 <- g1 + ylim(c(0, 100)) + labs(title = "(B)")
# ggsave("cite-by-5year-zoom.png", plot = g2, width = 3, height = 2.5)
g <- arrangeGrob(g1, g2, ncol = 2)
ggsave("cite-5year-side.png", plot = g,
       width = 6, height = 2.5)

#####STAT: REASON FOR RETRACTION#####
at_least_one <- function(vec, vec_chr) {
  ans <- logical(length(vec))
  for (i in seq_along(vec)) {
    curr <- FALSE
    for (curr_chr in vec_chr) {
      if (grepl(x = vec[i], pattern = curr_chr, fixed = TRUE)) curr <- TRUE
    }
    
    ans[i] <- curr
  }
  ans
}

both_retract <- rbind(
  cardio_retract %>% mutate(Group = "Cardio"),
  med_retract %>% mutate(Group = "Med")
)

all_reasons <- both_retract$Reason %>%
  gsub(pattern = "+", replacement = "", fixed = TRUE) %>%
  strsplit(split = ";", fixed = TRUE) %>%
  unlist() %>%
  unique() %>%
  sort()

bad_reasons <- all_reasons[c(2,
                             20,
                             37,
                             41,
                             42,
                             43,
                             44,
                             45,
                             46,
                             47,
                             48,
                             49,
                             54,
                             58,
                             60,
                             61,
                             66,
                             67,
                             68,
                             69,
                             80,
                             81,
                             82,
                             83,
                             87,
                             89)]

bad_prop <- both_retract %>%
  mutate(Avoidable = at_least_one(Reason, bad_reasons)) %>%
  group_by(Group) %>%
  summarise(n = n(), prop = mean(Avoidable), success = sum(Avoidable), fail = sum(!Avoidable))

# z-proportion
prop.test(x = bad_prop$success, n = bad_prop$n) %>%
  print()

#####FIG: REASON FOR RETRACTION#####
cardio_reasons <- get_col_counts(cardio_retract$Reason)
med_reasons <- get_col_counts(med_retract$Reason)

# get threshold for top 10 values
cardio_thresh <- quantile(as.numeric(cardio_reasons),
                          probs = 1 - 10 / length(cardio_reasons))
med_thresh <- quantile(as.numeric(med_reasons),
                       probs = 1 - 10 / length(med_reasons))

# create and combine count DFs
df_cardio <- data.frame(Group = "Cardiac",
                        Reason = names(cardio_reasons),
                        Count = as.numeric(cardio_reasons),
                        stringsAsFactors = FALSE) %>%
  mutate(Freq = Count / sum(Count)) %>%
  filter(Count > cardio_thresh)
df_med <- data.frame(Group = "Med",
                     Reason = names(med_reasons),
                     Count = as.numeric(med_reasons),
                     stringsAsFactors = FALSE) %>%
  mutate(Freq = Count / sum(Count)) %>%
  filter(Count > med_thresh)

df_reasons <- rbind(df_cardio, df_med) %>%
  mutate(Freq = 100 * Freq)

ggplot(df_reasons, aes(x = Reason, y = Freq, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(name = "Field", values = c("Cardiac" = "white",
                                               "Med" = "grey60")) +
  ylab("Frequency (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.margin = unit(c(0.25, 0.25, 0.25, 1.75), "cm"))
ggsave("retract-reasons.png", width = 6, height = 4)
