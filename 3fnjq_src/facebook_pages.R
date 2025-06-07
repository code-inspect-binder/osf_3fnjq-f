# Create the Facebook Pages Dataset

# DESCRIPTION:
# The dataset contains structural data collected from Facebook's Graph API (v.2.0). Data were queried from three verified public Facebook Pages each belonging to a different political candidate; namely, Stephen Harper, Justin Trudeau, and Tom Mulcair. Posts are limited to those published during the campaign; from August 4th until October 19th, 2015. The dataset contains every post published during this time. Each row of the dataset is a unique post (i.e., campaign message) and each column is a different field of data scraped from Facebook. Fields of data include metrics such as the number of likes, comments, and shares that each post received. 

# ---- packages and data ----

# Required packages.
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyr, anytime, lubridate)

# Load raw Facebook data.
load("~/GitHub/Facebook_Canada2015/data/raw/Facebook_Pages/elxn2015.Rdata")

# --- process raw data ----

# Identifies the candididate who authored each post. Candidates are identified according to their Page IDs. These IDs were obtained and then manually verified during data collection.  
elxn2015$Source <- elxn2015$id
elxn2015 <- separate(data = elxn2015, col = Source, into = c("candidate_page", "post_id"), sep = "_")
elxn2015$candidate_page[elxn2015$candidate_page=="9106562109"] <- "Harper"
elxn2015$candidate_page[elxn2015$candidate_page=="21751825648"] <- "Trudeau"
elxn2015$candidate_page[elxn2015$candidate_page=="149331805154700"] <- "Mulcair"

# Convert `candidate_page` to a factor. (Facilitates data visualizations and exploration).
elxn2015$candidate_page <- factor(elxn2015$candidate_page, levels = c("Harper", "Trudeau", "Mulcair"))

# Timestamps are initially encoded as factors. Convert them to POSIXct format to avoid errors. Dates and times are separated and saved as two new variables; namely, `date_published` and `time_published`. `date_published` must be converted back to POSIXct format. `Time_published` is left as a character string as this format proves easier to work with when plotting time-related data. 
elxn2015$created_time <- anytime(elxn2015$created_time, tz = "America/Los_Angeles")
elxn2015$date_published <- as.POSIXct(format(elxn2015$created_time, "%Y-%m-%d"))
elxn2015$time_published <- format(elxn2015$created_time, "%H:%M:%S")

# Create a new variable from `date_published` to indicate the month each post was published. 
elxn2015$month_published <- as.POSIXct(elxn2015$date_published, format="%H:%M:%S")
elxn2015$month_published <- format(elxn2015$month_published, "%B")

# `month_published` is converted to a factor with three levels such that values are chronologically ordered. 
elxn2015$month_published <- factor(elxn2015$month_published, levels = c("August", "September","October"))

# Create a column indicating the week of the campaign that each post was published; where the beginning of each week is a Monday.
elxn2015$campaign_week <- ""
elxn2015$campaign_week <- (interval('2015-08-03',elxn2015$date_published) %/% weeks(1))+1

# Note that the 2015 general election campaign was technically 11 weeks + 1 day long. Rather than assign an entire week to this day, it is instead identified uniquely as Election Day. Researchers can excercise their discretion on how best to analyze this day.
elxn2015$campaign_week <- paste("Week", as.character(elxn2015$campaign_week))
elxn2015$campaign_week[elxn2015$campaign_week == "Week 12"] <- "Election Day"

# Convert `campaign_week` from a character string to a factor with 11 levels to order weeks chronologically.
elxn2015$campaign_week <- factor(elxn2015$campaign_week, levels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Election Day"))

# For reasons unknown to me, the Facebook Graph API (v.2.0) returned a duplicate of one of Tom Mulcair's posts. The issue appears to be isolated to one instance. The following code ensures that each post is unique. The number of campaign messages is reduced from 1,712 to 1,711.
Facebook_Pages <- elxn2015[!duplicated(elxn2015[c("id")]),]

# Ensure consistent data types across data sets. IDs are appropriately treated as characters rather than numeric. 
Facebook_Pages$id <- as.character(Facebook_Pages$id)
Facebook_Pages$likes_count <- as.double(Facebook_Pages$likes_count)
Facebook_Pages$comments_count <- as.double(Facebook_Pages$comments_count)
Facebook_Pages$shares_count <- as.double(Facebook_Pages$shares_count)

# Save as the Facebook Pages Dataset.
write.csv(Facebook_Pages, file = "~/GitHub/Facebook_Canada2015/data/datasets/Facebook Pages Dataset/Facebook_Pages_Dataset.csv", fileEncoding = "UTF-8", row.names = FALSE)
saveRDS(Facebook_Pages, file="~/GitHub/Facebook_Canada2015/data/datasets/Facebook Pages Dataset/Facebook_Pages_Dataset.rds")

# end of session
rm(list=ls())
detach("package:tidyr", unload = TRUE)
detach("package:anytime", unload = TRUE)
detach("package:lubridate", unload = TRUE)
.rs.restartR()
