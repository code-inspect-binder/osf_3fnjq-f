# Create the Facebook Users Dataset

# DESCRIPTION:
# The Facebook Users Dataset consists of comments published to Facebook during the 2015 Canadian general election from August 4th to October 19th, 2015. After preprocessing, the dataset consists of 200,378 comments from 92,516 unique Facebook users. Of these, 86,759 comments can be attributed to 26,894 unique users with partisan preferences. 

# ---- packages and data ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyr, anytime, readr, stringr, lubridate)

# Load raw data. The data has only been modified to remove personally identifying information such as `from_id` and `from_name`. To track user contributions, the `user_id` uniquely identifies each individual with an anonymized ID.
Harper_User_Comments <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Harper_Page/Harper_User_Comments.csv")
Trudeau_User_Comments <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Trudeau_Page/Trudeau_User_Comments.csv")
Mulcair_User_Comments <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Mulcair_Page/Mulcair_User_Comments.csv")

Harper_User_Comments$candidate_page <-  "Harper"
Trudeau_User_Comments$candidate_page <-  "Trudeau"
Mulcair_User_Comments$candidate_page <-  "Mulcair"

# Concatenates the data from each party leader's Facebook page.
Facebook_Users <- rbind(Harper_User_Comments, Trudeau_User_Comments, Mulcair_User_Comments)

# ---- preprocess text data ----
# (More detailed notes on these procedures can be found in campaign_messages.R).

# Remove emoji-related UTF-8 encodings from user comments.
Facebook_Users$message <- sapply(Facebook_Users$message, FUN = function(x) gsub("[^\x01-\x7F]", "", x))
Facebook_Users$message <- sapply(Facebook_Users$message, FUN = function(x) gsub("[<].*[>]", "", x))

# Remove URLs from users' comments.
Facebook_Users$message <- sapply(Facebook_Users$message, FUN = function(x) gsub("http\\S+\\s*", "", x))

# Indicate missing values whenever text is absent then remove rows with missing values.
Facebook_Users$message[Facebook_Users$message==""] <- NA 
Facebook_Users <- Facebook_Users[complete.cases(Facebook_Users[ , "message"]),]

# Note: R does not treat all blank cells as missing values. In addition, there are some rows that when stripped of URLs and Unicode still contain punctuation marks. Most, if not all, of these rows hold no analytical value. These rows are removed by enforcing a word count threshold of greater than zero.

# Word count.
Facebook_Users$word_count <- str_count(Facebook_Users$message, '\\w+')
Facebook_Users <- subset(Facebook_Users, word_count > 0)
Facebook_Users[7] <- NULL

# ---- process structured data ----

# Creates new columns that identify each post and comment. Note that `id`` does not identify the user.
Facebook_Users$Source <- Facebook_Users$id
Facebook_Users <- separate(data = Facebook_Users, col = Source, into = c("post_id", "comment_id"), sep = "_")

# Note: The above code will give you a warning message that a full `id` was not found for some users; in each case, the `post_id` was present while the `comment_id` was missing. The latter were treated as missing values. In total thirteen users appear to have been effected.

# Converts `created_time` to POSIXct and creates new columns to separate dates from times. Dates are then encoded back to POSIXct, while `time_published` remains formatted as character strings. 
Facebook_Users$created_time <- anytime(Facebook_Users$created_time, tz = "America/Los_Angeles")
Facebook_Users$date_published <- format(Facebook_Users$created_time, "%Y-%m-%d")
Facebook_Users$time_published <- format(Facebook_Users$created_time, "%H:%M:%S")
Facebook_Users$date_published <- anytime(Facebook_Users$date_published)

# From the timestamps in created_time, create a new column indicating the month in which comments were published. Encode the new variable as a factor with three levels ordering months chronologically. 
Facebook_Users$month_published <- as.POSIXct(Facebook_Users$date_published, format="%H:%M:%S")
Facebook_Users$month_published <- format(Facebook_Users$month_published, "%B")
Facebook_Users$month_published <- factor(Facebook_Users$month_published, levels = c("August", "September", "October"))

# Create a column indicating the week of the campaign that each comment was published; where the beginning of each week is a Monday.
Facebook_Users$campaign_week <- ""
Facebook_Users$campaign_week <- (interval('2015-08-03',Facebook_Users$date_published) %/% weeks(1))+1

# Note that the 2015 general election campaign was technically 11 weeks + 1 day long. Rather than assign an entire week to this day, it is instead identified uniquely as Election Day. Researchers can excercise their discretion on how best to analyze this day.
Facebook_Users$campaign_week <- paste("Week", as.character(Facebook_Users$campaign_week))
Facebook_Users$campaign_week[Facebook_Users$campaign_week == "Week 12"] <- "Election Day"

# Convert `campaign_week` from a character string to a factor with 11 levels to order weeks chronologically.
Facebook_Users$campaign_week <- factor(Facebook_Users$campaign_week, levels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Election Day"))

# Convert `candidate_page` from a character string to a factor with 3 levels.(Facilitates data visualization and data exploration).
Facebook_Users$candidate_page <- factor(Facebook_Users$candidate_page, levels = c ("Harper", "Trudeau", "Mulcair"))

# ---- infer partisanship ----

# `partisanship`: The following operations were used to identify which, if any, users demonstrated a preference for one of the three political candidates. Users are refered to as partisan if they published at least one comment on Facebook and exclusively liked posts from ust one candidate.

# Load raw data on users' likes. Note that the data have only been modified to remove `user_id` and `from_name`.
Harper_User_Likes <- read_csv("data/raw/Harper_Page/Harper_User_Likes.csv")
Trudeau_User_Likes <- read_csv("data/raw/Trudeau_Page/Trudeau_User_Likes.csv")
Mulcair_User_Likes <- read_csv("data/raw/Mulcair_Page/Mulcair_User_Likes.csv")

# Adds a new column to identify the Facebook page being `liked`.
Harper_User_Likes$partisanship <- "Conservative"
Trudeau_User_Likes$partisanship <- "Liberal"
Mulcair_User_Likes$partisanship <- "Social Democrat"

# Concatenate the data frames on likes to create a full history.
Facebook_User_Likes <- rbind(Harper_User_Likes, Trudeau_User_Likes, Mulcair_User_Likes)

# Removes duplicate rows where the same user appears more than once on the same Page.
Facebook_User_Likes <- Facebook_User_Likes[!duplicated(Facebook_User_Likes[c("user_id" ,"partisanship")]),]

# Retrieves users who exclusively liked content from one page by removing users who liked more than one page.
Facebook_User_Likes <- Facebook_User_Likes[!(duplicated(Facebook_User_Likes$user_id) | duplicated(Facebook_User_Likes$user_id, fromLast = TRUE)), ]

# Match partisan assignment from Facebook_User_Likes to the Facebook Users Dataset using `user_id`.
Facebook_Users$partisanship <- NA
Facebook_Users$partisanship <- Facebook_User_Likes$partisanship[match(Facebook_Users$user_id, Facebook_User_Likes$user_id)]

# Convert `partisanship` from a character string to a factor with 3 levels.(Facilitates data visualization and data exploration).
Facebook_Users$partisanship <- factor(Facebook_Users$partisanship, levels = c ("Conservative", "Liberal", "Social Democrat"))

# Save Facebook Users Dataset.
write.csv(Facebook_Users, file = "~/GitHub/Facebook_Canada2015/data/datasets/Facebook Users Dataset/Facebook_Users_Dataset.csv", fileEncoding = "UTF-8", row.names = FALSE)
saveRDS(Facebook_Users, file = "~/GitHub/Facebook_Canada2015/data/datasets/Facebook Users Dataset/Facebook_Users_Dataset.rds")

# end of session
rm(list=ls())
detach("package:tidyr", unload = TRUE)
detach("package:anytime", unload = TRUE)
detach("package:readr", unload = TRUE)
detach("package:stringr", unload = TRUE)
detach("package:lubridate", unload = TRUE)
.rs.restartR()
