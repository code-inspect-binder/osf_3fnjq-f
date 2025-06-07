# Create the Campaign Messages Dataset

# DESCRIPTION:
# The Campaign Messages Dataset is a data frame consisting of candidate's campaign messages (i.e., Facebook posts) published to Facebook during the 2015 Canadian general election campaign between August 4th and October 19th, 2015 ( n = 1,529).

# ---- packages and data ---- 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(anytime, readr, lubridate)

# Load raw Facebook posts from party leaders' Facebook pages.
Harper_Posts <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Harper_Page/Harper_Posts.csv")
Trudeau_Posts <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Trudeau_Page/Trudeau_Posts.csv")
Mulcair_Posts <- read_csv("~/GitHub/Facebook_Canada2015/data/raw/Mulcair_Page/Mulcair_Posts.csv")

# Concatenate data across all three pages.
Campaign_Messages <- rbind(Harper_Posts, Trudeau_Posts, Mulcair_Posts)

# ---- process raw data ----

# The following are preprocessing procedures applied to text and structured data to remove noise from party leaders' Facebook pages.

# Candidates often linked to content found on their party's website or to sites located off of Facebook. For most studies, these URLs will complicate text analysis without adding any analytical value. URLs are, therefore, removed from all posts below. 
Campaign_Messages$message <- sapply(Campaign_Messages$message, FUN = function(x) gsub("http\\S+\\s*", "", x))

# Note: the above operation is applied to the `message` column. Links scraped from Facebook and found in the `links` field are uneffected. Researchers using this data may therefore still observe the kinds of websites that candidates published to Facebook in the `links` column.

# Some cells in the `messages` column may become blank after URLs have been removed. Rows without text are identified as missing values and removed from the dataset.
Campaign_Messages$message[Campaign_Messages$message==""] <- NA 
Campaign_Messages <- Campaign_Messages[complete.cases(Campaign_Messages[ , "message"]),]

# Converts `created_time` to POSIXct format and creates new variables to separate dates from times. Values in `date_published` are converted back to POSIXct format. `Time_published` is left as a character string. 
Campaign_Messages$created_time <- anytime(Campaign_Messages$created_time, tz = "America/Los_Angeles")
Campaign_Messages$date_published <- as.POSIXct(format(Campaign_Messages$created_time, "%Y-%m-%d"))
Campaign_Messages$time_published <- format(Campaign_Messages$created_time, "%H:%M:%S")

# Create a new variable indicating the month the post was published. Convert character strings to factor with three levels to order months chronologically.
Campaign_Messages$month_published <- as.POSIXct(Campaign_Messages$date_published, format="%H:%M:%S")
Campaign_Messages$month_published <- format(Campaign_Messages$month_published, "%B")
Campaign_Messages$month_published <- factor(Campaign_Messages$month_published, levels = c("August", "September", "October"))

# Create a column indicating the week of the campaign that each post was published; where the beginning of each week is a Monday.
Campaign_Messages$campaign_week <- ""
Campaign_Messages$campaign_week <- (interval('2015-08-03',Campaign_Messages$date_published) %/% weeks(1))+1

# Note that the 2015 general election campaign was technically 11 weeks + 1 day long. Rather than assign an entire week to this day, it is instead identified uniquely as Election Day. Researchers can excercise their discretion on how best to analyze this day.
Campaign_Messages$campaign_week <- paste("Week", as.character(Campaign_Messages$campaign_week))
Campaign_Messages$campaign_week[Campaign_Messages$campaign_week == "Week 12"] <- "Election Day"

# Convert `campaign_week` from a character string to a factor with 11 levels to order weeks chronologically.
Campaign_Messages$campaign_week <- factor(Campaign_Messages$campaign_week, levels = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7", "Week 8", "Week 9", "Week 10", "Week 11", "Election Day"))

# Convert `from_name` to factor with three levels. (Facilitates data visualization). 
Campaign_Messages$from_name <- factor(Campaign_Messages$from_name, levels = c ("Stephen Harper", "Justin Trudeau", "Tom Mulcair"))

# Ensure consistent data types across data sets. IDs are appropriately treated as characters rather than numeric. 
Campaign_Messages$from_id <- as.character(Campaign_Messages$from_id)
Campaign_Messages$type <- factor(Campaign_Messages$type, levels = c("event", "link", "photo", "status", "video"))

# Save the Campaign Messages Dataset.
write.csv(Campaign_Messages, file = "~/GitHub/Facebook_Canada2015/data/datasets/Campaign Messages Dataset/Campaign_Messages_Dataset.csv", fileEncoding = "UTF-8", row.names = FALSE)
saveRDS(Campaign_Messages, file="~/GitHub/Facebook_Canada2015/data/datasets/Campaign Messages Dataset/Campaign_Messages_Dataset.rds")

# end of session
rm(list=ls())
detach("package:anytime", unload = TRUE)
detach("package:readr", unload = TRUE)
detach("package:lubridate", unload = TRUE)
.rs.restartR()
