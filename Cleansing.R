# Set up -----------------------------------------------------------------------
sessionInfo()
library(tidyverse)
library(zoo)
install.packages("textcat")
library(textcat)
getwd()
list.files(getwd())
unclean_data <- read.csv("CourseraDataset-Unclean.csv")
summary(unclean_data)
colnames(unclean_data)

# Set columns name -------------------------------------------------------------
unclean_data <- rename(unclean_data, crs_title = "Course.Title")
unclean_data <- rename(unclean_data, will_learn = "What.you.will.learn")
unclean_data <- rename(unclean_data, offered = "Offered.By")
unclean_data <- rename(unclean_data, URL = "Course.Url")
unclean_data <- rename(unclean_data, skill = "Skill.gain")
colnames(unclean_data)
summary(unclean_data)



# Clean NA ---------------------------------------------------------------------
sapply(unclean_data, typeof)
unclean_data$Rating <- as.character(unclean_data$Rating)
# clean_NA1 <- function(df, col_clean){
#  extract_col <- df %>% select(!!sym(col_clean))
#  df[, col_clean] <- ifelse(is.na(extract_col),"No information", extract_col)
#  return(df)} ------ Fail

clean_NA1 <- function(input_df, colname, replace_value){
  input_df <- input_df %>% mutate(
    !!sym(colname) := ifelse(
      is.na(!!sym(colname)), replace_value, !!sym(colname)
    )
  )
  return(input_df)
}
colSums(is.na(unclean_data))
df <- unclean_data
df <- clean_NA1(df, "Rating", "No information")
colSums(is.na(df))
unique(df$Rating)
unclean_data <- clean_NA1(unclean_data, "Rating", "No information")
colSums(is.na(unclean_data))
summary(unclean_data)



# Clean Empty ---------------------------------------------------------------------
colSums(unclean_data == "")
clean_empty1 <- function(input_df, colname, replace_value){
  input_df <- input_df %>% mutate(
    !!sym(colname) := ifelse(
      !!sym(colname) == "", replace_value, !!sym(colname)
    )
  )
}
df <- unclean_data
df <- clean_empty1(df, "Level", "No information")
summary(df)
unclean_data <- clean_empty1(unclean_data, "Instructor", "No information")
summary(unclean_data)
view(unclean_data)


# Clean Duplication -----------------------------------------------------------
## Purpose: there are rows with same link but different keywords => append the keyword together
sum(duplicated(unclean_data$URL))

clean_dup1 <- function(input_df, col_dup, col_append){
  
  while( sum( duplicated(input_df[,col_dup] ) ) >= 1){
    dup_df <- input_df[which( duplicated(input_df[,col_dup]) ),] # get df of duplicated rows of URL
    link_1st_row <- as.character(dup_df[1, col_dup])
    row_indices <- as.numeric(which( input_df[,col_dup] == link_1st_row )) # get rows having same links
    row_indices_without_min <- row_indices[-which.min(row_indices)]
    
    for (i in row_indices_without_min){
      input_df[min(row_indices), col_append] <- paste(
        input_df[min(row_indices), col_append], input_df[i, col_append], sep = ", "
      )
    }
    input_df <- input_df[-row_indices_without_min,]
  }
  
  return(input_df)
}
  ## testing
df <- unclean_data
sum(duplicated(df$URL))
df <- clean_dup1(df, "URL", "Keyword")
summary(df)
unique(df$Keyword)
  ## finalize
unclean_data <- clean_dup1(unclean_data, "URL", "Keyword")
summary(unclean_data)


# Clean Social Sciences in Keyword col -----------------------------------------
summary(unclean_data)

df <- df %>% mutate(
  count_SocialSciences = str_count(Keyword, fixed("Social Sciences"))
  )

unique(df$count_SocialSciences)

colnames(df)
view(df)

df$Keyword <- ifelse( df$count_SocialSciences > 1, str_replace_all(df$Keyword, fixed("Social Sciences, Social Sciences"), "Social Sciences"), df$Keyword)
summary(df)

df <- unclean_data
df$Keyword <- ifelse(
  str_count(df$Keyword, fixed("Social Sciences")) > 1, str_replace_all( df$Keyword, fixed("Social Sciences, Social Sciences"), "Social Sciences"), df$Keyword
)

unclean_data <- df
summary(unclean_data)

unique(
  (unclean_data %>% mutate (
  count = str_count(Keyword, fixed("Social Sciences"))
  )
  )$count)

view(unclean_data)

# Clean specific signs [,],' ---------------------------------------------------
summary(unclean_data)

## function to view df better
view( unclean_data %>% head(10) )
glimpse(unclean_data %>% head(10))

## testing
df <- unclean_data %>% head(10) %>% select(crs_title, skill)
view(df)

clean_sign1 <- function(input_df, column, sign1, sign2 = "", sign3 = ""){
  input_df[,column] <- sapply(
    input_df[,column], function(x){
      x <- gsub(paste0(fixed(sign1), "|", fixed(sign2), "|", fixed(sign3)), "", x) #syntax: gsub(pattern, replacement value, object/text)
    }
  )
  return(input_df)
}
df <- clean_sign1(df, "skill","\\[", "\\]")
view(df)

## finalize
unclean_data <- clean_sign1(unclean_data, "Instructor", "'","\\[","\\]")
unclean_data <- clean_sign1(unclean_data, "Modules", "'","\\[","\\]")
unclean_data <- clean_sign1(unclean_data, "skill", "'","\\[","\\]")
unclean_data <- clean_sign1(unclean_data, "offered", "'","\\[","\\]")
view(unclean_data)
summary(unclean_data)



# Clean Review -----------------------------------------------------------------
unclean_data$Review <- str_replace_all(unclean_data$Review, fixed(" reviews"), "")
view(unclean_data)
unclean_data$Review <- as.character(unclean_data$Review)
summary(unclean_data)



# FINALIZE CLEANSING UNTIL THIS STEP -------------------------------------------
orig_df <- read.csv("CourseraDataset-Unclean.csv")
## Set columns name
orig_df <- rename(orig_df, crs_title = "Course.Title")
orig_df <- rename(orig_df, will_learn = "What.you.will.learn")
orig_df <- rename(orig_df, offered = "Offered.By")
orig_df <- rename(orig_df, URL = "Course.Url")
orig_df <- rename(orig_df, skill = "Skill.gain")
colnames(orig_df)
## Clean NA for Rating col
clean_NA_func <- function(input_df, colname, replace_value){
  input_df <- input_df %>% mutate(
    !!sym(colname) := ifelse(
      is.na(!!sym(colname)), replace_value, !!sym(colname)
    )
  )
  return(input_df)
}
colSums(is.na(orig_df))
orig_df <- clean_NA_func(orig_df,"Rating", "No information")
orig_df <- clean_NA_func(orig_df,"Review", 0)
view(orig_df)
## Clean Empty for Level col
colSums(orig_df == "")
clean_empty_func <- function(input_df, colname, replace_value) {
  input_df <- input_df %>% mutate(
    !!sym(colname) := ifelse(
      !!sym(colname) == "", replace_value, !!sym(colname)
    )
  )
}
orig_df <- clean_empty_func(orig_df, "Level", "No information")
orig_df <- clean_empty_func(orig_df, "Review", 0)
orig_df <- clean_empty_func(orig_df, "Duration", "No information")
orig_df <- clean_empty_func(orig_df, "Schedule", "No information")
orig_df <- clean_empty_func(orig_df, "will_learn", "No information")
orig_df <- clean_empty_func(orig_df, "skill", "No information")
orig_df <- clean_empty_func(orig_df, "Modules", "No information")
orig_df <- clean_empty_func(orig_df, "Instructor", "No information")
## Clean duplicated URL
### Purpose: there are rows with same link but different keywords => append the keyword together
clean_dup_func <- function(input_df, col_dup, col_append){
  
  while( sum( duplicated(input_df[,col_dup] ) ) >= 1){
    dup_df <- input_df[which( duplicated(input_df[,col_dup]) ),] # get df of duplicated rows of URL
    link_1st_row <- as.character(dup_df[1, col_dup])
    row_indices <- as.numeric(which( input_df[,col_dup] == link_1st_row )) # get rows having same links
    row_indices_without_min <- row_indices[-which.min(row_indices)]
    
    for (i in row_indices_without_min){
      input_df[min(row_indices), col_append] <- paste(
        input_df[min(row_indices), col_append], input_df[i, col_append], sep = ", "
      )
    }
    input_df <- input_df[-row_indices_without_min,]
  }
  
  return(input_df)
}
orig_df <- clean_dup_func(orig_df, "URL", "Keyword")
summary(orig_df)
## Clean "Social Sciences" duplication in Keyword col
orig_df$Keyword <- ifelse(
  str_count(orig_df$Keyword, fixed("Social Sciences")) > 1, str_replace_all(
    orig_df$Keyword, fixed("Social Sciences, Social Sciences"), "Social Sciences"
  ), orig_df$Keyword
)
unique(
  (orig_df %>% mutate(
  count_ss = str_count(orig_df$Keyword, fixed("Social Sciences")))
  )$count_ss
) ###check
## Clean specific signs [,],'
clean_sign_func <- function(input_df, colname, sign1, sign2 = "", sign3 = "") {
  input_df[,colname] <- sapply(input_df[,colname], function(x) {
    x <- gsub(paste0(fixed(sign1), "|", fixed(sign2), "|", fixed(sign3)), "", x)
  })
  return(input_df)
}
orig_df <- clean_sign_func(orig_df, "skill", "\\[", "\\]", "'")
orig_df <- clean_sign_func(orig_df, "Modules", "\\[", "\\]", "'")
orig_df <- clean_sign_func(orig_df, "Instructor", "\\[", "\\]", "'")
orig_df <- clean_sign_func(orig_df, "offered", "\\[", "\\]", "'")
## Clean word "reviews" in Review col
colnames(orig_df)
orig_df$Review <- str_replace_all(orig_df$Review, fixed(" reviews"), "")
## Change type of Review col
orig_df$Review <- as.double(orig_df$Review)
summary(orig_df)
view(orig_df)
# CLEAN DURATION ---------------------------------------------------------------
## Get a copy version to testing
df <- orig_df
summary(df)
view(df)
## Get rows having approx words
approx_rows <- df[which(str_detect(df$Duration, "pprox")),c("Duration", "URL")]
view(approx_rows)
unique(approx_rows$Duration)
## Change the longest row to n
approx_rows[which.max(nchar(approx_rows$Duration)), "Duration"] <- "2 hours"
## Delete all letter chars in Duration and add word "hours"
approx_rows[,"Duration"] <- gsub("[^0-9]", "", approx_rows[,"Duration"])
approx_rows[, "Duration"] <- paste0(approx_rows[,"Duration"], " hours")
## Add new column named category
approx_rows <- approx_rows %>% mutate(
  category = "Approximately"
)
## Apply above changes to original df
matches <- grepl("pprox", df$Duration)
df[matches, "Duration"] <- approx_rows$Duration
df <- left_join(df, approx_rows[,c("category", "URL")], by = "URL")
df <- df[order(df$Duration),]
view(df)
unique(df$Duration)
df[which(df$Duration == "one hour"), "Duration"] <- "1 hour"
## Get rows having following words: month, week, at
month_df <- df[which(str_detect(df$Duration, paste0(fixed("month"), "|", fixed("week"), "|", fixed(" at ")))), c("Duration", "URL", "category")]
view(month_df)
unique(month_df$Duration)
month_df[which(month_df$Duration == "1 week of study, 2 hours"), "category"] <- "Hours"
month_df[which(month_df$Duration == "1 week of study, 2 hours"), "Duration"] <- "14 hours"
summary(month_df)
month_df[which(str_detect(month_df$Duration, fixed("month"))), "Duration"] <- gsub(" month.*", "", month_df[which(str_detect(month_df$Duration, fixed("month"))), "Duration"])
unique(month_df$Duration)
month_df[which(!str_detect(month_df$Duration, fixed("hours"))),"Duration"] <- paste0(month_df[which(!str_detect(month_df$Duration, fixed("hours"))), "Duration"], " months")
month_df[which(month_df$Duration == "1 months"), "Duration"] <- "1 month"
view(month_df)
matching <- grepl(paste0(fixed("month"), "|", fixed("week"), "|", fixed(" at ")), df$Duration)
df[matching, "Duration"] <- month_df$Duration
unique(df$category)
view(df[which(str_detect(df$Duration, fixed("month"))),])
df[which(str_detect(df$Duration, fixed("month"))), "category"] <- "Months"
view(df)
view(unique(df$Duration))
## Get rows having "mins" "minutes"
mins_df <- df[which(str_detect(
  df$Duration, paste0(fixed("min"))
)),c("Duration", "URL")]
view(unique(mins_df$Duration))
view(mins_df)
mins_df[which(mins_df$Duration == "1 hour 10 mins"), "Duration"] <- "1.16 hours"
mins_df[which(mins_df$Duration == "1 hour 15 mins" | mins_df$Duration == "1 hour 15 minutes"), "Duration"] <- "1.25 hours"
mins_df[which(mins_df$Duration == "1 hour and 20 minutes"), "Duration"] <- "1.33 hours"
mins_df[which(mins_df$Duration == "2 hours 30 mins"), "Duration"] <- "2.5 hours"
### Apply changes to original dataset
matching_min <- grepl(paste0(fixed("min")), df$Duration)
df[matching_min, "Duration"] <- mins_df$Duration
view(unique(df[,c("Duration", "category")]))
df %>% filter(is.na(category)) %>% select(Duration, category)
df[which(df$Duration == "No information"), "category"] <- "No information"
df[which(is.na(df$category) & str_detect(df$Duration, fixed("hour"))), "category"] <- "Hours"
view(unique(df$category))
view(df %>% select(Duration, category))
sum(is.na(df$category))
summary(df)
colSums(is.na(df))
colSums(df == "")
sum(unique(df$URL))
view(df %>% select(Modules, Instructor, skill))
view(df)
unique(df$Modules)

df[which(df$Rating == "No information"), "Rating"] <- 0
df$Rating <- as.double(df$Rating)
view(table(df$category))


# Visualization: number of courses vs. rating-----------------------------------
ranges <- c(0, 1.0, 2.0, 3.0, 4.0, 5.1)
df$ranges <- cut(df$Rating, breaks = ranges, labels = c("0-1.0", "1.1-2.0", "2.1-3.0", "3.1-4.0", "4.1-5.0"), right = FALSE)
df %>% select(ranges, Rating) %>% head()
df[which(is.na(df$Review)), "Review"] <- 0
view(df)
df_rating <- as.data.frame(count(df, ranges))
view(df_rating)
rating_visual <- ggplot(df_rating, aes(x=ranges, y=n, fill = ranges)) + geom_col() + labs(x = "Rating ranges", y = "Courses", fill="Rating ranges") + ggtitle("Course vs. Rating")
ggsave("rating_visual.png", rating_visual)
save(df, file = "my_df.RData")


# Visualization: number of course vs. level 
view(df)
level_visual <- ggplot(df, aes(x=Level, fill = Level)) + geom_bar() + labs(y="Course count") + ggtitle("Courses vs. Level")
ggsave("level_visual.png", level_visual)


