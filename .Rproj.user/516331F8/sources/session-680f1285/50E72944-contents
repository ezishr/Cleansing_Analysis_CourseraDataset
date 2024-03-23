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



# Clean Duplications -----------------------------------------------------------
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

df$Keyword <- ifelse( df$count_SocialSciences > 1, str_replace_all(df$Keyword, fixed("Social Sciences, Social Sciences"), "Social Sciences"), df$Keyword)
summary(df)

df <- unclean_data
df$Keyword <- ifelse(
  str_count(df$Keyword, fixed("Social Sciences")) > 1, str_replace_all( df$Keyword, fixed("Social Sciences, Social Sciences"), "Social Sciences"), df$Keyword
)

unclean_data <- df
summary(unclean_data)



# Clean specific signs [,],' ---------------------------------------------------
summary(unclean_data)

unclean_data %>% mutate(count = 
    str_count())
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
unclean_data$Review <- str_replace_all(unclean_data$Review, fixed("reviews"), "")
view(unclean_data)
unclean_data$Review <- as.character(unclean_data$Review)
summary(unclean_data)



# Clean Duration ---------------------------------------------------------------

# CLEAN APPROXIMATE HOUR    
duration_col <- unclean_data %>% select(Duration, URL)
view(duration_col)
unique(duration_col$Duration)
rows_approx <- duration_col[which(str_detect(duration_col$Duration, "pprox")),]
summary(rows_approx)
view(rows_approx)
summary(duration_col)

rows_approx1 <- rows_approx
view(rows_approx1)
summary(rows_approx1)

unique_duration_values <- unique(rows_approx1$Duration)
summary(unique_duration_values)
sum(str_detect(unique_duration_values, "our")) #check if there is row having hour or minutes. result: all have hour

unique_duration_values[which.max(nchar(unique_duration_values))]

which.max(nchar(rows_approx1$Duration))

rows_approx1[which.max(nchar(rows_approx1$Duration)),"Duration"] <- "2" #Replace the string w/o number with number

rows_approx1[2890,]

# Use \\d to check if there is row with only letters
only_letters_cell <- apply(rows_approx1, 1, function(row) !any(grepl("\\d", row), na.rm = TRUE)) # 1 in apply() means apply according to rows, not columns
sum(only_letters_cell) # No rows with only letter

test_remove_letter_1 <- str_remove_all(rows_approx1$Duration, "[A-Za-z.() ]")
view(test_remove_letter_1)
summary(test_remove_letter_1)
unique(test_remove_letter_1)

rows_approx1$Duration <- str_remove_all(rows_approx1$Duration, "[A-Za-z.() ]")

unclean_data[which(str_detect(duration_col$Duration, "pprox")),"Duration"] <- rows_approx1$Duration #Paste the clean duration into original df
unique(unclean_data$Duration)


# CLEAN MONTHS & WEEKS - get the rows with month or week in Duration variable
first_attempt <- unclean_data[which(str_detect(unclean_data$Duration, "month|week")),]
summary(first_attempt)

second_attempt <- subset(unclean_data, str_detect(unclean_data$Duration, "month|week"))
summary(second_attempt)

third_attempt <- apply(unclean_data["Duration"],1,function(row) any(grepl("month|week",row, ignore.case = TRUE), na.rm = TRUE))
summary(third_attempt)
    # ALL attempts get same number of rows. Only third attempt doesn't return a df

month_week_duration <- unclean_data[which(str_detect(unclean_data$Duration, "month|week")), "Duration"]
view(month_week_duration)
unique(month_week_duration)











