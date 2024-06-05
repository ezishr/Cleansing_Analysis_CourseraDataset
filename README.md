# Coursera Dataset Cleaning and Visualization Project

## Project Overview

This project involves **cleansing** a Coursera dataset and creating visualizations using R. The main objectives are to preprocess the data to ensure quality and accuracy, and then generate insightful visualizations that can help in understanding various trends and patterns within the data.

## Project Structure

The project consists of the following files:

- `CourseraDataset-Unclean.csv`: The original Coursera dataset in CSV format.
- `my_df.RData`: An RData file containing the current state of the processed data.
- `Cleansing.R`: An R script containing all the code for data cleansing and visualization.
- `Coursera Data.Rproj`: An R workspace

## Data Source

The dataset used in this project is obtained from the Kaggle community. You can find the original dataset [here](https://www.kaggle.com/datasets/elvinrustam/coursera-dataset).

## Getting Started

### Prerequisites

To run this project, you will need to have R and RStudio installed on your system. Additionally, you'll need the following R packages:

- `tidyverse`: A collection of R packages for data manipulation and visualization.
- `dplyr`: A package for data manipulation.
- `ggplot2`: A package for data visualization.
- `readr`: A package for reading rectangular data.
- `zoo`: A package for working with regular and irregular time series.
- `textcat`: A package for language identification.

You can install these packages by running the following command in your R console:

```r
install.packages(c("tidyverse", "dplyr", "ggplot2", "readr", "zoo", "textcat"))
```
## Project Details

### Data Cleansing

The data cleansing process includes the following steps:

- **Handling Missing Values**: Identifying and appropriately handling missing data to prevent inaccuracies in analysis.
- **Converting Data Types**: Ensuring that all data types are correctly assigned for proper analysis and visualization.
- **Removing URL Duplicates**: Identifying and removing duplicate records to maintain data integrity.
- **Normalizing Data Formats**: Standardizing formats for consistency, such as date formats and categorical values.

### Visualizations

The project includes the following visualizations to provide insights into the dataset:

- **Distribution of Courses by Level**: A bar chart showing the number of courses in each level.
- **Comparison of Course Ratings Across Different Ranges**: A box plot comparing the distribution of course ratings across various categories.
- **Most Popular Courses Based on Enrollments**: A bar chart highlighting the courses with the highest number of enrollments.
