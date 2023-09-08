install.packages("gtsummary")
df <- read.csv("ds_salaries.csv")
head(df)
num_rows <- nrow(df)
print(num_rows)
# Check for missing values
sum(is.na(df))
# Understand the variables
str(df)
# Summary statistics for numerical variables
summary(df)
library("gtsummary")

# Summary for continuous variables
df%>% 
  tbl_summary(type = all_continuous() ~ "continuous2", statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median}", "{min}-{max}")),
  digits = all_continuous() ~ c(2, 2, 0, 0, 0), include = c(work_year, salary, salary_in_usd, remote_ratio),
  label = list(work_year ~ "Work Year", salary ~ "Salary", salary_in_usd ~ "Salary in USD", remote_ratio ~ "Remote Ratio")) %>%
  bold_labels() %>%

# Summary for categorical variables
  