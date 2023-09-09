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
  modify_caption("**Continuous Variables**")

# Summary for categorical variables

install.packages("tidyverse")
library(tidyverse)

small_companies <- subset(df, df$company_size == "S", select=c(salary))
medium_companies <- subset(df, df$company_size == "M", select=c(salary))
large_companies <- subset(df, df$company_size == "L", select=c(salary))
small_companies

low_salary <- subset(df, df$salary_in_usd < 150000 | df$salary_in_usd == 150000)
medium_salary <- subset(df, df$salary_in_usd > 100000 & df$salary_in_usd < 250000)
high_salary <- subset(df, df$salary_in_usd > 250000 | df$salary_in_usd == 250000)

df <- mutate(df, salary_category = 0)

by_year <- group_by(df, work_year, company_size)
new_df <- summarize(by_year, average_salary = mean(salary_in_usd, na.rm = TRUE))

# Bad plot 1
ggplot(df, aes(x = salary_in_usd, fill = as.factor(company_size)))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

# one of the two (above and below)

ggplot(new_df) +
  geom_bar(mapping = aes(x = work_year, y = average_salary, fill = company_size), stat = "identity")+
  labs(x = "Work Year", y = "Average Salary", title = "Average Salary by Work Year and Company Size")

# Good plot 1
ggplot(new_df) +
  geom_bar(mapping = aes(x = work_year, y = average_salary, fill = company_size), 
           stat = "summary", position = "dodge") + 
  labs(x = "Work Year", y = "Average Salary", title = "Average Salary by Work Year and Company Size")
