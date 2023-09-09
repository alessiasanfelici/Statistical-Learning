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

low_salary <- subset(df, df$salary_in_usd < 150000 | df$salary_in_usd == 150000)
medium_salary <- subset(df, df$salary_in_usd > 100000 & df$salary_in_usd < 250000)
high_salary <- subset(df, df$salary_in_usd > 250000 | df$salary_in_usd == 250000)

df <- mutate(df, salary_category = 0)

by_year <- group_by(df, work_year, company_size)
new_df <- summarize(by_year, average_salary = mean(salary_in_usd, na.rm = TRUE))

# Bad plot 1
ggplot(df, aes(x = salary_in_usd, fill = as.factor(company_size)))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')


# Bad plot 2

ggplot(new_df) +
  geom_bar(mapping = aes(x = work_year, y = average_salary, fill = company_size), stat = "identity")+
  labs(x = "Work Year", y = "Average Salary", title = "Average Salary by Work Year and Company Size")

#Good plot 1
rem_0 <- subset(df, df$remote_ratio == 0, select = c(salary_in_usd))
rem_50 <- subset(df, df$remote_ratio == 50, select = c(salary_in_usd))
rem_100 <- subset(df, df$remote_ratio == 100, select = c(salary_in_usd))
d0 <- density(as.vector(rem_0[["salary_in_usd"]]))
d50 <- density(as.vector(rem_50[["salary_in_usd"]]))
d100 <- density(as.vector(rem_100[["salary_in_usd"]]))
plot(d50, col = "red", ,main = "Density of salary according to remote ratio",
     xlab = "Salary in USD", ylab = "Density", xaxt = "n") 
# Customize the x-axis labels to be in thousands
axis(1, at = seq(1e5, 5e5, by = 1e5),
     labels = scales::comma_format(scale = 1e-3)(seq(1e5, 5e5, by = 1e5)))
lines(d0, col = "blue")
lines(d100, col = "green")
legend("topright", legend = c("Remote ratio = 0", "Remote ratio = 50", "Remote ratio = 100"),
       col = c("blue", "red", "green"), lty = 1)

ggplot(df, aes(x = salary_in_usd, fill = as.factor(remote_ratio))) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "Density of salary according to remote ratio",
       x = "Salary in USD",
       y = "Density") +
  scale_x_continuous(breaks = seq(1e5, 5e5, by = 1e5),
                     labels = scales::comma_format(scale = 1e-3)) +
  scale_fill_manual(values = c("Remote ratio = 0" = "blue",
                               "Remote ratio = 50" = "red",
                               "Remote ratio = 100" = "green")) +
  scale_color_manual(values = c("Remote ratio = 0" = "blue",
                                "Remote ratio = 50" = "red",
                                "Remote ratio = 100" = "green")) +
  theme(legend.position = "topright") +
  theme_minimal() + 
  guides(fill = guide_legend(title = "Remote Ratio"))

ggplot(df, aes(x = salary_in_usd, fill = as.factor(remote_ratio)))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

# Good plot 2
ggplot(new_df) +
  geom_bar(mapping = aes(x = work_year, y = average_salary, fill = company_size), 
           stat = "summary", position = "dodge") + 
  labs(x = "Work Year", y = "Average Salary", title = "Average Salary by Work Year and Company Size")
