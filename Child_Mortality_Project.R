# Loading the libraries
library(readxl)
library(dplyr)
library(janitor)
library(gt)
library(flextable)
library(tidyr)
library(ggplot2)

# Loading the data set

Child_Mortality_data_set_Cleaned <- read_excel("C:/Users/admin/Desktop/Masters-MSC/APHREA/Health Data Sets/Child Mortality data set- Cleaned.xlsx")

# Renaming the columns

colnames(Child_Mortality_data_set_Cleaned)=c("Continent","Country","Year","Age_Group","Cause_of_Death","Prevalence")

View(Child_Mortality_data_set_Cleaned)
str(Child_Mortality_data_set_Cleaned)
summary(Child_Mortality_data_set_Cleaned)

# Checking for any missing values
## In totality
## Per Column
sum(is.na(Child_Mortality_data_set_Cleaned))
colSums(is.na(Child_Mortality_data_set_Cleaned))
names(Child_Mortality_data_set_Cleaned$Continent)

boxplot(Prevalence ~ Continent, data = Child_Mortality_data_set_Cleaned,
        main = "Prevalence by Continent",
        ylab = "Prevalence",
        xlab = "Continent",
        col = "lightblue")

# To check if prevalence significantly differs across continents

Anova_mortality= aov(Prevalence ~ Continent, data = Child_Mortality_data_set_Cleaned)
summary(Anova_mortality)

# Summarizing mortality rate by continent and age group

summary_table =Child_Mortality_data_set_Cleaned %>%
  group_by(Continent, Age_Group) %>%
  summarise(mean_mortality = mean(Prevalence, na.rm = TRUE),
            sd_mortality = sd(Prevalence, na.rm = TRUE),
            n = n()) %>%
  ungroup()
summary_table

b=summary_table %>%
  gt() %>%
  tab_header(
    title = "Child Mortality Rate by Continent and Age Group"
  ) %>%
  fmt_number(
    columns = c(mean_mortality, sd_mortality),
    decimals = 4
  ) %>%
  cols_label(
    Continent = "Continent",
    Age_Group = "Age Group",
    mean_mortality = "Mean Mortality Rate",
    sd_mortality = "SD of Mortality Rate"
  ) %>%
  tab_options(
    table.font.size = 12,
    table.width = pct(100)
  )
b
