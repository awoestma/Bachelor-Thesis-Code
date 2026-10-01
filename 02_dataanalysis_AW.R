# Set the working directory to your path
setwd(".../AW_bachelorthesis")
getwd()

# Note: The figures and graphs from this script were only used by myself for own first impressions of the data. They would have designed much more beautiful and appropriate if they would have been integrated in my thesis.

# Overview: Variable names from the script and their correspondence to variables in the thesis----
## Semester Abroad Participation: "semester_abroad"
## Index Peers: "indsocialpeers"
## Meeting Friends: "freifreun"
## Motivation University Friends: "pwmpeer"
## Meeting Family: "freifam"
## Motivation University Family: "pwmelt"
## Age: "age30"
## Partner: "partner"
## Female: "female"
## Parental Education: "par_edu"
## Parental Financial Wealth: "deltwoh"
## Sociability: "extro"
## Average Grade: "vsbnoteo"
## Major Field of Study: "sfach1_g3"


# install necessary packages
install.packages("haven")
install.packages("psych")
install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("sandwich")
install.packages("lmtest")
install.packages("summarytools")
install.packages("lm.beta")

# add necessary packages to the library
library(psych)
library(haven)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(forcats)
library(sandwich)
library(lmtest)
library(summarytools)
library(lm.beta)


# Problem of the Large Sample: Many missing values! (Table B1)----
## load the large data set
dataSSY_big<-read_sav("data/prepared/dataSSY_big.sav") 

## through the writing and reading process, the NAs in the character variables got removed/filled with no value. I recode these cells back to "NA"
dataSSY_big$indsocialpeers_cha[dataSSY_big$indsocialpeers_cha == ""] <- NA
dataSSY_big$freifreun_cha[dataSSY_big$freifreun_cha == ""] <- NA
dataSSY_big$freifam_cha[dataSSY_big$freifam_cha == ""] <- NA
dataSSY_big$pwmpeer_cha[dataSSY_big$pwmpeer_cha == ""] <- NA
dataSSY_big$pwmelt_cha[dataSSY_big$pwmelt_cha == ""] <- NA
dataSSY_big$extro_cha[dataSSY_big$extro_cha == ""] <- NA
dataSSY_big$partner_cha[dataSSY_big$partner_cha == ""] <- NA
dataSSY_big$female_cha[dataSSY_big$female_cha == ""] <- NA

## Creation of factor variables for categorical variable: the factors are converted into labelled vectors after saving. Hence, they have to be transferred into factors again when loading the data set
dataSSY_big$freifreun_fac <- factor(dataSSY_big$freifreun)
dataSSY_big$pwmpeer_fac <- factor(dataSSY_big$pwmpeer)
dataSSY_big$pwmelt_fac <- factor(dataSSY_big$pwmelt)
dataSSY_big$freifam_fac <- factor(dataSSY_big$freifam)
dataSSY_big$partner_fac <- factor(dataSSY_big$partner)
dataSSY_big$par_edu_fac <- factor(dataSSY_big$par_edu)
dataSSY_big$female_cha_fac <- factor(dataSSY_big$female_cha)
dataSSY_big$sfach1_g3_fac <- factor(dataSSY_big$sfach1_g3)

# Number of missing values for the main variables of interest in the large sample:
# Semester abroad participation:
table(dataSSY_big$semester_abroad, useNA = "ifany")
# 0 NAs

# Hypothesis 1: 
table(dataSSY_big$indsocialpeers, useNA = "ifany")
# 22766 NAs out of 44965 (randomized 50/50 Split)

# Hypothesis 2: 
table(dataSSY_big$pwmpeer, useNA = "ifany")
# 15487 NAs out of 44965 (randomized 50/50 Split at one point in the question flow)

table(dataSSY_big$freifreun, useNA = "ifany")
# 24153 NAs out of 44965 (randomized 50/50 Split)

# Hypothesis 3: 
table(dataSSY_big$pwmelt, useNA = "ifany")
# 15461 NAs out of 44965 (randomized 50/50 Split at one point in the question flow)
table(dataSSY_big$freifam, useNA = "ifany")
# 24160 NAs out of 44965 (3/6 and 3/6 (i.e., similar to 50/50) split)
# Random sample splits in the survey design account for NAs between 35 and 55% in relevant variables! 

## Check which variables are the ones that account for the biggest reduction in sample size (due to missing values)
variables_NA <- c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")

# Create missing value summary
missing_table <- dataSSY_big %>%
  summarise(across(all_of(variables_NA), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_N"
  )

print(missing_table)

# exactly the social variables that are my main variables are the ones that account for most of the missing values! To ensure having the same number of observations in all regression models, I make my analyses with the "small sample" without any missings in the relevant variables.

# Comparison of Proportions, Means and SD in the Large and Small Sample (First part of data for Tables 2 and 3)----

## load the small data set
dataSSY_small<-read_sav("data/prepared/dataSSY_small.sav")

## same conversions as for the big data set have to be revised 
dataSSY_small$freifreun_fac <- factor(dataSSY_small$freifreun)
dataSSY_small$pwmpeer_fac <- factor(dataSSY_small$pwmpeer)
dataSSY_small$pwmelt_fac <- factor(dataSSY_small$pwmelt)
dataSSY_small$freifam_fac <- factor(dataSSY_small$freifam)
dataSSY_small$partner_fac <- factor(dataSSY_small$partner)
dataSSY_small$par_edu_fac <- factor(dataSSY_small$par_edu)
dataSSY_small$female_cha_fac <- factor(dataSSY_small$female_cha)
dataSSY_small$sfach1_g3_fac <- factor(dataSSY_small$sfach1_g3)

dataSSY_small$freifreun_fac <- relevel(dataSSY_small$freifreun_fac, ref = "3")
dataSSY_small$pwmpeer_fac <- relevel(dataSSY_small$pwmpeer_fac, ref = "1")
dataSSY_small$freifam_fac <- relevel(dataSSY_small$freifam_fac, ref = "3")
dataSSY_small$pwmelt_fac <- relevel(dataSSY_small$pwmelt_fac, ref = "1")
dataSSY_small$partner_fac <- relevel(dataSSY_small$partner_fac, ref = "0")
dataSSY_small$par_edu_fac <- relevel(dataSSY_small$par_edu_fac, ref = "0")
dataSSY_small$female_cha_fac <- relevel(dataSSY_small$female_cha_fac, ref = "0") # male as reference category
dataSSY_small$sfach1_g3_fac <- relevel(dataSSY_small$sfach1_g3_fac, ref = "6") # "Lehramt" as a reference category

# descriptive statistics with the small sample 
# List of variables
vars <- c("age30", "extro", "indsocialpeers", "pwmpeer",
  "pwmelt", "vsbnoteo", "deltwoh"
)

# Compute means for the large dataset
means_data_big <- sapply(
  dataSSY_big[vars],
  function(x) mean(x, na.rm = TRUE)
)

# Compute SD for the large dataset
sd_data_big <- sapply(
  dataSSY_big[vars],
  function(x) sd(x, na.rm = TRUE)
)

# Compute means for the small dataset
means_data_small <- sapply(
  dataSSY_small[vars],
  function(x) mean(x, na.rm = TRUE)
)

# Compute SD for the small dataset
sd_data_small <- sapply(
  dataSSY_small[vars],
  function(x) sd(x, na.rm = TRUE)
)

# Combine into a data frame for comparison
mean_sd_comparison <- data.frame(
  Variable = vars,
  Mean_data_big = means_data_big,
  SD_data_big = sd_data_big,
  Mean_data_small = means_data_small,
  SD_data_small = sd_data_small
)

# Print the table
print(mean_sd_comparison, row.names = FALSE)



# factor variables
factor_vars <- c("partner_fac", "sfach1_g3_fac", 
                 "freifreun_fac", "freifam_fac", 
                 "female_cha_fac", "par_edu_fac")

for (var in factor_vars) {
  
  # Data for first dataset
  vec1 <- as.character(dataSSY_big[[var]])
  tab1 <- prop.table(table(vec1))
  df1 <- as.data.frame(tab1)
  colnames(df1) <- c(var, "Proportion_data_large")
  
  # Data for second dataset
  vec2 <- as.character(dataSSY_small[[var]])
  tab2 <- prop.table(table(vec2))
  df2 <- as.data.frame(tab2)
  colnames(df2) <- c(var, "Proportion_data_small")
  
  # Merge into one table
  df <- merge(
    df1,
    df2,
    by = var,
    all = TRUE
  )
  
  cat("\nProportions comparison for:", var, "\n")
  print(df)
}

# in general minor changes in the distribution of important variables in both data sets. Thus: I use the small data set for regression analysis.
# Note: The remaining data for tables 2 and 3 (Descriptive statistics among the participation and non-participation group in the small sample) are perceived from the following analyses.


# Descriptive Statistics with the Small Sample----

## Semester Abroad Participation (Table 2)----
summary(dataSSY_small$semester_abroad)
# 4,49% of my sample have done exactly 1 semester abroad (and not having other experiences abroad) (Table 2)
barplot(100*prop.table(table(dataSSY_small$semester_abroad)), col=c("yellow"), ylab="share of people having participated in a semester abroad in %", 
        main="Distribution of Semester Abroad Participation", ylim = c(0,100))
# mode: no semester abroad
# median: no semester abroad

# Plot for visualising participation rate (not included in the thesis, just for purposes of own understanding)
# Preparation of the data
vis_sa <- as.data.frame(table(dataSSY_small$semester_abroad))
colnames(vis_sa) <- c("SemesterAbroad", "Count")
vis_sa$Percent <- 100 * vis_sa$Count / sum(vis_sa$Count)
vis_sa$SemesterAbroad <- factor(
  vis_sa$SemesterAbroad,
  levels = c(0, 1),
  labels = c("No Participation", "Participation"))

# Option 1: Create plot 
ggplot(vis_sa, aes(x = SemesterAbroad, y = Percent, fill = SemesterAbroad)) +
  geom_bar(stat = "identity", width = 0.6, color = NA) +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  labs(
    title = "Distribution of Semester Abroad Participation in the Sample",
    x = NULL,
    y = "Percentage of Respondents (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank() 
  ) +
  ylim(0, 100)


# Option 2
ggplot(vis_sa, aes(x = SemesterAbroad, y = Percent, fill = SemesterAbroad)) +
  
  # Light gray horizontal lines every 5 units, except 25/50/75/100
  geom_hline(
    yintercept = setdiff(seq(0, 100, 5), c(25, 50, 75, 100)),
    color = "gray90",
    size = 0.3
  ) +
  
  # Darker lines at 25, 50, 75, 100
  geom_hline(
    yintercept = c(25, 50, 75, 100),
    color = "gray50",
    size = 0.5
  ) +
  
  # Bars without border
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  
  # Y-axis breaks at 25-unit intervals
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  
  # Labels and theme
  labs(
    title = "Distribution of Semester Abroad Participation in the Sample",
    x = NULL,
    y = "Percentage of Respondents (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid = element_blank()  # Turn off default grid
  ) +
  ylim(0, 100)


## Social Embeddedness (+ Information for Table 1 and Data for Table 3)---- 
### Peers----
# Descriptives Peer Index ("indsocialpeers")
result<-corr.test(dataSSY_small[, c("sintkont", "sintfach", "sintsem")])
print(result, short=FALSE)

table(dataSSY_small$indsocialpeers)
summary(dataSSY_small$indsocialpeers)
hist(dataSSY_small$indsocialpeers) 
barplot(100*prop.table(table(dataSSY_small$indsocialpeers)), col=c("green"), ylab="Share of level of social integration across the observations in the sample (%)", 
        main="Distribution of Social Integration in the University Peer Network", ylim = c(0,25))

# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_indsocialpeers = mean(indsocialpeers, na.rm = TRUE),
    sd_indsocialpeers = sd(indsocialpeers, na.rm = TRUE),
    count = n()
  )

### Friends----
# Descriptives Meeting Friends ("freifreun")
table(dataSSY_small$freifreun)
class(dataSSY_small$freifreun)
levels(dataSSY_small$freifreun)
str(dataSSY_small$freifreun)
unique(dataSSY_small$freifreun)
barplot(100*prop.table(table(dataSSY_small$freifreun)), col=c("green"), ylab="Share of Respondents (%)",         main="Frequency of Meeting Friends in Students' Free Time", ylim = c(0,60))
summary(dataSSY_small$freifreun)
# pretty good normal distribution (with a small skew to the left)
# median: 4
# range: 1-6
# mode: 4


# Descriptives Motivation University Friends ("pwmpeer")
barplot(100*prop.table(table(dataSSY_small$pwmpeer)), col=c("green"), ylab="%",         main="Choosing university because of friends", ylim = c(0,60))
summary(dataSSY_small$pwmpeer)


# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_pwmpeer = mean(pwmpeer, na.rm = TRUE),
    sd_pwmpeer = sd(pwmpeer, na.rm = TRUE),
    count = n()
  )


### Family----
# Descriptives Meeting Family ("freifam")
table(dataSSY_small$freifam)
class(dataSSY_small$freifam)
levels(dataSSY_small$freifam)
str(dataSSY_small$freifam)
unique(dataSSY_small$freifam)
barplot(100*prop.table(table(dataSSY_small$freifam)), col=c("yellow"), ylab="Share of Respondents (%)",         main="Frequency of Meeting Family in Students' Free Time", ylim = c(0,60))
summary(dataSSY_small$freifam)
# okayish normal distribution 
# a bit right skewed
# median: 4
# mode: 3
# range: 1-6


# Descriptives Motivation University Family ("pwmelt")
barplot(100*prop.table(table(dataSSY_small$pwmelt)), col=c("green"), ylab="%",         main="Choosing university because of parents", ylim = c(0,60))
summary(dataSSY_small$pwmelt)
logpwmelt<-log(dataSSY_small$pwmelt)
table(logpwmelt)
# strong skew to the right 
# median: 2
# mode: 1
# mean: 2.58
# range: 1-5 

# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_pwmelt = mean(pwmelt, na.rm = TRUE),
    sd_pwmelt = sd(pwmelt, na.rm = TRUE),
    count = n()
  )


## Control Variables (+Information for Table 1 and Data for Table 3)----

# descriptives age by mobility group ("age30")
# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_age30 = mean(age30, na.rm = TRUE),
    sd_age30 = sd(age30, na.rm = TRUE),
    count = n()
  )


# descriptives sociability
table(dataSSY_small$extro)
class(dataSSY_small$extro)
levels(dataSSY_small$extro)
str(dataSSY_small$extro)
unique(dataSSY_small$extro)
barplot(100*prop.table(table(dataSSY_small$extro)), col=c("green"), ylab="%",         main="Being Extroverted", ylim = c(0,60))
summary(dataSSY_small$extro)
# not so well normally-distributed but okay
# median: 3
# mode: 4
# mean: 3.31
# range: 1-5

# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_extro = mean(extro, na.rm = TRUE),
    sd_extro = sd(extro, na.rm = TRUE),
    count = n()
  )


# relationship status
# dummy partner (Table 2)
summary(dataSSY_small$partner)
barplot(100*prop.table(table(dataSSY_small$partner)), col=c("green"), ylab="%",         main="Partnership: Yes or No", ylim = c(0,60))
# There are a few more people that are in a partnership than that are not.
# 53.2 % are in a partnership
# partner: 1: having a partner or being married; 0: being single



# Financial Resources 
# Option 1: pwmfin (Hochschulwahlmotiv: Ich habe meine aktuelle Hochschule gewählt, da ich aus finanziellen Gründen nicht fern vom Elternhaus studieren kann)
barplot(100*prop.table(table(dataSSY_small$pwmfin)), col=c("green"), ylab="%",         main="Financial issues are a reason to study close to parents", ylim = c(0,80))
summary(dataSSY_small$pwmfin)
logpwmfin<-log(dataSSY_small$pwmfin)
table(logpwmfin)
# strong right skew
# log-transformation not suitable
# mode: 1
# mean: 1.91
# median: 1
# 6 NAs

# Option 2: deltwoh (Parental financial wealth) (Table 3)
barplot(100*prop.table(table(dataSSY_small$deltwoh)), col=c("green"), ylab="%",         main="Perceived parents' wealth compared to other families", ylim = c(0,80))
summary(dataSSY_small$deltwoh)
# closely normally distributed!
# mean: 3 
# mode: 3
# median: 3
# range: 1-5


# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_deltwoh = mean(deltwoh, na.rm = TRUE),
    sd_deltwoh = sd(deltwoh, na.rm = TRUE),
    count = n()
  )



# gender (Table 2)
summary(dataSSY_small$female, na.rm = TRUE)
# 55.6 % of the sample are female
sum(dataSSY_small$female, na.rm = TRUE)       
length(na.omit(dataSSY_small$female))        
sd(dataSSY_small$female, na.rm = TRUE) 

# Average Grade
#histogram
x_min <- floor(min(dataSSY_small$vsbnoteo, na.rm = TRUE) * 10) / 10
x_max <- ceiling(max(dataSSY_small$vsbnoteo, na.rm = TRUE) * 10) / 10

# Create bin centers
centers <- seq(x_min, x_max, by = 0.1)

# Create breaks shifted by half a bin width
breaks <- centers - 0.05
breaks <- c(breaks, tail(breaks, 1) + 0.1)

hist(
  dataSSY_small$vsbnoteo,
  breaks = breaks,
  xaxt = "n",
  col = "skyblue",
  border = "white",
  main = "Histogram of vsbnoteo (Centered Bins)",
  xlab = "vsbnoteo"
)

# Add custom x-axis with centers
axis(1, at = centers)

# mean and SD by mobility group (for Table 3):
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    mean_vsbnoteo = mean(vsbnoteo, na.rm = TRUE),
    sd_vsbnoteo = sd(vsbnoteo, na.rm = TRUE),
    count = n()
  )



## Data for Table 2: Contingency tables and stacked bar charts----
# Note: I also computed frequency tables for categorical variables that are treated as quasi-metric variables in my thesis. Nevertheless, the results are not explicitly stated in my thesis, as means and SD were reported to consistently treat the variables as quasi-metric.


## Index Peers
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  tabyl(indsocialpeers, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$indsocialpeers)

# Bar chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(indsocialpeers_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, indsocialpeers_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = indsocialpeers_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(indsocialpeers_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Intergration in University peer network by Mobility Status",
    fill = "Degree of Integration"
  ) +
  ylim(0, 100)
# More social integration in the university peer network is associated with studying abroad.


## Meeting Friends
# Frequency Table
dataSSY_small %>%
  tabyl(freifreun, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$freifreun)
# the share of those who see their friends often is higher in the group that went abroad compared to the share within the group of those who did not go abroad.

# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(freifreun_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, freifreun_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = freifreun_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(freifreun_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Participation in a Semester Abroad",
    y = "%",
    title = "Frequency of Meeting Friends by Mobility Status",
    fill = "Frequency of Meeting Friends"
  ) +
  ylim(0, 100)


# Meeting Family
# Frequency Table
dataSSY_small %>%
  tabyl(freifam, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$freifam)
# there is a tendency that those who see their family more often are represented with a higher share in the non-participation group -> in line with my hypothesis 

# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(freifam_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, freifam_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = freifam_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(freifam_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Participation in a Semester Abroad",
    y = "%",
    title = "Frequency of Meeting Family by Mobility Status",
    fill = "Frequency of Meeting Family"
  ) +
  ylim(0, 100)



# Sociability
dataSSY_small %>%
  tabyl(extro, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$extro)
# Being more extroverted is positively associated with going abroad 

# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(extro_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, extro_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = extro_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(extro_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Degree of Self-Reported Extrovertedness by Mobility Status",
    fill = "I am an extrovert"
  ) +
  ylim(0, 100)


# Motivation University Family
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  tabyl(pwmelt, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$pwmelt)

# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(pwmelt_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, pwmelt_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = pwmelt_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(pwmelt_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Parents as a reason to study at study location",
    fill = "Degree of parents' importance"
  ) +
  ylim(0, 100)


# Motivation University Friends
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  tabyl(pwmpeer, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$pwmpeer)
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(pwmpeer_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, pwmpeer_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = pwmpeer_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(pwmpeer_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Friends as a reason to study at study location",
    fill = "Degree of Friends' importance"
  ) +
  ylim(0, 100)


# Relationship Status (Partner Dummy)
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  tabyl(partner, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$partner)
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(partner_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, partner_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = partner_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(partner_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Relationship Status by Mobility Status",
    fill = "Relationship: Yes (1) or No (0)"
  ) +
  ylim(0, 100)

# There is a quite equal number of people in a relationship and single people in both groups.

# Gender (female dummy)
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  filter(!is.na(female) & !is.na(semester_abroad)) %>%
  tabyl(female, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$female)


# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_small %>%
  filter(!is.na(female_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, female_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = female_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(female_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Gender Across Mobility Status",
    fill = "1: Female; 0: Male"
  ) +
  ylim(0, 100)

# summary by group (similar to what is shown in frequency table above already)
dataSSY_small %>%
  group_by(semester_abroad) %>%
  summarise(
    count_semesterabr = n(),  
    num_females = sum(female == 1, na.rm = TRUE),
    proportion_female = mean(female, na.rm = TRUE)
  )

# similar number of males and females in both groups.


# parental education
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  filter(!is.na(par_edu) & !is.na(semester_abroad)) %>%
  tabyl(par_edu, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$par_edu)


# major field of study
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_small %>%
  filter(!is.na(sfach1_g3_fac) & !is.na(semester_abroad)) %>%
  tabyl(sfach1_g3_fac, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_small$sfach1_g3_fac)


# Regressions ----
## Models 1, 3, 5, 7 (without control variables)----
### Model 1 (1st part of data for Table 4)----

data_m1 <- na.omit(dataSSY_small[, c("semester_abroad", "indsocialpeers")])
table(data_m1$semester_abroad)

# Fit model on cleaned data -> makes no difference for model computation but is important for doing the plot
model1 <- lm(semester_abroad ~ indsocialpeers, data = data_m1)

# Add predictions
data_m1$predicted <- predict(model1)

# Plot
ggplot(data_m1, aes(x = indsocialpeers, y = predicted)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted probabilities for participation in a semester abroad, dependent on\nsocial integration in the university peer network",
    x = "Degree of Social Integration in Peer Network",
    y = "Predicted Probability for Participation in a Semester Abroad"
  ) +
  theme_minimal()

summary(model1)
# Interpretation: An increase in social embeddedness in the university peer network is positively associated with an increase in the likelihood of studying abroad. 

# robust SE
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))  # Robust SEs
# no relevant difference with robust SEs

### Model 3 (1st part of data for Table 5)----
data_m3 <- na.omit(dataSSY_small[, c("semester_abroad", "freifreun_fac", "pwmpeer")])
table(data_m3$semester_abroad)
model3<-lm(semester_abroad~freifreun_fac+pwmpeer, data=data_m3)
summary(model3)

# Robust standard errors
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))  


# range test: Check how many observations get predictions out of the range between 0 and 1
# Make the range test
predicted_probs <- predict(model3)
range(predicted_probs)

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m3[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
## does not predict values higher than 1.
# 6 negative probabilities!

### Model 5 (1st part of data for Table 6)----
data_m5 <- na.omit(dataSSY_small[, c("semester_abroad", "freifam_fac", "pwmelt")])
table(data_m5$semester_abroad)

model5<-lm(semester_abroad~freifam_fac+pwmelt, data=data_m5)
summary(model5)

# Robust standard errors
coeftest(model5, vcov = vcovHC(model5, type = "HC1"))  

# range test:
predicted_probs <- predict(model5)
range(predicted_probs)

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m5[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
## does not predict values higher than 1 or lower than 0.

### Model 7 (1st part of data for Table 7)----
data_m7 <- na.omit(dataSSY_small[, c("semester_abroad", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac")])
table(data_m7$semester_abroad)

options (scipen=999)
model7<-lm(semester_abroad~., data=data_m7)
summary(model7)
# robust standard errors
coeftest(model7, vcov = vcovHC(model7, type = "HC1")) 

# Make the range test
predicted_probs <- predict(model7)
range(predicted_probs)

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m7[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# 12 negative probabilities.

# standardized coefficients
model7_beta <- lm.beta(model7)
summary(model7_beta)


## Models 2, 4, 6, 8 (with control variables)----

### Model 2 (2nd part of data for Table 4)----
data_m2 <- na.omit(dataSSY_small[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_m2$semester_abroad)
# There is more data available than in the very big final model.
options (scipen=999)
model2<-lm(semester_abroad~.+I(age30^2), data=data_m2)
summary(model2)
# robust standard errors
coeftest(model2, vcov = vcovHC(model2, type = "HC1")) 

# Make the range test
predicted_probs <- predict(model2)
range(predicted_probs)
# regression model also predicts negative probabilities!

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m2[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# for 309 observations, the predicted probabilities were negative 

### Model 4 (2nd part of data for Table 5)----
data_m4 <- na.omit(dataSSY_small[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "pwmpeer", "freifreun_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_m4$semester_abroad)
# There is less data available than for the model with all controls for hypothesis 1.
options (scipen=999)
model4<-lm(semester_abroad~.+I(age30^2), data=data_m4)
summary(model4)
# robust standard errors
coeftest(model4, vcov = vcovHC(model4, type = "HC1")) 

# Make the range test 
predicted_probs <- predict(model4)
range(predicted_probs)
# regression model also predicts negative probabilities!

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m4[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 329 (out of 1782) observations, my model estimated negative values which does not make sense in the case of probabilities. 

### Model 6 (2nd part of data for Table 6)----
data_m6 <- na.omit(dataSSY_small[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "pwmelt", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_m6$semester_abroad)

options (scipen=999)
model6<-lm(semester_abroad~.+I(age30^2), data=data_m6)
summary(model6)
# robust standard errors
coeftest(model6, vcov = vcovHC(model6, type = "HC1")) 

# Make the range test
predicted_probs <- predict(model6)
range(predicted_probs)
# regression model also predicts negative probabilities!

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m6[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 322 (out of 1782) observations, my model estimated negative values which does not make sense in the case of probabilities.  

### Model 8 (2nd part of data for Table 7)----
data_m8 <- na.omit(dataSSY_small[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_m8$semester_abroad)

options (scipen=999)
model8<-lm(semester_abroad~.+I(age30^2), data=data_m8)
summary(model8)
# robust standard errors
coeftest(model8, vcov = vcovHC(model8, type = "HC1")) 

# Make the range test
predicted_probs <- predict(model8)
range(predicted_probs)

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_m8[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 342 (out of 1782) observations, my model estimated negative values which does not make sense in the case of probabilities. 

# computing beta coefficients to compare effect sizes (based on observations from all semesters of interest and including all variables of interest)
model8_beta <- lm.beta(model8)
summary(model8_beta)






# Appendix----

## Appendix B: Robustness Check with the Large Sample----


### Descriptive Statistics with the Large Sample----

#### Semester Abroad Participation (Table 2)----
summary(dataSSY_big$semester_abroad)
# 4,77% of my sample have done exactly 1 semester abroad (and not having other experiences abroad) 
barplot(100*prop.table(table(dataSSY_big$semester_abroad)), col=c("yellow"), ylab="share of people having participated in a semester abroad in %", 
        main="Distribution of Semester Abroad Participation", ylim = c(0,100))
# mode: no semester abroad
# median: no semester abroad

# More beautiful version of the plot
# Preparation of the data
vis_sa <- as.data.frame(table(dataSSY_big$semester_abroad))
colnames(vis_sa) <- c("SemesterAbroad", "Count")
vis_sa$Percent <- 100 * vis_sa$Count / sum(vis_sa$Count)
vis_sa$SemesterAbroad <- factor(
  vis_sa$SemesterAbroad,
  levels = c(0, 1),
  labels = c("No Participation", "Participation"))

# Option 1: Create plot 
ggplot(vis_sa, aes(x = SemesterAbroad, y = Percent, fill = SemesterAbroad)) +
  geom_bar(stat = "identity", width = 0.6, color = NA) +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  labs(
    title = "Distribution of Semester Abroad Participation in the Large Sample",
    x = NULL,
    y = "Percentage of Respondents (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank() 
  ) +
  ylim(0, 100)


# Option 2
ggplot(vis_sa, aes(x = SemesterAbroad, y = Percent, fill = SemesterAbroad)) +
  
  # Light gray horizontal lines every 5 units, except 25/50/75/100
  geom_hline(
    yintercept = setdiff(seq(0, 100, 5), c(25, 50, 75, 100)),
    color = "gray90",
    size = 0.3
  ) +
  
  # Darker lines at 25, 50, 75, 100
  geom_hline(
    yintercept = c(25, 50, 75, 100),
    color = "gray50",
    size = 0.5
  ) +
  
  # Bars without border
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  
  # Y-axis breaks at 25-unit intervals
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  
  # Labels and theme
  labs(
    title = "Distribution of Semester Abroad Participation in the Large Sample",
    x = NULL,
    y = "Percentage of Respondents (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.grid = element_blank()  # Turn off default grid
  ) +
  ylim(0, 100)


####  Social Embeddedness (Table 2 and 3)----
##### Peers----
# Descriptives Index
table(dataSSY_big$indsocialpeers)
summary(dataSSY_big$indsocialpeers)
hist(dataSSY_big$indsocialpeers) 
barplot(100*prop.table(table(dataSSY_big$indsocialpeers)), col=c("green"), ylab="Share of level of social integration across the observations in the sample (%)", 
        main="Distribution of Social Integration in the University Peer Network", ylim = c(0,25))
# note: values in the middle are generally more frequent just by the construction of the index, as there are more possibilities to add to middle values than for values at the edges. Still, there is a skew to the left.
# mode: 4
# mean: 3.61
# range: 1-5
# median: 3.67

##### Friends----
# Descriptives Meeting Friends
table(dataSSY_big$freifreun)
class(dataSSY_big$freifreun)
levels(dataSSY_big$freifreun)
str(dataSSY_big$freifreun)
unique(dataSSY_big$freifreun)
barplot(100*prop.table(table(dataSSY_big$freifreun)), col=c("green"), ylab="Share of Respondents (%)",         main="Frequency of Meeting Friends in Students' Free Time", ylim = c(0,60))
summary(dataSSY_big$freifreun)
# pretty good normal distribution (with a small skew to the left)
# median: 4
# range: 1-6
# mode: 4

# Descriptives Motivation University Friends
barplot(100*prop.table(table(dataSSY_big$pwmpeer)), col=c("green"), ylab="%",         main="Choosing university because of friends", ylim = c(0,60))
summary(dataSSY_big$pwmpeer)


# Descriptives Meeting Family
table(dataSSY_big$freifam)
class(dataSSY_big$freifam)
levels(dataSSY_big$freifam)
str(dataSSY_big$freifam)
unique(dataSSY_big$freifam)
barplot(100*prop.table(table(dataSSY_big$freifam)), col=c("yellow"), ylab="Share of Respondents (%)",         main="Frequency of Meeting Family in Students' Free Time", ylim = c(0,60))
summary(dataSSY_big$freifam)
# okayish normal distribution 
# a bit right skewed
# median: 4
# mode: 3
# range: 1-6

##### Family----
# descriptives motivation university family
barplot(100*prop.table(table(dataSSY_big$pwmelt)), col=c("green"), ylab="%",         main="Choosing university because of parents", ylim = c(0,60))
summary(dataSSY_big$pwmelt)
logpwmelt<-log(dataSSY_big$pwmelt)
table(logpwmelt)
# log-transformation is not suitable!
# strong skew to the right 
# median: 2
# mode: 1
# mean: 2.51
# range: 1-5 


#### Control Variables (Tables 2 and 3)----

# Sociability
table(dataSSY_big$extro)
class(dataSSY_big$extro)
levels(dataSSY_big$extro)
str(dataSSY_big$extro)
unique(dataSSY_big$extro)
barplot(100*prop.table(table(dataSSY_big$extro)), col=c("green"), ylab="%",         main="Being Extroverted", ylim = c(0,60))
summary(dataSSY_big$extro)
# not so well normally-distributed but okay
# median: 3
# mode: 4
# mean: 3.3
# range: 1-5

# relationship status
# dummy partner
summary(dataSSY_big$partner)
barplot(100*prop.table(table(dataSSY_big$partner)), col=c("green"), ylab="%",         main="Partnership: Yes or No", ylim = c(0,60))
# There are a few more people that are in a partnership than that are not.
# 54.14 % are in a partnership
# partner: 1: having a partner or being married; 0: being single



# Financial situation/socioeconomic status 
# Option 1: pwmfin (Hochschulwahlmotiv: Ich habe meine aktuelle Hochschule gewählt, da ich aus finanziellen Gründen nicht fern vom Elternhaus studieren kann)
barplot(100*prop.table(table(dataSSY_big$pwmfin)), col=c("green"), ylab="%",         main="Financial issues are a reason to study close to parents", ylim = c(0,80))
summary(dataSSY_big$pwmfin)
logpwmfin<-log(dataSSY_big$pwmfin)
table(logpwmfin)
# strong skew to the right
# mode: 1
# mean: 1.93
# median: 1
# 15496 NAs

# Option 2: deltwoh 
barplot(100*prop.table(table(dataSSY_big$deltwoh)), col=c("green"), ylab="%",         main="Perceived parents' wealth compared to other families", ylim = c(0,80))
summary(dataSSY_big$deltwoh)
# closely normally distributed
# mean: 2.95 
# mode: 3
# median: 3
# range: 1-5
# only few NAs

# gender
summary(dataSSY_big$female, na.rm = TRUE)
# 54.32 % of the sample are female
sum(dataSSY_big$female, na.rm = TRUE)       
length(na.omit(dataSSY_big$female))        
sd(dataSSY_big$female, na.rm = TRUE) 

# Average Grade
#histogram
x_min <- floor(min(dataSSY_big$vsbnoteo, na.rm = TRUE) * 10) / 10
x_max <- ceiling(max(dataSSY_big$vsbnoteo, na.rm = TRUE) * 10) / 10

# Create bin centers
centers <- seq(x_min, x_max, by = 0.1)

# Create breaks shifted by half a bin width
breaks <- centers - 0.05
breaks <- c(breaks, tail(breaks, 1) + 0.1)

hist(
  dataSSY_big$vsbnoteo,
  breaks = breaks,
  xaxt = "n",
  col = "skyblue",
  border = "white",
  main = "Histogram of vsbnoteo (Centered Bins)",
  xlab = "vsbnoteo"
)

# Add custom x-axis with centers
axis(1, at = centers)



#### Contingency tables and stacked bar charts----

## Index Peers
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_big %>%
  tabyl(indsocialpeers, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$indsocialpeers)


# Bar chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(indsocialpeers_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, indsocialpeers_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = indsocialpeers_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(indsocialpeers_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Intergration in University peer network by Mobility Status",
    fill = "Degree of Integration"
  ) +
  ylim(0, 100)
# More social integration in the university peer network is associated with studying abroad.


## Meeting Friends
# Frequency Table
dataSSY_big %>%
  tabyl(freifreun, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$freifreun)

# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(freifreun_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, freifreun_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = freifreun_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(freifreun_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Participation in a Semester Abroad",
    y = "%",
    title = "Frequency of Meeting Friends by Mobility Status",
    fill = "Frequency of Meeting Friends"
  ) +
  ylim(0, 100)


# Meeting Family
# Frequency Table
dataSSY_big %>%
  tabyl(freifam, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$freifam)
# there is a tendency that those who see their family more often have a higher share among those who do not go abroad -> in line with my hypothesis 
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(freifam_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, freifam_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = freifam_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(freifam_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Participation in a Semester Abroad",
    y = "%",
    title = "Frequency of Meeting Family by Mobility Status",
    fill = "Frequency of Meeting Family"
  ) +
  ylim(0, 100)



# Sociability
dataSSY_big %>%
  tabyl(extro, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$extro)
# Being more extroverted is associated with going abroad (like in my (quasi) hypothesis!)
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(extro_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, extro_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = extro_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(extro_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Degree of Self-Reported Extrovertedness by Mobility Status",
    fill = "I am an extrovert"
  ) +
  ylim(0, 100)


# Motivation University Family
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_big %>%
  tabyl(pwmelt, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$pwmelt)
# relative amount of people with higher dependency on their parents when it comes to study location are present in the Y=1 group. However: more NAs!
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(pwmelt_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, pwmelt_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = pwmelt_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(pwmelt_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Parents as a reason to study at study location",
    fill = "Degree of parents' importance"
  ) +
  ylim(0, 100)


# Motivation University Friends
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_big %>%
  tabyl(pwmpeer, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$pwmpeer)
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(pwmpeer_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, pwmpeer_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = pwmpeer_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(pwmpeer_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Friends'/Peers' as a reason to study at study location",
    fill = "Degree of Friends'/Peers' importance"
  ) +
  ylim(0, 100)


# Relationship Status (Partner Dummy)
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_big %>%
  tabyl(partner, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$partner)
# Bar Chart
# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(partner_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, partner_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = partner_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(partner_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Relationship Status by Mobility Status",
    fill = "Relationship: Yes (1) or No (0)"
  ) +
  ylim(0, 100)

# There is a quite equal number of people in a relationship and not in both groups.

# Gender (female dummy)
# Frequency table with percentages by mobility status (columns = mobile/non-mobile)
dataSSY_big %>%
  filter(!is.na(female) & !is.na(semester_abroad)) %>%
  tabyl(female, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses
table(dataSSY_big$female)
# Interpretation: In the group that participated in a semester abroad, there are relevantly more women than men (62.3% female as opposed to 37.7% male)


# Prepare data for plotting (group by semester_abroad_cha)
plot_data <- dataSSY_big %>%
  filter(!is.na(female_cha), !is.na(semester_abroad_cha)) %>%
  count(semester_abroad_cha, female_cha) %>%
  group_by(semester_abroad_cha) %>%
  mutate(perc = 100 * n / sum(n))

# Plot: x = mobility, fill = female_cha
ggplot(plot_data, aes(x = semester_abroad_cha, y = perc, fill = fct_rev(female_cha))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Mobility Status (Semester Abroad)",
    y = "Percent",
    title = "Gender Across Mobility Status",
    fill = "1: Female; 0: Male"
  ) +
  ylim(0, 100)

# summary by group (similar to what is shown in frequency table above already)
dataSSY_big %>%
  group_by(semester_abroad) %>%
  summarise(
    count_semesterabr = n(),  
    num_females = sum(female == 1, na.rm = TRUE),
    proportion_female = mean(female, na.rm = TRUE)
  )



### Regression Analysis ----

# Ensure y is numeric
is.numeric(dataSSY_big$semester_abroad)
# Optional: check unique values
unique(dataSSY_big$semester_abroad)
table(dataSSY_big$semester_abroad)


#### Models L1, L3, L5, L7 (without control variables)----
##### Model L1 (1st part of data for table B2)----
# on data that has no missings in the respective columns
data_mL1 <- na.omit(dataSSY_big[, c("semester_abroad", "indsocialpeers")])
table(data_mL1$semester_abroad)
# smaller amount of observations I predict the model on
## no participation: 21123; participation: 1076

# Fit model on cleaned data -> makes no difference for model computation but is important for doing the plot
modelL1 <- lm(semester_abroad ~ indsocialpeers, data = data_mL1)

# Add predictions
data_mL1$predicted <- predict(modelL1)

# Plot
ggplot(data_mL1, aes(x = indsocialpeers, y = predicted)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted probabilities for participation in a semester abroad, dependent on\nsocial integration in the university peer network",
    x = "Degree of Social Integration in Peer Network",
    y = "Predicted Probability for Participation in a Semester Abroad"
  ) +
  theme_minimal()

summary(modelL1)
# both the intercept and the index are highly significant. 
# Interpretation: An increase in social embeddedness in the university peer network is positively associated with an increase in the likelihood of studying abroad. 

# robust SE
coeftest(modelL1, vcov = vcovHC(modelL1, type = "HC1"))  # Robust SEs
# no relevant difference with robust SEs

##### Model L3 (1st part of data for table B3)----
dataSSY_big$freifreun_fac <- relevel(dataSSY_big$freifreun_fac, ref = "3")
data_mL3 <- na.omit(dataSSY_big[, c("semester_abroad", "freifreun_fac", "pwmpeer")])
table(data_mL3$semester_abroad)
# smaller amount of observations I predict the model on
## no participation: 13168; participation: 576
### However, the share of people that went abroad is even a bit bigger in this subsample.
modelL3<-lm(semester_abroad~freifreun_fac+pwmpeer, data=data_mL3)
summary(modelL3)
# very small effects.
# intercept and pwmpeer are not significant
# freifreun is highly significant ***, especially for high frequency of meeting friends


# Robust standard errors
coeftest(modelL3, vcov = vcovHC(modelL3, type = "HC1"))  
# The coefficients rarely change but all levels of freifreun_fac become highly statistically significant!

# range test: Check how many observations get predictions out of the range between 0 and 1
# Make the range test
predicted_probs <- predict(modelL3)
range(predicted_probs)

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL3[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
## does not predict values higher than 1.
# 42 negative probabilities!
# attention: predicts negative probabilities if pwmpeer > 1
## does not predict values higher than 1.

##### Model L5 (1st part of data for table B4) ----
dataSSY_big$freifam_fac <- relevel(dataSSY_big$freifam_fac, ref = "3")
data_mL5 <- na.omit(dataSSY_big[, c("semester_abroad", "freifam_fac", "pwmelt")])
table(data_mL5$semester_abroad)
# smaller amount of observations I predict the model on
# no participation: 13174; participation: 577; slightly bigger sample

modelL5<-lm(semester_abroad~freifam_fac+pwmelt, data=data_mL5)
summary(modelL5)
# very small but significant effects
## intercept and pwmelt: ***

# Robust standard errors
coeftest(modelL5, vcov = vcovHC(modelL5, type = "HC1"))  


##### Model L7 (1st part of data for table B5)----
data_mL7 <- na.omit(dataSSY_big[, c("semester_abroad", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac")])
table(data_mL7$semester_abroad)
# 0: 6343; 1: 268 
options (scipen=999)
modelL7<-lm(semester_abroad~., data=data_mL7)
summary(modelL7)
# robust standard errors
coeftest(modelL7, vcov = vcovHC(modelL7, type = "HC1")) 

# Make the range test
predicted_probs <- predict(modelL7)
range(predicted_probs)

# These are the predictions that directly come from the regression line that I modeled! 
# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL7[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 98 (out of 6611) observations, my model estimated negative values which does not make sense in the case of probabilities. 

# standardized coefficients
modelL7_beta <- lm.beta(modelL7)
summary(modelL7_beta)


### Models L2, L4, L6, L8 (with control variables)----

#### Model L2 (2nd part of data for table B2)----
unique (dataSSY_big$female_cha_fac)
table(dataSSY_big$female_cha_fac)
dataSSY_big$female_cha_fac <- relevel(dataSSY_big$female_cha_fac, ref = "0") # male as reference category
unique (dataSSY_big$sfach1_g3_fac)
table(dataSSY_big$sfach1_g3_fac)
attributes(dataSSY_big$sfach1_g3)
dataSSY_big$sfach1_g3_fac <- relevel(dataSSY_big$sfach1_g3_fac, ref = "6") # "Lehramt" as a reference category
data_mL2 <- na.omit(dataSSY_big[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_mL2$semester_abroad)
# There is more data available than in the very big final model.
options (scipen=999)
modelL2<-lm(semester_abroad~.+I(age30^2), data=data_mL2)
summary(modelL2)
# robust standard errors
coeftest(modelL2, vcov = vcovHC(modelL2, type = "HC1")) 

# Make the range test
predicted_probs <- predict(modelL2)
range(predicted_probs)
# regression model also predicts negative probabilities!

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL2[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 1223 (out of 8714) observations, my model estimated negative values which does not make sense in the case of probabilities. 


#### Model L4 (2nd part of data for table B3)----
data_mL4 <- na.omit(dataSSY_big[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "pwmpeer", "freifreun_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_mL4$semester_abroad)
# There is less data available than for the model with all controls for hypothesis 1.
options (scipen=999)
modelL4<-lm(semester_abroad~.+I(age30^2), data=data_mL4)
summary(modelL4)
# robust standard errors
coeftest(modelL4, vcov = vcovHC(modelL4, type = "HC1")) 

# Make the range test 
predicted_probs <- predict(modelL4)
range(predicted_probs)
# regression model also predicts negative probabilities!
 
# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL4[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 580 (out of 3759) observations, my model estimated negative values which does not make sense in the case of probabilities. 


#### Model L6 (2nd part of data for table B4)----
data_mL6 <- na.omit(dataSSY_big[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "pwmelt", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_mL6$semester_abroad)
# Similar amount of data as for the big model in hypothesis 2.
options (scipen=999)
modelL6<-lm(semester_abroad~.+I(age30^2), data=data_mL6)
summary(modelL6)
# robust standard errors
coeftest(modelL6, vcov = vcovHC(modelL6, type = "HC1")) 

# Make the range test
predicted_probs <- predict(modelL6)
range(predicted_probs)
# regression model also predicts negative probabilities!

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL6[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# For 597 (out of 3753) observations, my model estimated negative values which does not make sense in the case of probabilities. 


#### Model 8 (same model as for the small sample and 2nd part of data for table B5)----
data_mL8 <- na.omit(dataSSY_big[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_mL8$semester_abroad)
# Same number of observations as in the small sample!

options (scipen=999)
modelL8<-lm(semester_abroad~.+I(age30^2), data=data_mL8)
summary(modelL8)
# robust standard errors
coeftest(modelL8, vcov = vcovHC(modelL8, type = "HC1")) 

# Make the range test
predicted_probs <- predict(modelL8)
range(predicted_probs)

# computing beta coefficients to compare effect sizes (based on observations from all semesters of interest and including all variables of interest)
modelL8_beta <- lm.beta(modelL8)
summary(modelL8_beta)


## Appendix A.1: Exclude first-semester students----
table(dataSSY_big$ssemhs_g1) # Hochschulsemester
attributes(dataSSY_big$ssemhs_g1)
table(dataSSY_big$ssemfa_g1) # Fachsemester
attributes(dataSSY_big$ssemhs_g1)

dataSSY_big %>%
  tabyl(ssemhs_g1, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses

dataSSY_big %>%
  tabyl(ssemfa_g1, semester_abroad) %>%
  adorn_totals("row") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()  # Adds raw counts in parentheses

test<-dataSSY_big

test$ssemhs_g1[test$ssemhs_g1 == -9990|test$ssemhs_g1 == -9982|test$ssemhs_g1 == -9996] <- NA

table(test$ssemhs_g1, useNA="ifany")


test<-test %>% filter(ssemhs_g1!=1)
table(test$semester_abroad)
table(test$ssemhs_g1, useNA="ifany")

2002/34039
# slightly higher participation share of 5.89%, but 2002 instead of 2143 semester abroad participants

dataSSY_big_no1<-test

# Run the same model but with another sample (without first semester students)
data_mL8_no1 <- na.omit(dataSSY_big_no1[, c("semester_abroad", "age30", "partner_fac", "female_cha_fac", "par_edu_fac", "extro", "indsocialpeers", "pwmpeer", "pwmelt", "freifreun_fac", "freifam_fac", "vsbnoteo", "deltwoh", "sfach1_g3_fac")])
table(data_mL8_no1$semester_abroad)
# number of observations: even less observations! 1220 in total
options (scipen=999)
modelL8_no1<-lm(semester_abroad~., data=data_mL8_no1)
summary(modelL8_no1)
coeftest(modelL8_no1, vcov = vcovHC(modelL8_no1, type = "HC1")) 

# Make the range test for this model
predicted_probs <- predict(modelL8_no1)
range(predicted_probs)
# range is in general bigger (more negative and also more positive) than in the model that includes first semester students

# Rows where predicted probability < 0 or > 1
out_of_bounds <- data_mL8_no1[predicted_probs < 0 | predicted_probs > 1, ]
# How many rows?
nrow(out_of_bounds)
# only 205 observations (out of 1889 in total) are estimated to be negative

# all semesters:
summary(modelL8)$r.squared
summary(modelL8)$adj.r.squared
sqrt(mean(residuals(modelL8)^2))

# no first semester students:
summary(modelL8_no1)$r.squared
summary(modelL8_no1)$adj.r.squared
sqrt(mean(residuals(modelL8_no1)^2))

# I include students from all semesters.

