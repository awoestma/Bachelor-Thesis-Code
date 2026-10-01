# Bachelor Thesis Amelie Wöstmann
# Title: Social Embeddedness and Participation in a Semester Abroad
# Script for Data Preparation

# set the working directory to your path
setwd(".../AW_bachelorthesis")
getwd()

# install necessary packages
install.packages("haven")
install.packages("psych")
install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("labelled")

# add necessary packages to the library
library(haven)
library(psych)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(forcats)
library(labelled)

# import the data
data_SSY <- read_sav("data/original/ssy21_bi_d_2-0-0_de.sav")

# Part 1: Preparation of the ("Large") Sample (<= 6 months)----
## voluntary stay abroad ----
### exclude the observations that obligatory went abroad
data_SSY %>% count(ainfpf1==2 | ainfpf2==2| ainfpf3==2|ainfpf4==2)
data_SSY<- data_SSY %>% filter(!(ainfpf1==2 | ainfpf2==2| ainfpf3==2|ainfpf4==2))

## German citizenship----
### only include students with a German citizenship
data_SSY %>% count(dnatdeu==1)
data_SSY<-data_SSY %>% filter(dnatdeu==1) 

## one-timers ----
### How many did at least 1 study-related stay abroad?
attributes(data_SSY$ainfaus_g1)
table(data_SSY$ainfaus_g1)
## 6592

### How many studied abroad (whole studies or semester)?
sum(data_SSY$ainfa4 == 1 | data_SSY$ainfa3 == 1 | data_SSY$ainfa2 == 1 | data_SSY$ainfa1 == 1)
### 3686
sum(data_SSY$ainfausart_g1==1)
### 3686
### the variable ainfausart_g1 counts the number of people that participated in a semester/study abroad.

### Only include one-timers (i.e., exclude those who have more than 1 "1" in one of the 4 columns about a stay abroad)
### Define the abroad columns
abroad_cols <- c("ainfa4", "ainfa3", "ainfa2", "ainfa1")

### Count how many 1s are in each row across those columns
abroad_ones <- rowSums(data_SSY[abroad_cols] == 1, na.rm = TRUE)

### Filter to keep rows where there is at most one '1'
data_SSY_one <- data_SSY[abroad_ones <= 1, ]
sum(data_SSY_one$ainfausart_g1==1)
data_SSY_one %>% count(ainfa4 == 1 | ainfa3 == 1 | ainfa2 == 1 | ainfa1 == 1)
### 3525 once studied abroad (1 semester or whole studies).

### Control if it worked: plug in ...==1 for ainfa1, ainfa2, ainfa3 und ainfa4 and check if there are other values "1" in the other columns. There were no other "1"s, so it worked! 
data_test<-data_SSY_one %>% filter(ainfa1==1) %>% select("id", "ainfa1", "ainfa2", "ainfa3", "ainfa4")
head(data_test)
data_test<-data_SSY_one %>% filter(ainfa2==1) %>% select("id", "ainfa1", "ainfa2", "ainfa3", "ainfa4")
head(data_test)
data_test<-data_SSY_one %>% filter(ainfa3==1) %>% select("id", "ainfa1", "ainfa2", "ainfa3", "ainfa4")
head(data_test)
data_test<-data_SSY_one %>% filter(ainfa4==1) %>% select("id", "ainfa1", "ainfa2", "ainfa3", "ainfa4")
head(data_test)
## It worked! There is only one "1" in one of the four columns for each observation.


## Y=0 without other experiences abroad----
### Exclude those who have not participated in a semester/studies abroad BUT in other experiences abroad
### Only have those in the data set that studied abroad and those who have no experience abroad at all (exclude other kinds of experience abroad and missing values)
### those coded with -9991 have not been asked about specifying their experiences abroad, as they do not have any experience abroad.
abroad_cols <- c("ainfa4", "ainfa3", "ainfa2", "ainfa1")
has_1<-rowSums(data_SSY_one[abroad_cols] == 1, na.rm = TRUE) >= 1
all_minus_9991 <- rowSums(data_SSY_one[abroad_cols] == -9991, na.rm = TRUE) == length(abroad_cols)
df_filtered <- data_SSY_one[has_1 | all_minus_9991, ]
table(df_filtered$ainfaus_g1)

### Control if it worked: 
data_test<-df_filtered %>% select("id", "ainfa1", "ainfa2", "ainfa3", "ainfa4")
head(data_test)
### It worked, as there are no observations that did not study abroad (1) but have other experiences abroad.
data_SSY_abroad_yesno <- df_filtered

sum(data_SSY_abroad_yesno$ainfa4 == 1 | data_SSY_abroad_yesno$ainfa3 == 1 | data_SSY_abroad_yesno$ainfa2 == 1 | data_SSY_abroad_yesno$ainfa1 == 1)
sum(data_SSY_abroad_yesno$ainfausart_g1==1)
### I kept all people that went abroad from above (3525). The variable ainfausart_g1 still captures those that went abroad for a semester or whole studies. 
row_has_one <- rowSums(data_SSY_abroad_yesno[abroad_cols] == 1, na.rm = TRUE) >= 1
sum(row_has_one)
### 3525 have done a semester or whole study abroad (and may have additionally other experiences abroad).
row_all_minus9991 <- rowSums(data_SSY_abroad_yesno[abroad_cols] == -9991, na.rm = TRUE) == length(abroad_cols)
sum(row_all_minus9991)
### 42822 have no study-related experience abroad at all

## duration <= 6 months----
### Only keep those whose study abroad experience (ainfa1234 == 1) was  <= 6 months
### Overview about how long the students were abroad for their semester/studies abroad. 

data_SSY_abroad_yesno %>% 
  filter(ainfa1 == 1) %>%
  count(ainfd1) %>% 
  print(n=26)
data_SSY_abroad_yesno %>% 
  filter(ainfa2 == 1) %>%
  count(ainfd2) %>% 
  print(n=26)
data_SSY_abroad_yesno %>% 
  filter(ainfa3 == 1) %>%
  count(ainfd3) %>% 
  print(n=26)
data_SSY_abroad_yesno %>% 
  filter(ainfa4 == 1) %>%
  count(ainfd4) %>% 
  print(n=26)

### Define column suffixes and construct column names
suffixes <- 4:1
abroad_cols <- paste0("ainfa", suffixes)
duration_cols <- paste0("ainfd", suffixes)

### Exclude those who have not answered how long their stay abroad was: Define invalid duration codes
invalid_codes <- c(-9990, -9991, -9996, -9993, -9982, -9981, -13, -967, -12, -11)

### For each pair, keep rows where:
# - No semester/study abroad experience, OR
# - Semester/study abroad experience AND duration is valid (<=6 and not in invalid codes)
valid_matrix <- mapply(function(abroad, duration) {
  data_SSY_abroad_yesno[[abroad]] != 1 | 
    (data_SSY_abroad_yesno[[abroad]] == 1 & data_SSY_abroad_yesno[[duration]] <= 6 & !(data_SSY_abroad_yesno[[duration]] %in% invalid_codes))
}, abroad_cols, duration_cols)

### Filter rows where all four conditions are TRUE
df_filtered <- data_SSY_abroad_yesno[rowSums(!valid_matrix) == 0, ]

### Check if everything worked
sum(
  (df_filtered$ainfa1 == 1 & (df_filtered$ainfd1 > 6 | df_filtered$ainfd1 %in% invalid_codes)) |
    (df_filtered$ainfa2 == 1 & (df_filtered$ainfd2 > 6 | df_filtered$ainfd2 %in% invalid_codes)) |
    (df_filtered$ainfa3 == 1 & (df_filtered$ainfd3 > 6 | df_filtered$ainfd3 %in% invalid_codes)) |
    (df_filtered$ainfa4 == 1 & (df_filtered$ainfd4 > 6 | df_filtered$ainfd4 %in% invalid_codes))
)

### Everything worked: There are no observations with study abroad (semester or study abroad) experiences longer than 6 months nor any with an undefined length of stay
data_SSY_semesterabr6<-df_filtered

### How is the duration of semester/study abroad distributed among one-timers (<= 6 months)?
data_SSY_semesterabr6 %>% 
  filter(ainfa1 == 1) %>%
  count(ainfd1) %>% 
  print(n=26)
data_SSY_semesterabr6 %>% 
  filter(ainfa2 == 1) %>%
  count(ainfd2) %>% 
  print(n=26)
data_SSY_semesterabr6 %>% 
  filter(ainfa3 == 1) %>%
  count(ainfd3) %>% 
  print(n=26)
data_SSY_semesterabr6 %>% 
  filter(ainfa4 == 1) %>%
  count(ainfd4) %>% 
  print(n=26)

### How many did a semester abroad 6 months or shorter?
sum(data_SSY_semesterabr6$ainfausart_g1==1)
### 2531 people studied abroad for 6 months or shorter (only once).

row_all_minus9991 <- rowSums(data_SSY_semesterabr6[abroad_cols] == -9991, na.rm = TRUE) == length(abroad_cols)
sum(row_all_minus9991)
### 42822 people do not have any study-related experience abroad.

### other experience abroad can be longer than 6 months: 
table(data_SSY_semesterabr6$ainfd2)



# Small Excurse (Appendix A.2)----
## Preparation of the Sample (<= 12 months) -> not used in the end! (Appendix A.2) ----

### Only keep those whose study abroad experience (ainfa1234 == 1) was <= 12 months

### Define column suffixes and construct column names
suffixes <- 4:1
abroad_cols <- paste0("ainfa", suffixes)
duration_cols <- paste0("ainfd", suffixes)

### Define invalid duration codes
invalid_codes <- c(-9990, -9991, -9996, -9993, -9982, -9981, -13, -967, -12, -11)

### For each pair, keep rows where:
# - No semester/studies abroad, OR
# - Semester/studies abroad AND duration is valid (<=12 and not in invalid codes)
valid_matrix <- mapply(function(abroad, duration) {
  data_SSY_abroad_yesno[[abroad]] != 1 | 
    (data_SSY_abroad_yesno[[abroad]] == 1 & data_SSY_abroad_yesno[[duration]] <= 12 & !(data_SSY_abroad_yesno[[duration]] %in% invalid_codes))
}, abroad_cols, duration_cols)

### Filter rows where all four conditions are TRUE
df_filtered <- data_SSY_abroad_yesno[rowSums(!valid_matrix) == 0, ]

### Check if everything worked
sum(
  (df_filtered$ainfa1 == 1 & (df_filtered$ainfd1 > 12 | df_filtered$ainfd1 %in% invalid_codes)) |
    (df_filtered$ainfa2 == 1 & (df_filtered$ainfd2 > 12 | df_filtered$ainfd2 %in% invalid_codes)) |
    (df_filtered$ainfa3 == 1 & (df_filtered$ainfd3 > 12 | df_filtered$ainfd3 %in% invalid_codes)) |
    (df_filtered$ainfa4 == 1 & (df_filtered$ainfd4 > 12 | df_filtered$ainfd4 %in% invalid_codes))
)

### Everything worked: There are no observations with study abroad (semester or study abroad) experiences longer than 12 months nor any with an undefined length of stay
data_SSY_semesterabr12<-df_filtered

### How is the duration of semester/study abroad distributed among one-timers (<= 12 months)?
data_SSY_semesterabr12 %>% 
  filter(ainfa1 == 1) %>%
  count(ainfd1) %>% 
  print(n=26)
data_SSY_semesterabr12 %>% 
  filter(ainfa2 == 1) %>%
  count(ainfd2) %>% 
  print(n=26)
data_SSY_semesterabr12 %>% 
  filter(ainfa3 == 1) %>%
  count(ainfd3) %>% 
  print(n=26)
data_SSY_semesterabr12 %>% 
  filter(ainfa4 == 1) %>%
  count(ainfd4) %>% 
  print(n=26)

### How many did a semester abroad 12 months or shorter?
sum(data_SSY_semesterabr12$ainfausart_g1==1)
### 3376 people studied abroad for 12 months or shorter (only once).

row_all_minus9991 <- rowSums(data_SSY_semesterabr12[abroad_cols] == -9991, na.rm = TRUE) == length(abroad_cols)
sum(row_all_minus9991)
### 42822 people do not have any study-related experience abroad.

3525-3376
### 3525: number of people with semester/study abroad participation without duration restriction (see above)
### 149 people studied abroad (once) for more than 12 months


## Exploration of another possibility to seperate whole studies from a semester abroad -> not used in the end! (Appendix A.2)----
## another possibility to filter for the data I am interested in: variable that shows if people already have graduated or aim to graduate in a degree abroad
attributes(data_SSY$sabser_r)
table(data_SSY$sabser_r)
attributes(data_SSY$sabsan_r)
table(data_SSY$sabsan_r)
## anonymized variables in my SUF version: no access to the data 
## Hence: Usage of aggregated data: sabser_g1 und sabsan_g1
attributes(data_SSY$sabser_g1)
table(data_SSY$sabser_g1)
attributes(data_SSY$sabsan_g1)
table(data_SSY$sabsan_g1)
## ==12 for both variables includes people that already got a degree abroad or aim to OR that already passed or aim to pass a an ecclesiastical examination. 

## apply it to the data sets that only include observations spending a semester/whole studies abroad for <= 6 months or <= 12 months
## 6 months
attributes(data_SSY_semesterabr6$sabser_g1)
table(data_SSY_semesterabr6$sabser_g1)
attributes(data_SSY_semesterabr6$sabsan_g1)
table(data_SSY_semesterabr6$sabsan_g1)
test<-data_SSY_semesterabr6 %>% filter(ainfa1==1|ainfa2==1|ainfa3==1|ainfa4==1)
table(test$sabser_g1)
table(test$sabsan_g1)
## In this data set, there are 170 observations that are pursuing a degree abroad or an ecclesiastical degree (including 10 who have also spent a semester/whole studies abroad) and 39 who have already obtained such a degree (including 5 among those who have also spent a semester/whole studies abroad)
## This suggests that those who have completed a degree abroad have probably been abroad for more than 6 months or a big part of the observations captured by level 12 of this variable have completed an ecclesiastical degree.

## Check the same thing within the sample of a duration of <= 12 months:
table(data_SSY_semesterabr12$sabsan_g1)
table(data_SSY_semesterabr12$sabser_g1)
test<-data_SSY_semesterabr12 %>% filter(ainfa1==1|ainfa2==1|ainfa3==1|ainfa4==1)
table(test$sabsan_g1)
table(test$sabser_g1)
## In this data set, there are 187 observations that are pursuing a degree abroad or an ecclesiastical degree (including 27 who have also spent a semester/whole studies abroad) and 46 who have already obtained such a degree (including 12 among those who have also spent a semester/whole studies abroad).
## This variable not suggest to be a good measure for differentiating between having spent only a semester or whole studies abroad. 

## Hence, I follow the approach of limiting the duration 
## I decided for the data set with a duration limit of 6 months
## However, I store the data set with a duration limit of 12 months but it is not further needed (except for possible future analyses and a robustness check that is not part of my work).
write_sav(data_SSY_semesterabr12,"data/prepared/dataSSY_semesterabr12.sav")


## I further follow the approach of limiting the duration to 6 months to for most closely differentiate between a semester and whole studies abroad.
data_SSY_semesterabr6 %>% count(ainfausart_g1==1)
data_SSY_semesterabr6 %>% count(ainfaus==1)
data_SSY_semesterabr6 %>% count(ainfaus==1|ainfaus==2|ainfaus==3|ainfaus==4)

# Part 2: Preparation of the ("Large") Sample (<= 6 months)----
## renaming some variables
data_SSY_semesterabr6<-data_SSY_semesterabr6 %>% 
  rename (
    semester_abroad = ainfausart_g1,
    number_abroad = ainfaus
  )

data_SSY_semesterabr6 %>% count(semester_abroad==1) # 2531 students did 1 semester abroad (but possibly additionally other study-related experiences)
data_SSY_semesterabr6 %>% count(number_abroad==1) # this variable shows how many experiences abroad in total an observation has done (also including other study-related experiences apart from a semester abroad)
table(data_SSY_semesterabr6$number_abroad)
## there are some students that have done several experiences abroad.

## Check if there are still students captured by the variable "number_abroad" that have done several semesters abroad:
data_SSY_ONLYsemesterabr6<- data_SSY_semesterabr6 %>% filter(ainfausart_g2!=1 & ainfausart_g3!=1 & ainfausart_g4!=1 & ainfausart_g5!=1 & ainfausart_g6!=1 & ainfausart_g7!=1)
table(data_SSY_ONLYsemesterabr6$number_abroad)
test<-data_SSY_ONLYsemesterabr6 %>% select(ainfa1, ainfa2, ainfa3, ainfa4, semester_abroad, number_abroad)
## there are 9 observations that have done 2 semesters/whole studies abroad although only 1 of them was among their last 4 experiences. They are excluded from the data set now: 
data_SSY_ONLYsemesterabr6<-data_SSY_ONLYsemesterabr6 %>% filter(number_abroad!=2)
table(data_SSY_ONLYsemesterabr6$number_abroad)
## There are 2143 students that were exactly once in a semester abroad (according to my definitions) for <= 6 months. These refer to Y=0 according to my definitions.
## 379 (+9) students were excluded, as they have participated in a semester abroad exactly once (or twice but this was not among their last 4 semesters abroad) but also participated in at least one more study-related experience abroad. 

final_ONLY6<-data_SSY_ONLYsemesterabr6

test<-final_ONLY6 %>% select(ainfa1, ainfa2, ainfa3, ainfa4, ainfaus_g1)
table(final_ONLY6$ainfaus_g1)
table(final_ONLY6$semester_abroad)
## 2143 students did exactly 1 semester abroad and had no other experience abroad.
## semester_abroad is the name of my dependent variable!


## Social Embeddedness Variables----
### Peers: Creation "Index peers"----
### Before the index was created, basic statistics of the underlying variables were analysed to assess the suitability of an index creation:
### sintkont, sintfach, sintsem
attributes(final_ONLY6$sintkont)
table(final_ONLY6$sintkont)
final_ONLY6$sintkont [final_ONLY6$sintkont == -9990|final_ONLY6$sintkont == -9991] <- NA
hist(final_ONLY6$sintkont)
summary(final_ONLY6$sintkont)

attributes(final_ONLY6$sintfach)
table(final_ONLY6$sintfach)
final_ONLY6$sintfach [final_ONLY6$sintfach == -9990|final_ONLY6$sintfach == -9991] <- NA
hist(final_ONLY6$sintfach)
summary(final_ONLY6$sintfach)

attributes(final_ONLY6$sintsem)
table(final_ONLY6$sintsem)
final_ONLY6$sintsem [final_ONLY6$sintsem == -9990|final_ONLY6$sintsem == -9991] <- NA
hist(final_ONLY6$sintsem)
summary(final_ONLY6$sintsem)
### all 3 variables are measured on the same scale and meausure similar constructs -> in favour of creation of an index
### all 3 variables are skewed to the left 

### Check for correlation of the variables
cor(final_ONLY6[, c("sintkont", "sintfach", "sintsem")], use = "complete.obs")
### Compute correlations with significance

result<-corr.test(final_ONLY6[, c("sintkont", "sintfach", "sintsem")])
print(result, short=FALSE)
### The three variables all correlate positively and quite strongly and this is statistically significant.

### Creation of an index "indsocialpeers"
indsocialpeers<-(final_ONLY6$sintkont+final_ONLY6$sintfach+final_ONLY6$sintsem)/3
final_ONLY6$indsocialpeers <- indsocialpeers # Add it to the data set
table(final_ONLY6$indsocialpeers)

### Friends----
#### Meeting Friends----
#### Cleaning of the variable
attributes(final_ONLY6$pfreifreun)
table(final_ONLY6$pfreifreun)
#### remove missings:
final_ONLY6$pfreifreun [final_ONLY6$pfreifreun == -9990|final_ONLY6$pfreifreun == -9991|final_ONLY6$pfreifreun == -9993] <- NA
final_ONLY6 <- final_ONLY6 %>% 
  rename(freifreun = pfreifreun)

#### Motivation University Friends----
attributes(final_ONLY6$pwmpeer)
table(final_ONLY6$pwmpeer)
final_ONLY6$pwmpeer [final_ONLY6$pwmpeer == -9990|final_ONLY6$pwmpeer == -9991|final_ONLY6$pwmpeer == -9993] <- NA

### Family----
#### Meeting family----
### Cleaning of the variable
attributes(final_ONLY6$pfreifam)
table(final_ONLY6$pfreifam)
final_ONLY6$pfreifam [final_ONLY6$pfreifam == -9990|final_ONLY6$pfreifam == -9991|final_ONLY6$pfreifam == -9993] <- NA
final_ONLY6 <- final_ONLY6 %>% 
  rename(freifam = pfreifam)

#### Motivation University Family----
#### Cleaning the variable
attributes(final_ONLY6$pwmelt)
table(final_ONLY6$pwmelt)
final_ONLY6$pwmelt [final_ONLY6$pwmelt == -9990|final_ONLY6$pwmelt == -9991|final_ONLY6$pwmelt == -9993] <- NA


## Control Variables----
## Sociability
attributes(final_ONLY6$pbigextro)
table(final_ONLY6$pbigextro)
final_ONLY6$pbigextro [final_ONLY6$pbigextro == -9990|final_ONLY6$pbigextro == -9991|final_ONLY6$pbigextro == -9993] <- NA
final_ONLY6 <- final_ONLY6 %>% 
  rename(extro = pbigextro)

## Relationship status "demofam"
attributes(final_ONLY6$demofam)
table(final_ONLY6$demofam)
final_ONLY6$demofam [final_ONLY6$demofam == -9990|final_ONLY6$demofam == -13] <- NA
## create a dummy variable
## look at levels and distribution from the initial variable
barplot(100*prop.table(table(final_ONLY6$demofam)), col=c("green"), ylab="%",         main="Relationship Status", ylim = c(0,60))
final_ONLY6$partner <- ifelse(final_ONLY6$demofam == 1, 0, 
                              ifelse(final_ONLY6$demofam %in% c(2, 3), 1, NA))
table(final_ONLY6$demofam)
table(final_ONLY6$partner)
## partner: 1: having a partner or being married; 0: being single


## Financial situation/socioeconomic status (control for living with parents): 
### Option 1: pwmfin (Hochschulwahlmotiv: Ich habe meine aktuelle Hochschule gewählt, da ich aus finanziellen Gründen nicht fern vom Elternhaus studieren kann)
attributes(final_ONLY6$pwmfin)
table(final_ONLY6$pwmfin)
final_ONLY6$pwmfin [final_ONLY6$pwmfin == -9990|final_ONLY6$pwmfin == -9991] <- NA
### Option 2: deltwoh 
### advantage: much more normally distributed than the other variable (see descriptives)
attributes(final_ONLY6$deltwoh)
table(final_ONLY6$deltwoh)
final_ONLY6$deltwoh [final_ONLY6$deltwoh == -9990] <- NA

## Further Preparations for Descriptive and Regression Analysis----
### Creation of Character Variables----
### Index and semester abroad
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    indsocialpeers_cha = as.character(as_factor(indsocialpeers)),
    semester_abroad_cha = as.character(as_factor(semester_abroad))
  )

### Meeting Friends
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    freifreun_cha = as.character(as_factor(freifreun))
  )

### Motivation University Friends
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    pwmpeer_cha = as.character(as_factor(pwmpeer))
  )


### Meeting Family
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    freifam_cha = as.character(as_factor(freifam))
  )

### Motivation University Family
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    pwmelt_cha = as.character(as_factor(pwmelt))
  )


### Sociability
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    extro_cha = as.character(as_factor(extro))
  )

### Relationship Status (Partner Dummy)
final_ONLY6$partner[final_ONLY6$partner==""]<-NA
final_ONLY6 <- final_ONLY6 %>%
  mutate(
    partner_cha = as.character(as_factor(partner))
  )

final_ONLY6_regressions<-final_ONLY6 

### Creation of Factor Variables for Categorical Variables----
final_ONLY6_regressions$freifreun_fac <- factor(final_ONLY6_regressions$freifreun)
final_ONLY6_regressions$pwmpeer_fac <- factor(final_ONLY6_regressions$pwmpeer)
final_ONLY6_regressions$freifam_fac <- factor(final_ONLY6_regressions$freifam)
final_ONLY6_regressions$partner_fac <- factor(final_ONLY6_regressions$partner)


### gender
final_ONLY6_regressions<-final_ONLY6_regressions %>% 
  rename (gender = demosex_g1)
attributes(final_ONLY6_regressions$gender)
### 1: female; 2: male
table(final_ONLY6_regressions$gender)
final_ONLY6_regressions$gender [final_ONLY6_regressions$gender == -9996] <- NA

### create a dummy where 1 == female and 0 == male
final_ONLY6_regressions <- final_ONLY6_regressions %>%
  mutate(female = ifelse(gender == 1, 1, 
                         ifelse(gender == 2, 0, NA)))
table(final_ONLY6_regressions$female, useNA = "ifany")

### for bar chart: 
final_ONLY6_regressions <- final_ONLY6_regressions %>%
  mutate(
    female_cha = as.character(as_factor(female)))


### age
table(final_ONLY6_regressions$demoage_r)
# The specific ages are anonymized. This is why I use the top-bottom coding "demoage_g1"

final_ONLY6_regressions<-final_ONLY6_regressions %>% 
  rename (age = demoage_g1)

table(final_ONLY6_regressions$age)
final_ONLY6_regressions$age [final_ONLY6_regressions$age == -9990] <- NA
summary(final_ONLY6_regressions$age, na.rm==T)
test<-final_ONLY6_regressions %>% filter(semester_abroad==1)
table(test$age)

### Create a second variable where I exclude the values for age > 30 to measure it metrically
final_ONLY6_regressions$age30<-final_ONLY6_regressions$age
final_ONLY6_regressions$age30[final_ONLY6_regressions$age == 31] <- NA
table(final_ONLY6_regressions$age30)


### Major Field of Study
### Education (Lehramt), Social Sciences, Arts, Humanities, Engineering, Business
final_ONLY6_regressions <- final_ONLY6_regressions %>%
  mutate(teaching = if_else(sabsan_g3 == 3, 1, 0))
test<-final_ONLY6_regressions %>% select(teaching, sfach1_g3)#
teaching1<-final_ONLY6_regressions %>% filter(teaching==1)
attributes(final_ONLY6_regressions$sfach1_g3)
table(teaching1$sfach1_g3)
#### The teachers are also captured with their main fields of study/teaching in the variable "sfach1_g3". Thus, I want to extract the teachers from the variable
table(final_ONLY6_regressions$sfach1_g3)
final_ONLY6_regressions$sfach1_g3[final_ONLY6_regressions$sfach1_g3==-9990|final_ONLY6_regressions$sfach1_g3==-9982]<-NA
table(final_ONLY6_regressions$sfach1_g3, useNA = "ifany")
### Now I add "teachers" as a new category to the variable with the value 6 and "shift" the observations being teachers from the old categories to the new one.
final_ONLY6_regressions <- final_ONLY6_regressions %>%
  mutate(sfach1_g3 = if_else(teaching == 1, 6, sfach1_g3))
table(final_ONLY6_regressions$sfach1_g3, useNA = "ifany")
table(final_ONLY6_regressions$teaching)
### it worked!
2468+149+732+1392+106+45+109+255+32
### these numbers correspond to the differences in values within the different levels of the variable sfach1_g3 before and after the mutation. These perfectly correspond to the number of teachers that are now catched in the level 6 (=5288).

### add the new label "Lehramt" for the level 6 to the variable "sfach1_g3"
attributes(final_ONLY6_regressions$sfach1_g3)
val_labels(final_ONLY6_regressions$sfach1_g3)
### Get existing labels
old_labels <- val_labels(final_ONLY6_regressions$sfach1_g3)
### Add the new label
new_labels <- c(old_labels, "Lehramt" = 6)
final_ONLY6_regressions <- final_ONLY6_regressions %>%
  mutate(sfach1_g3 = labelled(sfach1_g3, labels = new_labels))
attributes(final_ONLY6_regressions$sfach1_g3)


### Education of parents: akademisches oder nicht akademisches Elternhaus (akadse20_g1)
### 2 categories: academic (if at least 1 parent has an academic degree) or not (if no parent has an academic degree)
### quite balanced, not too many categories to not be overwhelmed in the very big model.
attributes(final_ONLY6_regressions$akadse20_g1)
final_ONLY6_regressions$akadse20_g1[final_ONLY6_regressions$akadse20_g1==-9996|final_ONLY6_regressions$akadse20_g1==1]<-NA
table(final_ONLY6_regressions$akadse20_g1)
final_ONLY6_regressions$par_edu <- ifelse(final_ONLY6_regressions$akadse20_g1 == 2, 0, 
                                          ifelse(final_ONLY6_regressions$akadse20_g1 == 3, 1, NA))
table(final_ONLY6_regressions$par_edu)
### it worked! 1: at least 1 parent has academic education; 0: both parents do not have academic education
final_ONLY6_regressions$par_edu_fac <- factor(final_ONLY6_regressions$par_edu)


### Average grade
### vsbnoteo (Durchschnittsnote der Studienzugangsberechtigung)
attributes(final_ONLY6_regressions$vsbnoteo)
table(final_ONLY6_regressions$vsbnoteo)
final_ONLY6_regressions$vsbnoteo[final_ONLY6_regressions$vsbnoteo==-9991|final_ONLY6_regressions$vsbnoteo==-9990|final_ONLY6_regressions$vsbnoteo==-9982|final_ONLY6_regressions$vsbnoteo==-9981]<-NA
table(final_ONLY6_regressions$vsbnoteo)

### female dummy for regression: 
# female
final_ONLY6_regressions$female_cha_fac <- factor(final_ONLY6_regressions$female_cha)


### sfach1_g3 dummy for regression:
final_ONLY6_regressions$sfach1_g3_fac <- factor(final_ONLY6_regressions$sfach1_g3)


# Creation of the "Small" Sample (my main sample for the analysis)----
## Reduction of the sample to the size of regression model 8 to ensure having the exact same sample in all models. This sample is also used for all descriptive analyses.
vars_NA <- c("semester_abroad", "age30", "partner_fac", "female_cha_fac", 
                   "par_edu_fac", "extro", "indsocialpeers", "pwmpeer", 
                   "pwmelt", "freifreun_fac", "freifam_fac", 
                   "vsbnoteo", "deltwoh", "sfach1_g3_fac")
## Keep all columns, drop rows where any listed variables are NA
data_regressions_small <- final_ONLY6_regressions[complete.cases(final_ONLY6_regressions[, vars_NA]), ]

## Creation of factor variables for categorical variables
data_regressions_small$freifreun_fac <- factor(data_regressions_small$freifreun)
data_regressions_small$pwmpeer_fac <- factor(data_regressions_small$pwmpeer)
data_regressions_small$pwmelt_fac <- factor(data_regressions_small$pwmelt)
data_regressions_small$freifam_fac <- factor(data_regressions_small$freifam)
data_regressions_small$partner_fac <- factor(data_regressions_small$partner)
data_regressions_small$par_edu_fac <- factor(data_regressions_small$par_edu)
data_regressions_small$female_cha_fac <- factor(data_regressions_small$female_cha)
data_regressions_small$sfach1_g3_fac <- factor(data_regressions_small$sfach1_g3)


write_sav(data_regressions_small, "data/prepared/dataSSY_small.sav")
# note: the factors are converted into labelled vectors after saving. Hence, they have to be transferred into factors again when loading the data set(see other script)

write_sav(final_ONLY6_regressions, "data/prepared/dataSSY_big.sav")




# Appendix ----
## Appendix A.1: Another Version of the Sample: including other experiences abroad----
## Y=0 are not the students without any experiences abroad but those who may also have other experiences abroad (apart from a semester/studies abroad)
data_appendix1<-data_SSY %>% filter(ainfa1!=1 & ainfa2!=1 & ainfa3!=1 & ainfa4!=1)
data_appendix1 <- data_SSY %>%
  filter(
    ainfa1 != 1 & ainfa2 != 1 & ainfa3 != 1 & ainfa4 != 1,
    !if_any(c(ainfa1, ainfa2, ainfa3, ainfa4), ~ .x %in% c(-9993, -9990, -9982, -9981))
  )
# Note: This data set only contains the group Y=0!. Hence, 45467 students have not done a semester abroad (but other experiences abroad) or no experiences abroad at all
table(data_appendix1$ainfaus_g1)
table(data_appendix1$ainfa1)
# 6592 have other study-related experiences, apart from a semester or whole degree program abroad.
# However, I decided against the usage of this data set because of a more straightforward interpretability and because the number of students in this group is not relevantly larger. Nevertheless, this sample can be used as a robustness check in  future research.


## Appendix C: Variables that were initially planned but not included in my work ----
### Potential for Future Research: Social Embeddedness/Attachment to Partner---- 
### motive for choosing university: partner
data_appendix2<-final_ONLY6_regressions
attributes(data_appendix2$pwmpar)
table(data_appendix2$pwmpar)
data_appendix2$pwmpar [data_appendix2$pwmpar == -9990|data_appendix2$pwmpar == -9991|data_SSY_ONLYsemesterabr6$pwmpar == -9993] <- NA
barplot(100*prop.table(table(data_appendix2$pwmpar)), col=c("green"), ylab="%",         main="Choosing university because of partner", ylim = c(0,80))
summary(data_appendix2$pwmpar)
# very strong skew to the right and mode at 1 but there must be the underlying variable of having a partner or not! 
# median: 1
# mode: 1
# mean: 1.73

### Potential for Future Research: Control Variable Living Situation----
# Living with parents during the lecture period
attributes(data_appendix2$wohnel)
table(data_appendix2$wohnel)
data_appendix2$wohnel [data_appendix2$wohnel == -9990|data_appendix2$wohnel == -9991|data_appendix2$wohnel == -9982|data_appendix2$wohnel == -9981] <- NA
barplot(100*prop.table(table(data_appendix2$wohnel)), col=c("green"), ylab="%",         main="Living with parents: Yes or No", ylim = c(0,80))
summary(data_appendix2$wohnel)
# 23.8 % of the respondents live with parents
# those who live in a Studentenwohnheim and who do not live in a Mehrzimmerwohnung did not see the question.

# Living with partner during the lecture period
attributes(data_appendix2$wohnpar)
table(data_appendix2$wohnpar)
data_appendix2$wohnpar [data_appendix2$wohnpar == -9990|data_appendix2$wohnpar == -9991|data_appendix2$wohnpar == -9982|data_appendix2$wohnpar == -9981] <- NA
barplot(100*prop.table(table(data_appendix2$wohnpar)), col=c("green"), ylab="%",         main="Living with partner: Yes or No", ylim = c(0,80))
summary(data_appendix2$wohnpar)
# 23,9 % of the respondents live with their partner. This is very similar to the number of respondents that live with their parents.
# those who live in a Studentenwohnheim and who do not live in a Mehrzimmerwohnung did not see the question.

# Living with peers in a shared flat during the lecture period
attributes(data_appendix2$wohnwg)
table(data_appendix2$wohnwg)
data_appendix2$wohnwg [data_appendix2$wohnwg == -9990|data_appendix2$wohnwg == -9991|data_appendix2$wohnwg == -9981] <- NA
barplot(100*prop.table(table(data_appendix2$wohnwg)), col=c("green"), ylab="%",         main="Living in a shared flat: Yes or No", ylim = c(0,80))
summary(data_appendix2$wohnwg)
# 33.2 % (~ 1/3) of the respondents live (with peers) in a shared flat (WG). Thereby, there are more people living in shared flats than living with their partner or parents.
# those who live in a Studentenwohnheim and who do not live in a Mehrzimmerwohnung did not see the question.


# Creation of a dummy variable for the living situation 
table(data_appendix2$wohnpar==1 & data_appendix2$wohnel ==1)
table(data_appendix2$wohnpar==1 & data_appendix2$wohnwg ==1)
table(data_appendix2$wohnel==1 & data_appendix2$wohnwg ==1)
# The levels of this variable would not be mutually exclusive! 
table(data_appendix2$wohnpar==1 & data_appendix2$wohnel ==1 & data_appendix2$wohnwg==1)

# Create a dummy variable for living situation with mutually exclusive categories
subset_ls <- data_appendix2 %>% filter(!((wohnpar == 1 & wohnel == 1) |
                                        (wohnpar == 1 & wohnwg == 1) |
                                        (wohnel == 1 & wohnwg == 1) |
                                        (wohnpar == 1 & wohnel == 1 & wohnwg == 1)))
table(subset_ls$wohnpar==1 & subset_ls$wohnel ==1)
table(subset_ls$wohnpar==1 & subset_ls$wohnwg ==1)
table(subset_ls$wohnel==1 & subset_ls$wohnwg ==1)
table(subset_ls$wohnpar==1 & subset_ls$wohnel ==1 & subset_ls$wohnwg==1)
# It worked!

subset_ls$dumls3 <- NA  # initialize
subset_ls$dumls3[subset_ls$wohnpar == 1] <- "partner"
subset_ls$dumls3[subset_ls$wohnwg == 1] <- "shared flat"
subset_ls$dumls3[subset_ls$wohnel == 1] <- "parents"
table(subset_ls$dumls3)
table(subset_ls$wohnpar)
table(subset_ls$wohnwg)
table(subset_ls$wohnel)
# It worked!

# Add variable to the old data set and set missing values to NA
data_appendix2 <- data_appendix2 %>%
  left_join(select(subset_ls, id, dumls3), by = "id")
table(data_appendix2$dumls3, useNA = "ifany")
table(data_appendix2$wohnpar, useNA = "ifany")
table(data_appendix2$wohnel, useNA = "ifany")
table(data_appendix2$wohnwg, useNA = "ifany")
table(data_appendix2$semester_abroad==1 & data_appendix2$dumls3=="shared flat")
table(data_appendix2$semester_abroad==1 & data_appendix2$dumls3=="parents")
table(data_appendix2$semester_abroad==1 & data_appendix2$dumls3=="partner")

# Replace empty strings with NA
data_appendix2$dumls3[data_appendix2$dumls3 == ""] <- NA

# Descriptives of the new dummy variable
table(data_appendix2$dumls3, useNA = "ifany")
# Create the table including NAs
dumls_tab <- table(data_appendix2$dumls3, useNA = "ifany")

# Convert to proportions and scale to percent
dumls_prop <- 100 * prop.table(dumls_tab)

# Rename NA category to "others"
names(dumls_prop)[is.na(names(dumls_prop)) | names(dumls_prop) == "NA"] <- "others"

# Plot
barplot(dumls_prop,
        col = "green",
        ylab = "%",
        main = "Living situation during the lecture period",
        ylim = c(0, 80))
prop.table(table(data_appendix2$dumls3, useNA = "ifany"))
# 20% live with their parents, 19.69% with their partner and 28.32% in a shared flat. 31.97% have not answered the question or live in other constellations

# initial proportions -> are very similar to new ones from the dummy variable!
proportions <- 100 * c(
  mean(data_appendix2$wohnel == 1, na.rm = TRUE),
  mean(data_appendix2$wohnpar == 1, na.rm = TRUE),
  mean(data_appendix2$wohnwg == 1, na.rm = TRUE)
)
# Create names for the bars
names(proportions) <- c("wohnel", "wohnpar", "wohnwg")
# Create barplot
barplot(proportions,
        col = "green",
        ylab = "%",
        main = "Living Situations (Yes = 1)",
        ylim = c(0, 80))


# In regression analysis: 
# Living situation
## make dumls3 a factor -> dummy variable
data_appendix2$dumls3_fac <- factor(data_appendix2$dumls3)
data_appendix2$dumls3_fac [data_appendix2$dumls3_fac == ""] <- NA
data_appendix2$dumls3_fac <- relevel(data_appendix2$dumls3_fac, ref = "shared flat")
data_ls <- na.omit(data_appendix2[, c("semester_abroad", "dumls3_fac")])
summary(lm(semester_abroad~dumls3_fac, data=data_appendix2))
# Interpretation: Living with parents or with partner as opposed to living in a shared flat is both highly significantly negatively associated with participation in a semester abroad.

write_sav(data_appendix2,"data/prepared/data_appendix2.sav")
