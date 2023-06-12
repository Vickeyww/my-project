---
title: "Analysis of Data-Related Job Postings in City of New York"
author: "Lai Zhiyue"
---
#Prepare the Enviornment 

```{r setup, include=TRUE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
# clear the working environment
rm(list=ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(stringr)
library(knitr)
library(readr)
library(pacman)
p_load("jiebaR", "quanteda", "tidyr", "readxl", "stringr", "tidytext", "tm", "gridExtra", "topicmodels", "tidyverse", "ggplot2", "wordcloud2")
```
#read data
```{r}
ny_job <-readr::read_csv("E:/DABE/TEXTURE/final/nyc-jobs.csv")
summary(ny_job)
```
#data cleaning
```{r}
#Convert to date format
ny_job$'Posting Date' <- as.Date(ny_job$'Posting Date', format = "%Y-%m-%d")
#Filter out data related work
ny_job_filtered <- ny_job %>% 
filter(grepl("Engineering", `Job Category`)|
         grepl("Technology", `Job Category`)| grepl("Data", `Job Category`))

```
#change of time
```{r}
date_num <- ny_job %>%
  group_by(`Posting Date`) %>%
  summarize(count = n())
ggplot(subset(date_num, `Posting Date` >= as.Date("2014-01-01")),aes(x = `Posting Date`, y = count)) + 
  labs(x = "Posting Date", y = "Number", title = "The change of Recruitement Number")+
geom_bar(stat = "identity")+
 theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 14))
ggsave("job_postings0.png", width = 6, height = 4, dpi = 300)
```

#change of time in tech
```{r}
date_num_tech <- ny_job_filtered %>%
  group_by(`Posting Date`) %>%
  summarize(count = n())
#plot
ggplot(subset(date_num_tech, `Posting Date` >= as.Date("2016-01-01")),aes(x = `Posting Date`, y = count)) + 
  geom_bar(stat = "identity", width = 2)+
  scale_x_date(date_breaks = "1 years",date_labels = "%Y")+
  labs(x = "Posting Date", y = "Number", title = "The change of Recruitement Number(data related jobs)")+
   theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 14))
ggsave("job_postings.png", width = 6, height = 4, dpi = 300)
```
#Posting type of pie chart
```{r}
date_num_Salary <- ny_job_filtered %>%
  group_by(`Posting Type`) %>%
  summarize(count = n())
total_count <- sum(date_num_Salary$count)
percentage_labels <- paste0(round(date_num_Salary$count/total_count*100), "%")
ggplot(date_num_Salary, aes(x = "", y = count, fill = `Posting Type`)) +
  geom_bar(stat = "identity", width = 1, color = "white", show.legend = TRUE) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Posting Type", 
       title = "Distribution of Posting Type") +
  scale_fill_manual(values = c("#f6cf71", "#019868", "#ec0b88"),
                    guide = guide_legend(title = "Posting Type")) +
  geom_text(aes(label = scales::percent(count/sum(count)), 
            y = count), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
  
ggsave("job_postings1.png", width = 6, height = 4, dpi = 300)
```


#Full-Time/Part-Time indicator of pie chart
```{r}
date_num_Salary <- ny_job_filtered %>%
  group_by(`Full-Time/Part-Time indicator`) %>%
  summarize(count = n())
total_count <- sum(date_num_Salary$count)
percentage_labels <- paste0(round(date_num_Salary$count/total_count*100), "%")
ggplot(subset(date_num_Salary,`Full-Time/Part-Time indicator`!=" "), aes(x = "", y = count, fill = `Full-Time/Part-Time indicator`)) +
  geom_bar(stat = "identity", width = 1, color = "white", show.legend = TRUE) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Full-Time/Part-Time indicator", 
       title = "Distribution of Full-Time/Part-Time indicator") +
  scale_fill_manual(values = c("#f6cf71", "#ec0b88"),
                    guide = guide_legend(title = "Full-Time/Part-Time indicator")) +
  geom_text(aes(label = scales::percent(count/sum(count)), 
            y = count), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
  
ggsave("job_postings2.png", width = 6, height = 4, dpi = 300)
```

#Ranking of recruitment numbers by company
```{r}
ny_job_summary1 <- ny_job %>% 
  group_by(Agency) %>% 
  summarize(n_positions1 = sum(`Of Positions`, na.rm = TRUE))%>%
  arrange(desc(n_positions1)) # Sort by the value of the n_positions column from highest to lowest

kable(ny_job_summary1, title = "The number of data related jobs in each agency ", row.names = TRUE)
ggsave("number of jobs1.png", width = 4, height = 4, dpi = 300)

```
#Ranking of recruitment numbers by company
```{r}
#data related jobs
ny_job_summary <- ny_job_filtered %>% 
  group_by(Agency) %>% 
  summarize(n_positions = sum(`Of Positions`, na.rm = TRUE))%>%
  arrange(desc(n_positions)) 
ny_job_summary_per <- ny_job_summary %>% 
  left_join(ny_job_summary1, by = "Agency") %>% 
  mutate(percentage = round(n_positions / n_positions1 * 100, 2),
         percentage = paste0(percentage, "%"))
kable(ny_job_summary_per, title = "The number of data related jobs in each agency ", row.names = TRUE,)
ggsave("number of jobs.png",width = 30, height = 20, dpi = 300)
```

#data related wordclouds
```{r}
word_freq1 <- table(ny_job_filtered$`Civil Service Title`) 
  
wordcloud2(data = word_freq1, backgroundColor = "white",color = "random-light", 
           shape = "circle", fontWeight = "bold") 
ggsave("wordcloud2.png", width = 8, height = 8, dpi = 300)
```
#Frequency of Skills in Preferred Skills
```{r}
#selecting data related keywords
ny_job_skills <- ny_job %>% 
  filter(!is.na('Preferred Skills')) %>% 
  mutate(techniques = case_when(
    str_detect(`Preferred Skills`, "(?i)SQL|(?i)Mysql") ~ "Sql",
    str_detect(`Preferred Skills`, "(?i)python") ~ "Python",
    str_detect(`Preferred Skills`, "(?i)Excel") ~ "Excel",
    str_detect(`Preferred Skills`, "(?i)java") ~ "Java",
    str_detect(`Preferred Skills`, "(?i)quantitative") ~ "Quantitative",
    str_detect(`Preferred Skills`, "(?i)database") ~ "Database",
    str_detect(`Preferred Skills`, "(?i)model|(?i)models") ~ "Model",
    TRUE ~ "Other")
    )
ny_job_skills_summary <- ny_job_skills %>% 
  group_by(techniques) %>% 
  summarize(freq = n()) %>% 
  arrange(desc(freq))
# Rearrange the X-axis variables in order of frequency from highest to lowest
ny_job_skills_summary$techniques <- reorder(ny_job_skills_summary$techniques, ny_job_skills_summary$freq, FUN = function(x) -x)

# Converting X-axis variables to ordered factor variables
ny_job_skills_summary$techniques <- factor(ny_job_skills_summary$techniques, ordered = TRUE)

#plotting
ggplot(subset(ny_job_skills_summary, techniques != "Other"), aes(x = techniques, y = freq, fill = techniques)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "#f6cf71", 
      "#019868",  
      "#ec0b88",  
      "#651eac",  
      "#e18a1e",  
      "#9dd292",   
      "#2b7de5"   
    )) +
  labs(x = "Techniques", y = "Frequency", title = "Main Techniques Frequency ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))
ggsave("frequency.png", width = 8, height = 8, dpi = 300)
```
#frequency of skills2(exluding excel)
```{r}
#selecting data related keywords
ny_job_skills2 <- ny_job %>% 
  filter(!is.na('Preferred Skills')) %>% 
  mutate(techniques2 = case_when(
    str_detect(`Preferred Skills`, "(?i)SQL|(?i)Mysql") ~ "Sql",
    str_detect(`Preferred Skills`, "(?i)python") ~ "Python",
    str_detect(`Preferred Skills`, "(?i)java") ~ "Java",
    str_detect(`Preferred Skills`, "(?i)quantitative") ~ "Quantitative",
    str_detect(`Preferred Skills`, "(?i)database") ~ "Database",
    str_detect(`Preferred Skills`, "(?i)model|(?i)models") ~ "Model",
    TRUE ~ "Other"))
ny_job_skills_summary2 <- ny_job_skills2 %>% 
  group_by(techniques2) %>% 
  summarize(freq = n()) %>% 
  arrange(desc(freq))
# Rearrange the X-axis variables in order of frequency from highest to lowest
ny_job_skills_summary2$techniques2 <- reorder(ny_job_skills_summary2$techniques2, ny_job_skills_summary2$freq, FUN = function(x) -x)

# Converting X-axis variables to ordered factor variables
ny_job_skills_summary2$techniques2 <- factor(ny_job_skills_summary2$techniques2, ordered = TRUE)

#plotting
ggplot(subset(ny_job_skills_summary2, techniques2 != "Other"), aes(x = techniques2, y = freq, fill = techniques2)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "#f6cf71", 
      "#ec0b88",  
      "#651eac",  
      "#e18a1e",  
      "#9dd292",   
      "#2b7de5"   
    )) +
  labs(x = "Techniques", y = "Frequency", title = "Main Techniques Frequency(exclude 'Excel')") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))
ggsave("frequency2.png", width = 8, height = 8, dpi = 300)
```
#Salary by Job Level
```{r}
#creating new df
job_salary <- ny_job %>%
  mutate(salary_diff = `Salary Range To` - `Salary Range From`) %>% 
  filter(grepl("Annual", `Salary Frequency`))
#plot 
ggplot(subset(job_salary, job_salary$Level != "4A" & job_salary$Level != "4B" & job_salary$Level != "M6"), aes(x = Level, y = salary_diff, fill = Level)) +
  geom_boxplot(color = "black") +
    scale_fill_manual(values = c("#F8B195", "#F67280", "#C06C84", "#B0A8B9", "#E6AF2E", "#ACF0F2", "#A0CED9", "#6C5B7B","#FFB6C1","#87CEEB","#F0E68C")) +
  labs(title = "Salary by Job Level", x = "Job Level", y = "Salary")+
   theme(axis.text = element_text(size = 13),
        plot.title = element_text(size = 14))
ggsave("salary-joblevel.png", width = 15, height = 8, dpi = 300)
```
#Salary by Agency
```{r}
job_salary <- job_salary %>% 
  filter(`Salary Range From` != 0 )
ggplot(data = job_salary, aes(x = job_salary$Level, y = job_salary$`Salary Range To`,group = job_salary$Level))+
  geom_boxplot(fill = "gray", color = "black") +
  labs(title = "Salary by Job Level", x = "Job Level", y = "Salary")+
   labs(title = "Salary by Job Level", x = "Job Level", y = "Salary")+
   theme(axis.text = element_text(size = 13),
        plot.title = element_text(size = 14))
ggsave("salary-joblevel2.png", width = 15, height = 8, dpi = 300)
```
#degree
```{r}
degree <- job_salary %>% 
  filter(!is.na(`Minimum Qual Requirements`)) %>% 
  filter(`Salary Range From` != 0 )%>%
  mutate(degree_type = case_when(
    str_detect(`Minimum Qual Requirements`, "(?i)high school") ~ "high school",
    str_detect(`Minimum Qual Requirements`, "(?i)baccalaureate|(?i)Undergraduate|(?i)Bachelor") ~ "Bachelor",
    str_detect(`Minimum Qual Requirements`, "(?i)master|(?i)master's|(?i)Msc|(?i)mastera") ~ "master",
    str_detect(`Minimum Qual Requirements`, "(?i)Ph.D|(?i)Sc.D|(?i)doctor|(?i)doctors") ~ "Ph.D",
    TRUE ~ "Other"))
```
#degree of boxplot
```{r}
ggplot(subset(degree, degree_type != "Other"), aes(x = degree_type, y = `Salary Range From`, fill = degree_type)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("#F8B195", "#F67280","#FFB6C1","#87CEEB","#F0E68C")) +
  labs(title = "Salary by Academic Qualifications", x = "Academic Qualifications", y = "Minimum Salary")+
   labs(title = "Salary by Job Level", x = "Job Level", y = "Salary")+
   theme(axis.text = element_text(size = 13),
        plot.title = element_text(size = 14))
ggsave("salary-degree.png", width = 15, height = 8, dpi = 300)
```

#tf-idf
```{r}
library(quanteda)
# use jiebaR to parse Chinese word, can also load in our own dictionary 
document <- ny_job_filtered$`Job Description`
document<- str_remove_all(document, "[[:digit:][:punct:]][:space:] \n\t]\\?")

# Use jiebaR to split words
Fen <- jiebaR::worker(type="mix")
document_fen <- apply_list(as.list(document), Fen)

# Converting text to tokens objects
toks <- tokens(document, what = "word")

# Remove stop words and short words
toks <- tokens_remove(toks, stopwords("english"), min_nchar = 2)
toks[[1]]

# DFM 
dfm <- dfm(toks)
dfm

dfm_tfidf <- dfm_tfidf(dfm)
dfm_tfidf<- dfm_remove(dfm_tfidf, pattern = "")

```

```{r}
library(pacman)
p_load("jiebaR", "quanteda", "tidyr", "readxl", "stringr", "tidytext", "tm", "gridExtra", "topicmodels", "tidyverse", "ggplot2", "wordcloud2")
library(stringi)
# Calculate the TF-IDF weights for each word
tfidf_tidy <- tidy(dfm_tfidf)

tfidf_list <- tfidf_tidy %>%
  group_by(term) %>%
  mutate(tfidf = sum(count)) %>%
  ungroup() %>%
  select(term, tfidf) %>%
  unique() %>%
  arrange(-tfidf)

# Filter the top 100 high-frequency words
tf_top <- tfidf_list %>%
  slice_max(tfidf, n = 100) %>%
  mutate(tfidf = round(tfidf))
# Mapping word clouds
wordcloud2(tf_top,size=0.5,backgroundColor = "white",color = "random-light", 
           shape = "circle", fontWeight = "bold")
ggsave("job_des.png", width = 15, height = 8, dpi = 300)
```