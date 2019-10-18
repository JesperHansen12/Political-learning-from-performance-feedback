library(quanteda)
library(readtext)
library(tidyverse)
library(plm)
library(ggplot2)
library(Hmisc)
library(stargazer)
library(pglm)
library(splines)
library(caret)

###########################################
##### Loading data and creating corpus ####
###########################################
dest <- "U:/Politisk læringsprojekt/referater_test"

list.files(dest)

reports <- readtext(dest, 
                    docvarsfrom = "filenames",
                    verbosity = 3)
summary(reports, 5)

# Generating a corpus
corp_reports <- corpus(reports)

ndoc(corp_reports)

###########################################
############ KWIC-analyses ################
###########################################
kwic_perf <- kwic(corp_reports, pattern = c("*afgangs*", "*eksam*", "*gennemsnit*",    
                                            "*karaktere*", "*måling*",
                                            "*præst*", "*prøver*", "*resultat*",
                                            "test*", "*undervisningseffekt*"), window = 4)

set.seed(1)

kwic_perf_df <- as.data.frame(kwic_perf)
kwic_perf[sample(nrow(kwic_perf), 10),]
kwic_perf_df %>%
  count(keyword) %>%
  arrange(desc(n))

# Keyword-in-context analysis for the change dictionary
kwic_change <- kwic(corp_reports, pattern = c("*forandr*", "*forrykk*", "*forvandl*",
                                              "*modific*", "*omdann", "*omform", "*omskab",
                                              "*reformer*",  "*reorganiser*", "*revolutioner*",
                                              "*transformer*", "*ændr*"), window = 4)

set.seed(1)

kwic_change_df <- as.data.frame(kwic_change)
kwic_change[sample(nrow(kwic_change), 10),]

kwic_change_df %>%
  count(keyword) %>%
  arrange(desc(n))

#######################################
#### Constructing DFM from corpus #####
#######################################
dtm_reports <- dfm(corp_reports, tolower = TRUE, remove_punct = TRUE, 
                   remove_url = TRUE, remove_hyphens = TRUE, 
                   remove = c(stopwords("danish"),
                              "*1*", "*2*", "*3*", "*3*", "*4*", "*5*", "*6*", "*7*", "*8*", "*9*",
                              "*uændr*"),
                   stem = TRUE, verbose = TRUE)

dtm_reports <- dfm_trim(dtm_reports, min_termfreq = 10, min_docfreq = .15)

dim(dtm_reports)

#######################################
#### Creating dictionary from DFM #####
#######################################
report_dict <- dictionary(list(attention = c("*afgangs*", "*eksam*", "*gennemsnit*",    
                                             "*karaktere*", "*måling*",
                                             "*præst*", "*prøver*", "*resultat*",
                                             "test*", "*undervisningseffekt*"),
                               reform = c("*forandr*", "*forrykk*", "*forvandl*",
                                          "*modific*", "*omdann", "*omform", "*omskab",
                                          "*reformer*",  "*reorganiser*", "*revolutioner*",
                                          "*transformer*","*ændr*")))

dictionary_reports <- dfm_lookup(dtm_reports, report_dict, nomatch = "unmatched")

head(dictionary_reports)

######################################
###### Convert DFM to data frame #####
######################################
dictionary_reports_df <- convert(dictionary_reports, to = "data.frame")

head(dictionary_reports_df)

dictionary_reports_df <- dictionary_reports_df %>%
  mutate(kommune = str_extract_all(document, "^[A-z].*[^.//0-9//0-9-//0-9//0-9-//0-9//0-9//0-9//0-9//.pdf]"),
         date = str_extract_all(document, "(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[0-2])-([12][0-9]{3})"),
         total_words = attention + reform + unmatched,
         attention = attention,
         reform = reform,
         attention_perc = attention / total_words, 
         reform_perc = reform / total_words,
         year = substring(date, 7, 10),
         month = substring(date, 4, 5))

dictionary_reports_df$date <- unlist(dictionary_reports_df$date)

dictionary_reports_df <- dictionary_reports_df %>%
  mutate(dato = as.Date(date, format = "%d-%m-%Y"))

View(dictionary_reports_df)

dictionary_reports_df %>%
  group_by(year) %>%
  count()

dictionary_reports_df <- dictionary_reports_df %>%
  mutate(performance_sd = attention_perc / sd(attention_perc),
         reform_sd = reform_perc / sd(reform_perc))

#####################################
######### Joining datasets ##########
#####################################

# Need to rename municipalities before joining data
dictionary_reports_df$kommune <- dictionary_reports_df$kommune %>%
  str_replace_all("Aeroe", "Ærø Kommune") %>%
  str_replace_all("Albertslun", "Albertslund Kommune") %>%
  str_replace_all("Alleroe", "Allerød Kommune") %>%
  str_replace_all("Assens", "Assens Kommune") %>%
  str_replace_all("Balleru", "Ballerup Kommune") %>%
  str_replace_all("Billun", "Billund Kommune") %>%
  str_replace_all("Bornholm", "Bornholms Kommune") %>%
  str_replace_all("Broendby", "Brøndby Kommune") %>%
  str_replace_all("Broenderslev", "Brønderslev Kommune") %>%
  str_replace_all("Dragoer", "Dragør Kommune") %>%
  str_replace_all("Egedal", "Egedal Kommune") %>%
  str_replace_all("Esbjerg", "Esbjerg Kommune") %>%
  str_replace_all("Fanoe", "Fanø Kommune") %>%
  str_replace_all("Favrskov", "Favrskov Kommune") %>%
  str_replace_all("Faxe", "Faxe Kommune") %>%
  str_replace_all("Fredensborg", "Fredensborg Kommune") %>%
  str_replace_all("Fredericia", "Fredericia Kommune") %>%
  str_replace_all("Frederiksberg", "Frederiksberg Kommune") %>%
  str_replace_all("Frederikshavn", "Frederikshavn Kommune") %>%
  str_replace_all("Frederikssun", "Frederikssund Kommune") %>%
  str_replace_all("Furesoe", "Furesø Kommune") %>%
  str_replace_all("Faaborg", "Faaborg-Midtfyn Kommune") %>%
  str_replace_all("Gentofte", "Gentofte Kommune") %>%
  str_replace_all("Gladsaxe", "Gladsaxe Kommune") %>%
  str_replace_all("Glostru", "Glostrup Kommune") %>%
  str_replace_all("Greve", "Greve Kommune") %>%
  str_replace_all("Gribskov", "Gribskov Kommune") %>%
  str_replace_all("Guldborgsun", "Guldborgsund Kommune") %>%
  str_replace_all("Haderslev", "Haderslev Kommune") %>%
  str_replace_all("Halsnaes", "Halsnæs Kommune") %>%
  str_replace_all("Hedenste", "Hedensted Kommune") %>%
  str_replace_all("Helsingoer", "Helsingør Kommune") %>%
  str_replace_all("Herlev", "Herlev Kommune") %>%
  str_replace_all("Herning", "Herning Kommune") %>%
  str_replace_all("Hilleroe", "Hillerød Kommune") %>%
  str_replace_all("Hjoerring", "Hjørring Kommune") %>%
  str_replace_all("Hoersholm", "Hørsholm Kommune") %>%
  str_replace_all("Holbaek", "Holbæk Kommune") %>%
  str_replace_all("Holstebro", "Holstebro Kommune") %>%
  str_replace_all("Horsens", "Horsens Kommune") %>%
  str_replace_all("Hvidovre", "Hvidovre Kommune") %>%
  str_replace_all("Ikast", "Ikast-Brande Kommune") %>%
  str_replace_all("Ishoej", "Ishøj Kommune") %>%
  str_replace_all("Jammerbugt", "Jammerbugt Kommune") %>%
  str_replace_all("Kalundborg", "Kalundborg Kommune") %>%
  str_replace_all("Kerteminde", "Kerteminde Kommune") %>%
  str_replace_all("Koebenhavn", "Københavns Kommune") %>%
  str_replace_all("Koege", "Køge Kommune") %>%
  str_replace_all("Kolding", "Kolding Kommune") %>%
  str_replace_all("Laesoe", "Læsø Kommune") %>%
  str_replace_all("Langelan", "Langeland Kommune") %>%
  str_replace_all("Lejre", "Lejre Kommune") %>%
  str_replace_all("Lemvig", "Lemvig Kommune") %>%
  str_replace_all("Lollan", "Lolland Kommune") %>%
  str_replace_all("Lyngby", "Lyngby-Taarbæk Kommune") %>%
  str_replace_all("Mariagerfjor", "Mariagerfjord Kommune") %>%
  str_replace_all("Middelfart", "Middelfart Kommune") %>%
  str_replace_all("Morsoe", "Morsø Kommune") %>%
  str_replace_all("Naestve", "Næstved Kommune") %>%
  str_replace_all("Norddjurs", "Norddjurs Kommune") %>%
  str_replace_all("Nordfyn", "Nordfyns Kommune") %>%
  str_replace_all("Nyborg", "Nyborg Kommune") %>%
  str_replace_all("Odder", "Odder Kommune") %>%
  str_replace_all("Odense", "Odense Kommune") %>%
  str_replace_all("Odsherre", "Odsherred Kommune") %>%
  str_replace_all("Randers", "Randers Kommune") %>%
  str_replace_all("Rebil", "Rebild Kommune") %>%
  str_replace_all("Ringste", "Ringsted Kommune") %>%
  str_replace_all("Rinkoebing", "Ringkøbing-Skjern Kommune") %>%
  str_replace_all("Roedovre", "Rødovre Kommune") %>%
  str_replace_all("Roskilde", "Roskilde Kommune") %>%
  str_replace_all("Rudersdal", "Rudersdal Kommune") %>%
  str_replace_all("Samsoe", "Samsø Kommune") %>%
  str_replace_all("Silkeborg", "Silkeborg Kommune") %>%
  str_replace_all("Skanderborg", "Skanderborg Kommune") %>%
  str_replace_all("Skive", "Skive Kommune") %>%
  str_replace_all("Slagelse", "Slagelse Kommune") %>%
  str_replace_all("Soenderborg", "Sønderborg Kommune") %>%
  str_replace_all("Solroe", "Solrød Kommune") %>%
  str_replace_all("Soroe", "Sorø Kommune") %>%
  str_replace_all("Stevns", "Stevns Kommune") %>%
  str_replace_all("Struer", "Struer Kommune") %>%
  str_replace_all("Svendborg", "Svendborg Kommune") %>%
  str_replace_all("Syddjurs", "Syddjurs Kommune") %>%
  str_replace_all("Thiste", "Thisted Kommune") %>%
  str_replace_all("Toender", "Tønder Kommune") %>%
  str_replace_all("Taarnby", "Tårnby Kommune") %>%
  str_replace_all("Taastru", "Høje-Taastrup Kommune") %>%
  str_replace_all("Vallensbaek", "Vallensbæk Kommune") %>%
  str_replace_all("Varde", "Varde Kommune") %>%
  str_replace_all("Vejen", "Vejen Kommune") %>%
  str_replace_all("Vejle", "Vejle Kommune") %>%
  str_replace_all("Vesthimmerlan", "Vesthimmerlands Kommune") %>%
  str_replace_all("Viborg", "Viborg Kommune") %>%
  str_replace_all("Vordingborg", "Vordingborg Kommune") %>%
  str_replace_all("Aabenraa", "Aabenraa Kommune") %>%
  str_replace_all("Aalborg", "Aalborg Kommune") %>%
  str_replace_all("Aarhus", "Aarhus Kommune") 

kommunedata <- read.csv("U:/Politisk læringsprojekt/kommunedata.csv", header = TRUE, sep = ";")

kommunedata <- kommunedata %>%
  mutate(reference = faktisk_perf - forventet_perf)

dictionary_reports_df$year <- as.integer(dictionary_reports_df$year)

dictionary_reports_df$kommune <- unlist(dictionary_reports_df$kommune)

kommunedata$kommune <- as.character(kommunedata$kommune)

joined_dict <- dictionary_reports_df %>%
  left_join(kommunedata, by = c("year" ,"kommune"))

joined_dict %>%
  View()

##################################
##### Modeling the relations #####
##################################
joined_dict$school_size_sd <- joined_dict$school_size / sd(joined_dict$school_size)

mod_perf <- plm(performance_sd ~ reference, index = c("kommune", "date"), 
                effect = "twoways", model = "pooling", data = joined_dict)

summary(mod_perf)

mod_perf2 <- plm(performance_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                 effect = "twoways", model = "pooling", data = joined_dict)

summary(mod_perf2)

stargazer(mod_perf, mod_perf2, type = "text")

mod_reform <- plm(reform_sd ~ reference, index = c("kommune", "date"), 
                  effect = "twoways", model = "pooling", data = joined_dict)

summary(mod_reform)

mod_reform2 <- plm(reform_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                   effect = "twoways", model = "pooling", data = joined_dict)

summary(mod_reform2)

stargazer(mod_perf, mod_perf2, mod_reform, mod_reform2, type = "text")

# Linear models with year dummies
att_lm <- lm(performance_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + 
               as.factor(island) + as.factor(year), data = joined_dict)

reform_lm <- lm(reform_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + 
                  as.factor(island) + as.factor(year), data = joined_dict)

stargazer(att_lm, reform_lm, type = "text")

write.csv2(joined_dict, "report_analysis_3713")

##################################
####### Poisson models ###########
##################################
mod_perf <- pglm(performance_sd ~ reference, index = c("kommune", "date"), 
                 effect = "twoways", model = "pooling", family = "poisson", data = joined_dict)

summary(mod_perf)

mod_perf2 <- pglm(performance_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                  effect = "twoways", model = "pooling", family = "poisson", data = joined_dict)

summary(mod_perf2)

stargazer(mod_perf, mod_perf2, type = "text")

mod_reform <- pglm(reform_sd ~ reference, index = c("kommune", "date"), 
                   effect = "twoways", model = "pooling", family = "poisson", data = joined_dict)

summary(mod_reform)

mod_reform2 <- pglm(reform_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                    effect = "twoways", model = "pooling", family = "poisson", data = joined_dict)

summary(mod_reform2)

ggplot(joined_dict, aes(performance_sd))+
  geom_density()+
  theme_bw()+
  labs(x = "% student grade dialogue in documents (standardized)", y = "Density", title = "Figure E1. Density plot of percentage student grade dialogue in documents")

ggplot(joined_dict, aes(reform_sd))+
  geom_density()+
  theme_bw()+
  labs(x = "% Reform discussion in documents (standardized)", y = "Density", title = "Figure E2. Density plot of percentage reform discussion in documents")


##################################
######## Without outliers ########
##################################
subset <- joined_dict %>%
  filter(reference <= .5)

mod_perf <- plm(performance_sd ~ reference, index = c("kommune", "date"), 
                effect = "twoways", model = "pooling", data = subset)

summary(mod_perf)

mod_perf2 <- plm(performance_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                 effect = "twoways", model = "pooling", data = subset)

summary(mod_perf2)

stargazer(mod_perf, mod_perf2, type = "text")

mod_reform <- plm(reform_sd ~ reference, index = c("kommune", "date"), 
                  effect = "twoways", model = "pooling", data = subset)

summary(mod_reform)

mod_reform2 <- plm(reform_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                   effect = "twoways", model = "pooling", data = subset)

summary(mod_reform2)

stargazer(mod_perf, mod_perf2, mod_reform, mod_reform2, type = "text")

# Without documents with extraortdinary high values of reform discussions
subset_ref <- joined_dict %>%
  filter(reform_sd <= 6)

mod_reform2 <- plm(reform_sd ~ reference + log(udg_pr_6_16) + log(indbyggertal) + as.factor(island) + school_size_sd, index = c("kommune", "date"), 
                   effect = "twoways", model = "pooling", data = subset_ref)

summary(mod_reform2)

##################################
###### Exploratory analysis ######
##################################
ggplot(joined_dict, aes(x = reference, y = performance_sd))+
  geom_smooth(lty = 2, color = "black",  level = .9, stat = "smooth")+
  theme_bw()+
  labs(x = "Performance - reference point",
       y = "Attention to student grades (std.)",
       caption = "Automatic smoothing function with 90 % confidence intervals. N = 3,713")+
  xlim(-0.5, 0.5)

ggplot(joined_dict, aes(x = reference, y = reform_sd))+
  geom_smooth(lty = 2, color = "black", level = .9)+
  theme_bw()+
  labs(x = "Performance - reference point",
       y = "Reform discussions (std.)",
       caption = "Automatic smoothing function with 90 % confidence intervals. N = 3,713")+
  xlim(-0.5, 0.5)

ggplot(joined_dict, aes(x = log(udg_pr_6_16), y = reform_sd))+
  geom_smooth(lty = 2, color = "black", level = .9)+
  theme_bw()+
  labs(x = "Expenditures per student",
       y = "Reform discussions (std.)",
       caption = "Automatic smoothing function with 90 % confidence intervals. N = 3,713")


##################################
#### Aggregating data by year ####
##################################
aggr_dict <- joined_dict %>%
  group_by(year, kommune) %>%
  summarise(performance_sd = mean(performance_sd),
            date = max(date),
            performance_freq = sum(attention),
            reform_sd = mean(reform_sd),
            reform_freq = sum(reform),
            faktisk_perf = max(faktisk_perf),
            forventet_perf = max(forventet_perf),
            reference = faktisk_perf - forventet_perf,
            indbyggertal = max(indbyggertal),
            exp = max(udg_pr_6_16),
            borgmesterskift = max(skift_borgm),
            partiskift = max(skift_parti), 
            island = max(island),
            school_size = max(school_size)) 

aggr_dict$performance_sd <- aggr_dict$performance_sd / sd(aggr_dict$performance_sd)

aggr_dict$reform_sd <- aggr_dict$reform_sd / sd(aggr_dict$reform_sd)

aggr_dict$school_size_sd <- aggr_dict$school_size / sd(aggr_dict$school_size)

aggr_dict %>%
  View()

write.csv2(joined_dict, "aggregated_data")

# Linear models with year dummies
att_lm1 <- lm(performance_sd ~ reference  + as.factor(year), data = aggr_dict)

reform_lm1 <- lm(reform_sd ~ reference  + as.factor(year), data = aggr_dict)

att_lm <- lm(performance_sd ~ reference + log(exp) + log(indbyggertal) + 
               as.factor(island) + as.factor(year), data = aggr_dict)

reform_lm <- lm(reform_sd ~ reference + log(exp) + log(indbyggertal) + 
                  as.factor(island) + as.factor(year), data = aggr_dict)

stargazer(att_lm1, reform_lm1, att_lm, reform_lm, type = "text")

# Pooling time series models
mod_perf1 <- plm(performance_sd ~ reference , 
                 index = c("kommune", "year"), 
                 effect = "twoways", model = "pooling", data = aggr_dict)

summary(mod_perf1)


mod_perf2 <- plm(performance_sd ~ reference + log(exp) + log(indbyggertal) + as.factor(island) + school_size_sd, 
                 index = c("kommune", "year"), 
                 effect = "twoways", model = "pooling", data = aggr_dict)

summary(mod_perf2)

mod_ref1 <- plm(reform_sd ~ reference, 
                index = c("kommune", "year"), 
                effect = "twoways", model = "pooling", data = aggr_dict)

summary(mod_ref1)

mod_ref2 <- plm(reform_sd ~ reference + log(exp) + log(indbyggertal) + as.factor(island) + school_size_sd, 
                index = c("kommune", "year"), 
                effect = "twoways", model = "pooling", data = aggr_dict)

summary(mod_ref2)

stargazer(mod_perf1, mod_perf2, type = "text")

##################################
######### Lagging change #########
##################################
reflag1 <- plm(lag(reform_sd, 1) ~ reference, data = aggr_dict, index = c("kommune", "year"),
               effect = "twoways", model = "pooling")

reflag2 <- plm(lag(reform_sd, 2) ~ reference, data = aggr_dict, index = c("kommune", "year"),
               effect = "twoways", model = "pooling")

reflag3 <- plm(lag(reform_sd, 3) ~ reference, data = aggr_dict, index = c("kommune", "year"),
               effect = "twoways", model = "pooling")

stargazer(reflag1, reflag2, reflag3, type = "text")

##################################
####### Data visualization #######
##################################
ggplot(joined_dict, aes(as.Date(dato), performance_sd))+
  geom_smooth(color = "black", level = .9, linetype = 2)+
  theme_bw()+
  labs(y = "Amount of attention to student grades", x = "Year",
       title = "Figure 1. Attention to student grades over time (standardized)",
       caption = "Note: Automatic smoothing function with 90 % confidence intervals. N = 3,713 reports.")+
  geom_vline(aes(xintercept = as.Date("20-01-2017", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("20-04-2017", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof performance \nscores for 2016")+
  geom_vline(aes(xintercept = as.Date("30-01-2018", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("30-04-2018", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof performance \nscores for 2017")+
  geom_vline(aes(xintercept = as.Date("23-01-2019", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("10-04-2019", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof scores \nfor 2018")+
  xlim(as.Date("01-01-2016", format = "%d-%m-%Y"), as.Date("01-05-2019", format = "%d-%m-%Y"))

##################################
###### Descriptives ##############
##################################
cordata <- joined_dict %>%
  select(performance_sd, reform_sd, year, reference, island, indbyggertal, exp)

res <- cor(cordata)
round(res, 2)

cordata <- aggr_dict %>%
  select(performance_sd, reform_sd, year, reference, island, indbyggertal, exp)

res <- cor(cordata)
round(res, 2)

ggplot(aggr_dict, aes(performance_sd))+
  geom_density()+
  theme_bw()+
  labs(x = "% student grade dialogue in documents (standardized)", y = "Density", title = "Figure D1. Density plot of percentage student grade dialogue in documents")

ggplot(aggr_dict, aes(reform_sd))+
  geom_density()+
  theme_bw()+
  labs(x = "% Reform discussion in documents (standardized)", y = "Density", title = "Figure D2. Density plot of percentage reform discussion in documents")

write.csv2(aggr_dict, "aggregated_data")
























