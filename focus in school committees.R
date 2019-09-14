library(ggplot2)
library(tm)
library(pdftools)
library(tidyverse)
library(dplyr)
library(tidytext)
library(textreadr)
library(textshape)
library(topicmodels)
library(quanteda)
library(readtext)
library(plm)
library(xts)
library(zoo)
library(grid)
library(coefplot)

# Skaber corpus og joiner data
dest <- "C:/Users/jespe/Desktop/Odense"

txts <- readtext(dest, encoding = "UTF-8",
                 docvarsfrom = "filenames")

kommunedata <- read.csv("C:/Users/jespe/Desktop/kommunedata.csv", header = TRUE, sep = ";")

kommunedata <- kommunedata %>%
  mutate(reference = faktisk_perf - forventet_perf)

fulltext <- corpus(txts)

# Skaber dtm og pre-processer tekst
dtm <- dfm(fulltext, verbose = TRUE,  tolower = TRUE, remove_punct = TRUE, 
           stem = TRUE, remove = stopwords("da"))

dtm <- dfm_trim(dtm, min_termfreq = 5, min_docfreq = .2)

# Laver om til df og lav ordbog med alle synonymer for elevers karakterer
# Ekspert tilføjede ordene "nationale tests" (splittet op i "nationale"
# og "test" på  grund af fravær af n-grams), "prøver", samt "trivsel", 
# som dog blev droppet på grund af fokus på karakterer frem for trivsel
dtm_df <- convert(dtm, to = "data.frame")

performance <- dictionary(list(performance = c("afgangs*", "eksamen*", "gennemsnit*",    
                                               "karakter*", "måling*", "nationale*",
                                                "præst*", "prøver*", "resultat*",
                                                "test*", "undervisningseffekt*")))

dict_dtm <- dfm_lookup(dtm, performance, nomatch = "unmatched")

dicto_dtm <- convert(dict_dtm, to = "data.frame")

new_dict <- dicto_dtm %>%
  mutate(kommune = str_extract_all(document, "^[A-z].*[^.//0-9//0-9-//0-9//0-9-//0-9//0-9//0-9//0-9//.pdf]"),
         date = str_extract_all(document, "(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[0-2])-([12][0-9]{3})"),
         total_words = performance + unmatched,
         performance = performance / total_words, 
         year = substring(date, 7, 10),
         month = substring(date, 4, 5))

new_dict$kommune <- unlist(new_dict$kommune)

new_dict <- new_dict[-352, ]

new_dict$date <- unlist(new_dict$date)

new_dict <- new_dict %>%
  mutate(dato = as.Date(date, format = "%d-%m-%Y"))

# Nødt til at lave kommunenavne om for at joine data
new_dict$kommune <- new_dict$kommune %>%
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

# Joiner datasæt
kommunedata$kommune <- as.character(kommunedata$kommune)

new_dict$year <- as.integer(new_dict$year)

newest_dict <- new_dict %>%
  left_join(kommunedata, by = c("year" ,"kommune"))

newest_dict %>%
  View()

# Standardiserer performance-variabel
newest_dict$performance_sd <- newest_dict$performance /0.002813475

# Test for om performance-snak stiger efter performance-scores publiceres
ggplot(newest_dict, aes(as.Date(dato), performance_sd))+
  geom_smooth()+
  theme_bw()+
  labs(y = "Amount of performance dialogue on the agenda", x = "Year",
       title = "Figure 4. Performance dialogue over time (standardized)")+
  geom_vline(aes(xintercept = as.Date("20-01-2017", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("20-04-2017", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof performance \nscores for 2016")+
  geom_vline(aes(xintercept = as.Date("30-01-2018", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("30-04-2018", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof performance \nscores for 2017")+
  geom_vline(aes(xintercept = as.Date("23-01-2019", format = "%d-%m-%Y")))+
  geom_text(x = as.Date("10-04-2019", format = "%d-%m-%Y"), y = 1, 
            label = "Publishing \nof scores \nfor 2018")

# Laver tidsserieanalyse af vigtigheden af performance og økonomi for at tale om præstationer
tidy_newdict <- newest_dict %>%
  group_by(year, kommune) %>%
  summarise(performance = mean(performance),
            performance = sum(performance),
            reference = max(reference),
            faktisk_perf = max(faktisk_perf),
            forventet_perf = max(forventet_perf),
            indbyggertal = max(indbyggertal),
            exp = max(udg_pr_6_16),
            borgmesterskift = max(skift_borgm),
            partiskift = max(skift_parti)) 

tidy_newdict <- tidy_newdict %>%
  group_by(year) %>%
  arrange(desc(reference)) %>%
  mutate(social = row_number(),
         newyear = gsub("^*", "01-01-", year))

# Standardiserer variable og kører reg - først med pooling
tidy_newdict$performance_sd <- tidy_newdict$performance /0.001165783

hist <- plm(performance_sd ~ reference + log(exp) + log(indbyggertal), index = c("newyear", "kommune"), data = tidy_newdict,
            model = "pooling", effect = "twoways")
summary(hist)


social <- plm(performance_sd ~ social + log(exp) + log(indbyggertal), index = c("newyear", "kommune"), data = tidy_newdict,
            model = "pooling", effect = "twoways")
summary(social)
coefplot(hist, intercept = FALSE)

# Lineære modeller med tidsdummies viser det samme
histlm <- lm(performance_sd ~ reference + log(exp) + log(indbyggertal) + as.factor(newyear), data = tidy_newdict)
sociallm <- lm(performance_sd ~ social + log(exp) + log(indbyggertal) + as.factor(newyear), data = tidy_newdict)

# First differencing viser det samme
histfd <- plm(performance_sd ~ reference + log(exp) + log(indbyggertal), index = c("newyear", "kommune"), data = tidy_newdict,
              model = "fd", effect = "individual")
summary(histfd)

socialfd <- plm(performance_sd ~ social + log(exp) + log(indbyggertal), index = c("newyear", "kommune"), data = tidy_newdict,
              model = "fd", effect = "individual")
summary(socialfd)

# Visualisering af data
coefplot(hist, intercept = FALSE, strict = TRUE, coefs = "reference",
         newNames = c("log(indbyggertal)" = "Citizens", "log(exp)" = "Expenditures", 
                      "reference" = "Performance\nfeedback"))+
  labs(x = "Regression coefficient", 
       title = NULL)+
           theme_bw()

ggplot(tidy_newdict, aes(performance_sd))+
  geom_density()+
  theme_bw()+
  labs(x = "% student grade dialogue in documents (standardized)", y = "Density", title = "Figure D1. Density plot of percentage student grade dialogue in documents")
