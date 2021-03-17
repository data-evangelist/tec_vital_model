##############################################################################
#                                                                             #
#                         Parish Vitality - ASA Decline Analysis              #
#                                                                             #
#                         Author: The Rev. Alistair So-Schoos                 #
#                                                                             #
#                                                                             #
#                                                                             #
# DESCRIPTION: Further processing of Boruta-results - discovering features    #
#              that are important for predicting decline                      #
#                                                                             #
#                                                                             #       
#                                                                             #
#                                                                             #
###############################################################################

#Note: For best results, please run Parish_Vitality_Neuralnet.R before running this script.

#                           Clean global environment----

rm(list=ls())

#Load required libraries----

suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(scales))
suppressMessages(library(tidyverse))
suppressMessages(library(DomoR))
suppressMessages(library(data.table))
suppressMessages(library(neuralnet))
suppressMessages(library(Boruta))
suppressMessages(library(randomForest))
suppressMessages(library(caret))

#Require the following for spliting datasets
require(caTools)


#Initiate Domo User Settings----

DomoR::init('instance_name','access_token')

sdir <- "~/Desktop/Parish_Vitality/"

#Read in processed Parochial Data----
#Select for open parishes only so that the ASA data will not be overly skewed.
#Limit worsh17 to greater 0 as well to aid in percentage calculation.

parish_combo_vita <- read_csv("Desktop/Parish_Vitality/parish_combo_vita.csv") %>% 
  dplyr::filter(close_status == 0) %>% 
  dplyr::filter(worsh17 >= 1) %>% 
  dplyr::filter(worsh08 >= 1)
View(parish_combo_vita)

#Create variable describing the percentage change in ASA between 2008 and 2017
parish_combo_vita$tenyrASAdelta <- (1 - (parish_combo_vita$worsh08/parish_combo_vita$worsh17))

summary(parish_combo_vita$tenyrASAdelta)

#Create a variable defining decline as a reduction of more than 18.1 (< -18.1)

#View the distribution of this variable.
hist(parish_combo_vita$tenyrASAdelta, 300, col="red")

#QQplot for normal distribution
qqnorm(parish_combo_vita$tenyrASAdelta)

#Shapiro test
shapiro.test(parish_combo_vita$tenyrASAdelta[1:5000])

#Define a standard deviation function

sd.p <- function(x){sd(x)*sqrt((length(x)-1)/length(x))}

#Calculate the standard deviation of the delta in question

sd_delta <- sd.p(parish_combo_vita$tenyrASAdelta)

#One SD below the mean is approximately -72
#We define significant decline as 

#Conduct a test for feature imnportance----
# Perform Boruta search

boruta_output4 <- Boruta(tenyrASAdelta ~ ., data=na.omit(parish_combo_vita), doTrace=3)  

names(boruta_output4)  #Shows what's in the output file.

# Get significant variables including tentatives
boruta_signif4 <- getSelectedAttributes(boruta_output4, withTentative = TRUE)
print(boruta_signif4)  

# Do a tentative rough fix----
roughFixMod4 <- TentativeRoughFix(boruta_output4)
boruta_signif4 <- getSelectedAttributes(roughFixMod4)
print(boruta_signif4)

# Plot variable importance
plot(boruta_output4, cex.axis=.7, las=2, xlab="", main="Variable Importance in Combined Set")  

#Trend of pledge over total revenue for the lower quartile of declining parishes----

#Subset of all parishes
lowerqrt <- parish_combo_vita %>%
  dplyr::filter(tenyrASAdelta <= -0.58) %>%
  dplyr::mutate(., PR17 = pledge17/totrev17) %>%
  dplyr::mutate(., PR16 = pledge16/totrev16) %>%
  dplyr::mutate(., PR15 = pledge15/totrev15) %>%
  dplyr::mutate(., PR13 = pledge13/totrev13) %>% 
  dplyr::mutate(., PR10 = pledge10/totrev10) %>%
  dplyr::mutate(., PR09 = pledge09/totrev09) %>%
  dplyr::mutate(., PR08 = pledge08/totrev08) %>%
  dplyr::mutate(., PR07 = pledge07/totrev07) %>%
  dplyr::mutate(., PR06 = pledge06/totrev06) %>%
  dplyr::mutate(., PR05 = pledge05/totrev05)


#Select diocese, province, and individual pledge to total revenue ratios (PR..)
P1 <- subset(lowerqrt, select = c(edw_org_id, PR17, PR16, PR15, PR13, PR10, 
                                  PR09, PR08, PR07, PR06, PR05), na.rm = T) 

#Remove all zeros, all ratio greater 1 and NAs.
P2 <- P1[P1$PR17 > 0 & P1$PR16 > 0 & P1$PR15 > 0 & P1$PR13 > 0 & P1$PR10 > 0 
         & P1$PR09 > 0 & P1$PR08 > 0 & P1$PR07 > 0 & P1$PR06 > 0 & P1$PR05 > 0 &
           P1$PR17 <= 1 & P1$PR16 <= 1 & P1$PR15 <= 1 & P1$PR13 <= 1 & P1$PR10 <= 1 
         & P1$PR09 <= 1 & P1$PR08 <= 1 & P1$PR07 <=1  & P1$PR06 <=1 & P1$PR05 <=1 
         & !is.na(P1$PR17) & !is.na(P1$PR16) & !is.na(P1$PR15) & !is.na(P1$PR13)
         & !is.na(P1$PR10) & !is.na(P1$PR09) & !is.na(P1$PR08) & !is.na(P1$PR07)
         & !is.na(P1$PR06) & !is.na(P1$PR05), ]  


#Prepare a dataframe for visualization---

a <- mean(P2$PR05)*100
b <- mean(P2$PR06)*100
c <- mean(P2$PR07)*100
d <- mean(P2$PR08)*100
e <- mean(P2$PR09)*100
f <- mean(P2$PR10)*100



i <- mean(P2$PR13)*100

k <- mean(P2$PR15)*100
l <- mean(P2$PR16)*100
m <- mean(P2$PR17)*100

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

j <- (i+k)/2

#For 2011 and 2012, 
#2011 -
g <- f - (f-i)*.33

#2012 -
h <- f - (f-i)*.66


Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
Ratio <- c(a, b, c, d, e, f, g, h, i, j, k, l, m)

TECDF <- data.frame(Year, Ratio)

lowerqrt_trend <- TECDF %>% 
  ggplot(mapping = aes(x = Year, y = Ratio)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Trend in Pledge Income as a Percentage of Total Revenue from 2005 to 2017", subtitle = "Lower Quartile of Declining Parishes",
       caption = "Data points for 2011, 2012 & 2014 were estimated based on available data") +
  xlab("Year") +
  ylab("Percentage of Pledge Income")  

#Trend of pledge over total revenue for the parishes that have grown----
#By at least 10% over 10 years

#Subset of all parishes
topPR <- parish_combo_vita %>%
  dplyr::filter(tenyrASAdelta > 0.10) %>%
  dplyr::mutate(., PR17 = pledge17/totrev17) %>%
  dplyr::mutate(., PR16 = pledge16/totrev16) %>%
  dplyr::mutate(., PR15 = pledge15/totrev15) %>%
  dplyr::mutate(., PR13 = pledge13/totrev13) %>% 
  dplyr::mutate(., PR10 = pledge10/totrev10) %>%
  dplyr::mutate(., PR09 = pledge09/totrev09) %>%
  dplyr::mutate(., PR08 = pledge08/totrev08) %>%
  dplyr::mutate(., PR07 = pledge07/totrev07) %>%
  dplyr::mutate(., PR06 = pledge06/totrev06) %>%
  dplyr::mutate(., PR05 = pledge05/totrev05)


#Select diocese, province, and individual pledge to total revenue ratios (PR..)
P3 <- subset(topPR, select = c(edw_org_id, PR17, PR16, PR15, PR13, PR10, 
                               PR09, PR08, PR07, PR06, PR05), na.rm = T) 

#Remove all zeros, all ratio greater 1 and NAs.
P4 <- P3[P3$PR17 > 0 & P3$PR16 > 0 & P3$PR15 > 0 & P3$PR13 > 0 & P3$PR10 > 0 
         & P3$PR09 > 0 & P3$PR08 > 0 & P3$PR07 > 0 & P3$PR06 > 0 & P3$PR05 > 0 &
           P3$PR17 <= 1 & P3$PR16 <= 1 & P3$PR15 <= 1 & P3$PR13 <= 1 & P3$PR10 <= 1 
         & P3$PR09 <= 1 & P3$PR08 <= 1 & P3$PR07 <=1  & P3$PR06 <=1 & P3$PR05 <=1 
         & !is.na(P3$PR17) & !is.na(P3$PR16) & !is.na(P3$PR15) & !is.na(P3$PR13)
         & !is.na(P3$PR10) & !is.na(P3$PR09) & !is.na(P3$PR08) & !is.na(P3$PR07)
         & !is.na(P3$PR06) & !is.na(P3$PR05), ]  


#Prepare a dataframe for visualization---

a <- mean(P4$PR05)*100
b <- mean(P4$PR06)*100
c <- mean(P4$PR07)*100
d <- mean(P4$PR08)*100
e <- mean(P4$PR09)*100
f <- mean(P4$PR10)*100



i <- mean(P4$PR13)*100

k <- mean(P4$PR15)*100
l <- mean(P4$PR16)*100
m <- mean(P4$PR17)*100

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

j <- (i+k)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

g <- f - (f-i)*.33

#2012 is 2010 - (2010 - 2013) x 66%
h <- f - (f-i)*.66


Year <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
Ratio <- c(a, b, c, d, e, f, g, h, i, j, k, l, m)

topdf <- data.frame(Year, Ratio)

topPR_trend <- topdf %>% 
  ggplot(mapping = aes(x = Year, y = Ratio)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Trend in Pledge Income as a Percentage of Total Revenue from 2005 to 2017", subtitle = "Parishes with at least 10% in ASA over 10 Years",
       caption = "Data points for 2011, 2012 & 2014 were estimated based on available data") +
  xlab("Year") +
  ylab("Percentage of Pledge Income")  

#Create pledge to total revenue ratio columns for the entire dataframe----

parish_combo_vita <- parish_combo_vita %>% 
  dplyr::mutate(., PR17 = pledge17/totrev17) %>%
  dplyr::mutate(., PR16 = pledge16/totrev16) %>%
  dplyr::mutate(., PR15 = pledge15/totrev15) %>%
  dplyr::mutate(., PR13 = pledge13/totrev13) %>% 
  dplyr::mutate(., PR10 = pledge10/totrev10) %>%
  dplyr::mutate(., PR09 = pledge09/totrev09) %>%
  dplyr::mutate(., PR08 = pledge08/totrev08) %>%
  dplyr::mutate(., PR07 = pledge07/totrev07) %>%
  dplyr::mutate(., PR06 = pledge06/totrev06) %>%
  dplyr::mutate(., PR05 = pledge05/totrev05)

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_combo_vita$PR14  <- (parish_combo_vita$PR13 + parish_combo_vita$PR15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_combo_vita$PR11 <- parish_combo_vita$PR10 - (parish_combo_vita$PR10 - parish_combo_vita$PR13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_combo_vita$PR12 <- parish_combo_vita$PR10 - (parish_combo_vita$PR10 - parish_combo_vita$PR13)*.66

#Congregational life indicators - feature engineering---

#parish_life = confrm(ad & yth) + ssenr + bury + baptisms

parish_combo_vita$parish_life17 <- parish_combo_vita$adconf17 + parish_combo_vita$chconf17 + 
  parish_combo_vita$ssenr17 + parish_combo_vita$bury17 + parish_combo_vita$adbapt17 +
  parish_combo_vita$chbapt17

parish_combo_vita$parish_life16 <- parish_combo_vita$adconf16 + parish_combo_vita$chconf16 + 
  parish_combo_vita$ssenr16 + parish_combo_vita$bury16 + parish_combo_vita$adbapt16 +
  parish_combo_vita$chbapt16

#chconf15 is not available, use chconf16 instead
parish_combo_vita$parish_life15 <- parish_combo_vita$adconf15 + parish_combo_vita$chconf16 +      
  parish_combo_vita$ssenr15 + parish_combo_vita$bury15 + parish_combo_vita$adbapt15 +
  parish_combo_vita$chbapt15

parish_combo_vita$parish_life13 <- parish_combo_vita$adconf13 + parish_combo_vita$chconf13 + 
  parish_combo_vita$ssenr13 + parish_combo_vita$bury13 + parish_combo_vita$adbapt13 +
  parish_combo_vita$chbapt13

parish_combo_vita$parish_life10 <- parish_combo_vita$adconf10 + parish_combo_vita$chconf10 + 
  parish_combo_vita$ssenr10 + parish_combo_vita$bury10 + parish_combo_vita$adbapt10 +
  parish_combo_vita$chbapt10

parish_combo_vita$parish_life09 <- parish_combo_vita$adconf09 + parish_combo_vita$chconf09 + 
  parish_combo_vita$ssenr09 + parish_combo_vita$bury09 + parish_combo_vita$adbapt09 +
  parish_combo_vita$chbapt09

parish_combo_vita$parish_life08 <- parish_combo_vita$adconf08 + parish_combo_vita$chconf08 + 
  parish_combo_vita$ssenr08 + parish_combo_vita$bury08 + parish_combo_vita$adbapt08 +
  parish_combo_vita$chbapt08

parish_combo_vita$parish_life07 <- parish_combo_vita$adconf07 + parish_combo_vita$chconf07 + 
  parish_combo_vita$ssenr07 + parish_combo_vita$bury07 + parish_combo_vita$adbapt07 +
  parish_combo_vita$chbapt07

parish_combo_vita$parish_life06 <- parish_combo_vita$adconf06 + parish_combo_vita$chconf06 + 
  parish_combo_vita$ssenr06 + parish_combo_vita$bury06 + parish_combo_vita$adbapt06 +
  parish_combo_vita$chbapt06

#bury05 is not available, use bury06 instead
parish_combo_vita$parish_life05 <- parish_combo_vita$adconf05 + parish_combo_vita$chconf05 + 
  parish_combo_vita$ssenr05 + parish_combo_vita$bury06 + parish_combo_vita$adbapt05 +
  parish_combo_vita$chbapt05

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_combo_vita$parish_life14  <- (parish_combo_vita$parish_life13 + parish_combo_vita$parish_life15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_combo_vita$parish_life11 <- parish_combo_vita$parish_life10 - (parish_combo_vita$parish_life10 - parish_combo_vita$parish_life13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_combo_vita$parish_life12 <- parish_combo_vita$parish_life10 - (parish_combo_vita$parish_life10 - parish_combo_vita$parish_life13)*.66


#enhanced_membership = (memb + commun (ad+yth))/2 + othact
parish_combo_vita$en_memb17 <- (parish_combo_vita$memb17 + parish_combo_vita$commun17)/2 + parish_combo_vita$othact17

parish_combo_vita$en_memb16 <- (parish_combo_vita$memb16 + parish_combo_vita$commun16)/2 + parish_combo_vita$othact16

parish_combo_vita$en_memb15 <- (parish_combo_vita$memb15 + parish_combo_vita$commun15)/2 + parish_combo_vita$othact15

#commun13 is not available, use commun15 instead
parish_combo_vita$en_memb13 <- (parish_combo_vita$memb13 + parish_combo_vita$commun15)/2 + parish_combo_vita$othact13

parish_combo_vita$en_memb10 <- (parish_combo_vita$memb12 + parish_combo_vita$commun09)/2 + parish_combo_vita$othact10

parish_combo_vita$en_memb09 <- (parish_combo_vita$memb09 + parish_combo_vita$commun09)/2 + parish_combo_vita$othact09

parish_combo_vita$en_memb08 <- (parish_combo_vita$memb08 + parish_combo_vita$commun08)/2 + parish_combo_vita$othact08

parish_combo_vita$en_memb07 <- (parish_combo_vita$memb07 + parish_combo_vita$commun07)/2 + parish_combo_vita$othact07

parish_combo_vita$en_memb06 <- (parish_combo_vita$memb06 + parish_combo_vita$commun06)/2 + parish_combo_vita$othact06

parish_combo_vita$en_memb05 <- (parish_combo_vita$memb05 + parish_combo_vita$commun05)/2 + parish_combo_vita$othact05

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_combo_vita$en_memb14  <- (parish_combo_vita$en_memb13 + parish_combo_vita$en_memb15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_combo_vita$en_memb11 <- parish_combo_vita$en_memb10 - (parish_combo_vita$en_memb10 - parish_combo_vita$en_memb13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_combo_vita$en_memb12 <- parish_combo_vita$en_memb10 - (parish_combo_vita$en_memb10 - parish_combo_vita$en_memb13)*.66

summary(parish_combo_vita$en_memb17)

#Merge in parish IDs and whether it employs clergy currently----
#Has_clergy_now - 1 means yes, 0 means no.

cure_status <- DomoR::fetch('file_token') 

parish_vita1 <- merge(parish_combo_vita, cure_status, by = "edw_org_id", all.x = T)  

#Parish liturgical indicators - feature engineering----

#Create and adjust attendance as a the ratio of ASA over memb----

#Attendance first created in Parish_Vitality_Neuralnet.R

#Add in missing years here

parish_vita1$attendance06 <- parish_vita1$worsh06

#2010 data not available, repeat 2009 using same figures
parish_vita1$attendance10 <- parish_vita1$worsh09

#2013 ASA not available, use the closest year available
parish_vita1$attendance13 <- parish_vita1$worsh15




#Changing the feature of attendance----
#ASA/en_memb

parish_vita1$attendance17 <- parish_vita1$worsh17/parish_vita1$en_memb17

parish_vita1$attendance16 <- parish_vita1$worsh16/parish_vita1$en_memb16

parish_vita1$attendance15 <- parish_vita1$worsh15/parish_vita1$en_memb15

parish_vita1$attendance13 <- parish_vita1$worsh15/parish_vita1$en_memb13

#2010 data not available, repeat 2009 using same figures
parish_vita1$attendance10 <- parish_vita1$worsh09/parish_vita1$en_memb09

parish_vita1$attendance09 <- parish_vita1$worsh09/parish_vita1$en_memb09

parish_vita1$attendance08 <- parish_vita1$worsh08/parish_vita1$en_memb08

parish_vita1$attendance07 <- parish_vita1$worsh07/parish_vita1$en_memb07

parish_vita1$attendance06 <- parish_vita1$worsh06/parish_vita1$en_memb06

parish_vita1$attendance05 <- parish_vita1$worsh05/parish_vita1$en_memb05

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$attendance14  <- (parish_vita1$attendance13 + parish_vita1$attendance15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$attendance11 <- parish_vita1$attendance10 - (parish_vita1$attendance10 - parish_vita1$attendance13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$attendance12 <- parish_vita1$attendance10 - (parish_vita1$attendance10 - parish_vita1$attendance13)*.66

#Continue building all_EU----

#Done previously:

# #Feature of all Eucharists
# 
# parish_combo$all_EU17 <- parish_combo$sundeu17 + parish_combo$wkdyeu17 + parish_combo$priveu17
# 
# parish_combo$all_EU16 <- parish_combo$sundeu16 + parish_combo$wkdyeu16 + parish_combo$priveu16
# 
# parish_combo$all_EU15 <- parish_combo$sundeu15 + parish_combo$wkdyeu15 + parish_combo$priveu15
# 
# parish_combo$all_EU07 <- parish_combo$sundeu07 + parish_combo$wkdyeu07 + parish_combo$priveu07
# 
# parish_combo$all_EU06 <- parish_combo$sundeu06 + parish_combo$wkdyeu06 + parish_combo$priveu06
# 
# parish_combo$all_EU05 <- parish_combo$sundeu05 + parish_combo$wkdyeu05 + parish_combo$priveu05

parish_vita1$all_EU13 <- parish_vita1$sundeu13 + parish_vita1$wkdyeu13 + parish_vita1$priveu13

#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$all_EU14  <- (parish_vita1$all_EU13 + parish_vita1$all_EU15)/2

#Insufficient worship data between 2008 and 2012

#Estimate missing Easter data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$easter14  <- (parish_vita1$easter13 + parish_vita1$easter15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$easter11 <- parish_vita1$easter10 - (parish_vita1$easter10 - parish_vita1$easter13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$easter12 <- parish_vita1$easter10 - (parish_vita1$easter10 - parish_vita1$easter13)*.66

#Build feature exploring easter attendance as a portion of the enhanced membership----
#pascha_ratio <- easter/en_memb

parish_vita1$pascha_ratio17 <- parish_vita1$easter17/parish_vita1$en_memb17

summary(parish_vita1$pascha_ratio17)
#Need to take care of NA's and cases where en_memb = 0 when doing further analysis and modeling

parish_vita1$pascha_ratio16 <- parish_vita1$easter16/parish_vita1$en_memb16

parish_vita1$pascha_ratio15 <- parish_vita1$easter15/parish_vita1$en_memb15

parish_vita1$pascha_ratio14 <- parish_vita1$easter14/parish_vita1$en_memb14

parish_vita1$pascha_ratio13 <- parish_vita1$easter13/parish_vita1$en_memb13

parish_vita1$pascha_ratio12 <- parish_vita1$easter12/parish_vita1$en_memb12

parish_vita1$pascha_ratio11 <- parish_vita1$easter11/parish_vita1$en_memb11

parish_vita1$pascha_ratio10 <- parish_vita1$easter10/parish_vita1$en_memb10

parish_vita1$pascha_ratio09 <- parish_vita1$easter09/parish_vita1$en_memb09

parish_vita1$pascha_ratio08 <- parish_vita1$easter08/parish_vita1$en_memb08

parish_vita1$pascha_ratio07 <- parish_vita1$easter07/parish_vita1$en_memb07

parish_vita1$pascha_ratio06 <- parish_vita1$easter06/parish_vita1$en_memb06

parish_vita1$pascha_ratio05 <- parish_vita1$easter05/parish_vita1$en_memb05


# Giving_rate = (platpl + pledge)/plcard 

#Feature of Giving Rate----

parish_vita1$giving_rate17 <- (parish_vita1$pledge17/parish_vita1$plcard17)/100 #Offset previous data wrangling effects

parish_vita1$giving_rate16 <- parish_vita1$pledge16/parish_vita1$plcard16

parish_vita1$giving_rate15 <- parish_vita1$pledge15/parish_vita1$plcard15

#platpl13 is not available, use platpl15 instead
parish_vita1$giving_rate13 <- parish_vita1$pledge13/parish_vita1$plcard13

#platpl10 is not available, use platpl09 instead
parish_vita1$giving_rate10 <- parish_vita1$pledge10/parish_vita1$plcard10

parish_vita1$giving_rate09 <- parish_vita1$pledge09/parish_vita1$plcard09

parish_vita1$giving_rate08 <- parish_vita1$pledge08/parish_vita1$plcard08

parish_vita1$giving_rate07 <- parish_vita1$pledge07/parish_vita1$plcard07

parish_vita1$giving_rate06 <- parish_vita1$pledge06/parish_vita1$plcard06

parish_vita1$giving_rate05 <- parish_vita1$pledge05/parish_vita1$plcard05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$giving_rate14  <- (parish_vita1$giving_rate13 + parish_vita1$giving_rate15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$giving_rate11 <- parish_vita1$giving_rate10 - (parish_vita1$giving_rate10 - parish_vita1$giving_rate13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$giving_rate12 <- parish_vita1$giving_rate10 - (parish_vita1$giving_rate10 - parish_vita1$giving_rate13)*.66

#Feature of Adjusted Revenue Ratio----
#Non-operating revenue as a percentage of total revenue
# adjrev = nonop/totrev17 
# *Note: nonop = capit + endow + misrev + transr 

parish_vita1$adjrev17 <- parish_vita1$nonop_17/parish_vita1$totrev17

parish_vita1$adjrev16 <- parish_vita1$nonop_16/parish_vita1$totrev16

parish_vita1$adjrev15 <- parish_vita1$nonop_15/parish_vita1$totrev15

parish_vita1$adjrev13 <- parish_vita1$nonop_13/parish_vita1$totrev13

parish_vita1$adjrev10 <- parish_vita1$nonop_10/parish_vita1$totrev10

parish_vita1$adjrev09 <- parish_vita1$nonop_09/parish_vita1$totrev09

parish_vita1$adjrev08 <- parish_vita1$nonop_08/parish_vita1$totrev08

parish_vita1$adjrev07 <- parish_vita1$nonop_07/parish_vita1$totrev07

parish_vita1$adjrev06 <- parish_vita1$nonop_06/parish_vita1$totrev06

parish_vita1$adjrev05 <- parish_vita1$nonop_05/parish_vita1$totrev05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$adjrev14  <- (parish_vita1$adjrev13 + parish_vita1$adjrev15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$adjrev11 <- parish_vita1$adjrev10 - (parish_vita1$adjrev10 - parish_vita1$adjrev13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$adjrev12 <- parish_vita1$adjrev10 - (parish_vita1$adjrev10 - parish_vita1$adjrev13)*.66

# *Note: norm + diohlp = operev
# *Note: platpl + inves + othinc + beques = norm
# norm_operev_ratio = norm/operev (should be mostly one; less than one means receiving dio help)

parish_vita1$norm_operev_ratio17 <- parish_vita1$norm_17/parish_vita1$operev17

parish_vita1$norm_operev_ratio16 <- parish_vita1$norm_16/parish_vita1$operev16

parish_vita1$norm_operev_ratio15 <- parish_vita1$norm_15/parish_vita1$operev15

parish_vita1$norm_operev_ratio13 <- parish_vita1$norm_13/parish_vita1$operev13

parish_vita1$norm_operev_ratio10 <- parish_vita1$norm_10/parish_vita1$operev10

parish_vita1$norm_operev_ratio09 <- parish_vita1$norm_09/parish_vita1$operev09

parish_vita1$norm_operev_ratio08 <- parish_vita1$norm_08/parish_vita1$operev08

parish_vita1$norm_operev_ratio07 <- parish_vita1$norm_07/parish_vita1$operev07

parish_vita1$norm_operev_ratio06 <- parish_vita1$norm_06/parish_vita1$operev06

parish_vita1$norm_operev_ratio05 <- parish_vita1$norm_05/parish_vita1$operev05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$norm_operev_ratio14  <- (parish_vita1$norm_operev_ratio13 + parish_vita1$norm_operev_ratio15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$norm_operev_ratio11 <- parish_vita1$norm_operev_ratio10 - (parish_vita1$norm_operev_ratio10 - parish_vita1$norm_operev_ratio13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$norm_operev_ratio12 <- parish_vita1$norm_operev_ratio10 - (parish_vita1$norm_operev_ratio10 - parish_vita1$norm_operev_ratio13)*.66

#Feature of cash and investment vs operating expenses----
#Cash and investment on hand at the end of each year

# cashinves_opex = (cash + invest)/opex
#This variable shows us how much the reserves can help pay the operating expenses when needed.

parish_vita1$cashinves_opex17 <- (parish_vita1$cash17 + parish_vita1$invest17)/parish_vita1$opexp17 

parish_vita1$cashinves_opex16 <- (parish_vita1$cash16 + parish_vita1$invest16)/parish_vita1$opexp16 

parish_vita1$cashinves_opex15 <- (parish_vita1$cash15 + parish_vita1$invest15)/parish_vita1$opexp15 

parish_vita1$cashinves_opex13 <- (parish_vita1$cash13 + parish_vita1$invest13)/parish_vita1$opexp13 

parish_vita1$cashinves_opex10 <- (parish_vita1$cash10 + parish_vita1$invest10)/parish_vita1$opexp10 

parish_vita1$cashinves_opex09 <- (parish_vita1$cash09 + parish_vita1$invest09)/parish_vita1$opexp09 

parish_vita1$cashinves_opex08 <- (parish_vita1$cash08 + parish_vita1$invest08)/parish_vita1$opexp08 

parish_vita1$cashinves_opex07 <- (parish_vita1$cash07 + parish_vita1$invest07)/parish_vita1$opexp07 

parish_vita1$cashinves_opex06 <- (parish_vita1$cash06 + parish_vita1$invest06)/parish_vita1$opexp06 

parish_vita1$cashinves_opex05 <- (parish_vita1$cash05 + parish_vita1$invest05)/parish_vita1$opexp05 

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$cashinves_opex14  <- (parish_vita1$cashinves_opex13 + parish_vita1$cashinves_opex15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$cashinves_opex11 <- parish_vita1$cashinves_opex10 - (parish_vita1$cashinves_opex10 - parish_vita1$cashinves_opex13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$cashinves_opex12 <- parish_vita1$cashinves_opex10 - (parish_vita1$cashinves_opex10 - parish_vita1$cashinves_opex13)*.66

#Parish Expense Indicators 
#(Capital expenditures, assessments to diocese, other expenses, outreach and mission expenses, 
#seminary expenses, outreach expenses, operating expenses, total expenses)


# business_exp = (capexp + todioc)/totexp
#Business expenses as a portion of total expenses----

#todioc17 not available use todioc16 instead.
parish_vita1$business_exp17 <- (parish_vita1$capexp17 + parish_vita1$todioc16)/parish_vita1$totrev17

parish_vita1$business_exp16 <- (parish_vita1$capexp16 + parish_vita1$todioc16)/parish_vita1$totrev16

parish_vita1$business_exp15 <- (parish_vita1$capexp15 + parish_vita1$todioc15)/parish_vita1$totrev15

parish_vita1$business_exp13 <- (parish_vita1$capexp13 + parish_vita1$todioc13)/parish_vita1$totrev13

parish_vita1$business_exp10 <- (parish_vita1$capexp10 + parish_vita1$todioc10)/parish_vita1$totrev10

parish_vita1$business_exp09 <- (parish_vita1$capexp09 + parish_vita1$todioc09)/parish_vita1$totrev09

parish_vita1$business_exp08 <- (parish_vita1$capexp08 + parish_vita1$todioc08)/parish_vita1$totrev08

parish_vita1$business_exp07 <- (parish_vita1$capexp07 + parish_vita1$todioc07)/parish_vita1$totrev07

parish_vita1$business_exp06 <- (parish_vita1$capexp06 + parish_vita1$todioc06)/parish_vita1$totrev06

parish_vita1$business_exp05 <- (parish_vita1$capexp05 + parish_vita1$todioc05)/parish_vita1$totrev05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$business_exp14  <- (parish_vita1$business_exp13 + parish_vita1$business_exp15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$business_exp11 <- parish_vita1$business_exp10 - (parish_vita1$business_exp10 - parish_vita1$business_exp13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$business_exp12 <- parish_vita1$business_exp10 - (parish_vita1$business_exp10 - parish_vita1$business_exp13)*.66


#Feature of Mission-related Expenses----
#Mission expenses as a portion of total expenses
# missio_exp = (misexp + semexp + tranex + outexp)/totexp

parish_vita1$missio_exp17 <- (parish_vita1$misexp17 + parish_vita1$semexp17 + parish_vita1$tranex17 + parish_vita1$outexp17)/parish_vita1$totrev17

parish_vita1$missio_exp16 <- (parish_vita1$misexp16 + parish_vita1$semexp16 + parish_vita1$tranex16 + parish_vita1$outexp16)/parish_vita1$totrev16

parish_vita1$missio_exp15 <- (parish_vita1$misexp15 + parish_vita1$semexp15 + parish_vita1$tranex15 + parish_vita1$outexp15)/parish_vita1$totrev15

parish_vita1$missio_exp13 <- (parish_vita1$misexp13 + parish_vita1$semexp13 + parish_vita1$tranex13 + parish_vita1$outexp13)/parish_vita1$totrev13

parish_vita1$missio_exp10 <- (parish_vita1$misexp10 + parish_vita1$semexp10 + parish_vita1$tranex10 + parish_vita1$outexp10)/parish_vita1$totrev10

parish_vita1$missio_exp09 <- (parish_vita1$misexp09 + parish_vita1$semexp09 + parish_vita1$tranex09 + parish_vita1$outexp09)/parish_vita1$totrev09

parish_vita1$missio_exp08 <- (parish_vita1$misexp08 + parish_vita1$semexp08 + parish_vita1$tranex08 + parish_vita1$outexp08)/parish_vita1$totrev08

parish_vita1$missio_exp07 <- (parish_vita1$misexp07 + parish_vita1$semexp07 + parish_vita1$tranex07 + parish_vita1$outexp07)/parish_vita1$totrev07

parish_vita1$missio_exp06 <- (parish_vita1$misexp06 + parish_vita1$semexp06 + parish_vita1$tranex06 + parish_vita1$outexp06)/parish_vita1$totrev06

#semexp05 not available, use semexp06 instead.
parish_vita1$missio_exp05 <- (parish_vita1$misexp05 + parish_vita1$semexp06 + parish_vita1$tranex05 + parish_vita1$outexp05)/parish_vita1$totrev05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$missio_exp14  <- (parish_vita1$missio_exp13 + parish_vita1$missio_exp15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$missio_exp11 <- parish_vita1$missio_exp10 - (parish_vita1$missio_exp10 - parish_vita1$missio_exp13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$missio_exp12 <- parish_vita1$missio_exp10 - (parish_vita1$missio_exp10 - parish_vita1$missio_exp13)*.66

#Feature of surplus or deficit budgets----
#oper_ov_exp = totrev/totexp
#Balanced budget = 1, surplus > 1 and deficit < 1

parish_vita1$oper_ov_exp17 <- parish_vita1$totrev17/parish_vita1$totexp17

parish_vita1$oper_ov_exp16 <- parish_vita1$totrev16/parish_vita1$totexp16

parish_vita1$oper_ov_exp15 <- parish_vita1$totrev15/parish_vita1$totexp15

parish_vita1$oper_ov_exp13 <- parish_vita1$totrev13/parish_vita1$totexp13

parish_vita1$oper_ov_exp10 <- parish_vita1$totrev10/parish_vita1$totexp10

parish_vita1$oper_ov_exp09 <- parish_vita1$totrev09/parish_vita1$totexp09

parish_vita1$oper_ov_exp08 <- parish_vita1$totrev08/parish_vita1$totexp08

parish_vita1$oper_ov_exp07 <- parish_vita1$totrev07/parish_vita1$totexp07

parish_vita1$oper_ov_exp06 <- parish_vita1$totrev06/parish_vita1$totexp06

parish_vita1$oper_ov_exp05 <- parish_vita1$totrev05/parish_vita1$totexp05

#Estimate missing data----
#Missing data estimations
#For 2014, take the average of 2013 and 2015:

parish_vita1$oper_ov_exp14  <- (parish_vita1$oper_ov_exp13 + parish_vita1$oper_ov_exp15)/2

#For 2011 and 2012, 
#2011 is 2010 - (2010 - 2013) x 33%

parish_vita1$oper_ov_exp11 <- parish_vita1$oper_ov_exp10 - (parish_vita1$oper_ov_exp10 - parish_vita1$oper_ov_exp13)*.33

#2012 is 2010 - (2010 - 2013) x 66%
parish_vita1$oper_ov_exp12 <- parish_vita1$oper_ov_exp10 - (parish_vita1$oper_ov_exp10 - parish_vita1$oper_ov_exp13)*.66

#Select a subset of the new features

parish_vita2 <- parish_vita1 %>% 
  select(grep("PR", names(parish_vita1)), grep("parish_life", names(parish_vita1)),
         grep("attendance", names(parish_vita1)), grep("all_EU", names(parish_vita1)),
         grep("pascha_ratio", names(parish_vita1)), grep("giving_rate", names(parish_vita1)),
         grep("adjrev", names(parish_vita1)), grep("norm_operev_ratio", names(parish_vita1)),
         grep("cashinves_opex", names(parish_vita1)), grep("business_exp", names(parish_vita1)),
         grep("missio_exp", names(parish_vita1)), grep("oper_ov_exp", names(parish_vita1)),
         edw_org_id, Has_clergy_now, tenyrASAdelta, clerics_employed, TLIFECODE, TSEGNUM
  )

#Remove Inf's and NA's

parish_vita2 <- parish_vita2[is.finite(rowSums(parish_vita2)),]

#Create a new variable classifying at risk vs. vibrant parishes based on tenyrASAdelta----
# 1 = -0.56 or less; 2 = between -0.26 and -0.56; 3 = between -0.04 and -0.26; 4 = greater than -0.04

#Quartile summary of tenyrASAdelta----
summary(parish_vita2$tenyrASAdelta)

# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -15.57895  -0.55475  -0.25967  -0.37688  -0.04124   0.88350 

parish_vita2 <- parish_vita2 %>% 
  mutate(., vital_class = NA ) %>% 
  mutate(., vital_class = ifelse(tenyrASAdelta <= -0.56, 1, vital_class)) %>% 
  mutate(., vital_class = ifelse(tenyrASAdelta <= -0.26 & tenyrASAdelta > -0.56, 2, vital_class)) %>% 
  mutate(., vital_class = ifelse(tenyrASAdelta <= -0.04 & tenyrASAdelta > -0.26, 3, vital_class)) %>% 
  mutate(., vital_class = ifelse(tenyrASAdelta > -0.04, 4, vital_class))

#Remove tenyrASAdelta

parish_vita3 <- parish_vita2 %>%
  select(-tenyrASAdelta)

#Split dataset into training, test, and validation sets.

# spec <- c(train = .6, test = .2, validate = .2)
# 
# g = sample(cut( 
#   seq(nrow(parish_vita3)), 
#   nrow(parish_vita3)*cumsum(c(0,spec)),
#   labels = names(spec)
# ))
# 
# res <- split(parish_vita3, g)

#Split sample into training and test sets----

sample <- sample.split(parish_vita3$vital_cla ss, SplitRatio = .75)
train <- subset(parish_vita3, sample == TRUE)
test <- subset(parish_vita3, sample == FALSE)
dim(train)
dim(test)

#Change vital_class to factors for classification

train$vital_class <- as.character(train$vital_class)
train$vital_class <- as.factor(train$vital_class)

#Implement RandomForest on training set----
set.seed(300)

rf_vital_parish <- randomForest(
  vital_class ~ .,
  data=train,
  mtry=16,
  importance = TRUE
)

pred <- predict(rf_vital_parish, newdata=test[-156])

cm <- table(test[,156], pred)


#Create a new file and export to Domo directly----
DomoR::create(parish_vita2, name="Parish Vitality DF with ratios", description="Parish Vitality Project")

#For updating the above file in Domo----
DomoR::replace_ds("file_token", parish_vita2)

#Conduct a test for feature imnportance----
# Perform Boruta search

parish_vital4 <- parish_vita2 %>% 
  select(-vital_class)

boruta_output5 <- Boruta(tenyrASAdelta ~ ., data=parish_vital4, doTrace=3)  

names(boruta_output5)  #Shows what's in the output file.

# Get significant variables including tentatives
boruta_signif5 <- getSelectedAttributes(boruta_output5, withTentative = TRUE)
print(boruta_signif5)  

# Do a tentative rough fix----
roughFixMod5 <- TentativeRoughFix(boruta_output5)
boruta_signif5 <- getSelectedAttributes(roughFixMod5)
print(boruta_signif5)

# [1] "PR10"             "PR09"             "PR12"             "parish_life17"    "parish_life16"    "parish_life15"    "parish_life13"    "parish_life10"   
# [9] "parish_life09"    "parish_life08"    "parish_life07"    "parish_life06"    "parish_life05"    "parish_life14"    "parish_life11"    "parish_life12"   
# [17] "attendance17"     "attendance16"     "attendance15"     "attendance09"     "attendance08"     "attendance07"     "attendance05"     "attendance06"    
# [25] "attendance10"     "attendance13"     "attendance14"     "attendance11"     "attendance12"     "all_EU17"         "all_EU16"         "all_EU15"        
# [33] "all_EU07"         "all_EU06"         "all_EU05"         "all_EU13"         "all_EU14"         "pascha_ratio17"   "pascha_ratio16"   "pascha_ratio15"  
# [41] "pascha_ratio14"   "pascha_ratio13"   "pascha_ratio12"   "pascha_ratio11"   "pascha_ratio10"   "pascha_ratio09"   "pascha_ratio08"   "giving_rate16"   
# [49] "giving_rate13"    "giving_rate10"    "giving_rate09"    "giving_rate06"    "giving_rate14"    "giving_rate11"    "giving_rate12"    "missio_exp11"    
# [57] "missio_exp12"     "Has_clergy_now"   "clerics_employed"

# Plot variable importance
plot(boruta_output5, cex.axis=.7, las=2, xlab="", main="Variable Importance in Random Forest Set")  

#Amend selected columns in parish_vita3 based on the above results above

#Function to insert more elements to a vector----

ins <- function(a, to.insert=list(), pos=c()) {
  
  c(a[seq(pos[1])], 
    to.insert[[1]], 
    a[seq(pos[1]+1, pos[2])], 
    to.insert[[2]], 
    a[seq(pos[2], length(a))]
  )
}

idnames <- c("edw_org_id", "vital_class")
tapnames <- c("TLIFECODE", "TSEGNUM")

list_of_names <- ins(boruta_signif5, list(idnames, tapnames), pos=c(9, 17))

parish_vita5 <- parish_vita3[, colnames(parish_vita3) %in% list_of_names]

#Rerun Random Forest modeling based on the newly engineered dataframe----

#Split sample into training and test sets----

sample <- sample.split(parish_vita5$vital_class, SplitRatio = .75)
train <- subset(parish_vita5, sample == TRUE)
test <- subset(parish_vita5, sample == FALSE)
dim(train)
dim(test)

#Write CSV files for use later in NeuralNet----

sample1 <- sample.split(parish_vita2$vital_class, SplitRatio = .75)
train1 <- subset(parish_vita2, sample == TRUE)
test1 <- subset(parish_vita2, sample == FALSE)

write.csv(train1,file=paste0(sdir, "parish_vitaltrain.csv"))
write.csv(test1,file=paste0(sdir, "parish_vitaltest.csv"))

#Create new files and export to Domo directly----
#Files for Domo Data Science suite

DomoR::create(train1, name="Parish Vitality Prediction Train", description="Parish Vitality Project")
#Created file has this number:"4e6e593e-45e0-4caa-a70f-467dc5b3734c"

DomoR::create(test1, name="Parish Vitality Prediction Test", description="Parish Vitality Project")
#Created file has this number:"6b57f92f-e0ba-4b36-8a14-ffde0b5ce4d5"

#Change vital_class to factors for classification

train$vital_class <- as.character(train$vital_class)
train$vital_class <- as.factor(train$vital_class)

#Implement RandomForest on training set----
set.seed(300)

rf_vital_parish <- randomForest(
  vital_class ~ .,
  data=train,
  mtry=62,
  importance = TRUE
)

pred <- predict(rf_vital_parish, newdata=test[-63])

cm <- table(test[,63], pred)

#Evaluation of Random Forest modeling----
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, 
                     repeats = 10)

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16, 32, 62))

set.seed(300)

m_rf <- train(vital_class ~ ., data=train, method="rf",
              metric="Kappa", trControl=ctrl,
              tuneGrid=grid_rf)

#Comparison with a boosted tree----

grid_c50 <- expand.grid(.model="tree",
                        .trials=c(10, 20, 30, 40),
                        .winnow="FALSE")

set.seed(300)

m_c50 <- train(vital_class ~ ., data=train, method="C5.0",
               metric="Kappa", trControl=ctrl,
               tuneGrid=grid_c50)

#Read in census data in RDS format----

census_train <- readRDS(file=paste0(sdir, "train_NHGIS.rds"), refhook = NULL)

census_test <- readRDS(file=paste0(sdir, "test_NHGIS.rds"), refhook = NULL)

census_combo <- rbind(census_train, census_test)

