##############################################################################
#                                                                             #
#                         Parish Vitality                                     #
#                                                                             #
#                         Author: The Rev. Alistair So-Schoos                 #
#                                                                             #
#                                                                             #
#                                                                             #
# DESCRIPTION: Cleaning of Domo processed parish closure dataframe            #
#                                                                             #
#                                                                             #       
#                                                                             #
#                                                                             #
###############################################################################

#                           Clean global environment----

rm(list=ls())

#Load required libraries----

suppressMessages(library(dplyr))
suppressMessages(library(scales))
suppressMessages(library(tidyverse))
suppressMessages(library(DomoR))
suppressMessages(library(data.table))

#Initiate Domo User Settings----

DomoR::init('company_name','enter_access_key')

sdir <- "~/Desktop/Parish_Vitality/"

#Load data from Domo----

#Parochial master data file.
#Process using dplyr.


PRDF <- DomoR::fetch('file_code') %>%  
  mutate_at(vars(comyth17,
                 commad17,
                 commun17,
                 othact17,
                 worsh17,
                 wkdyworsh17,
                 easter17,
                 sundeu17,
                 wkdyeu17,
                 priveu17,
                 sunoff17,
                 wkdyof17,
                 adconf17,
                 adform17,
                 plcard17,
                 pledge17,
                 platpl17,
                 inves_17,
                 othinc17,
                 beques17,
                 norm_17,
                 diohlp17,
                 operev17,
                 capit_17,
                 endow17,
                 misrev17,
                 transr17,
                 nonop_17,
                 totrev17,
                 othexp17,
                 outexp17,
                 opexp17,
                 capexp17,
                 misexp17,
                 semexp17,
                 tranex17,
                 nonexp17,
                 totexp17,
                 cash17,
                 invest17,
                 memb17), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  dplyr::mutate_at(vars(commad16,
                        commun16,
                        othact16,
                        worsh16,
                        wkdyworsh16,
                        weekly16,
                        easter16,
                        sundeu16,
                        wkdyeu16,
                        priveu16,
                        sunoff16,
                        wkdyof16,
                        marry16,
                        bury16,
                        adbapt16,
                        chbapt16,
                        adconf16,
                        chconf16,
                        receiv16,
                        ssenr16,
                        adeduc16,
                        adform16,
                        plcard16,
                        pledge16,
                        platpl16,
                        invest16,
                        othinc16,
                        beques16,
                        norm_16,
                        diohlp16,
                        operev16,
                        capit_16,
                        endow16,
                        misrev16,
                        transr16,
                        nonop_16,
                        totrev16,
                        todioc16,
                        othexp16,
                        outexp16,
                        opexp16,
                        capexp16,
                        misexp16,
                        semexp16,
                        tranex16,
                        nonexp16,
                        totexp16,
                        cash16,
                        invest16_1,
                        memb16), 
                   funs(as.numeric)) 

PRDF <- PRDF %>% 
  mutate_at(vars(memlst13,
                 gains13,
                 loss13,
                 othact13,
                 easter13,
                 sundeu13,
                 wkdyeu13,
                 priveu13,
                 sunoff13,
                 wkdyof13,
                 marry13,
                 bury13,
                 adbapt13,
                 chbapt13,
                 adconf13,
                 chconf13,
                 receiv13,
                 ssenr13,
                 adeduc13,
                 plcard13,
                 pledge13,
                 inves_13,
                 othinc13,
                 beques13,
                 norm_13,
                 diohlp13,
                 operev13,
                 capit_13,
                 endow13,
                 misrev13,
                 transr13,
                 nonop_13,
                 totrev13,
                 todioc13,
                 outexp13,
                 othexp13,
                 opexp13,
                 capexp13,
                 misexp13,
                 semexp13,
                 tranex13,
                 nonexp13,
                 totexp13,
                 cash13,
                 invest13), 
            funs(as.numeric)) %>% 
  rename(., memb13 = memlst13)

PRDF <- PRDF %>% 
  mutate_at(vars(memlst15,
                 gains15,
                 loss15,
                 memb15,
                 comyth15,
                 commad15,
                 commun15,
                 othact15,
                 worsh15,
                 weekly15,
                 easter15,
                 sundeu15,
                 wkdyeu15,
                 priveu15,
                 sunoff15,
                 wkdyof15,
                 marry15,
                 bury15,
                 adbapt15,
                 chbapt15,
                 adconf15,
                 receiv15,
                 ssenr15,
                 adeduc15,
                 adform15,
                 plcard15,
                 pledge15,
                 platpl15,
                 othinc15,
                 inves_15,
                 beques15,
                 norm_15,
                 diohlp15,
                 operev15,
                 capit_15,
                 endow15,
                 misrev15,
                 transr15,
                 nonop_15,
                 totrev15,
                 todioc15,
                 othexp15,
                 opexp15,
                 outexp15,
                 capexp15,
                 misexp15,
                 semexp15,
                 tranex15,
                 nonexp15,
                 totexp15,
                 cash15,
                 invest15,
                 memb15), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  mutate_at(vars(permembpledge10,
                 perasapledge10,
                 perunitpledge10,
                 easter10,
                 marry10,
                 bury10,
                 adbapt10,
                 chbapt10,
                 adconf10,
                 chconf10,
                 receiv10,
                 ssenr10,
                 adeduc10,
                 plcard10,
                 pledge10,
                 inves_10,
                 othinc10,
                 beques10,
                 norm_10,
                 diohlp10,
                 operev10,
                 capit_10,
                 endow10,
                 misrev10,
                 transr10,
                 nonop_10,
                 totrev10,
                 todioc10,
                 outexp10,
                 othexp10,
                 opexp10,
                 capexp10,
                 misexp10,
                 semexp10,
                 tranex10,
                 nonexp10,
                 totexp10,
                 cash10,
                 invest10), 
            funs(as.numeric)) %>% 
  mutate(., memb10 = round(pledge10/permembpledge10))


PRDF <- PRDF %>% 
  mutate_at(vars(memb09,
                 memlast09,
                 increase09,
                 decrease09,
                 worsh09,
                 platpl09,
                 commun09,
                 comyth09,
                 othact09,
                 pledge09,
                 easter09,
                 marry09,
                 bury09,
                 adbapt09,
                 chbapt09,
                 adconf09,
                 chconf09,
                 receiv09,
                 ssenr09,
                 adeduc09,
                 plcard09,
                 pledge09,
                 inves_09,
                 othinc09,
                 beques09,
                 norm_09,
                 diohlp09,
                 operev09,
                 capit_09,
                 endow09,
                 misrev09,
                 transr09,
                 nonop_09,
                 totrev09,
                 todioc09,
                 outexp09,
                 othexp09,
                 opexp09,
                 capexp09,
                 misexp09,
                 semexp09,
                 tranex09,
                 nonexp09,
                 totexp09,
                 cash09,
                 cash09a,
                 invest09), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  mutate_at(vars(memb08,
                 worsh08,
                 platpl08,
                 commun08,
                 comyth08,
                 othact08,
                 pledge08,
                 easter08,
                 marry08,
                 bury08,
                 adbapt08,
                 chbapt08,
                 adconf08,
                 chconf08,
                 receiv08,
                 ssenr08,
                 adeduc08,
                 plcard08,
                 pledge08,
                 inves_08,
                 othinc08,
                 beques08,
                 norm_08,
                 diohlp08,
                 operev08,
                 capit_08,
                 endow08,
                 misrev08,
                 transr08,
                 nonop_08,
                 totrev08,
                 todioc08,
                 outexp08,
                 othexp08,
                 opexp08,
                 capexp08,
                 misexp08,
                 semexp08,
                 tranex08,
                 nonexp08,
                 totexp08,
                 cash08,
                 invest08), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  mutate_at(vars(memb07,
                 worsh07,
                 confrm07,
                 bapt07,
                 sundeu07,
                 wkdyeu07,
                 priveu07,
                 sunoff07,
                 wkdyof07,
                 platpl07,
                 commun07,
                 comyth07,
                 othact07,
                 pledge07,
                 easter07,
                 marry07,
                 bury07,
                 adbapt07,
                 chbapt07,
                 adconf07,
                 chconf07,
                 receiv07,
                 ssenr07,
                 adeduc07,
                 plcard07,
                 inves_07,
                 othinc07,
                 beques07,
                 norm_07,
                 diohlp07,
                 operev07,
                 capit_07,
                 endow07,
                 misrev07,
                 transr07,
                 nonop_07,
                 totrev07,
                 todioc07,
                 outexp07,
                 othexp07,
                 opexp07,
                 capexp07,
                 misexp07,
                 semexp07,
                 tranex07,
                 nonexp07,
                 totexp07,
                 cash07,
                 invest07), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  mutate_at(vars(memb06,
                 worsh06,
                 confrm06,
                 bapt06,
                 sundeu06,
                 wkdyeu06,
                 priveu06,
                 sunoff06,
                 wkdyof06,
                 platpl06,
                 pledge06,
                 commun06,
                 comyth06,
                 othact06,
                 pledge06,
                 easter06,
                 marry06,
                 bury06,
                 adbapt06,
                 chbapt06,
                 adconf06,
                 chconf06,
                 receiv06,
                 ssenr06,
                 adeduc06,
                 plcard06,
                 inves_06,
                 othinc06,
                 beques06,
                 norm_06,
                 diohlp06,
                 operev06,
                 capit_06,
                 endow06,
                 misrev06,
                 transr06,
                 nonop_06,
                 totrev06,
                 todioc06,
                 outexp06,
                 othexp06,
                 opexp06,
                 capexp06,
                 misexp06,
                 semexp06,
                 tranex06,
                 nonexp06,
                 totexp06,
                 cash06,
                 invest06), 
            funs(as.numeric))

PRDF <- PRDF %>% 
  mutate_at(vars(memb05,
                 worsh05,
                 worsh05a,
                 confrm05,
                 bapt05,
                 sundeu05,
                 wkdyeu05,
                 priveu05,
                 sunoff05,
                 wkdyof05,
                 platpl05,
                 pledge05,
                 commun05,
                 comyth05,
                 othact05,
                 pledge05,
                 easter05,
                 marry05,
                 adbapt05,
                 chbapt05,
                 adconf05,
                 chconf05,
                 receiv05,
                 ssenr05,
                 adeduc05,
                 plcard05,
                 inves_05,
                 othinc05,
                 beques05,
                 norm_05,
                 diohlp05,
                 operev05,
                 capit_05,
                 endow05,
                 misrev05,
                 transr05,
                 nonop_05,
                 totrev05,
                 todioc05,
                 outexp05,
                 othexp05,
                 opexp05,
                 capexp05,
                 misexp05,
                 tranex05,
                 nonexp05,
                 totexp05,
                 cash05,
                 invest05,
                 closed_year,
                 memb16,
                 memb12,
                 memb04,
                 memb03,
                 memb02,
                 memb01,
                 memb00), 
            funs(as.numeric))


write.csv(PRDF,file=paste0(sdir, "PRDF.csv"))

closed_selections <- PRDF %>% 
  dplyr::filter(closed_year >= 2005 & closed_year <= 2017)

closed2013 <- PRDF %>% 
  dplyr::filter(closed_year == 2013)

closed2017 <- PRDF %>% 
  dplyr::filter(closed_year == 2017)

closed2017_18_19 <- PRDF %>% 
  dplyr::filter(closed_year >= 2017)

open_selection1 <- PRDF %>% 
  dplyr::filter(closed_indicator == 'N') %>% 
  dplyr::filter(operev17 > 100000000)   #.00 decimal removed, thus adding two digits 

A <-  open_selection1[sample(1:nrow(open_selection1), 35),]

open_selection2 <- PRDF %>% 
  dplyr::filter(closed_indicator == 'N') %>% 
  dplyr::filter(operev17 > 45000000 &  operev17 < 100000000) 

B <- open_selection2[sample(1:nrow(open_selection2), 110),]

open_selection3 <- PRDF %>% 
  dplyr::filter(closed_indicator == 'N') %>% 
  dplyr::filter(operev17 > 10000000 &  operev17 < 45000000) 

C <- open_selection3[sample(1:nrow(open_selection3), 390),]

open_selection4 <- PRDF %>% 
  dplyr::filter(closed_indicator == 'N') %>% 
  dplyr::filter(operev17 > 5000000 &  operev17 < 10000000) 

D <- open_selection4[sample(1:nrow(open_selection4), 120),]

open_selection5 <- PRDF %>% 
  dplyr::filter(closed_indicator == 'N') %>% 
  dplyr::filter(operev17 < 5000000) 

E <- open_selection5[sample(1:nrow(open_selection5), 170),]

#Create training set----

parishvital_training <- rbind(closed_selections, A, B, C, D, E)

#Create test set----

parishvital_testset <- anti_join(PRDF, parishvital_training, by='client_nbr')


#Create files for Domo----
DomoR::create(parishvital_training, name="Parish Training Set", description="Parish Vitality Project")

DomoR::create(parishvital_testset, name="Parish Test Set", description="Parish Vitality Project")
