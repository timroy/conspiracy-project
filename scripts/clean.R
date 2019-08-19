rm(list = ls())

pacman::p_load(tidyverse, haven, anchors, dummies)

data.og <- data.original <- read_dta("./data_raw/NDI_Ukraine_Combined.dta")
spatial <- read_rds("./data_clean/spatial_survey_data.rds")
first_wave <- read_dta("./data_raw/NDI_Ukraine_Round1_Final.dta") # original coding

temp <- data.og %>% dplyr::select(c(## Outcome Variables
                                    # Why was Boris Nemstov killed (wave 1)
                                    BOROPP,	# Because of his overall opposition to the Russian gov
                                    BORTHRT,	#	Threatened to expose Russian involvement in Ukr conflict
                                    BORISLAM,	# Spoke out against Islamist attacks in Paris
                                    BORNEG,	#	To create a negative image of Putin/Russian gov
                                    BORDONTK,	#	Don't know who Boris Nemstov is
                                    BORUNCRT,	#	Don't know why he was killed
                                    BORDK,	# Why was Boris Nemstov killed	DK
                                    BORRA,	# Why was Boris Nemstov killed	RA
                                    # Reason for the start of the conflict (wave 1)
                                    #CSRUSS, # Russian desire to regain control of lost territory since collapse of USSR
                                    #CSUKRGOV, # Ukrainian government's failure to protect the rights of Russian speakers
                                    #CSHISTDIV, # Historical divisions between East/West Ukraine
                                    #CSWESTSUP, # Western support for Maidan protestors
                                    #CSDONBRUS, # Historical ties between Donbas and Russia
                                    #CSDESTBLZ, # Russian desire to destabilize the Ukrainian government
                                    #CSSTPEU, # Russian desire to stop Ukraine from moving towards NATO/EU membership
                                    #CSYANUCORR, # Failure of Yanukovych gov to curb corruption
                                    ######## Waves 1-3
                                    CSBLAME,
                                    ######## Waves 1, 4, 7, & 8
                                    MH17, # In July 2014, Malaysia Airlines Flight 17 crashed over Donetsk. In your opinion, what is the most likely reason for this?										
                                    ######## Wave 7
                                    INFRUSPR,	# Is Russian propaganda being spread in Ukraine? 	
                                    ######## Wave 8
                                    INFGENPR, #Is propaganda or disinformation being spread in Ukraine?
                                    INFPRORI,	# if yes, Who is responsible for spreading propaganda?	
                                    # What are your main sources of information for receiving news about 
                                    # politics and current events? Please tell me, which is your first main 
                                    # source of information and your second main source.				
                                    ######## Waves 1-2
                                    INFSOUFC, # First choice
                                    INFSOUSC, # Second choice
                                    ######## Wave 7
                                    INFSOMAI, # Main source
                                    ######## Waves 5 & 7
                                    INFSOTVF, #First choice TV
                                    INFSOTVS, #Second choice TV
                                    INFSOINF, #First choice Internet
                                    INFSOINS, #Second choice Internet
                                    INFSOSMF, #First choice Social media
                                    INFSOSMS, #Second choice Social media
                                    ####### Wave 5
                                    INFRAD01, # First choice Radio
                                    INFRAD02, # Second choice Radio
                                    INFSONEF, # First choice Newspaper
                                    INFSONES, # Second choice newspaper
                                    ######## Waves 1-6 & 8
                                    INFINTAC,	#How often do you access the internet for news?
                                    ######## Wave 7
                                    ACCSPUKR,	# The EU decided to fast track Ukraine’s accession to EU	Sample
                                    ACCSN, # ??????		
                                    ACCW0UKR, # Would you pass this story on?
                                    ACCIFTLD,	#	If you were told…Answer: 3P
                                    ACCW1UKR,	#	Would you pass this story on?
                                    ACCFRUKR,	#	If a friend or family member told you…Answer: 3P
                                    ACCW2UKR,	#	Would you pass this story on?
                                    ACCTVUKR,	#	If a TV news broadcaster reported…Answer: 3P
                                    ACCW3UKR,	#	Would you pass this story on?
                                    #
                                    BLCKDESP,	#Ukraine lifted its blockade on trade with Donbas	Sample
                                    BLCKDE,	# ?? NA (check this)
                                    BLCKDEW0,	#	Would you pass this story on?
                                    BLCIFTLD,	#	If you heard...
                                    BLCKDEW1,	#	Would you pass this story on?
                                    BLCKDDNR,	#	If a DNR spokesman denied this, would you believe it?
                                    BLCKDEW2,	#	Would you pass this story on?
                                    BLCKUGOV,	#	If the Ukrainian government denied this, would you believe it?
                                    BLCKDEW3,	#	Would you pass this story on?
                                    #
                                    BMHOSPSP,	# Ukrainian soldiers responsible for bombing Donbas hospital	Sample
                                    BMHOSP,	# ??	NA (check this)
                                    BMHFRSPRP,	#	If a friend says this is Russian propoganda, would you believe it?
                                    BMHTVRUS,	#	A Ukrainian TV broadcaster said this is Russian propoganda, would you believe it?
                                    BMHFCRUS,	#	A Ukrainian fact-checking website said this is Russian propaganda, would you believe it?
                                    #
                                    NOLANGSP,	# Russian language would no longer be taught in schools	Sample
                                    NORSLANG,	#	NA ?? (Check this)
                                    NOLIFTLD,	#	Would you believe it?
                                    NOLANGFC,	# A Ukrainian fact-checking website says this is untrue, would you believe it?
                                    NORLANGF,	#	A friend or family member says this is untrue, would you believe it?
                                    #
                                    EUHLDSP,	# EU put Ukraine's accession to the EU on hold	Sample
                                    EUHLDAC,	# NA ??? (check this)	
                                    EUHLDW0,	#	Would you pass this story on?
                                    EUHLDIFTLD,	#	Would you believe it?
                                    EUHLDW1,	#	Would you pass this story on?
                                    EUHLDACF,	#	If a friend or family member told you, would you believe it?
                                    EUHLDW2,	#	Would you pass this story on?
                                    EUHLDTV,	#	If a TV news broadcaster reported this, would you believe it?
                                    EUHLDW3,	#	Would you pass this story on?
                                    # 
                                    HRABSESP,	# There was an increase in human rights abuses in Donbas	Sample
                                    HRABUSES,	# NA ?? (check this)	
                                    HRIFTLDABS,	# Would you believe it?
                                    HRIFRUSPAR,	#	If the Russian parliament reported this, would you believe it?
                                    HRIFEURPAR,	#	If the European parliament reported this, would you believe it?
                                    # 
                                    LTNGSP, # Shop assistant said there's increased looting in Donbas
                                    LTDNETSK, # NA
                                    LTIFTLD, # Would you believe it?
                                    LTREBLS, # If there's looting by rebels (opolchenie) in Donetsk?
                                    LTFGHT, # If there's looting by fighters (boeviki) in Donetsk?
                                    #
                                    ### Explanatory Variables ###
                                    #
                                    # Democracy - Wave 1-4, 6 & 8
                                    DEMLEVEL, # Democracy worse, same, or improving?
                                    DEMLVLYA, # Democracy under Yanukovych
                                    DEMLVLPO, # Democracy under Poroshenko
                                    # Anomia - Wave 3,4, & 6
                                    LSFALLEN, # On average, living standards have fallen in Ukraine since 20 Who do you most blame for this?								
                                    # Wave 1-4, 6, & 8
                                    # In your opinion are any of the following interested in hearing your opinions?						
                                    HEARORD, # Order
                                    HEARPRES, # President
                                    HEARNATG, # National government
                                    HEARMEMP, # Member of Parliament
                                    HEARGOVE, # Governor
                                    HEARMAYR, # Mayor
                                    HEARLCON, # Local councilor
                                    HEARPOLP, # Political Party
                                    # Happiness - Wave 7
                                    LSCONTNT, # To what extent do you feel content about your situation in life at the moment? (ordinal 1-5)							
                                    LSIMPRVE, # To what extent, if at all, do you feel you are able to improve your situation in life? (ordinal 1-3)							
                                    # Waves 1-4, 6-8
                                    LSNEXTGN, # Overall, do you expect the next generation to be …? (1 = worse, 2 = same, 3 = better)
                                    # Wave 2 
                                    INVLVPOL#, # How important is it for you to be involved in political life? (0 = Not important, 10 = extremely important)					
                                    # Demographics
                                    #RSPSEX, #sex (1 = male, 2 = female)
                                    #RSPAGE, #age
                                    #RSPEDUC, #education
                                    #RSPEMPL, #employment
                                    #RSPRELIG, #religion
                                    #Russian = RSPETHRU, #russian
                                    #Ukrainian = RSPETHUK, #ukrainian
                                    #Tatar = RSPETHTA, #tatar
                                    #Bulgarian = RSPETHBU, #bulgarian
                                    #Polish = RSPETHPO, #polish
                                    #Moldovan = RSPETHMO, #moldovan
                                    #Belarussian = RSPETHBE, #belarussian
                                    #Jewish =  RSPETHJW, #jewish
                                    #Other = RSPETHOT, #other ethnicity
                                    #RSPLANG, #language most spoken at home (1 = Russian, 2 = Ukrainian, 3 = Both, 4 = Other (There is no 4 in dataset, there is a 97 though))
                                    #convlang, # interview language (1= Ukrainian, 2= Russian, 3,4,5,6 = middle/other
                                    #macroreg,
                                    #oblast,
                                    #precinct,
                                    #wave,
                                    #nhhpsu, #total number of HHs in PSU
                                    #npsuss, #total number of PSUs per strata
                                    #indwt
))

# Attitudes towards political figures? (Which ones to choose)

# Binding original dataset with already recoded dataset from ethnicity-project
data <- cbind(spatial, temp)

# Checking variables and recoding
# Check if unique values match outputs from attributes() for all vars

#f <- function(x){
#  list(attributes(x), unique(x))
#}
#colnum2 <- 2*ncol(data)
#checkup <- sapply(data, f)
#print(checkup[1:50])
#print(checkup[51:100])
#print(checkup[101:150])
#print(checkup[151:colnum2])

# Coding -7, -8 and -9 as NAs for all variables
data <-  data %>% mutate_if(is.numeric, funs(ifelse(. %in% c(-8., -9, -7), NA, .)))

# MH17 - Wave 8 has option for Russia - probs will have to treat it as "Other"
attributes(data$MH17)
table(data$MH17)
wave8 <- dplyr::filter(data, wave == 8) 
notwave8 <- dplyr::filter(data, wave != 8) 
sum(wave8$MH17 == 97, na.rm = T) # other (28)
sum(wave8$MH17 == 4, na.rm = T) # Russia (2413)
sum(notwave8$MH17 == 97, na.rm = T) # ???? (780)  # What are these
sum(notwave8$MH17 == 4, na.rm = T) # ???? (497)

data <- replace.value(data, "MH17", from = 97, to = 4) # make 97s 4 (for now)

# Recoding binary Boris Nemtsov variables to a single categorical one
data$BORISASS <- NA
for(i in 1:nrow(filter(data, wave == 1))) {
  if(data$BOROPP[i] == 1) {
    data$BORISASS[i] <- 5 #"Opposition"
  }
  if(data$BORTHRT[i] == 1) {
    data$BORISASS[i] <- 4 #"Threat"
  }
  if(data$BORISLAM[i] == 1) {
    data$BORISASS[i] <- 3 #"Islam"
  }
  if(data$BORNEG[i] == 1) {
    data$BORISASS[i] <- 2 #"Negative"
  }
  if(data$BORUNCRT[i] == 1) {
    data$BORISASS[i] <- 1 #"Who?"
  }
}

#data$BORISASS <- factor(data$BORISASS)
#data$BORISASS <- relevel(data$BORISASS, ref = "Who?")

# Propaganda
data <- replace.value(data, "INFPRORI", from = 97, to = 4)

# Recoding No's (2) to 0 (some of these variables have Maybes (3))
vars <- c(
  # Propaganda
  "INFRUSPR",
  "INFGENPR",
  # Reason for conflict onset
  "CSRUSS", 
  "CSHISTDIV", 
  "CSDESTBLZ", 
  "CSUKRGOV",
  "CSWESTSUP",
  "CSDONBRUS",
  "CSSTPEU",
  "CSYANUCORR",
  # Would you pass these stories on?
  "ACCW0UKR", 
  "ACCW1UKR",
  "ACCW2UKR",
  "ACCW3UKR",
  "BLCKDEW0",
  "BLCKDEW1",
  "BLCKDEW2",
  "BLCKDEW3",
  "EUHLDW0",
  "EUHLDW1",
  "EUHLDW2",
  "EUHLDW3",
  # Political figures interested in hearing opinion
  "HEARPRES", # President
  "HEARNATG", # National government
  "HEARMEMP", # Member of Parliament
  "HEARGOVE", # Governor
  "HEARMAYR", # Mayor
  "HEARLCON", # Local councilor
  "HEARPOLP") # Political Party 

for(i in 1:length(vars)) {
  data <- anchors::replace.value(data, vars[i], from = 2, to = 0)
}

# Info source waves 1-2
data <- anchors::replace.value(data, c("INFSOUFC", "INFSOUSC"), to = 12, from = 97)
# group by source
data$INFSOUFC_grouped <- data$INFSOUFC
data$INFSOUSC_grouped <- data$INFSOUSC
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 1, from = c(4, 8)) # Ukrainian media
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 2, from = c(5, 9)) # Russian media
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 3, from = c(6, 10)) # Western media
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 4, from = 7) # Newspapers
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 5, from = 11) # Friends and family
data <- anchors::replace.value(data, c("INFSOUFC_grouped", "INFSOUSC_grouped"), 
                               to = 6, from = 12) # Other

# creating dummies (will need them when we use simcf)
data$UkrMedia <- ifelse(data$INFSOUFC_grouped == 1, 1, 0)
data$RusMedia <- ifelse(data$INFSOUFC_grouped == 2, 1, 0)
data$WestMedia <- ifelse(data$INFSOUFC_grouped == 3, 1, 0)
data$Newspaper <- ifelse(data$INFSOUFC_grouped == 4, 1, 0)
data$FrndFam <- ifelse(data$INFSOUFC_grouped == 5, 1, 0)
data$InfOther <- ifelse(data$INFSOUFC_grouped == 6, 1, 0)

# Main info source (wave 7)
data$TV_source <- ifelse(data$INFSOMAI == 1, 1, 0)
data$INT_source <- ifelse(data$INFSOMAI == 2, 1, 0)
data$SOC_source <- ifelse(data$INFSOMAI == 3, 1, 0)
data$Other_source <- ifelse(data$INFSOMAI == 97, 1, 0)

# Composite measure for  HEAR___ questions (anomia) HEARINDEX 
pacman::p_load(ltm)
heardat <- dplyr::select(data, (c("HEARPRES", # President
                       "HEARNATG", # National government
                       "HEARMEMP", # Member of Parliament
                       "HEARGOVE", # Governor
                       "HEARMAYR", # Mayor
                       "HEARLCON", # Local councilor
                       "HEARPOLP"#, # Political party
                       #"DEMLEVEL"
                       )))

cronbach.alpha(heardat, standardized = FALSE, CI = FALSE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = T)

data$HEARINDEX <- rowSums(heardat, na.rm = T)

# Binary data does not have missing values -> 
# need to go to missing values columns, and wherever there are 1s put NA in the column on the outcome var

# Fixing NAs in BORIS
for(i in 1:nrow(filter(data, wave == 1))) {
  if(data$BORDK[i] == 1) {
    data$BOROPP[i] <- NA
    data$BORTHRT[i] <- NA
    data$BORISLAM[i] <- NA
    data$BORNEG[i] <- NA
  }
}

for(i in 1:nrow(filter(data, wave == 1))) {
  if(data$BORDONTK[i] == 1) {
    data$BOROPP[i] <- NA
    data$BORTHRT[i] <- NA
    data$BORISLAM[i] <- NA
    data$BORNEG[i] <- NA
  }
}

for(i in 1:nrow(filter(data, wave == 1))) {
  if(data$BORRA[i] == 1) {
    data$BOROPP[i] <- NA
    data$BORTHRT[i] <- NA
    data$BORISLAM[i] <- NA
    data$BORNEG[i] <- NA
  }
}

for(i in 1:nrow(filter(data, wave == 1))) {
  if(data$BORUNCRT[i] == 1) {
    data$BOROPP[i] <- NA
    data$BORTHRT[i] <- NA
    data$BORISLAM[i] <- NA
    data$BORNEG[i] <- NA
  }
}

# Get macroreg code for wave 1 #############

# West 11, 14, 20, 23, 25, 9, 5, 18
# Center/North 26, 24, 3, 19, 12, 17, 4, 8
# East 6, 21, 7, 13, 10
# South 16, 15, 22
# Kyiv 2

data$data_region_Kyiv_city <- ifelse(data$oblast == 2, 1, 0)

data$data_region_West <- ifelse(data$oblast == 11 | 
                                   data$oblast == 14 | 
                                   data$oblast == 20 |
                                   data$oblast == 23 |
                                   data$oblast == 25 |
                                   data$oblast == 9 |
                                   data$oblast == 5 |
                                   data$oblast == 18, 1, 0)

data$data_region_South <- ifelse(data$oblast == 16 |
                                    data$oblast == 15 |
                                    data$oblast == 22, 1, 0)

data$data_region_East <- ifelse(data$oblast == 6 |
                                   data$oblast == 21 |
                                   data$oblast == 7 |
                                   data$oblast == 13 |
                                   data$oblast == 10, 1, 0)

data$data_region_Center_North <- ifelse(data$oblast == 26 |
                                           data$oblast == 24 |
                                           data$oblast == 3 |
                                           data$oblast == 19 |
                                           data$oblast == 12 |
                                           data$oblast == 17 |
                                           data$oblast == 4 |
                                           data$oblast == 8, 1, 0)

data$Religious <- data$RSPRELIG
data <- replace.value(data, "Religious", to = 1, from = c(2,3,4,5,6))
data <- replace.value(data, "Religious", to = 0, from = 7)

# CSBLAME - Other (97 to 9)
data <- replace.value(data, "CSBLAME", to = 9, from = 97)

# Taking raked weight from wave 1 and inputting it into entire dataset
### MAKE SURE RAKED WEIGHT IS THE SAME AS INDWT ###
#data$indwt[1:nrow(dplyr::filter(data, wave == 1))] <- first_wave$rakedwt
data$indwt[1:nrow(dplyr::filter(data, wave == 1))] <- first_wave$unrakedwt

# Should I make a minority ethnicity level instead/also? - minorities believe more in conspiracies

if(!file.exists("./data_clean")) dir.create("./data_clean")

write_rds(data, "./data_clean/conspiracy_data_clean.rds")
