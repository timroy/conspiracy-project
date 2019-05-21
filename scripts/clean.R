pacman::p_load(tidyverse, haven, anchors, dummies)
data.og <- read_dta("./data_raw/NDI_Ukraine_Combined.dta")

data <- data.og %>% dplyr::select(c(## Outcome Variables
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
                                    CSRUSS, # Russian desire to regain control of lost territory since collapse of USSR
                                    CSUKRGOV, # Ukrainian government's failure to protect the rights of Russian speakers
                                    CSHISTDIV, # Historical divisions between East/West Ukraine
                                    CSWESTSUP, # Western support for Maidan protestors
                                    CSDONBRUS, # Historical ties between Donbas and Russia
                                    CSDESTBLZ, # Russian desire to destabilize the Ukrainian government
                                    CSSTPEU, # Russian desire to stop Ukraine from moving towards NATO/EU membership
                                    CSYANUCORR, # Failure of Yanukovych gov to curb corruption
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
                                    INVLVPOL, # How important is it for you to be involved in political life? (0 = Not important, 10 = extremely important)					
                                    # Demographics
                                    RSPSEX, #sex (1 = male, 2 = female)
                                    RSPAGE, #age
                                    RSPEDUC, #education
                                    RSPEMPL, #employment
                                    RSPRELIG, #religion
                                    Russian = RSPETHRU, #russian
                                    Ukrainian = RSPETHUK, #ukrainian
                                    Tatar = RSPETHTA, #tatar
                                    Bulgarian = RSPETHBU, #bulgarian
                                    Polish = RSPETHPO, #polish
                                    Moldovan = RSPETHMO, #moldovan
                                    Belarussian = RSPETHBE, #belarussian
                                    Jewish =  RSPETHJW, #jewish
                                    Other = RSPETHOT, #other ethnicity
                                    RSPLANG, #language most spoken at home (1 = Russian, 2 = Ukrainian, 3 = Both, 4 = Other (There is no 4 in dataset, there is a 97 though))
                                    convlang, # interview language (1= Ukrainian, 2= Russian, 3,4,5,6 = middle/other
                                    macroreg))

# Attitudes towards political figures? (Which ones to choose) 
                                    
# Checking variables and recoding
# Check if unique values match outputs from attributes() for all vars
f <- function(x){
  list(attributes(x), unique(x))
}
colnum2 <- 2*ncol(data)
checkup <- sapply(data, f)
print(checkup[1:50])
print(checkup[51:100])
print(checkup[101:150])
print(checkup[151:colnum2])

# Coding -8 and -9 as NAs for all variables
data <-  data %>% mutate_if(is.numeric, funs(ifelse(. %in% c(-8., -9, -7), NA, .)))

# MH17 - Wave 8 has option for Russia - probs will have to treat it as "Other"
sum(data$MH17 == 97, na.rm = T)
sum(data$MH17 == 4, na.rm = T)
sum(data$MH17 == -8, na.rm = T)
sum(data$MH17 == -9, na.rm = T)
#data <- replace.value(data, "MH17", from = 97, to = 4) # should replace 97 to 4?

# Propaganda
data <- replace.value(data, "INFPRORI", from = 97, to = 4)

# Recoding No's (2) to 0 (some of these variables have Maybes (3))
data <- replace.value(data, c(# Propaganda
                              "INFRUSPR",
                              "INFGENPR",
                              # Reason for conflict onset
                              "CSRUSS", 
                              "CSHISTDIV", 
                              "CSDESTBLZ", 
                              "CSUKRGOV",
                              "CSWESTSUP",
                              "CSDPNBRUS",
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
                              "HEARPOLP"), # Political Party 
                              from = 2, to = 0)

# Factorize macroreg and make East the reference category
data$macroreg <- relevel(data$macroreg, ref = "East")
data <- data %>% mutate(macroreg = fct_recode(macroreg,  "Center_North" ="Center/North",
                                              "Kyiv_city" ="Kyiv city"))
# Sex
data <- cbind(data, dummy(data$RSPSEX))
data <- plyr::rename(data, c("data1" = "Male")) 
data <- plyr::rename(data, c("data2" = "Female"))

# Recoding Language at Home variable as factor with Russian as baseline
data$Language <- factor(data$RSPLANG,
                        labels = c("Russian", "Ukrainian", "Both", "Other"))
data$Language <- relevel(data$Language, ref = "Russian")

# Binary variable for people who identify as both Russian and Ukrainian (Rus_Ukr)
data$Rus_Ukr <- with(data, ifelse(Russian == 1 & Ukrainian == 1, 1, 0))

# Binary variable for people who identify solely as Russian (Only_Rus)
data$Only_Rus <- with(data, ifelse(Russian == 1 & Ukrainian == 0 &
                                     Tatar == 0 & Bulgarian == 0 &
                                     Polish == 0 & Jewish == 0 & 
                                     Moldovan == 0 & Other == 0 &
                                     Belarussian == 0, 1, 0))

# Binary variable for people who identify solely as Ukrainian (Only_Ukr)
data$Only_Ukr <- with(data, ifelse(Russian == 0 & Ukrainian == 1 &
                                     Tatar == 0 & Bulgarian == 0 &
                                     Polish == 0 & Jewish == 0 & 
                                     Moldovan == 0 & Other == 0 &
                                     Belarussian == 0, 1, 0)) 

# Binary variable for people who identify as other than only Russian or only Ukrainian or both (All_Other)
data$All_Other <- with(data, ifelse(Only_Ukr == 0 & 
                                      Only_Rus == 0 & 
                                      Rus_Ukr == 0, 1, 0))

# Factorize ethnicity
data$Ethnicity <- NA
for(i in 1:nrow(data)) {
  if(data$Only_Rus[i] == 1) {
    data$Ethnicity[i] <- "Only Russian"
  }
  if(data$Rus_Ukr[i] == 1) {
    data$Ethnicity[i] <- "Russian & Ukrainian"
  }
  if(data$Only_Ukr[i] == 1) {
    data$Ethnicity[i] <- "Only Ukrainian"
  }
  if(data$All_Other[i] == 1) {
    data$Ethnicity[i] <- "Other"
  }
}

data$Ethnicity <- factor(data$Ethnicity, labels = c("Russian", "Ukrainian", "Both", "Other"))
data$Ethnicity <- relevel(data$Ethnicity, ref = "Russian")

# Should I make a minority level instead?
