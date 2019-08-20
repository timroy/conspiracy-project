#rm(list = ls())

options(scipen = 999)

pacman::p_load(survey, tidyverse, haven, nnet, mlogit, texreg, stargazer, clusterSEs, MultinomialCI, rcompanion)

if(!file.exists("./figs")) dir.create("./figs")
if(!file.exists("./tables")) dir.create("./tables")

data.og <- read_rds("./data_clean/conspiracy_data_clean.rds")
data.attr <- read_dta("./data_raw/NDI_Ukraine_Combined.dta")

# Histogram function
histogram <- function(df) {
  
  # Histograms of all the variables
  df %>%
    gather("key", "value") %>%
    group_by(key) %>% 
    mutate(avg = mean(value, na.rm = T)) %>% 
    ggplot(aes(value)) + 
    geom_histogram() +
    facet_wrap(~key, scales = "free") +
    #geom_vline(aes(xintercept = avg, group = key), color = "red") +
    theme_minimal()
}

data <- data.og

# taking out wave1
#data <- dplyr::filter(data.og, wave!= 1)
wave1 <- dplyr::filter(data.og, wave== 1)

# Table for oblast/wave
data <- mutate(data, wave = as_factor(wave), oblast = as_factor(oblast))

pacman::p_load(kableExtra, janitor)
data %>% group_by(wave, oblast) %>% dplyr::summarise(count = n()) %>%
  spread(wave, count)%>%
  adorn_totals(., where = c("row", "col")) %>%
  kable(format = "latex", booktabs = TRUE, caption = "Sample size per wave per oblast") %>%
  column_spec(9, bold = TRUE) %>%
  row_spec(26, bold =TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  
  cat(file = "./tables/oblast.tex")

# creating survey object
svdata <- svydesign(ids = ~precinct,
                    strata = ~wave + oblast, 
                    weights = ~indwt,
                    nest = TRUE,
                    data = data)

#############################################################################################

# Descriptive MH17 plots
# 1 = mechanical failure
# 2 = D/LPR
# 3 = Ukrainian forces
# 4 = Other
wavemeans <- svyby(~ MH17,
                   ~ wave,
                   svymean, na.rm = T,
                   design = svdata, deff = F)
confints <- confint(wavemeans)

svytable(~MH17 + wave, svdata)

props <- svymean(~interaction(MH17, wave), na.rm = T, svdata)

props2 <- groupwiseMean(as.factor(MH17) ~ wave, na.rm = T, data = data.og)

props <-  cbind(props, confint(props))
props <- as.data.frame(props)
Response <- rep(seq(1, 4, 1), 8)
Wave <- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4),
          rep(6, 4), rep(7, 4), rep(8, 4))
props <- cbind(props, Response, Wave)
props <- filter(props, Wave != 2 & Wave != 3 &
                  Wave != 5 & Wave != 6)

props$Wave <- as.factor(props$Wave)
props$Response <- factor(props$Response, levels = c(1,2,3,4),
                         labels = c("Mechanical Failure", 
                                    "Separatist Rebels",
                                    "Ukrainian Forces",
                                    "Other (Russia in Wave 8)"))

pacman::p_load("wesanderson")
plot_1 <- ggplot(props, aes(x = Wave, y = props, group = Response)) +
  theme_minimal() +
  geom_errorbar(aes(ymax = `97.5 %`, ymin = `2.5 %`), size = 0.8, width = 0.3, 
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Response), size = 4, 
             position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  labs(title = "Proportion of Respondents Stating Who Shot Down MH17", y = NULL, x = NULL) +
  theme(axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 17)) +
  theme(legend.position=c(0.5,0.15), legend.title = element_blank(), 
        legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid')) +
  guides(color = guide_legend(reverse = TRUE)) +
  coord_flip();plot_1

ggsave("./figs/descriptives_MH17_wave.png", plot_1)#, height = 6, width = 9)
####################################################################################################

# Descriptive Propaganda plots/stats
ruspropaganda <- svymean(~ INFRUSPR, na.rm = T, svdata); ruspropaganda # wave 7 (rus propaganda)
confint(ruspropaganda)

propaganda <- svymean(~ INFGENPR, na.rm = T, svdata);propaganda # wave 8 (any propaganda)
confint(propaganda)
mean(data$INFGENPR, na.rm = T) # Not far off from svymean

props <- svymean(~interaction(INFPRORI), na.rm = T, svdata) # 1= Russia, 2 = Ukraine, 3 = West
props <- cbind(props, confint(props))
props <- as.data.frame(props)
Response <- 1:4
#Ethnicity <- c(rep("Russian", 4), rep("Ukrainian", 4), rep("Both", 4), rep("Other", 4))
props <- cbind(Response, #Ethnicity, 
               props)
props$Response <- factor(props$Response, levels = c(1,2,3,4),
                         labels = c("Russia",
                                    "Ukraine",
                                    "The West",
                                    "Other"))
plot_2 <- ggplot(props, aes(x = Response, y = props)) +
  theme_minimal() +
  geom_errorbar(aes(ymax = `97.5 %`, ymin = `2.5 %`), size = 0.8, width = 0.3) +
  geom_point(size = 4) +
  labs(title = "Proportion of Respondents Stating Who Spreads Propaganda", y = NULL, x = NULL) +
  theme(axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 15)) +
  #theme(legend.position=c(0.85,0.8), #legend.title = element_blank(), 
  #legend.text = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid')) +
  guides(color = guide_legend(reverse = TRUE)) +
  coord_flip();plot_2 

ggsave("./figs/descriptives_INFPRORI.png", plot_2, height = 4)
###########################################################################################

# Descriptive Boris Nemtsov plots/stats
BOR <- dplyr::select(data.og, c(BOROPP, BORTHRT, BORISLAM, BORNEG, BORUNCRT))

boris <- data.frame(rep(NA, 5))

boris$Proportion <- sapply(X = , BOR, mean, na.rm = T)
boris$Statement <- c("BOROPP", "BORTHRT", "BORISLAM", "BORNEG", "BORUNCRTN")

boris$Statement <- factor(boris$Statement, levels = boris[order(boris$Proportion), "Statement"])


boris_plot <- ggplot(data = boris, aes(x = Statement, y = Proportion)) +
  geom_point(size = 4) +
  theme_minimal() +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Spoke out against islamist attacks",
    "To create a negative image of Putin",
    "Don't know who Boris Nemtsov is",
    "Opposition to Russian government",
    "Threatened to expose Russian \n involvement in conflict")) +
  labs(title = "Why was Boris Nemtsov Killed?", y = NULL, x = NULL) +
  theme(axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, .5)) 

ggsave("./figs/descriptive_boris.png", boris_plot, height = 4.5)

#Proportions <- rbind(groupwiseMean(formula =  BOROPP ~ Ethnicity, data = data.og, 0.95, 3)[3],
#               groupwiseMean(formula =  BORTHRT ~ Ethnicity, data = data.og, 0.95, 3)[3],
#               groupwiseMean(formula =  BORISLAM ~ Ethnicity, data = data.og, 0.95, 3)[3],
#               groupwiseMean(formula =  BORNEG ~ Ethnicity, data = data.og, 0.95, 3)[3])
#Ethnicity <- rep(c("Russian", "Ukrainian", "Both", "Other"), 4)
#Statement <- c(rep("BOROPP", 4), rep("BORTHRT", 4), rep("BORISLAM", 4), rep("BORNEG", 4))

#borismeans <- data.frame(cbind(Ethnicity, Statement, Proportions))
#borismeans$Ethnicity <- factor(borismeans$Ethnicity)
#borismeans$Statement <- factor(borismeans$Statement)

#################### Proportion with confidence intervals plot BORISASS #####################
x <- c(table(wave1$BORISASS), na = sum(is.na(wave1$BORISASS))) # successes
low <- multinomialCI(x,alpha=0.05,verbose=FALSE)[, 1] #lower CI
up <- multinomialCI(x,alpha=0.05,verbose=FALSE)[, 2] #upper CI
prop <- wave1 %>%
  group_by(BORISASS) %>%
  summarise(count = n() / nrow(.)) # proportions
#prop[6,1] <- 6 # removing hashtag to put "Missing value" on the plot
props <- cbind(prop, low, up)

# relevel to make plot ordered from high to low
props$BORISASS <- factor(props$BORISASS, 
                           levels = props[order(props$count), "BORISASS"])

boris_plot2 <- ggplot(data = props, aes(x = BORISASS, y = count)) +
  geom_errorbar(aes(ymax = up, ymin = low), size = 0.8, width = 0.3) +
  geom_point(size = 4) +
  theme_minimal() +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Spoke out against islamist attacks",
    "To create a negative image of Putin",
    "Don't know who Boris Nemtsov is",
    "Opposition to Russian government",
    "Threatened to expose Russian \n involvement in conflict",
    "Missing Value")) +
  labs(title = "Why was Boris Nemtsov Killed?", 
       y = NULL, 
       x = NULL) +
  theme(axis.text = element_text(size=13), 
        axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, .3));boris_plot2

ggsave("./figs/descriptive_boris_2.png", boris_plot2, width = 7, height = 4.5)

#ggplot(borismeans, aes(x = Statement, y = Proportions, group = Ethnicity)) +
#  theme_minimal() +
#  geom_point(aes(color = Ethnicity), size = 4,
#             position = position_dodge(width = 0.4)) +
#  geom_point(shape = 1, size = 4, color = "black", 
#             position = position_dodge(width = 0.4)) +
#  scale_x_discrete(labels = c("Opposition to Russian government",
#                              "Threatened to expose Russian involvement in conflict",
#                              "Spoke out against islamist attacks",
#                              "To create a negative imagre of Putin",
#                              "Don't know who Boris Nemtsov is")) +
#  scale_color_manual(values = wes_palette("Royal1")) +
#  labs(title = "Proportion of Respondents Agreeing With Statement", y = NULL, x = NULL) +
#  theme(axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
#  theme(plot.title = element_text(size = 17)) +
#  theme(legend.position=c(0.9,0.85), legend.title = element_blank(), 
#        legend.text = element_text(size = 13),
#        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
#  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid')) +
#  guides(color = guide_legend(reverse = TRUE)) +
#  coord_flip()

###########################################################################################

#CSRUSS, # Russian desire to regain control of lost territory since collapse of USSR
#CSUKRGOV, # Ukrainian government's failure to protect the rights of Russian speakers
#CSHISTDIV, # Historical divisions between East/West Ukraine
#CSWESTSUP, # Western support for Maidan protestors
#CSDONBRUS, # Historical ties between Donbas and Russia
#CSDESTBLZ, # Russian desire to destabilize the Ukrainian government
#CSSTPEU, # Russian desire to stop Ukraine from moving towards NATO/EU membership
#CSYANUCORR, # Failure of Yanukovych gov to curb corruption

CSVARS <- dplyr::select(data.og, c(CSRUSS, CSUKRGOV, CSHISTDIV, CSWESTSUP, 
                                   CSDONBRUS, CSDESTBLZ, CSSTPEU, CSYANUCORR))
CSmean <- data.frame(rep(NA, 8))
CSmean$Proportion <- sapply(CSVARS, mean, na.rm = T) # calculate proportions
CSmean$Statement <- rownames(CSmean) # create column for variable names

# relevel to make plot ordered from high to low
CSmean$Statement <- factor(CSmean$Statement, 
                           levels = CSmean[order(CSmean$Proportion), "Statement"])

CSVARS_plot <- 
  ggplot(CSmean, aes(x = Statement, y = Proportion)) +
  geom_point(size = 4) +
  theme_minimal() +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Ukrainian gov's failure to \n protect the rights of Russians",
    "Historial ties betw \n Donbas and Russia",
    "Historical divisions betw \n East and West of Ukraine",
    "Support from the \n West for Maidan",
    "Failure of Yanukovych \n gov to curb corruption",
    "Russian desire to regain territory",
    "Russian desire to \n destabilize Ukraine",
    "Russian desire to stop Ukraine's \n movement to NATO and EU")) +
  labs(title = "Why did the Conflict Start?", y = NULL, x = NULL) +
  theme(axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 1)) 

ggsave("./figs/descriptive_CSVARS.png", CSVARS_plot)

CSmeans <- rbind(groupwiseMean(formula = CSRUSS ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSUKRGOV ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSHISTDIV ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSWESTSUP ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSDONBRUS ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSDESTBLZ ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSSTPEU ~ Ethnicity, data = data.og, 0.95, 3)[3],
                 groupwiseMean(formula = CSYANUCORR ~ Ethnicity, data = data.og, 0.95, 3)[3])
Ethnicity <- rep(c("Russian", "Ukrainian", "Both", "Other"), 8)
Statement <- c(rep("CSRUSS", 4), rep("CSUKRGOV", 4), rep("CSHISTDIV", 4), rep("CSWESTSUP", 4), 
               rep("CSDONBRUS", 4), rep("CSDESTBLZ", 4), rep("CSSTPEU", 4), rep("CSYANUCORR", 4))
CSfinalmeans <- cbind(Ethnicity, Statement, CSmeans)

# get CI
CS <- list(wave1$CSRUSS, wave1$CSUKRGOV, wave1$CSHISTDIV, wave1$CSWESTSUP,
        wave1$CSDONBRUS, wave1$CSDESTBLZ, wave1$CSSTPEU, wave1$CSYANUCORR)
pe <- rep(NA, 8)
up <- rep(NA, 8)
low <- rep(NA, 8)
table(CS[[1]])[2]
for(i in 1:8) {
x <- table(CS[[i]])[2]  # successes
y <- table(CS[[i]])[1] # failures
pe[i] <- unlist(binom.test(c(x, y))[c(5,4)])[1] #pe 
up[i] <- unlist(binom.test(c(x, y))[c(5,4)])[2] #CI up
low[i] <- unlist(binom.test(c(x, y))[c(5,4)])[3] #CI low
} 

props <- data.frame(cbind(pe, up, low))
props$var <- c("CSRUSS", "CSUKRGOV", "CSHISTDIV", "CSWESTSUP", 
         "CSDONBRUS", "CSDESTBLZ", "CSSTPEU", "CSYANUCORR")

# relevel to make plot ordered from high to low
props$var <- factor(props$var, 
                         levels = props[order(props$pe), "var"])

CS_plot2 <- ggplot(data = props, aes(x = var, y = pe)) +
  geom_errorbar(aes(ymax = up, ymin = low), size = 0.8, width = 0.3) +
  geom_point(size = 4) +
  theme_minimal() +
  coord_flip() +
  scale_x_discrete(labels = c(
    "Ukrainian gov's failure to \n protect the rights of Russians",
    "Historial ties betw \n Donbas and Russia",
    "Historical divisions betw \n East and West of Ukraine",
    "Support from the \n West for Maidan",
    "Failure of Yanukovych \n gov to curb corruption",
    "Russian desire to regain territory",
    "Russian desire to \n destabilize Ukraine",
    "Russian desire to stop Ukraine's \n movement to NATO and EU")) +
  labs(title = "Why did the Conflict Start?", 
       y = NULL, 
       x = NULL) +
  theme(axis.text = element_text(size=13), 
        axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 15))

ggsave("./figs/descriptive_CSVARS_2.png", CS_plot2)

# CSBLAME descriptives - wave 1-3
#attributes(data.original$CSBLAME)
#unique(data.original$CSBLAME)

#histogram(data.frame(dplyr::select(data.og, CSBLAME)))

##############################################################################
##############################################################################
################################  Regressions  ###############################
##############################################################################
##############################################################################

# Some regressions (simcf needs dummies to work so we create two sets of IVs)
IVsBase_simcf <- "Only_Ukr + Rus_Ukr + All_Other + 
            data_Lang_Ukrainian + data_Lang_Both + data_Lang_Other + 
            data_convlang_1 + data_convlang_3 + 
            log(db_dist) + log(cr_dist) + log(eu_dist) +
            Female + RSPAGE + RSPEDUC"

IVsBase <- "Only_Ukr + Rus_Ukr + All_Other + 
            data_Lang_Ukrainian + data_Lang_Both + data_Lang_Other + 
            data_convlang_1 + data_convlang_3 + 
            log(db_dist) + log(cr_dist) + log(eu_dist) +
            Female + RSPAGE + RSPEDUC"

#data$RSPRELIG <- factor(data$RSPRELIG)

data.og %>%  group_by(wave) %>%  summarize_at(vars(), mean, na.rm = T)
data.og %>%  group_by(wave) %>%  summarize_at(vars(MH17), mean, na.rm = T)
table(data.og$wave, data.og$MH17)

# MH17 Asked in waves 1,4,7,8 - Using 1 as baseline
wave_fixed_effects_MH17 <- "+ data_wave_4 + data_wave_7" #+ data_wave_8"
#wave_fixed_effects_MH17 <- "+ as.factor(wave)"

# East as baseline
macro_reg_fixed_effect <- " + data_region_Center_North + 
                              data_region_Kyiv_city + 
                              data_region_South + 
                              data_region_West"

IVsBase_wd <- paste("~", IVsBase, wave_fixed_effects_MH17)
IVsBase_wdm <- paste(IVsBase_wd, macro_reg_fixed_effect)

data.og$MH17 <- as.numeric(data.og$MH17)

MH17.model <- list()

# Only Waves 1,4,7 (excluding religious)
MH17.model[[1]] <- multinom(formula =  formula(update.formula(IVsBase_wdm, 
                                                              MH17 ~ .
                                                              + LSNEXTGN # removes 2000 obs
)),
data = data.og, 
#weights = indwt,
na.action = "na.exclude",
subset = wave != 8
);screenreg(MH17.model[[1]])

# Waves 1,4 (all vars) 
MH17.model[[2]] <- multinom(formula =  formula(update.formula(IVsBase_wdm, 
                                                              MH17 ~ .
                                                              + INFINTAC
                                                              + LSNEXTGN # removes 2000 obs
                                                              + HEARINDEX
                                                              + DEMLEVEL
                                                              + Religious # removes wave 7
                                                              - data_wave_8
                                                              - data_wave_7
)),
data = data.og, 
#weights = indwt,
na.action = "na.exclude",
subset = wave != 7 & wave != 8
);screenreg(MH17.model[[2]])

# Wave 1 only
MH17.model[[3]] <- multinom(formula =  formula(update.formula(IVsBase_wdm, 
                                                              MH17 ~ .
                                                              # Internet access
                                                              + INFINTAC
                                                              # Info source
                                                              + RusMedia
                                                              + WestMedia
                                                              + Newspaper
                                                              + FrndFam
                                                              + InfOther
                                                              # Anomia
                                                              + DEMLEVEL
                                                              + HEARINDEX
                                                              + LSNEXTGN # removes 2000 obs
                                                              + Religious # removes wave 7
                                                              - data_wave_8
                                                              - data_wave_7
                                                              - data_wave_4
)),
data = data.og, 
#weights = indwt,
na.action = "na.exclude",
model = T,
subset = wave == 1
);screenreg(MH17.model[[3]])

# Wave 8 Regression (Outcome 4 = Blame Russia)
MH17.model[[4]] <- multinom(formula =  formula(update.formula(IVsBase_wdm, 
                                                              MH17 ~ .
                                                              + LSNEXTGN # removes 2000 obs
                                                              + HEARINDEX
                                                              + DEMLEVEL
                                                              + INFINTAC
                                                              - data_wave_4
                                                              - data_wave_7
                                                              + Religious
)),
data = data.og, 
#weights = indwt,
na.action = "na.exclude",
subset = wave == 8
);screenreg(MH17.model[[4]])

# Wave 7 only
MH17.model[[5]] <- multinom(formula =  formula(
  update.formula(IVsBase_wdm, 
                 MH17 ~ .
                 + LSNEXTGN # removes 2000 obs
                 + LSCONTNT
                 + LSIMPRVE
                 + TV_source 
                 + INT_source
                 + SOC_source
                 #+ Other_source # need to leave one out as reference
                 - data_wave_4
                 - data_wave_7
                 - data_wave_8
  )),
  data = data.og, 
  #weights = indwt,
  na.action = "na.exclude",
  subset = wave == 7
);screenreg(MH17.model[[5]])

screenreg(MH17.model[[4]], stars = 0.05)

plotreg(MH17.model[[4]])

stargazer(list(MH17.model[[1]], MH17.model[[2]]), 
          star.cutoffs = .05, 
          font.size = "tiny", 
          header = F,
          out = "./tables/MH17.models.tex", 
          column.sep.width = "1pt", 
          no.space = T,
          #notes.align = "l",
          add.lines = (list(c("Obsverations", "", "8965", "",  "", "5287", ""))),
          keep.stat = c("n", "aic")
          )

stargazer(list(MH17.model[[3]], MH17.model[[4]]), 
          star.cutoffs = .05, 
          font.size = "tiny",
          header = F,
          out = "./tables/MH17.models.2.tex", 
          column.sep.width = "1pt", 
          no.space = T, 
          keep.stat = c("n", "aic"), 
          notes.align = "l",
          add.lines = (list(c("Obsverations", "", "2237", "",  "", "2815", ""))), # add obs
          notes = "Demographics, region dummies, and constant ommitted to save space."
          )

stargazer(MH17.model[[5]], 
          star.cutoffs = .05, 
          font.size = "tiny", 
          header = F,
          out = "./tables/MH17.models.3.tex", 
          keep.stat = c("n", "aic"), 
          #notes.align = "l", 
          column.sep.width = "1pt",
          add.lines = (list(c("Obsverations", "", "3016", ""))),
          no.space = T
          )
          

#mlogit(MH17 ~ Female, data.og)

#mlogit(formula =  update.formula(IVsBase_wdm, MH17 ~ .), 
#                            data = data.og, na.action = "na.exclude")

library(simcf)
#?mlogitsimev
MH17.model[[1]]$wts
MH17.model[[1]]$coefnames
pe <- MH17.model[[1]]$wts[c(25:46, 48:69, 71:92)]
vc <- vcov(MH17.model[[1]])
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
simB <- array(NA, dim = c(sims, length(pe)/3, 3))
simB[,,1] <- simbetas[, 1:22]
simB[,,2] <- simbetas[, 23:44]
simB[,,3] <- simbetas[, 45:66]

# Alternative code to test (not working)
#ncoef <- as.numeric(length(pe)/3) # set number of coefs
#simB <- array(NA, dim = c(sims, ncoef, 3))
#simB[,,1] <- simbetas[, 1:ncoef]
#simB[,,2] <- simbetas[, ncoef+1:2*ncoef]
#simB[,,3] <- simbetas[, as.numeric(2*ncoef+1):length(pe)]

# Mean distances
db_dist <- mean(log(data.og$db_dist), na.rm = T)
sv_db_dist <- svymean(~log(db_dist), svdata, na.rm = T)[1]
cr_dist<- mean(log(data.og$cr_dist), na.rm = T)
sv_cr_dist <- svymean(~log(cr_dist), svdata, na.rm = T)[1]
eu_dist<- mean(log(data.og$cr_dist), na.rm = T)
sv_eu_dist <- svymean(~log(cr_dist), svdata, na.rm = T)[1]

# Counterfactual object for PEs
counterfactual <- data.frame(
  Only_Ukr = c(1,0,0,0,1,0,0,0,1,0,0,0),
  Rus_Ukr = c(0,1,0,0,0,1,0,0,0,1,0,0),
  All_Other = c(0,0,0,1,0,0,0,1,0,0,0,1), # Other
  data_Lang_Ukrainian = rep(1, 12), # Ukrainian
  data_Lang_Both = rep(0, 12),
  data_Lang_Other = rep(0, 12),
  data_convlang_1 = rep(0, 12), # setting convlang as Russian
  data_convlang_3 = rep(0, 12), # since ukr and both are set as 0
  Female = rep(1, 12),
  RSPAGE = rep(47, 12),
  RSPEDUC = rep(4, 12),
  #data_religion_2 = rep(0, 12), # Orthodox
  #data_religion_3 = rep(0, 12),
  #data_religion_4 = rep(0, 12),
  #data_religion_5 = rep(0, 12),
  #data_religion_6 = rep(0, 12),
  #data_religion_7 = rep(0, 12),
  data_region_Center_North = rep(0, 12),
  data_region_Kyiv_city = c(0,0,0,0,1,1,1,1,0,0,0,0), 
  data_region_South = c(0,0,0,0,0,0,0,0,1,1,1,1), # south
  data_region_West = rep(0, 12),
  # data_wave_1 = rep(0, 12),
  data_wave_4 = rep(1, 12),
  data_wave_7 = rep(0, 12), #coef is 0 for wave 7, so i made the prediction use wave 4
  data_wave_8 = rep(0, 12),
  db_dist = rep(db_dist, 12),
  cr_dist = rep(cr_dist, 12),
  eu_dist = rep(eu_dist, 12),
 #Religious = rep(1, 12),
 LSNEXTGN = rep(2, 12)
)

# Create object
xhyp_all <- list(x = counterfactual, model = MH17.model[[1]])

# Estimate PEs
#mlogit.ev.MH17 <- mlogitsimev(x= xhyp_all, b = simB, ci = 0.95, constant = 1);mlogit.ev.MH17

# Get formula from model
simformula <- as.formula(MH17.model[[1]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = data.og, na.rm = TRUE)

# Scenario 1: Moving from Russian to Both
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfName(xhyp.1, "Russian vs Both in Kiev", scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr",
                   x = 1, 
                   xpre = 0,
                   scen=1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=4, xpre=4, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_wave_4", x=1, xpre = 1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_wave_7", x=0, xpre = 0, scen=1)
#xhyp.1 <- cfChange(xhyp.1, "data_wave_8", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region__South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre = db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre = cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre = eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=2, xpre = 2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)

Rus_Both_Ethn_MH17 <- mlogit.fd.MH17.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.1

# Scenario 2: Moving from Russian to Both (Language)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x= 0, xpre = 0)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x= 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_", x= 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", 
                   x=1, 
                   xpre=0, scen=1)

Rus_Both_Lang_MH17 <- mlogit.fd.MH17.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.2

# Scenario 3: Moving from Russian to Both (Convlang)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", 
                   x=0, 
                   xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=1, xpre=0, scen=1)

Rus_Both_conv_MH17 <- mlogit.fd.MH17.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.3

# Plotting
pe <- c(mlogit.fd.MH17.1$pe, mlogit.fd.MH17.2$pe, mlogit.fd.MH17.3$pe)
upper <- c(mlogit.fd.MH17.1$upper, mlogit.fd.MH17.2$upper, mlogit.fd.MH17.3$upper)
lower <- c(mlogit.fd.MH17.1$lower, mlogit.fd.MH17.2$lower, mlogit.fd.MH17.3$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("Ethnicity", 4), 
              rep("Language", 4),
              rep("Convenient Language", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

pacman::p_load("wesanderson")
all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("First Differences When Moving from Russian to Both") +
  theme(legend.position=c(0.85,0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Rus_Both_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

## Russian to Ukrainian
# Scenario 4: Moving from Russian to Ukrainian (Ethnicity)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x = 0, xpre = 0)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", 
                   x = 1, 
                   xpre = 0)
Rus_Ukr_Ethn_MH17 <- mlogit.fd.MH17.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.4

# Scenario 5: Moving from Russian to Ukrainian (Language)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x = 1, xpre = 0)
Rus_Ukr_Lang_MH17 <- mlogit.fd.MH17.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.5

# Scenario 6: Moving from Russian to Ukrainian (Convlang)
xhyp.1 <- cfChange(xhyp.1, "data_lang_Ukrainian", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x = 1, xpre = 0)
Rus_Ukr_conv_MH17 <- mlogit.fd.MH17.6 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.6

# Plotting
pe <- c(mlogit.fd.MH17.4$pe, mlogit.fd.MH17.5$pe, mlogit.fd.MH17.6$pe)
upper <- c(mlogit.fd.MH17.4$upper, mlogit.fd.MH17.5$upper, mlogit.fd.MH17.6$upper)
lower <- c(mlogit.fd.MH17.4$lower, mlogit.fd.MH17.5$lower, mlogit.fd.MH17.6$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("Ethnicity", 4), 
              rep("Language", 4),
              rep("Convenient Language", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("First Differences When Moving from Russian to Ukrainian") +
  theme(legend.position=c(0.85,0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Rus_Ukr_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Plotting with both Ethnicity and Language changes
pe <- c(mlogit.fd.MH17.1$pe, mlogit.fd.MH17.2$pe, mlogit.fd.MH17.3$pe,
        mlogit.fd.MH17.4$pe, mlogit.fd.MH17.5$pe, mlogit.fd.MH17.6$pe)
upper <- c(mlogit.fd.MH17.1$upper, mlogit.fd.MH17.2$upper, mlogit.fd.MH17.3$upper,
           mlogit.fd.MH17.4$upper, mlogit.fd.MH17.5$upper, mlogit.fd.MH17.6$upper)
lower <- c(mlogit.fd.MH17.1$lower, mlogit.fd.MH17.2$lower, mlogit.fd.MH17.3$lower,
           mlogit.fd.MH17.4$lower, mlogit.fd.MH17.5$lower, mlogit.fd.MH17.6$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("Ethnicity \n Rus -> Both", 4), 
              rep("Language \n Rus -> Both", 4),
              rep("Convenient Lang \n Rus -> Both", 4),
              rep("Ethnicity \n Rus -> Ukr", 4), 
              rep("Language \n Rus -> Ukr", 4),
              rep("Convenient Lang \n Rus -> Ukr", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.5, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes in Ethnic Identity (xpre -> xpost)") +
  theme(legend.position=c(0.85,0.7), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Ethnicity_Combined_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Scenario 7: Moving from LSNEXTGN = 2 to 1
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x = 1, xpre = 2)
NEXTGN_MH17_1 <- mlogit.fd.MH17.7 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
constant = 1);mlogit.fd.MH17.7



# Scenario 8: Moving from Female to Male
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x = 2, xpre = 2)
xhyp.1 <- cfChange(xhyp.1, "Female", x = 0, xpre = 1)
SEX_MH17 <- mlogit.fd.MH17.8 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.8

# Scenario 9: Moving from Age 47 to 31
xhyp.1 <- cfChange(xhyp.1, "Female", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x = 31, xpre = 47)
AGE_MH17 <- mlogit.fd.MH17.9 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                            constant = 1);mlogit.fd.MH17.9

# Scenario 10: Moving from Educ 4 to 3
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x = 47, xpre = 47)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x = 3, xpre = 4)
EDUC_MH17 <- mlogit.fd.MH17.10 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                            constant = 1);mlogit.fd.MH17.10

# Plotting over time
# Scenario X Moving from data_wave_1 -> 4
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x = 4, xpre = 4)
xhyp.1 <- cfChange(xhyp.1, "data_wave_4", x = 1, xpre = 0)
Wave1_4_MH17 <- mlogit.fd.MH17.x <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                              constant = 1);mlogit.fd.MH17.x

# Scenario Y Moving from data_wave_4 -> 7
xhyp.1 <- cfChange(xhyp.1, "data_wave_4", x = 0, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_wave_7", x = 1, xpre = 0)
Wave4_7_MH17 <- mlogit.fd.MH17.y <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                                constant = 1);mlogit.fd.MH17.y

# Scenario Z Moving from data_wave_1 -> 7
xhyp.1 <- cfChange(xhyp.1, "data_wave_4", x = 0, xpre = 0)
xhyp.1 <- cfChange(xhyp.1, "data_wave_7", x = 1, xpre = 0)
Wave1_7_MH17 <- mlogit.fd.MH17.z <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                                constant = 1);mlogit.fd.MH17.z
# Plotting changes over time
pe <- c(Wave1_4_MH17$pe,
        Wave4_7_MH17$pe,
        Wave1_7_MH17$pe)
upper <- c(Wave1_4_MH17$upper,
        Wave4_7_MH17$upper,
        Wave1_7_MH17$upper)
lower <- c(Wave1_4_MH17$lower,
        Wave4_7_MH17$lower,
        Wave1_7_MH17$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("Wave 1 -> 4", 4),
              rep("Wave 4 -> 7", 4),
              rep("Wave 1 -> 7", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("First Differences for Changes over Time (xpre -> xpost)") +
  # legend position x = left-right, y = up down
  theme(legend.position=c(0.85, 0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Time_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Wave 1 and 4 (model 2)
MH17.model[[2]]$wts
MH17.model[[2]]$coefnames
pe <- MH17.model[[2]]$wts[c(28:52, 54:78, 80:104)]
vc <- vcov(MH17.model[[2]])
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
ncoef <- as.numeric(length(pe)/3) # set number of coefs
simB <- array(NA, dim = c(sims, ncoef, 3))
simB[,,1] <- simbetas[, 1:25]
simB[,,2] <- simbetas[, 26:50]
simB[,,3] <- simbetas[, 51:75]

# Get formula from model
simformula <- as.formula(MH17.model[[2]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = data.og, na.rm = TRUE)

# Scenario 1: Moving from DEMLEVEL 2 to 1
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x = 0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=4, xpre=4, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_wave_4", x=1, xpre=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region__South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre = db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre = cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre = eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=1, 
                   xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=4, xpre =4, scen=1)
hyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Religious", x=1, xpre =1, scen=1)

DEMLEVEL_MH17_2 <- mlogit.fd.MH17.7.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                  constant = 1);mlogit.fd.MH17.7.2

# HEARINDEX 2 -> 1
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=2, xpre=2)
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre = 2)

HEARINDEX_MH17_2 <- mlogit.fd.MH17.7.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.7.3

# INFINTAC from 4 (a few times a month) to 2 (several times a week) (stand dev)
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x= 2, xpre = 4)
INFINTAC_MH17 <- mlogit.fd.MH17.7.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                  constant = 1);mlogit.fd.MH17.7.4

# Scneario 11: Religious from 1 to 0 (need to use this model for a demographics effect plot)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=4, xpre=4)
xhyp.1 <- cfChange(xhyp.1, "Religious", x= 0, xpre = 1)
RLGS_MH17 <- mlogit.fd.MH17.11 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                                  constant = 1);mlogit.fd.MH17.11

# Plotting Wave 1 and 4
pe <- c(mlogit.fd.MH17.7$pe, # LSNEXTGN 
        mlogit.fd.MH17.8$pe, # FEMALE
        mlogit.fd.MH17.7.2$pe, 
        mlogit.fd.MH17.7.3$pe, mlogit.fd.MH17.7.4$pe)
upper <- c(mlogit.fd.MH17.7$upper, # LSNEXTGN 
           mlogit.fd.MH17.8$upper, # FEMALE
           mlogit.fd.MH17.7.2$upper, 
        mlogit.fd.MH17.7.3$upper, mlogit.fd.MH17.7.4$upper)
lower <- c(mlogit.fd.MH17.7$lower, # LSNEXTGN 
           mlogit.fd.MH17.8$lower, # FEMALE
           mlogit.fd.MH17.7.2$lower, 
        mlogit.fd.MH17.7.3$lower, mlogit.fd.MH17.7.4$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("LSNEXTGN 2 -> 1 \n (Waves 1,4 & 7)", 4),
              rep("Female -> Male \n (Waves 1,4 & 7)", 4),
              rep("DEMLEVEL 2 -> 1", 4),
              rep("HEARINDEX 2 -> 1", 4),
              rep("INFINTAC 4 -> 2", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("First Differences for Changes of Interest (xpre -> xpost) (Waves 1 & 4)") +
  # legend position x = left-right, y = up down
  theme(legend.position=c(0.85, 0.7), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Waves1_4_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Plotting Demographics for models 1 and 2
pe <- c(SEX_MH17$pe,
        AGE_MH17$pe,
        EDUC_MH17$pe,
        RLGS_MH17$pe)
upper <-  c(SEX_MH17$upper,
              AGE_MH17$upper,
              EDUC_MH17$upper,
              RLGS_MH17$upper)
lower <- c(SEX_MH17$lower,
              AGE_MH17$lower,
              EDUC_MH17$lower,
              RLGS_MH17$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(
              rep("Female -> Male \n (1, 4 & 7)", 4),
              rep("Age 47 -> 31 \n (1, 4 & 7)", 4),
              rep("Education 4 -> 3 \n (1, 4 & 7)", 4),
              rep("Religious 1 -> 0 \n (1 & 4)", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Demographic Changes (xpre -> xpost)") +
  # legend position x = left-right, y = up down
  theme(legend.position=c(0.85, 0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Demographics_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)


# Do demograpics and anomia for wave 8 (model 4)
MH17.model[[4]]$wts
MH17.model[[4]]$coefnames
pe <- MH17.model[[4]]$wts[c(27:50, 52:75, 77:100)]
vc <- vcov(MH17.model[[4]])
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
ncoef <- as.numeric(length(pe)/3) # set number of coefs
simB <- array(NA, dim = c(sims, ncoef, 3))
simB[,,1] <- simbetas[, 1:24]
simB[,,2] <- simbetas[, 25:48]
simB[,,3] <- simbetas[, 49:72]

# Get formula from model
simformula <- as.formula(MH17.model[[4]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = data.og, na.rm = TRUE)

# Scenario 1: Moving from DEMLEVEL 2 to 1
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfName(xhyp.1, "Russian vs Both in Kiev", scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x = 0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=4, xpre=4, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region__South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre = db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre = cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre = eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=2, xpre = 2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=1, 
                   xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)
hyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Religious", x=1, xpre =1, scen=1)

DEMLEVEL_MH17_4 <- mlogit.fd.MH17.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.1

# Scenario 2: Moving from HEARINDEX 2 to 1
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=2, 
                   xpre =1, scen=1)

HEARINDEX_MH17_4 <- mlogit.fd.MH17.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.2

# Scenario 3: Moving from LSNEXTGN 2 to 1
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=1, 
                   xpre =2, scen=1)

NEXTGN_MH17_4 <- mlogit.fd.MH17.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.3

# Scenario 4: Moving from RSPAGE mean to RSPAGE mean + sd(RSPAGE)
age.sd <- round(sd(data.og$RSPAGE)) # set standard error (18)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre=2)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47-age.sd, xpre=47)

mlogit.fd.MH17.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.4

# Scenario 5: Moving from RSPEDUC 4 to 3
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47-age.sd, xpre=47)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=3, xpre=4, scen=1)

mlogit.fd.MH17.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.5


# Plotting Wave 8
pe <- c(mlogit.fd.MH17.1$pe, mlogit.fd.MH17.2$pe, mlogit.fd.MH17.3$pe,
        mlogit.fd.MH17.4$pe, mlogit.fd.MH17.5$pe)
upper <- c(mlogit.fd.MH17.1$upper, mlogit.fd.MH17.2$upper, mlogit.fd.MH17.3$upper,
           mlogit.fd.MH17.4$upper, mlogit.fd.MH17.5$upper)
lower <- c(mlogit.fd.MH17.1$lower, mlogit.fd.MH17.2$lower, mlogit.fd.MH17.3$lower,
           mlogit.fd.MH17.4$lower, mlogit.fd.MH17.5$lower)
PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("DEMLEVEL \n 2 -> 1", 4), 
              rep("HEARINDEX \n 2 to 1", 4),
              rep("LSNEXTGN \n 2 -> 1", 4),
              rep("RSPAGE \n 47 -> 31", 4),
              rep("RSPEDUC \n 4 -> 3", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Russia"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), name = "First Difference (xpre/Russian - xpost/Both)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes of Interest (xpre -> xpost) (Wave 8)") +
  theme(legend.position=c(0.85,0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Wave_8_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

# Wave 7 Info source (model 5)
MH17.model[[5]]$wts
MH17.model[[5]]$coefnames
pe <- MH17.model[[5]]$wts[c(28:52, 54:78, 80:104)]
vc <- vcov(MH17.model[[5]])
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
ncoef <- as.numeric(length(pe)/3) # set number of coefs
simB <- array(NA, dim = c(sims, ncoef, 3))
simB[,,1] <- simbetas[, 1:25]
simB[,,2] <- simbetas[, 26:50]
simB[,,3] <- simbetas[, 51:75]

# Get formula from model
simformula <- as.formula(MH17.model[[5]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = data.og, na.rm = TRUE)

# Scenario 1: Moving from TV_source to INT_source
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x = 0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=4, xpre=4, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region__South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre = db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre = cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre = eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSCONTNT", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSIMPRVE", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "TV_source", 
                   x=0, 
                   xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INT_source", 
                   x=1, 
                   xpre =0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "SOC_source", x=0, xpre =0, scen=1)

TV_INT_MH17 <- mlogit.fd.MH17.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.1

# Scenario 2: Moving from TV_source to SOC_source
xhyp.1 <- cfChange(xhyp.1, "INT_source", x=0, xpre =0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "TV_source", 
                   x=0, 
                   xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "SOC_source", 
                   x=1, 
                   xpre =0, scen=1)

TV_SOC_MH17 <- mlogit.fd.MH17.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.2

# Scenario 3: Moving from INT_source to SOC_source
xhyp.1 <- cfChange(xhyp.1, "TV_source", x=0, xpre =0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INT_source", x=0, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "SOC_source", x=1, xpre =0, scen=1)

INT_SOC_MH17 <- mlogit.fd.MH17.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.3

# Scenario 4: Moving from LSNEXTGN 2 to 1
xhyp.1 <- cfChange(xhyp.1, "TV_source", x=1, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INT_source", x=0, xpre =0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "SOC_source", x=0, xpre =0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=1, xpre =2, scen=1)

mlogit.fd.MH17.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.4

# Scenario 5: Moving from LSCONTNT 2 to 1
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSCONTNT", x=1, xpre =3, scen=1)

CONTNT_MH17_5 <- mlogit.fd.MH17.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.5

# Scenario 6: Moving from LSIMPRVE 2 to 1
xhyp.1 <- cfChange(xhyp.1, "LSCONTNT", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSIMPRVE", x=1, xpre =2, scen=1)

IMPRV_MH17_5 <- mlogit.fd.MH17.6 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.6

# Plotting
pe <- c(mlogit.fd.MH17.1$pe, mlogit.fd.MH17.2$pe, mlogit.fd.MH17.3$pe,
        mlogit.fd.MH17.4$pe, mlogit.fd.MH17.5$pe, mlogit.fd.MH17.6$pe)
upper <- c(mlogit.fd.MH17.1$upper, mlogit.fd.MH17.2$upper, mlogit.fd.MH17.3$upper,
           mlogit.fd.MH17.4$upper, mlogit.fd.MH17.5$upper, mlogit.fd.MH17.6$upper)
lower <- c(mlogit.fd.MH17.1$lower, mlogit.fd.MH17.2$lower, mlogit.fd.MH17.3$lower,
           mlogit.fd.MH17.4$lower, mlogit.fd.MH17.5$lower, mlogit.fd.MH17.6$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("TV -> INT", 4), 
              rep("TV -> SOC", 4),
              rep("INT -> SOC", 4),
              rep("LSNEXTGN \n 2 -> 1", 4),
              rep("LSCONTNT \n 2 -> 1", 4),
              rep("LSIMPRVE \n 2 -> 1", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes in Info Source and Anomia (xpre -> xpost) (Wave 7)") +
  theme(legend.position=c(0.85,0.75), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Wave7_MH17.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Plotting Anomica for modls 1,2,4,5
pe <- c(NEXTGN_MH17_1$pe,
        NEXTGN_MH17_4$pe,
        HEARINDEX_MH17_2$pe,
        HEARINDEX_MH17_4$pe,
        DEMLEVEL_MH17_2$pe,
        DEMLEVEL_MH17_4$pe,
        IMPRV_MH17_5$pe,
        CONTNT_MH17_5$pe)
upper <- c(NEXTGN_MH17_1$upper,
        NEXTGN_MH17_4$upper,
        HEARINDEX_MH17_2$upper,
        HEARINDEX_MH17_4$upper,
        DEMLEVEL_MH17_2$upper,
        DEMLEVEL_MH17_4$upper,
        IMPRV_MH17_5$upper,
        CONTNT_MH17_5$upper)
lower <- c(NEXTGN_MH17_1$lower,
        NEXTGN_MH17_4$lower,
        HEARINDEX_MH17_2$lower,
        HEARINDEX_MH17_4$lower,
        DEMLEVEL_MH17_2$lower,
        DEMLEVEL_MH17_4$lower,
        IMPRV_MH17_5$lower,
        CONTNT_MH17_5$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("LSNEXTGN \n 2 -> 1 \n (1, 4 & 7)", 4), 
              rep("LSNEXTGN \n 2 -> 1 (8)", 4),
              rep("HEARINDEX \n 2 -> 1 (1 & 4)", 4),
              rep("HEARINDEX \n 2 -> 1 (8)", 4),
              rep("DEMLEVEL \n 2 -> 1 (1 & 4)", 4),
              rep("DEMLEVEL \n 2 -> 1 (8)", 4),
              rep("LSIMPRVE \n 2 -> 1 (7)", 4),
              rep("LSCONTNT \n 3 -> 1 (7)", 4))
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes in Anomia (xpre -> xpost)") +
  theme(legend.position=c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Anomia_MH17.png", device = "png", all_fd_plot, height = 7, width = 9)

rm(PEs)

# Wave 1 Info Source
MH17.model[[3]]$wts
MH17.model[[3]]$coefnames
pe <- MH17.model[[3]]$wts[c(32:60, 62:90, 92:120)]
vc <- vcov(MH17.model[[3]])
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
ncoef <- as.numeric(length(pe)/3) # set number of coefs
simB <- array(NA, dim = c(sims, ncoef, 3))
simB[,,1] <- simbetas[, 1:29]
simB[,,2] <- simbetas[, 30:58]
simB[,,3] <- simbetas[, 59:87]

# Get formula from model
simformula <- as.formula(MH17.model[[3]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = data.og, na.rm = TRUE)

# Set counterfactual
# Scenario 1: Russian Media to Ukrainian Media
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre=cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre=eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x=4, xpre=4, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=2, xpre=2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RusMedia", 
                   x=0, 
                   xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Newspaper", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "FrndFam", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "InfOther", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=2, xpre=2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre=2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Religious", x=1, xpre=1, scen=1)

Rus_Ukr_Media_MH17 <- mlogit.fd.MH17.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.1

# Scenario 2: RusMedia to WestMedia
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=1, xpre=0, scen=1)

Rus_West_Media_MH17 <- mlogit.fd.MH17.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.2

# Scenario 3: UkrMedia to WestMedia
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=1, xpre=0, scen=1)

Ukr_West_Media_MH17 <- mlogit.fd.MH17.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.3

# Scenario 4: RusMedia to Newspaper
xhyp.1 <- cfChange(xhyp.1, "Westmedia", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RusMedia", 
                   x=0,
                   xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Newspaper", 
                   x=1,
                   xpre=0, scen=1)

mlogit.fd.MH17.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.4

# Scenario 5: UkrMedia to Newspaper
xhyp.1 <- cfChange(xhyp.1, "Newspaper", 
                   x=1, 
                   xpre=0, scen=1)

xhyp.1 <- cfChange(xhyp.1, "UkrMedia", 
                   x=0, 
                   xpre=1, scen = 1)

mlogit.fd.MH17.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.MH17.5

# Plotting
pe <- c(mlogit.fd.MH17.1$pe, 
        mlogit.fd.MH17.2$pe, 
        mlogit.fd.MH17.3$pe#,
        #mlogit.fd.MH17.4$pe, 
        #mlogit.fd.MH17.5$pe
        )
upper <- c(mlogit.fd.MH17.1$upper, 
           mlogit.fd.MH17.2$upper,
           mlogit.fd.MH17.3$upper#,
           #mlogit.fd.MH17.4$upper,
           #mlogit.fd.MH17.5$upper
           )
lower <- c(mlogit.fd.MH17.1$lower, 
           mlogit.fd.MH17.2$lower,
           mlogit.fd.MH17.3$lower#,
           #mlogit.fd.MH17.4$lower, 
           #mlogit.fd.MH17.5$lower
           )

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("RusMedia -> UkrMedia", 4), # Scenario 1
              rep("RusMedia -> WestMedia", 4), # Scenario 2
              rep("UkrMedia -> WestMedia", 4)#, # Scenario 3
              #rep("RusMedia -> Newspaper", 4), # Scenario 4
              #rep("UkrMedia -> Newspaper", 4)
              ) # Scenario 5
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

pacman::p_load("wesanderson")
all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes in Info Source (xpre -> xpost) (Wave 1)") +
  theme(legend.position=c(0.85,0.65), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Media_MH17_2.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Plotting Info source for waves 1,4 and 7
pe <- c(Rus_Ukr_Media_MH17$pe,
        Rus_West_Media_MH17$pe,
        Ukr_West_Media_MH17$pe,
        INFINTAC_MH17$pe,
        TV_SOC_MH17$pe,
        TV_INT_MH17$pe,
        INT_SOC_MH17$pe)
upper <- c(Rus_Ukr_Media_MH17$upper,
        Rus_West_Media_MH17$upper,
        Ukr_West_Media_MH17$upper,
        INFINTAC_MH17$upper,
        TV_SOC_MH17$upper,
        TV_INT_MH17$upper,
        INT_SOC_MH17$upper)
lower <- c(Rus_Ukr_Media_MH17$lower,
        Rus_West_Media_MH17$lower,
        Ukr_West_Media_MH17$lower,
        INFINTAC_MH17$lower,
        TV_SOC_MH17$lower,
        TV_INT_MH17$lower,
        INT_SOC_MH17$lower)

PEs <- data.frame(cbind(pe, upper, lower))
PEs <- round(PEs, 3)
Scenario <- c(rep("RusMedia -> \n UkrMedia (1)", 4), # Scenario 1
              rep("RusMedia -> \n WestMedia (1)", 4), # Scenario 2
              rep("UkrMedia -> \n WestMedia (1)", 4), # Scenario 3
              rep("INFINTAC 4 -> 2 \n (1 & 4) (Less -> More)", 4), # Scenario 4
              rep("TV -> \n Social Media (7)", 4),
              rep("TV -> \n Internet (7)", 4),
              rep("Internet -> \n Social Media (7)", 4)
) # Scenario 5
Outcomes <- rep(c("Mechanical", "Rebels", "Military", "Other"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

pacman::p_load("wesanderson")
all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("MH17: First Differences for Changes in Info Source (xpre -> xpost)") +
  theme(legend.position=c(0.85,0.75), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Media_MH17.png", device = "png", all_fd_plot, height = 7, width = 9)

rm(PEs)

##################################################################################

# Info source (independent var)

wave1 <- dplyr::filter(data.og, wave == 1)

histogram(data.frame(wave1$INFSOUFC))
table(data$INFSOUFC)

histogram(data.frame(wave1$INFSOUFC_grouped))
table(wave1$INFSOUFC_grouped)

histogram(data.frame(wave1$INFINTAC)) # Ordinal variable (1 = everyday, 6 = never)
##################################################################################

wave1$INFSOUFC_grouped <- factor(wave1$INFSOUFC_grouped, # levels = c(1,2,3,4),
                         labels = c("Ukrainian Media", 
                                    "Russian Media",
                                    "Western Media",
                                    "Newspaper",
                                    "Family & Friends",
                                    "Other"))

# Boris Nemtsov binary questions
IVsBase_reg <- paste("~", IVsBase)
IVsBase_wm <- paste("~", IVsBase, macro_reg_fixed_effect)

BORISASS.model <- list()

BORISASS.model[[1]] <- multinom(formula = update.formula(IVsBase_wm, BORISASS ~ . 
                                                         + INFINTAC  
                                                         #+ RusMedia # Rus as reference
                                                         + UkrMedia
                                                         + WestMedia
                                                         + Newspaper
                                                         + FrndFam
                                                         + InfOther
                                                         + DEMLEVEL
                                                         + LSNEXTGN
                                                         + HEARINDEX
                                                         - as.factor(RSPRELIG)
                                                         + Religious),
                                data = wave1, 
                                #weights = indwt, # including weights results in unlikely coefs
                                na.action = "na.exclude")

screenreg(BORISASS.model, custom.model.names = c("Neg Image", "Islam", "Threat", "Opposition"))

stargazer(BORISASS.model, 
          star.cutoffs = .05, 
          font.size = "tiny",
          header = F,
          out = "./tables/BORIS.models.tex", 
          column.sep.width = "1pt", 
          no.space = T, 
          keep.stat = c("n", "aic"), 
          notes.align = "l"#,
          #add.lines = (list(c("Obsverations", "", "2237", "",  "", "2815", ""))), # add obs
          #notes = "Demographics, region dummies, and constant ommitted to save space."
)

# clustered SEs for multinomial logistic regression
cl.mlogit   <- function(fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  dfc <- (M/(M-1))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  coeftest(fm, vcovCL) 
}

#cl.mlogit(BORISASS.model[[1]], wave1$oblast)
#class(BORISASS.model[[1]])
#?nnet

# glm.cluster method
vc.cl <- miceadds::glm.cluster(wave1, BORISASS.model[[1]], "oblast", "binomial")$vcov

# Simulating point estimates with confidence intervals
library(simcf)
#?mlogitsimev
BORISASS.model[[1]]$coefnames
BORISASS.model[[1]]$wts
pe <- BORISASS.model[[1]]$wts[c(32:60, 62:90, 92:120, 122:150)]
vc <- vcov(BORISASS.model[[1]]) 
#vc <- vcovCL(BORISASS.model[[1]], "oblast", "binomial")
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)
simB <- array(NA, dim = c(sims, length(pe)/4, 4))
simB[,,1] <- simbetas[, 1:29]
simB[,,2] <- simbetas[, 30:58]
simB[,,3] <- simbetas[, 59:87]
simB[,,4] <- simbetas[, 88:116]

# continue from here
counterfactual <- data.frame(
  Only_Ukr = c(1,0,0,0,1,0,0,0,1,0,0,0),
  Rus_Ukr = c(0,1,0,0,0,1,0,0,0,1,0,0),
  All_Other = c(0,0,0,1,0,0,0,1,0,0,0,1), # Other
  data_Lang_Ukrainian = rep(1, 12), # Ukrainian
  data_Lang_Both = rep(0, 12),
  data_Lang_Other = rep(0, 12),
  data_convlang_1 = rep(0, 12), # setting convlang as Russian
  data_convlang_3 = rep(0, 12), # since ukr and both are set as 0
  Female = rep(1, 12),
  RSPAGE = rep(47, 12),
  RSPEDUC = rep(4, 12),
  #data_religion_2 = rep(0, 12), # Orthodox
  #data_religion_3 = rep(0, 12),
  #data_religion_4 = rep(0, 12),
  #data_religion_5 = rep(0, 12),
  #data_religion_6 = rep(0, 12),
  #data_religion_7 = rep(0, 12),
  data_region_Center_North = rep(0, 12),
  data_region_Kyiv_city = c(0,0,0,0,1,1,1,1,0,0,0,0), 
  data_region_South = c(0,0,0,0,0,0,0,0,1,1,1,1), # south
  data_region_West = rep(0, 12),
  db_dist = rep(db_dist, 12),
  cr_dist = rep(cr_dist, 12),
  eu_dist = rep(eu_dist, 12),
  INFINTAC = rep(4, 12),
  #`as.factor(INFSOUFC_grouped)2` = rep(0, 12),
  #`as.factor(INFSOUFC_grouped)3` = rep(0, 12),
  #`as.factor(INFSOUFC_grouped)4` = rep(0, 12),
  #`as.factor(INFSOUFC_grouped)5` = rep(0, 12),
  #`as.factor(INFSOUFC_grouped)6` = rep(0, 12),
  RusMedia = rep(0, 12),
  WestMedia = rep(0, 12),
  Newspaper = rep(0, 12),
  FrndFam = rep(0, 12),
  InfOther = rep(0, 12),
  DEMLEVEL = rep(2, 12),
  LSNEXTGN = rep(2, 12),
  HEARINDEX = rep(1, 12),
  Religious = rep(1, 12)
)

xhyp_all <- list(x = counterfactual, model = BORISASS.model[[1]])

mlogit.ev <- mlogitsimev(x= xhyp_all, b = simB, ci = 0.95, constant = 1)
mlogit.ev # confidence intervals HUGE - use weights and cluster SEs!!!!

# This method seems to work
pacman::p_load(glm.predict)
glm.predict::multinom.predicts(model = BORISASS.model[[1]], 
                               values = "1;0;0;
                               1;0;0;
                               1;0;
                               mean;mean;mean;mode;mode;mode;0;1;0;0;2;0;0;0;0;0;2;2;2;0", 
                               data = wave1)# sigma = vc.cl)

# get formula from model
simformula <- as.formula(BORISASS.model[[1]])[1:3]

# subset for complete.cases and only variables used in model
selectdata <- extractdata(simformula, data = wave1, na.rm = TRUE)

# Scenario 1: Moving from Russian to Both
xhyp.1 <- cfMake(simformula, selectdata, nscen = 1)
xhyp.1 <- cfName(xhyp.1, "Russian vs Both in Kiev", scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr",
                   x = 1, 
                   xpre = 0,
                   scen=1)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Other", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Female", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x=47, xpre=47, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Center_North", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region__South", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_region_West", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(db_dist)", x=db_dist, xpre = db_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(cr_dist)", x=cr_dist, xpre = cr_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "log(eu_dist)", x=eu_dist, xpre = eu_dist, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Newspaper", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "FrndFam", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "InfOther", x=0, xpre = 0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", x=2, xpre = 2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre =2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre =1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Religious", x=1, xpre =1, scen=1)

Rus_Both_Ethn_BOR <- mlogit.fd.BOR.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.1

# Scenario 2: Moving from Russian to Both (Language)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x= 0, xpre = 0)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", x= 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", 
                   x=1, 
                   xpre=0, scen=1)

Rus_Both_Lang_BOR <- mlogit.fd.BOR.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.2

# Scenario 3: Moving from Russian to Both (Convlang)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Both", 
                   x=0, 
                   xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=1, xpre=0, scen=1)

Rus_Both_conv_BOR <- mlogit.fd.BOR.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.3

## Russian to Ukrainian
# Scenario 4: Moving from Russian to Ukrainian (Ethnicity)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_3", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Rus_Ukr", x = 0, xpre = 0)
xhyp.1 <- cfChange(xhyp.1, "Only_Ukr", 
                   x = 1, 
                   xpre = 0)
Rus_Ukr_Ethn_BOR <- mlogit.fd.BOR.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.4

# Scenario 5: Moving from Russian to Ukrainian (Language)
xhyp.1 <- cfChange(xhyp.1, "data_Lang_Ukrainian", x = 1, xpre = 0)
Rus_Ukr_Lang_BOR <- mlogit.fd.BOR.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.5

# Scenario 6: Moving from Russian to Ukrainian (Convlang)
xhyp.1 <- cfChange(xhyp.1, "data_lang_Ukrainian", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x = 1, xpre = 0)
Rus_Ukr_conv_BOR <- mlogit.fd.BOR.6 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.6

# Plotting with both Ethnicity and Language changes
pe <- c(mlogit.fd.BOR.1$pe, mlogit.fd.BOR.2$pe, mlogit.fd.BOR.3$pe,
        mlogit.fd.BOR.4$pe, mlogit.fd.BOR.5$pe, mlogit.fd.BOR.6$pe)
upper <- c(mlogit.fd.BOR.1$upper, mlogit.fd.BOR.2$upper, mlogit.fd.BOR.3$upper,
           mlogit.fd.BOR.4$upper, mlogit.fd.BOR.5$upper, mlogit.fd.BOR.6$upper)
lower <- c(mlogit.fd.BOR.1$lower, mlogit.fd.BOR.2$lower, mlogit.fd.BOR.3$lower,
           mlogit.fd.BOR.4$lower, mlogit.fd.BOR.5$lower, mlogit.fd.BOR.6$lower)

PEs <- data.frame(cbind(pe, upper, lower))
# remove Outcome 1 (who?)
PEs <- PEs[c(-1, -6, -11, -16, -21, -26), ]
PEs <- round(PEs, 3)
Scenario <- c(rep("Ethnicity \n Rus -> Both", 4), 
              rep("Language \n Rus -> Both", 4),
              rep("Convenient Lang \n Rus -> Both", 4),
              rep("Ethnicity \n Rus -> Ukr", 4), 
              rep("Language \n Rus -> Ukr", 4),
              rep("Convenient Lang \n\n Rus -> Ukr", 4))
Outcomes <- rep(c("Neg Image", "Islam", "Threat", "Opposition"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.5, 0.4, 0.1), name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("Boris Nemtsov: First Differences for Changes in Ethnic Identity (xpre -> xpost)") +
  theme(legend.position=c(0.85,0.7), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Ethnicity_Combined_BORIS.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Changes in Demographics (Religious, Education, Sex, Age)
# Scenario 1: Female -> Male
# Reset Counterfactual
xhyp.1 <- cfChange(xhyp.1, "data_convlang_1", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "Female", x = 0, xpre = 1)
mlogit.fd.BOR.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.1

#Scenario 2: Education 4 -> 3
xhyp.1 <- cfChange(xhyp.1, "Female", x = 1, xpre = 1)
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x = 3, xpre = 4)
mlogit.fd.BOR.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.2

# Scenario 3: RSPAGE 47 to 31
xhyp.1 <- cfChange(xhyp.1, "RSPEDUC", x = 4, xpre = 4)
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x = 47 - age.sd, xpre = 47)
mlogit.fd.BOR.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.3

# Scenario 4: Religious 1 to 0
xhyp.1 <- cfChange(xhyp.1, "RSPAGE", x = 47, xpre = 47)
xhyp.1 <- cfChange(xhyp.1, "Religious", x = 0, xpre = 1)
mlogit.fd.BOR.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.4

# Reset Counterfactual for next scenarios
xhyp.1 <- cfChange(xhyp.1, "Religious", x = 1, xpre = 1)

# Plotting Demographics changes
pe <- c(mlogit.fd.BOR.1$pe, mlogit.fd.BOR.2$pe, mlogit.fd.BOR.3$pe,
        mlogit.fd.BOR.4$pe)
upper <- c(mlogit.fd.BOR.1$upper, mlogit.fd.BOR.2$upper, mlogit.fd.BOR.3$upper,
           mlogit.fd.BOR.4$upper)
lower <- c(mlogit.fd.BOR.1$lower, mlogit.fd.BOR.2$lower, mlogit.fd.BOR.3$lower,
           mlogit.fd.BOR.4$lower)

PEs <- data.frame(cbind(pe, upper, lower))
# remove Outcome 1 (who?)
PEs <- PEs[c(-1, -6, -11, -16), ]
PEs <- round(PEs, 3)
Scenario <- c(rep("Female -> \n Male", 4), 
              rep("Education \n 4 -> 3", 4),
              rep("Age \n 47 -> 31", 4),
              rep("Religious \n 1 -> 0", 4))
Outcomes <- rep(c("Neg Image", "Islam", "Threat", "Opposition"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.5, 0.4, 0.1), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("Boris Nemtsov: First Differences for Demographic Changes (xpre -> xpost)") +
  theme(legend.position=c(0.0875,0.875), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Demographics_BORIS.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Media Changes
# Scenario 1 RusMedia to UkrMedia
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre=1, scen=1)
mlogit.fd.BOR.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.1

# Scenario 2: RusMedia to WestMedia
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=1, xpre=0, scen=1)

mlogit.fd.BOR.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.2

# Scenario 3: UkrMedia to WestMedia
xhyp.1 <- cfChange(xhyp.1, "RusMedia", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "WestMedia", x=1, xpre=0, scen=1)

mlogit.fd.BOR.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.3

# Scenario 4: RusMedia to Newspaper
xhyp.1 <- cfChange(xhyp.1, "Westmedia", x=0, xpre=0, scen=1)
xhyp.1 <- cfChange(xhyp.1, "RusMedia", 
                   x=0,
                   xpre=1, scen=1)
xhyp.1 <- cfChange(xhyp.1, "Newspaper", 
                   x=1,
                   xpre=0, scen=1)

mlogit.fd.BOR.4 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.4

# Scenario 5: UkrMedia to Newspaper
xhyp.1 <- cfChange(xhyp.1, "Newspaper", 
                   x=1, 
                   xpre=0, scen=1)

xhyp.1 <- cfChange(xhyp.1, "UkrMedia", 
                   x=0, 
                   xpre=1, scen = 1)

mlogit.fd.BOR.5 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                                constant = 1);mlogit.fd.BOR.5

# Scenario 6: INFINTAC 4 to 2
xhyp.1 <- cfChange(xhyp.1, "Newspaper", 
                   x=0, 
                   xpre=0, scen=1)

xhyp.1 <- cfChange(xhyp.1, "UkrMedia", 
                   x=1, 
                   xpre=1, scen = 1)

xhyp.1 <- cfChange(xhyp.1, "INFINTAC", 
                   x=2, 
                   xpre=4, scen = 1)

mlogit.fd.BOR.6 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.6

# Reset counterfactual
xhyp.1 <- cfChange(xhyp.1, "INFINTAC", 
                   x=4, 
                   xpre=4, scen = 1)

# Plotting
pe <- c(mlogit.fd.BOR.1$pe, 
        mlogit.fd.BOR.2$pe, 
        mlogit.fd.BOR.3$pe,
        #mlogit.fd.BOR.4$pe, 
        #mlogit.fd.BOR.5$pe,
        mlogit.fd.BOR.6$pe
)
upper <- c(mlogit.fd.BOR.1$upper, 
           mlogit.fd.BOR.2$upper,
           mlogit.fd.BOR.3$upper,
           #mlogit.fd.BOR.4$upper,
           #mlogit.fd.BOR.5$upper
           mlogit.fd.BOR.6$upper
)
lower <- c(mlogit.fd.BOR.1$lower, 
           mlogit.fd.BOR.2$lower,
           mlogit.fd.BOR.3$lower,
           #mlogit.fd.BOR.4$lower, 
           #mlogit.fd.BOR.5$lower
           mlogit.fd.BOR.6$lower
)

PEs <- data.frame(cbind(pe, upper, lower))

# remove Outcome 1 (who?)
PEs <- PEs[c(-1, -6, -11, -16), ]

PEs <- round(PEs, 3)
Scenario <- c(rep("RusMedia -> \n UkrMedia", 4), # Scenario 1
              rep("RusMedia ->\n WestMedia", 4), # Scenario 2
              rep("UkrMedia -> \n WestMedia", 4), # Scenario 3
              #rep("RusMedia -> Newspaper", 4), # Scenario 4
              #rep("UkrMedia -> Newspaper", 4),
              rep("INFINTAC 4 -> 2 \n
                  (Less -> More)", 4)
) # Scenario 4
Outcomes <- rep(c("Neg Image", "Islam", "Threat", "Opposition"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

pacman::p_load("wesanderson")
all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("Boris Nemtsov: First Differences for Changes in Info Source (xpre -> xpost)") +
  theme(legend.position=c(0.85,0.4), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Media_BORIS.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)

# Anomia
# Scenario 1: LSNEXTGN 2 to 1
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=1, xpre=2, scen=1)
mlogit.fd.BOR.1 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.1

# SCenario 2: HEARINDEX 2 to 1
xhyp.1 <- cfChange(xhyp.1, "LSNEXTGN", x=2, xpre=2, scen=1)
xhyp.2 <- cfChange(xhyp.1, "HEARINDEX", x=1, xpre=2, scen=1)
mlogit.fd.BOR.2 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.2

# Scneario 3: DEMLEVEL 2 to 1 
xhyp.2 <- cfChange(xhyp.1, "HEARINDEX", x=2, xpre=2, scen=1)
xhyp.1 <- cfChange(xhyp.1, "DEMLEVEL", x=1, xpre=2, scen=1)
mlogit.fd.BOR.3 <- mlogitsimfd(xhyp.1, b = simB, ci = 0.95, 
                               constant = 1);mlogit.fd.BOR.3

# Plotting Anomia changes
pe <- c(mlogit.fd.BOR.1$pe, mlogit.fd.BOR.2$pe, mlogit.fd.BOR.3$pe)
upper <- c(mlogit.fd.BOR.1$upper, mlogit.fd.BOR.2$upper, mlogit.fd.BOR.3$upper)
lower <- c(mlogit.fd.BOR.1$lower, mlogit.fd.BOR.2$lower, mlogit.fd.BOR.3$lower)

PEs <- data.frame(cbind(pe, upper, lower))

# Remove outcome 1 from plot (who?)
PEs <- PEs[c(-1,-6, -11), ]

PEs <- round(PEs, 3)
Scenario <- c(rep("LSNEXTGN \n 2 -> 1", 4), 
              rep("HEARINDEX \n 2 -> 1", 4),
              rep("DEMLEVEL \n 2 -> 1", 4))
Outcomes <- rep(c("Neg Image", "Islam", "Threat", "Opposition"), nrow(PEs)/4)
PEs <- cbind(Scenario, Outcomes, PEs)

all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
  geom_point(shape = 1, size = 4, color = "black", 
             position = position_dodge(width = 0.4)) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_y_continuous(breaks = seq(-0.5, 0.4, 0.1), 
                     name = "First Difference (xpost - xpre)") +
  coord_flip() +
  theme_minimal()+
  ggtitle("Boris Nemtsov: First Differences for Changes in Anomia (xpre -> xpost)") +
  theme(legend.position=c(0.92,0.7), legend.title = element_blank(), legend.text = element_text(size = 13),
        axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot

ggsave(file = "./figs/Anomia_BORIS.png", device = "png", all_fd_plot, height = 6, width = 9)

rm(PEs)
##################################################################################

#histogram(dplyr::select(data.og, c(CSRUSS, CSHISTDIV, CSDESTBLZ, CSUKRGOV,
 #                                  CSWESTSUP, CSDONBRUS, CSSTPEU, CSYANUCORR)))

CS.model <- list()

CS.model[[1]] <- glm(formula = update.formula(IVsBase_wm, CSYANUCORR ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")

CS.model[[2]] <- glm(formula = update.formula(IVsBase_wm, CSUKRGOV ~ . 
                                              + INFINTAC  
                                             + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[3]] <- glm(formula = update.formula(IVsBase_wm, CSHISTDIV ~ . 
                                              + INFINTAC 
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther
                                              +  DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[4]] <- glm(formula = update.formula(IVsBase_wm, CSWESTSUP ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[5]] <- glm(formula = update.formula(IVsBase_wm, CSDONBRUS ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[6]] <- glm(formula = update.formula(IVsBase_wm, CSRUSS ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
                                              
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[7]] <- glm(formula = update.formula(IVsBase_wm, CSDESTBLZ ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
#weights = indwt,
family = "binomial")
CS.model[[8]] <- glm(formula = update.formula(IVsBase_wm, CSSTPEU ~ . 
                                              + INFINTAC  
                                              + RusMedia
                                              + WestMedia
                                              + Newspaper
                                              + FrndFam
                                              + InfOther
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                              - as.factor(RSPRELIG)
                                              + Religious
),  
data = wave1,
#weights = indwt,
family = "binomial")


screenreg(CS.model, 
          custom.model.names = c("YANUCORR", "UKRGOV", "HISTDIV", "WESTSUP", 
                                 "DONBRUS", "RUSSIA","DESTABLZ", "STOPEU"))

stargazer(CS.model, 
          star.cutoffs = .05, 
          font.size = "tiny",
          header = F,
          out = "./tables/CS.models.tex", 
          column.sep.width = "1pt", 
          no.space = T, 
          keep.stat = c("n", "aic"), 
          notes.align = "l"#,
          #add.lines = (list(c("Obsverations", "", "2237", "",  "", "2815", ""))), # add obs
          #notes = "Demographics, region dummies, and constant ommitted to save space."
)

# Cluster SEs
pacman::p_load(miceadds)

# estimating clustered standard errors
CS.model.b <- list()
for(i in 1:8){
CS.model.b[[i]] <- miceadds::glm.cluster(wave1, 
                                         formula(CS.model[[i]]), 
                                         "oblast", 
                                         family="binomial")
}
# recording SEs
clSEs <- list()
for(i in 1:8) {
  clSEs[[i]] <- coeftest(CS.model.b[[i]])[, 2]
}
# recording pvals
pvals <- list()
for(i in 1:8) {
  pvals[[i]] <- coeftest(CS.model.b[[i]])[, 4]
}
screenreg(CS.model, 
          override.se = clSEs,
          override.pvalues = pvals,
          custom.model.names = c("YANUCORR", "UKRGOV", "HISTDIV", "WESTSUP", 
                                 "DONBRUS", "RUSSIA","DESTABLZ", "STOPEU"))


# sandwich method for robust SEs
pacman::p_load(sandwich)
se_corrected <- list()
pval_corrected <- list()

for(i in 1:8) {
vc <- sandwich::vcovHC(CS.model[[i]])
se_corrected <- sqrt(diag(vc))
ct <- coeftest(CS.model[[8]], vcov = vc)
se <- ct[, 2]
pval_corrected <- ct[, 4]
}

screenreg(CS.model[[8]],
          override.se = se_corrected,
          override.pvalues = pval_corrected)

# Russia territory
pe <- CS.model[[6]]$coefficients # point estiamte
vc <- vcov(CS.model[[6]]) #var-cov matrix

#simulate parameters for predictive distributions
sims <- 1000
simbetas <- MASS::mvrnorm(sims, pe, vc)

# subset for complete cases and only variables used in model
selectdata <- extractdata(CS.model[[6]]$formula, data = wave1, na.rm = T)
 
   # Scenario 1: Moving from Russian to Both Ethnicity
 xhyp.CS.1 <- cfMake(CS.model[[6]]$formula, selectdata, nscen = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=1, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "All_Other", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Other", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(cr_dist)", x=cr_dist, xpre=cr_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(eu_dist)", x=eu_dist, xpre=eu_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x=47, xpre=47, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x=47, xpre=47, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Center_North", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_South", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_West", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "FrndFam", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "InfOther", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x=1, xpre=1, scen=1)
 Rus_Both_Ethn_CS6 <-  logit.CS.1 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.1
  
 # Scenario 2: Moving from Russian to Both Language
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=1, xpre=0, scen=1)
 Rus_Both_Lang_CS6 <-  logit.CS.2 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.2
 
 # SCenario 3: Moving from Russian to Both Convlang
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=1, xpre=0, scen=1)
 Rus_Both_conv_CS6 <-  logit.CS.3 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.3
 
 # Scenario 4: Moving from Russian to Ukrainian (Ethnicity)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=0, scen=1)
 Rus_Ukr_Ethn_CS6 <-  logit.CS.4 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.4
 
 # Scenario 5: Moving from Russian to Ukrainian (Language)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=0, scen=1)
 Rus_Ukr_Lang_CS6 <-  logit.CS.5 <- logitsimfd(xhyp.CS.1, simbetas, 
                                               ci=0.95, constant=1);logit.CS.5
 
 # Scenario 6: Moving from Russian to Ukrainian (convlang)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=1, xpre=0, scen=1)
 Rus_Ukr_conv_CS6 <-  logit.CS.6 <- logitsimfd(xhyp.CS.1, simbetas, 
                                               ci=0.95, constant=1);logit.CS.6
 
 # reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 
 # Media Changes
 # Scenario 1 RusMedia to UkrMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 
 Rus_Ukr_Media_CS6 <- logit.CS.7 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.7
 
 # Scenario 2: RusMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Rus_West_Media_CS6 <- logit.CS.8 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.8
 
 # Scenario 3: UkrMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Ukr_West_Media_CS6 <- logit.CS.9 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.9
 
 # Scenario 4: RusMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Westmedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", 
                    x=0,
                    xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                    x=1,
                    xpre=0, scen=1)
 
Rus_News_Media_CS6 <- logit.CS.10 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.10
 
 # Scenario 5: UkrMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                    x=1, 
                    xpre=0, scen=1)
 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia", 
                    x=0, 
                    xpre=1, scen = 1)
 
 Ukr_News_Media_CS6 <- logit.CS.11 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.11
 
 # Scenario 6: INFINTAC 4 to 2
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper",  x=0,  xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia",  x=1,  xpre=1, scen = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC",  x=2,  xpre=4, scen = 1)
 INFINTAC_CS6 <- logit.CS.12 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.12
 
 # Reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=4, xpre=4, scen = 1)
 
 # Anomia Changes
 # Scenario 1: LSNEXTGN 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=1, xpre=2, scen=1)
 NEXTGN_CS6 <- logit.CS.13 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.13
 
 # SCenario 2: HEARINDEX 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=2, scen=1)
 HEARINDEX_CS6 <- logit.CS.14 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.14
 
 # Scneario 3: DEMLEVEL 2 to 1 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=1, xpre=2, scen=1)
 DEMLEVEL_CS6 <- logit.CS.15 <- logitsimfd(xhyp.CS.1, b = simbetas, 
                                           ci = 0.95,  constant = 1);logit.CS.15
 
 # Changes in Demographics (Religious, Education, Sex, Age)
 # Scenario 1: Female -> Male
 # Reset Counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x = 2, xpre = 2)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 0, xpre = 1)
 SEX_CS6 <- logit.CS.1 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.1
 
 #Scenario 2: Education 4 -> 3
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 1, xpre = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 3, xpre = 4)
 EDUC_CS6 <-logit.CS.2 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.2
 
 # Scenario 3: RSPAGE 47 to 31
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 4, xpre = 4)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47 - age.sd, xpre = 47)
 AGE_CS6 <- logit.CS.3 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.3
 
 # Scenario 4: Religious 1 to 0
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47, xpre = 47)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 0, xpre = 1)
 RLGS_CS6 <- logit.CS.4 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                      constant = 1);logit.CS.4
 
 # Reset Counterfactual for next scenarios
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 1, xpre = 1)
  
 # Destabilize Ukraine
 pe <- CS.model[[7]]$coefficients # point estiamte
 vc <- vcov(CS.model[[7]]) #var-cov matrix
 
 #simulate parameters for predictive distributions
 sims <- 1000
 simbetas <- MASS::mvrnorm(sims, pe, vc)
 
 # subset for complete cases and only variables used in model
 selectdata <- extractdata(CS.model[[7]]$formula, data = wave1, na.rm = T)
 
   # Scenario 1: Moving from Russian to Both
 xhyp.CS.1 <- cfMake(CS.model[[7]]$formula, selectdata, nscen = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=1, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "All_Other", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Other", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(cr_dist)", x=cr_dist, xpre=cr_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(eu_dist)", x=eu_dist, xpre=eu_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x=47, xpre=47, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Center_North", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_South", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_West", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "FrndFam", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "InfOther", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x=1, xpre=1, scen=1)
 Rus_Both_Ethn_CS7 <-  logit.CS.1 <- logitsimfd(xhyp.CS.1, simbetas, 
                                                ci=0.95, constant=1);logit.CS.1

  # Scenario 2: Moving from Russian to Both Language
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=1, xpre=0, scen=1)
 Rus_Both_Lang_CS7 <-  logit.CS.2 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.2
 
 # SCenario 3: Moving from Russian to Both Convlang
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=1, xpre=0, scen=1)
 Rus_Both_conv_CS7 <-  logit.CS.3 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.3
 
 # Scenario 4: Moving from Russian to Ukrainian (Ethnicity)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=0, scen=1)
 Rus_Ukr_Ethn_CS7 <-  logit.CS.4 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.4
 
 # Scenario 5: Moving from Russian to Ukrainian (Language)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=0, scen=1)
 Rus_Ukr_Lang_CS7 <-  logit.CS.5 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.5
 
 # Scenario 7: Moving from Russian to Ukrainian (convlang)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=1, xpre=0, scen=1)
 Rus_Ukr_conv_CS7 <-  logit.CS.6 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, 
                                               constant=1);logit.CS.6
 
 # reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 
 # Media Changes
 # Scenario 1 RusMedia to UkrMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 Rus_Ukr_Media_CS7 <- logit.CS.7 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                 constant = 1);logit.CS.7
 
 # Scenario 2: RusMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Rus_West_Media_CS7 <- logit.CS.8 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                  constant = 1);logit.CS.8
 
 # Scenario 3: UkrMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Ukr_West_Media_CS7 <- logit.CS.9 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                  constant = 1);logit.CS.9
 
 # Scenario 4: RusMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Westmedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", 
                       x=0,
                       xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                       x=1,
                       xpre=0, scen=1)
 
 Rus_News_Media_CS7 <- logit.CS.10 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                   constant = 1);logit.CS.10
 
 # Scenario 5: UkrMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                       x=1, 
                       xpre=0, scen=1)
 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia", 
                       x=0, 
                       xpre=1, scen = 1)
 
 Ukr_News_Media_CS7 <- logit.CS.11 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                   constant = 1);logit.CS.11
 
 # Scenario 6: INFINTAC 4 to 2
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper",  x=0,  xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia",  x=1,  xpre=1, scen = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC",  x=2,  xpre=4, scen = 1)
 INFINTAC_CS7 <- logit.CS.12 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                             constant = 1);logit.CS.12
 
 # Reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=4, xpre=4, scen = 1)
 
 # Anomia Changes
 # Scenario 1: LSNEXTGN 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=1, xpre=2, scen=1)
 NEXTGN_CS7 <- logit.CS.13 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                           constant = 1);logit.CS.13
 
 # SCenario 2: HEARINDEX 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=2, scen=1)
 HEARINDEX_CS7 <- logit.CS.14 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                              constant = 1);logit.CS.14
 
 # Scneario 3: DEMLEVEL 2 to 1 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=1, xpre=2, scen=1)
 DEMLEVEL_CS7 <- logit.CS.15 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                             constant = 1);logit.CS.15
 
 # Changes in Demographics (Religious, Education, Sex, Age)
 # Scenario 1: Female -> Male
 # Reset Counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x = 2, xpre = 2)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 0, xpre = 1)
 SEX_CS7 <- logit.CS.1 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.1
 
 #Scenario 2: Education 4 -> 3
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 1, xpre = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 3, xpre = 4)
 EDUC_CS7 <-logit.CS.2 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.2
 
 # Scenario 3: RSPAGE 47 to 31
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 4, xpre = 4)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47 - age.sd, xpre = 47)
 AGE_CS7 <- logit.CS.3 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                     constant = 1);logit.CS.3
 
 # Scenario 4: Religious 1 to 0
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47, xpre = 47)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 0, xpre = 1)
 RLGS_CS7 <- logit.CS.4 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                      constant = 1);logit.CS.4
 
 # Reset Counterfactual for next scenarios
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 1, xpre = 1)
 
 # Stop EU accession #
 pe <- CS.model[[8]]$coefficients # point estiamte
 vc <- vcov(CS.model[[8]]) #var-cov matrix
 
 #simulate parameters for predictive distributions
 sims <- 1000
 simbetas <- MASS::mvrnorm(sims, pe, vc)
 
 # subset for complete cases and only variables used in model
 selectdata <- extractdata(CS.model[[8]]$formula, data = wave1, na.rm = T)
 
# Scenario 1: Moving from Russian to Both
xhyp.CS.1 <- cfMake(CS.model[[8]]$formula, selectdata, nscen = 1)
xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=0, xpre=0, scen=1)
xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=1, xpre=0, scen=1)
xhyp.CS.1 <- cfChange(xhyp.CS.1, "All_Other", x=0, xpre=0, scen=1)
xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Other", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(db_dist)", x=db_dist, xpre=db_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(cr_dist)", x=cr_dist, xpre=cr_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "log(eu_dist)", x=eu_dist, xpre=eu_dist, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x=47, xpre=47, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Center_North", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_Kyiv_city", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_South", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_region_West", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "FrndFam", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "InfOther", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x=1, xpre=1, scen=1)
 Rus_Both_Ethn_CS8 <-  logit.CS.1 <- logitsimfd(xhyp.CS.1, simbetas, 
                                                ci=0.95, constant=1);logit.CS.1
 
 # Scenario 2: Moving from Russian to Both Language
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Rus_Ukr", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=1, xpre=0, scen=1)
 Rus_Both_Lang_CS8 <-  logit.CS.2 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.2
 
 # SCenario 3: Moving from Russian to Both Convlang
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Both", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=1, xpre=0, scen=1)
 Rus_Both_conv_CS8 <-  logit.CS.3 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.3
 
 # Scenario 4: Moving from Russian to Ukrainian (Ethnicity)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_3", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=0, scen=1)
 Rus_Ukr_Ethn_CS8 <-  logit.CS.4 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.4
 
 # Scenario 5: Moving from Russian to Ukrainian (Language)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Only_Ukr", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=0, scen=1)
 Rus_Ukr_Lang_CS8 <-  logit.CS.5 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.5
 
 # Scenario 8: Moving from Russian to Ukrainian (convlang)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_Lang_Ukrainian", x=1, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=1, xpre=0, scen=1)
 Rus_Ukr_conv_CS8 <-  logit.CS.6 <- logitsimfd(xhyp.CS.1, simbetas, ci=0.95, constant=1);logit.CS.6
 
 # reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "data_convlang_1", x=0, xpre=0, scen=1)
 
 # Media Changes
 # Scenario 1 RusMedia to UkrMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 Rus_Ukr_Media_CS8 <- logit.CS.7 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                 constant = 1);logit.CS.7
 
 # Scenario 2: RusMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Rus_West_Media_CS8 <- logit.CS.8 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                  constant = 1);logit.CS.8
 
 # Scenario 3: UkrMedia to WestMedia
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "WestMedia", x=1, xpre=0, scen=1)
 
 Ukr_West_Media_CS8 <- logit.CS.9 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                  constant = 1);logit.CS.9
 
 # Scenario 4: RusMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Westmedia", x=0, xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RusMedia", 
                       x=0,
                       xpre=1, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                       x=1,
                       xpre=0, scen=1)
 
 Rus_News_Media_CS8 <- logit.CS.10 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                   constant = 1);logit.CS.10
 
 # Scenario 5: UkrMedia to Newspaper
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper", 
                       x=1, 
                       xpre=0, scen=1)
 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia", 
                       x=0, 
                       xpre=1, scen = 1)
 
 Ukr_News_Media_CS8 <- logit.CS.11 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                                   constant = 1);logit.CS.11
 
 # Scenario 6: INFINTAC 4 to 2
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Newspaper",  x=0,  xpre=0, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "UkrMedia",  x=1,  xpre=1, scen = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC",  x=2,  xpre=4, scen = 1)
 INFINTAC_CS8 <- logit.CS.12 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                             constant = 1);logit.CS.12
 
 # Reset counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "INFINTAC", x=4, xpre=4, scen = 1)
 
 # Anomia Changes
 # Scenario 1: LSNEXTGN 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=1, xpre=2, scen=1)
 NEXTGN_CS8 <- logit.CS.13 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                           constant = 1);logit.CS.13
 
 # SCenario 2: HEARINDEX 2 to 1
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "LSNEXTGN", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=1, xpre=2, scen=1)
 HEARINDEX_CS8 <- logit.CS.14 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                              constant = 1);logit.CS.14
 
 # Scneario 3: DEMLEVEL 2 to 1 
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "HEARINDEX", x=2, xpre=2, scen=1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x=1, xpre=2, scen=1)
 DEMLEVEL_CS8 <- logit.CS.15 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                             constant = 1);logit.CS.15
 
 # Changes in Demographics (Religious, Education, Sex, Age)
 # Scenario 1: Female -> Male
 # Reset Counterfactual
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "DEMLEVEL", x = 2, xpre = 2)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 0, xpre = 1)
 SEX_CS8 <- logit.CS.1 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                constant = 1);logit.CS.1
 
 #Scenario 2: Education 4 -> 3
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Female", x = 1, xpre = 1)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 3, xpre = 4)
 EDUC_CS8 <-logit.CS.2 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                constant = 1);logit.CS.2
 
 # Scenario 3: RSPAGE 47 to 31
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPEDUC", x = 4, xpre = 4)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47 - age.sd, xpre = 47)
 AGE_CS8 <- logit.CS.3 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                               constant = 1);logit.CS.3
 
 # Scenario 4: Religious 1 to 0
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "RSPAGE", x = 47, xpre = 47)
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 0, xpre = 1)
 RLGS_CS8 <- logit.CS.4 <- logitsimfd(xhyp.CS.1, b = simbetas, ci = 0.95, 
                                constant = 1);logit.CS.4
 
 # Reset Counterfactual for next scenarios
 xhyp.CS.1 <- cfChange(xhyp.CS.1, "Religious", x = 1, xpre = 1)
 
#################################################################################
 
 # Plotting ethnic identity variables for all outcomes
 
 pe <- c(Rus_Ukr_Ethn_CS6$pe, Rus_Ukr_Ethn_CS7$pe, Rus_Ukr_Ethn_CS8$pe,
         Rus_Ukr_Lang_CS6$pe, Rus_Ukr_Lang_CS7$pe, Rus_Ukr_Lang_CS8$pe,
         Rus_Ukr_conv_CS6$pe, Rus_Ukr_conv_CS7$pe, Rus_Ukr_conv_CS8$pe,
         Rus_Both_Ethn_CS6$pe, Rus_Both_Ethn_CS7$pe, Rus_Both_Ethn_CS8$pe,
         Rus_Both_Lang_CS6$pe, Rus_Both_Lang_CS7$pe, Rus_Both_Lang_CS8$pe,
         Rus_Both_conv_CS6$pe, Rus_Both_conv_CS7$pe, Rus_Both_conv_CS8$pe)
 upper <- c(Rus_Ukr_Ethn_CS6$upper, Rus_Ukr_Ethn_CS7$upper, Rus_Ukr_Ethn_CS8$upper,
            Rus_Ukr_Lang_CS6$upper, Rus_Ukr_Lang_CS7$upper, Rus_Ukr_Lang_CS8$upper,
            Rus_Ukr_conv_CS6$upper, Rus_Ukr_conv_CS7$upper, Rus_Ukr_conv_CS8$upper,
            Rus_Both_Ethn_CS6$upper, Rus_Both_Ethn_CS7$upper, Rus_Both_Ethn_CS8$upper,
            Rus_Both_Lang_CS6$upper, Rus_Both_Lang_CS7$upper, Rus_Both_Lang_CS8$upper,
            Rus_Both_conv_CS6$upper, Rus_Both_conv_CS7$upper, Rus_Both_conv_CS8$upper)
 lower <- c(Rus_Ukr_Ethn_CS6$lower, Rus_Ukr_Ethn_CS7$lower, Rus_Ukr_Ethn_CS8$lower,
            Rus_Ukr_Lang_CS6$lower, Rus_Ukr_Lang_CS7$lower, Rus_Ukr_Lang_CS8$lower,
            Rus_Ukr_conv_CS6$lower, Rus_Ukr_conv_CS7$lower, Rus_Ukr_conv_CS8$lower,
            Rus_Both_Ethn_CS6$lower, Rus_Both_Ethn_CS7$lower, Rus_Both_Ethn_CS8$lower,
            Rus_Both_Lang_CS6$lower, Rus_Both_Lang_CS7$lower, Rus_Both_Lang_CS8$lower,
            Rus_Both_conv_CS6$lower, Rus_Both_conv_CS7$lower, Rus_Both_conv_CS8$lower)
 
 PEs <- data.frame(cbind(pe, upper, lower))
 PEs <- round(PEs, 3)
 Scenario <- c(rep("Ethnicity \n Rus -> Ukr", 3), 
               rep("Language \n Rus -> Ukr", 3),
               rep("Convenient Lang \n Rus -> Ukr", 3),
               rep("Ethnicity \n Rus -> Both", 3), 
               rep("Language \n Rus -> Both", 3),
               rep("Convenient Lang \n Rus -> Both", 3)
               )
 Outcomes <- rep(c("Russian Desire Territory", "Destabilize Ukraine", 
                   "Stop EU Accession"), nrow(PEs)/3)
 PEs <- cbind(Scenario,
              Outcomes,
              PEs)
 
 pacman::p_load("wesanderson")
 all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
   geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
   geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                 position = position_dodge(width = 0.4)) +
   geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
   geom_point(shape = 1, size = 4, color = "black", 
              position = position_dodge(width = 0.4)) +
   scale_color_manual(values = wes_palette("Royal1")) +
   scale_y_continuous(breaks = seq(-0.2, 0.4, 0.05), 
                      name = "First Difference (xpost - xpre)") +
   coord_flip() +
   theme_minimal()+
   ggtitle("Conflict Start: First Differences for Changes in Ethnic Identity (xpre -> xpost)") +
   theme(legend.position=c(0.2,0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
         axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
   theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot
 
 ggsave(file = "./figs/Ethnicity_Combined_CS.png", device = "png", all_fd_plot, height = 6, width = 9)
 
 rm(PEs)
 
 # Plotting Media Changes
 pe <- c(Rus_Ukr_Media_CS6$pe, 
         Rus_Ukr_Media_CS7$pe, 
         Rus_Ukr_Media_CS8$pe, 
         Rus_West_Media_CS6$pe,
         Rus_West_Media_CS7$pe,
         Rus_West_Media_CS8$pe,
         Ukr_West_Media_CS6$pe,
         Ukr_West_Media_CS7$pe,
         Ukr_West_Media_CS8$pe,
         INFINTAC_CS6$pe,
         INFINTAC_CS7$pe,
         INFINTAC_CS8$pe
 )
 
 upper <- c(Rus_Ukr_Media_CS6$upper, 
         Rus_Ukr_Media_CS7$upper, 
         Rus_Ukr_Media_CS8$upper, 
         Rus_West_Media_CS6$upper,
         Rus_West_Media_CS7$upper,
         Rus_West_Media_CS8$upper,
         Ukr_West_Media_CS6$upper,
         Ukr_West_Media_CS7$upper,
         Ukr_West_Media_CS8$upper,
         INFINTAC_CS6$upper,
         INFINTAC_CS7$upper,
         INFINTAC_CS8$upper
 )
 lower <- c(Rus_Ukr_Media_CS6$lower, 
         Rus_Ukr_Media_CS7$lower, 
         Rus_Ukr_Media_CS8$lower, 
         Rus_West_Media_CS6$lower,
         Rus_West_Media_CS7$lower,
         Rus_West_Media_CS8$lower,
         Ukr_West_Media_CS6$lower,
         Ukr_West_Media_CS7$lower,
         Ukr_West_Media_CS8$lower,
         INFINTAC_CS6$lower,
         INFINTAC_CS7$lower,
         INFINTAC_CS8$lower
 )
 
 PEs <- data.frame(cbind(pe, upper, lower))
 PEs <- round(PEs, 3)
 Scenario <- c(rep("RusMedia -> \n UkrMedia", 3), # Scenario 1
               rep("RusMedia ->\n WestMedia", 3), # Scenario 2
               rep("UkrMedia -> \n WestMedia", 3), # Scenario 3
               #rep("RusMedia -> Newspaper", 3), # Scenario 4
               #rep("UkrMedia -> Newspaper", 3),
               rep("INFINTAC 4 -> 2 \n
                   (Less -> More)", 3)
               ) # Scenario 4
 Outcomes <- rep(c("Russian Desire Territory", "Destabilize Ukraine", 
                   "Stop EU Accession"), nrow(PEs)/3)
 PEs <- cbind(Scenario, Outcomes, PEs)
 
 pacman::p_load("wesanderson")
 all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
   geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
   geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                 position = position_dodge(width = 0.4)) +
   geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
   geom_point(shape = 1, size = 4, color = "black", 
              position = position_dodge(width = 0.4)) +
   scale_color_manual(values = wes_palette("Royal1")) +
   scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1), 
                      name = "First Difference (xpost - xpre)") +
   coord_flip() +
   theme_minimal()+
   ggtitle("Conflict Start: First Differences for Changes in Info Source (xpre -> xpost)") +
   theme(legend.position=c(0.8,0.85), legend.title = element_blank(), legend.text = element_text(size = 13),
         axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
   theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot
 
   ggsave(file = "./figs/Media_CS.png", device = "png", all_fd_plot, height = 6, width = 9)
 
 rm(PEs)

 
 # Plotting Anomia changes
 pe <- c(NEXTGN_CS6$pe, 
         NEXTGN_CS7$pe, 
         NEXTGN_CS8$pe,
         HEARINDEX_CS6$pe, 
         HEARINDEX_CS7$pe, 
         HEARINDEX_CS8$pe,
         DEMLEVEL_CS6$pe, 
         DEMLEVEL_CS7$pe, 
         DEMLEVEL_CS8$pe)
 upper <- c(NEXTGN_CS6$upper, 
            NEXTGN_CS7$upper, 
            NEXTGN_CS8$upper,
            HEARINDEX_CS6$upper, 
            HEARINDEX_CS7$upper, 
            HEARINDEX_CS8$upper,
            DEMLEVEL_CS6$upper, 
            DEMLEVEL_CS7$upper, 
            DEMLEVEL_CS8$upper)
 lower <- c(NEXTGN_CS6$lower, 
            NEXTGN_CS7$lower, 
            NEXTGN_CS8$lower,
            HEARINDEX_CS6$lower, 
            HEARINDEX_CS7$lower, 
            HEARINDEX_CS8$lower,
            DEMLEVEL_CS6$lower, 
            DEMLEVEL_CS7$lower, 
            DEMLEVEL_CS8$lower)
 
 PEs <- data.frame(cbind(pe, upper, lower))
 PEs <- round(PEs, 3)
 Scenario <- c(rep("LSNEXTGN \n 2 -> 1", 3), 
               rep("HEARINDEX \n 2 -> 1", 3),
               rep("DEMLEVEL \n 2 -> 1", 3))
 Outcomes <- rep(c("Russian Desire Territory", "Destabilize Ukraine", 
                   "Stop EU Accession"), nrow(PEs)/3)
 PEs <- cbind(Scenario, Outcomes, PEs)
 
 all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
   geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
   geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                 position = position_dodge(width = 0.4)) +
   geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
   geom_point(shape = 1, size = 4, color = "black", 
              position = position_dodge(width = 0.4)) +
   scale_color_manual(values = wes_palette("Royal1")) +
   scale_y_continuous(breaks = seq(-0.5, 0.4, 0.1), 
                      name = "First Difference (xpost - xpre)") +
   coord_flip() +
   theme_minimal()+
   ggtitle("Conflict Start: First Differences for Changes in Anomia (xpre -> xpost)") +
   theme(legend.position=c(0.85,0.9), legend.title = element_blank(), legend.text = element_text(size = 13),
         axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
   theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot
 
 ggsave(file = "./figs/Anomia_CS.png", device = "png", all_fd_plot, height = 6, width = 9)
 
 rm(PEs)
 
 # Plotting Demographic Changes
 pe <- c(SEX_CS6$pe,
         SEX_CS7$pe,
         SEX_CS8$pe,
         EDUC_CS6$pe,
         EDUC_CS7$pe,
         EDUC_CS8$pe,
         AGE_CS6$pe,
         AGE_CS7$pe,
         AGE_CS8$pe,
         RLGS_CS6$pe,
         RLGS_CS7$pe,
         RLGS_CS8$pe
 )
 upper <- c(SEX_CS6$upper,
         SEX_CS7$upper,
         SEX_CS8$upper,
         EDUC_CS6$upper,
         EDUC_CS7$upper,
         EDUC_CS8$upper,
         AGE_CS6$upper,
         AGE_CS7$upper,
         AGE_CS8$upper,
         RLGS_CS6$upper,
         RLGS_CS7$upper,
         RLGS_CS8$upper
 )
 lower <- c(SEX_CS6$lower,
         SEX_CS7$lower,
         SEX_CS8$lower,
         EDUC_CS6$lower,
         EDUC_CS7$lower,
         EDUC_CS8$lower,
         AGE_CS6$lower,
         AGE_CS7$lower,
         AGE_CS8$lower,
         RLGS_CS6$lower,
         RLGS_CS7$lower,
         RLGS_CS8$lower
 )
 PEs <- data.frame(cbind(pe, upper, lower))
 PEs <- round(PEs, 3)
 Scenario <- c(rep("Female -> \n Male", 3), 
               rep("Education \n 4 -> 3", 3),
               rep("Age \n 47 -> 31", 3),
               rep("Religious \n 1 -> 0", 3))
 Outcomes <- rep(c("Russian Desire Territory", 
                   "Destabilize Ukraine", 
                   "Stop EU Accession"), nrow(PEs)/3)
 PEs <- cbind(Scenario, Outcomes, PEs)
 
 all_fd_plot <- ggplot(PEs, aes(x = Scenario, y = pe, group = Outcomes)) +
   geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "grey") +
   geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.3, size = 0.7,
                 position = position_dodge(width = 0.4)) +
   geom_point(aes(color = Outcomes), size = 4, position = position_dodge(width = 0.4)) +
   geom_point(shape = 1, size = 4, color = "black", 
              position = position_dodge(width = 0.4)) +
   scale_color_manual(values = wes_palette("Royal1")) +
   scale_y_continuous(breaks = seq(-0.5, 0.4, 0.1), 
                      name = "First Difference (xpost - xpre)") +
   coord_flip() +
   theme_minimal()+
   ggtitle("Conflict Start: First Differences for Demographic Changes (xpre -> xpost)") +
   theme(legend.position=c(0.85,0.71), legend.title = element_blank(), legend.text = element_text(size = 13),
         axis.text = element_text(size=13), axis.title = element_text(size = 13)) +
   theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid'));all_fd_plot
 
 ggsave(file = "./figs/Demographics_CS.png", device = "png", all_fd_plot, height = 6, width = 9)
 
 rm(PEs)
 
 