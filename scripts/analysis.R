rm(list = ls())

options(scipen = 999)

pacman::p_load(survey, tidyverse, haven, nnet, mlogit, texreg, clusterSEs, rcompanion)

if(!file.exists("./figs")) dir.create("./figs")
if(!file.exists("./tables")) dir.create("./tables")

data.og <- read_rds("./data_clean/conspiracy_data_clean.rds")

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

# taking out wave1
data <- dplyr::filter(data.og, wave!= 1)

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
Response <- rep(seq(1, 4, 1), 7)
Wave <- c(rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4),
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
propaganda <- svymean(~ INFGENPR, na.rm = T, svdata);propaganda
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
  theme(plot.title = element_text(size = 17)) +
  #theme(legend.position=c(0.85,0.8), #legend.title = element_blank(), 
  #legend.text = element_text(size = 13)) +
  theme(legend.background = element_rect(colour = 'grey90', size = 0.7, linetype='solid')) +
  guides(color = guide_legend(reverse = TRUE)) +
  coord_flip();plot_2 

ggsave("./figs/descriptives_INFPRORI.png", plot_2)
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

Proportions <- rbind(groupwiseMean(formula =  BOROPP ~ Ethnicity, data = data.og, 0.95, 3)[3],
               groupwiseMean(formula =  BORTHRT ~ Ethnicity, data = data.og, 0.95, 3)[3],
               groupwiseMean(formula =  BORISLAM ~ Ethnicity, data = data.og, 0.95, 3)[3],
               groupwiseMean(formula =  BORNEG ~ Ethnicity, data = data.og, 0.95, 3)[3])
Ethnicity <- rep(c("Russian", "Ukrainian", "Both", "Other"), 4)
Statement <- c(rep("BOROPP", 4), rep("BORTHRT", 4), rep("BORISLAM", 4), rep("BORNEG", 4))

# get confidence intervals

borismeans <- data.frame(cbind(Ethnicity, Statement, Proportions))
borismeans$Ethnicity <- factor(borismeans$Ethnicity)
borismeans$Statement <- factor(borismeans$Statement)

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
CSmean$Proportion <- sapply(CSVARS, mean, na.rm = T)
CSmean$Statement <- rownames(CSmean)

# relevel to make plot ordered from high to low
CSmean$Statement <- factor(CSmean$Statement, levels = CSmean[order(CSmean$Proportion), "Statement"])

CSVARS_plot <- ggplot(CSmean, aes(x = Statement, y = Proportion)) +
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

# get CI

CSfinalmeans <- cbind(Ethnicity, Statement, CSmeans)

# CSBLAME descriptives - wave 1-3
#attributes(data.original$CSBLAME)
#unique(data.original$CSBLAME)

#histogram(data.frame(dplyr::select(data.og, CSBLAME)))

##########################################################################################

# Some regressions (simcf needs dummies to work so we create two sets of IVs)
IVsBase_simcf <- "Only_Ukr + Rus_Ukr + All_Other + 
            data_Lang_Ukrainian + data_Lang_Both + data_Lang_Other + 
            data_convlang_1 + data_convlang_3 + 
            log(db_dist) + log(cr_dist) + log(eu_dist) +
            Female + RSPAGE + RSPEDUC +  
            data_religion_2 + data_religion_3 + data_religion_4 + 
            data_religion_5 + data_religion_6 + data_religion_7"

IVsBase <- "Only_Ukr + Rus_Ukr + All_Other + 
            data_Lang_Ukrainian + data_Lang_Both + data_Lang_Other + 
            data_convlang_1 + data_convlang_3 + 
            log(db_dist) + log(cr_dist) + log(eu_dist) +
            Female + RSPAGE + RSPEDUC + as.factor(RSPRELIG)"

#data$RSPRELIG <- factor(data$RSPRELIG)

data.og %>%  group_by(wave) %>%  summarize_at(vars(MH17), mean, na.rm = T)

# MH17 Asked in waves 1,4,7,8 - Using 1 as baseline
wave_fixed_effects_MH17 <- "+ data_wave_4 + data_wave_7 + data_wave_8"

# East as baseline
macro_reg_fixed_effect <- " + data_region_Center_North + 
                              data_region_Kyiv_city + 
                              data_region_South + 
                              data_region_West"

IVsBase_wd <- paste("~", IVsBase_simcf, wave_fixed_effects_MH17)
IVsBase_wdm <- paste(IVsBase_wd, macro_reg_fixed_effect)

MH17.model <- list()

MH17.model[[1]] <- multinom(formula =  update.formula(IVsBase_wdm,
                                              MH17 ~ .), 
                    data = data.og, na.action = "na.exclude")
screenreg(MH17.model[[1]])

#mlogit(MH17 ~ Female, data.og)

#mlogit(formula =  update.formula(IVsBase_wdm, MH17 ~ .), 
#                            data = data.og, na.action = "na.exclude")


library(simcf)
#?mlogitsimev
MH17.model[[1]]$wts
pe <- MH17.model[[1]]$wts[c(31:58, 60:87, 89:116)]
#pe1 <- coef(MH17.model[[1]])
vc <- vcov(MH17.model[[1]])

sims <- 1000
simbetas <- mvrnorm(sims, pe, vc)
simB <- array(NA, dim = c(sims, length(pe)/3, 3))
simB[,,1] <- simbetas[, 1:28]
simB[,,2] <- simbetas[, 29:56]
simB[,,3] <- simbetas[, 57:84]

screenreg(MH17.model[[1]])

# Mean distances
db_dist <- mean(log(data.og$db_dist), na.rm = T)
sv_db_dist <- svymean(~log(db_dist), svdata, na.rm = T)[1]
cr_dist<- mean(log(data.og$cr_dist), na.rm = T)
sv_cr_dist <- svymean(~log(cr_dist), svdata, na.rm = T)[1]
eu_dist<- mean(log(data.og$cr_dist), na.rm = T)
sv_eu_dist <- svymean(~log(cr_dist), svdata, na.rm = T)[1]

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
  data_religion_2 = rep(0, 12), # Orthodox
  data_religion_3 = rep(0, 12),
  data_religion_4 = rep(0, 12),
  data_religion_5 = rep(0, 12),
  data_religion_6 = rep(0, 12),
  data_religion_7 = rep(0, 12),
  data_region_Center_North = rep(0, 12),
  data_region_Kyiv_city = c(0,0,0,0,1,1,1,1,0,0,0,0), 
  data_region_South = c(0,0,0,0,0,0,0,0,1,1,1,1), # south
  data_region_West = rep(0, 12),
  # data_wave_1 = rep(0, 12),
  data_wave_4 = rep(0, 12),
  data_wave_7 = rep(1, 12),
  data_wave_8 = rep(0, 12),
  db_dist = rep(db_dist, 12),
  cr_dist = rep(cr_dist, 12),
  eu_dist = rep(eu_dist, 12)
)

xhyp_all <- list(x = counterfactual, model = MH17.model[[1]])

mlogit.ev <- mlogitsimev(x= xhyp_all, b = simB, ci = 0.95)#, constant = 1);mlogit.ev
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

#BORIS.model <- list()
#
#BORIS.model[[1]] <- glm(formula = update.formula(IVsBase_wm, BOROPP ~ . 
#                                                 + INFINTAC  
#                                                 + INFSOUFC_grouped
#                                                 + DEMLEVEL
#                                                 + LSNEXTGN
#                                                 + HEARINDEX
#                                                 - as.factor(RSPRELIG)
#                                                 + Religious
#), 
#data = wave1,
#family = "binomial")
#BORIS.model[[2]] <- glm(formula = update.formula(IVsBase_wm, BORTHRT ~ . 
#                                                 + INFINTAC  
#                                                 + INFSOUFC_grouped
#                                                 + DEMLEVEL
#                                                 + LSNEXTGN
#                                                 + HEARINDEX
#                                                 - as.factor(RSPRELIG)
#                                                 + Religious
#), 
#data = wave1,
#family = "binomial")
#BORIS.model[[3]] <- glm(formula = update.formula(IVsBase_wm, BORISLAM ~ . 
#                                                 + INFINTAC 
#                                                 + INFSOUFC_grouped
#                                                 + DEMLEVEL
#                                                 + LSNEXTGN
#                                                 + HEARINDEX
#                                                 - as.factor(RSPRELIG)
#                                                 + Religious
#), 
#data = wave1,
#family = "binomial")
#BORIS.model[[4]] <- glm(formula = update.formula(IVsBase_wm, BORNEG ~ . 
#                                                 + INFINTAC  
#                                                 + INFSOUFC_grouped 
#                                                 + DEMLEVEL
#                                                 + LSNEXTGN
#                                                 + HEARINDEX
#                                                 - as.factor(RSPRELIG)
#                                                 + Religious
#), 
#data = wave1,
#family = "binomial")

#screenreg(BORIS.model, custom.model.names = c("OPPOSITION", "THREAT", "ISLAM", "NEGATIVEIMAGEPUTIN"))

BORISASS.model <- list()

BORISASS.model[[1]] <- multinom(formula = update.formula(IVsBase_wm, BORISASS ~ . 
                                          + INFINTAC  
                                          + INFSOUFC_grouped 
                                          + DEMLEVEL
                                          + LSNEXTGN
                                          + HEARINDEX
                                          - as.factor(RSPRELIG)
                                          + Religious),
                                          data = wave1, 
                                          na.action = "na.exclude")

BORISASS.model

screenreg(BORISASS.model, custom.model.names = c("Neg Image", "Islam", "Threat", "Opposition"))

##################################################################################

#histogram(dplyr::select(data.og, c(CSRUSS, CSHISTDIV, CSDESTBLZ, CSUKRGOV,
 #                                  CSWESTSUP, CSDONBRUS, CSSTPEU, CSYANUCORR)))

CS.model <- list()

CS.model[[1]] <- glm(formula = update.formula(IVsBase_wm, CSRUSS ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                           #   - as.factor(RSPRELIG)
                                              + Religious
                                              
), 
data = wave1,
family = "binomial")
CS.model[[2]] <- glm(formula = update.formula(IVsBase_wm, CSUKRGOV ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")
CS.model[[3]] <- glm(formula = update.formula(IVsBase_wm, CSHISTDIV ~ . 
                                              + INFINTAC 
                                              +  INFSOUFC_grouped
                                              +  DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")
CS.model[[4]] <- glm(formula = update.formula(IVsBase_wm, CSWESTSUP ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")
CS.model[[5]] <- glm(formula = update.formula(IVsBase_wm, CSDONBRUS ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")
CS.model[[6]] <- glm(formula = update.formula(IVsBase_wm, CSDESTBLZ ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")
CS.model[[7]] <- glm(formula = update.formula(IVsBase_wm, CSSTPEU ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
),  
data = wave1,
family = "binomial")

CS.model[[8]] <- glm(formula = update.formula(IVsBase_wm, CSYANUCORR ~ . 
                                              + INFINTAC  
                                              + INFSOUFC_grouped 
                                              + DEMLEVEL
                                              + LSNEXTGN
                                              + HEARINDEX
                                            #  - as.factor(RSPRELIG)
                                              + Religious
), 
data = wave1,
family = "binomial")

screenreg(CS.model, custom.model.names = c("RUSSIA", "UKRGOV", "HISTDIV", "WESTSUP", 
                                           "DONBRUS", "DESTABLZ", "STOPEU", "YANUCORR"))


#################################################################################