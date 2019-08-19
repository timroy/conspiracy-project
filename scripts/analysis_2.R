#rm(list = ls())

#data.og <- read_rds("./data_clean/conspiracy_data_clean.rds")

# creating survey object
svdata <- svydesign(ids = ~precinct,
                    strata = ~wave + oblast, 
                    weights = ~indwt,
                    nest = TRUE,
                    data = data.og)

# Multinomial Logit with Replicate Weights
repsvdata <- as.svrepdesign(svdata, type = "bootstrap", replicates = 75) # creating replicate weights

# Loading svrepmisc package
#devtools::install_github("carlganz/svrepmisc")
library(svrepmisc)

# creating function to tidy output of svymultinom
mat2vec <- function(X) {
  if (is.vector(X)) {
    return(X)
  }
  levs <- colnames(X)
  rows <- rownames(X)
  X <- as.vector(X)
  if (is.null(levs) | is.null(rows)) {
    return(X)
  }
  rows <- as.vector(outer(rows,levs,paste,sep="."))
  names(X) <- rows
  X
  
}


#' @export
print.svrepstatmisc <- function(x, df.residual=NULL, ...) {
  ### COPY AND PASTED FROM LUMLEY
  if (is.list(x)){
    x<-x[[1]]
  }
  vv<-sqrt(diag(as.matrix(attr(x,"var"))))
  
  if (is.null(df.residual)) {
    df.residual <- attr(x, "df.residual")
  }
  
  tvals <- x/vv
  attributes(tvals) <- NULL
  ### probably wrong in some cases
  ### may need pnorm in some cases
  if (df.residual > 0) {
    pvals <- 2 * stats::pt(-abs(tvals), df.residual)
  } else {
    pvals <- NA
    warning("Not enough replicates to compute p-values.")
  }
  m <- cbind(x,vv,tvals,pvals)
  colnames(m)<-c(attr(x,"statistic"),"SE", "t value", "Pr(>|t|)")
  stats::printCoefmat(m)
  
}


#' @export
#' @importFrom stats qt vcov
# adapted from confint.lm
confint.svrepstatmisc <- function (object, parm, level = 0.95, df.residual=NULL, ...)
{
  cf <- object
  pnames <- names(cf)
  if (is.null(df.residual)) {
    df.residual <- attr(object, "df.residual")
  }
  if (df.residual <= 0)
    stop("Not enough duplicates to compute confidence intervals.")
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, df.residual)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


#' A tidier for svrepstatmisc
#'
#' @param x a svrepstatmisc object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates and confidence intervals
#' @param quick	whether to compute a smaller and faster version, containing only the term and estimate columns.
#' @param df.residual degrees of freedom for t-distribution in confidence interval (will be extracted from \code{x} if not specified)
#' @param ... extra arguments
#' @importFrom stats coef
#' @importFrom broom tidy
#' @export
tidy.svrepstatmisc <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, df.residual = NULL, ...) {
  if (is.list(x)){
    x<-x[[1]]
  }
  
  co <- as.data.frame(x)[[1]]
  
  if (quick) {
    ret <- data.frame(term = names(x), estimate = unname(co), stringsAsFactors = FALSE)
    return(process_svrepstatmisc(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  }
  
  
  if (is.null(df.residual)) {
    df.residual <- attr(x, "df.residual")
  }
  
  vv <- sqrt(diag(as.matrix(attr(x, "var"))))
  tvals <- x / vv
  attributes(tvals) <- NULL
  if (df.residual > 0) {
    pvals <- 2 * stats::pt(-abs(tvals), df.residual)
  } else {
    pvals <- NA
    warning("Not enough replicates to compute p-values and confidence intervals.")
  }
  
  ret <- data.frame(
    term = names(x),
    estimate = unname(co),
    std.error = vv,
    statistic = tvals,
    p.value = pvals,
    stringsAsFactors = FALSE
  )
  process_svrepstatmisc(ret, x, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate, df.residual = df.residual)
}


process_svrepstatmisc <- function(ret, x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, df.residual = NULL) {
  if (exponentiate) {
    trans <- exp
  } else {
    trans <- identity
  }
  
  if (is.null(df.residual)) {
    df.residual <- attr(x, "df.residual")
  }
  
  if (conf.int & df.residual > 0) {
    CI <- suppressMessages(trans(stats::confint(x, level = conf.level)))
    colnames(CI) = c("conf.low", "conf.high")
    CI <- as.data.frame(CI)
    CI$term <- rownames(CI)
    rownames(CI) <- NULL
    ret <- merge(ret, CI, by = "term", all.x = TRUE)
  }
  
  ret$estimate <- trans(ret$estimate)
  rownames(ret) <- NULL
  ret
}

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

data.og %>%  group_by(wave) %>%  summarize_at(vars(MH17), mean, na.rm = T)

# MH17 Asked in waves 1,4,7,8 - Using 1 as baseline
# wave 8 has diff options - regression without wave 8?
wave_fixed_effects_MH17 <- "+ data_wave_4 + data_wave_7" #+ data_wave_8"
#wave_fixed_effects_MH17 <- "+ as.factor(wave)"

# East as baseline
macro_reg_fixed_effect <- " + data_region_Center_North + 
                              data_region_Kyiv_city + 
                              data_region_South + 
                              data_region_West"

# Wave FE
IVsBase_wd <- paste("~", IVsBase, wave_fixed_effects_MH17)

# Region FE
IVsBase_wdm <- paste(IVsBase_wd, macro_reg_fixed_effect)

# Create list for models
MH17.sv.model <- list()

# Set formula
m.MH17 <- formula(update.formula(IVsBase_wdm, 
                       MH17 ~ .
                       + LSNEXTGN # removes 2000 obs
                       
))

# Formula 2 (with Religious, exclude wave 7)
m.MH17.2 <- formula(update.formula(IVsBase_wdm, 
                                 MH17 ~ .
                                 + LSNEXTGN # removes 2000 obs
                                 + Religious # removes wave 7 obs
                                 
))

# Run Regression 1
MH17.sv.model[[1]] <- svymultinom(formula = m.MH17,
                                  design = repsvdata, 
                                  Hess = T
)

# Run Regression 2
MH17.sv.model[[2]] <- svymultinom(formula = m.MH17.2,
                                  design = repsvdata, 
                                  Hess = T,
                                  subset = Wave != 7 # take out Wave 7 so that we can use Religious
)

# Tidying above output
MH17.tidy <- tidy.svrepstatmisc(MH17.sv.model, conf.int = TRUE, conf.level = 0.95)
nrow(MH17.tidy) # number of coefficients (divide by 3)

# Predicting through simulations using simcf
library(simcf)
pe <- as.vector(MH17.tidy$estimate) # point estimate
vc <- vcov(MH17.sv.model[[1]]) #var-cov matrix

# clustered  var-cov matrix
#miceadds::glm.cluster(data.og, m.MH17, "oblast", "binomial")

# simulate parameters for predictive distributions
sims <- 1000
simbetas <- mvrnorm(sims, pe, vc) 
nrow(simbetas)
ncol(simbetas)

#each array is half the columns of the total number of parameters
simB <- array(NA, dim = c(sims, 22, 3)) # re-arrange simulates to array format
simB[,,1] <- simbetas[, seq(1, 64, 3)] # for MNL simulation
simB[,,2] <- simbetas[, seq(2, 65, 3)]
simB[,,3] <- simbetas[, seq(3, 66, 3)]

# Taking mean distances
db_dist <- svymean(~log(db_dist), svdata, na.rm = T)[1]
cr_dist <- svymean(~log(cr_dist), svdata, na.rm =T)[1]
eu_dist <- svymean(~log(eu_dist), svdata, na.rm =T)[1]

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
  data_region_Center_North = rep(0, 12),
  data_region_Kyiv_city = c(0,0,0,0,1,1,1,1,0,0,0,0), 
  data_region_South = c(0,0,0,0,0,0,0,0,1,1,1,1), # south
  data_region_West = rep(0, 12),
  data_wave_4 = rep(1, 12),
  data_wave_7 = rep(0, 12),
  #data_wave_8 = rep(0, 12),
  db_dist = rep(db_dist, 12),
  cr_dist = rep(cr_dist, 12),
  eu_dist = rep(eu_dist, 12),
  LSNXTGEN = rep(2, 12)
)

xhyp_MH17_svy <- list(x = counterfactual, model = m.MH17)
mlogit.ev <- mlogitsimev(x= xhyp_MH17_svy, b = simB, ci = 0.95, constant = 1);mlogit.ev
