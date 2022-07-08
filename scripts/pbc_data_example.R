################
#### Header ####
################

# Title: Pammtools Time Dependant Covariates Tutorial
# Author: Andreas Bender, Fabian Scheipl, Philipp Kopper:
# modified by Joshua Wilson
# Date: 08-07-2022
# References:
# https://adibender.github.io/pammtools/articles/tdcovar.html
# https://dataset.lixoft.com/data-set-examples/pbc-data-set/#:~:text=Primary%20Biliary%20Cirrhosis%20is%20a,be%20mediated%20by%20immunologic%20mechanisms.
###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("tidyr", "readr", "dplyr", "pammtools", "mgcv")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# load packages, install if not available provided user approves
lapply(packages, require, character.only = TRUE)

##########################
#### Data Description ####
##########################

# import and check data
data("pbc", package = "survival")

# pbc is end points only
pbc <- pbc %>%
    # remove unused columns
    select(id, time, status, bili, protime) %>%
    # log transform bili & protime
    mutate(bili = log(bili), protime = log(protime)) %>%
    # pbcseq only has data for id up to 312, so drop the rest in pbc
    filter(id <= 312) %>%
    # below makes status 0 for alive or 1 for dead.
    mutate(status = 1L * (status == 2))

head(pbc)
# id       = case number
# time     = days between registration & earlier of death, transplant, study end
# status   = 0=alive, 1=dead
# bili     = serum bilirubin in mg/dl
# protime  = prothrombin time in seconds

# pbcseq is all points except end point
pbcseq <- pbcseq %>%
    select(id, day, status, bili, protime) %>%
    mutate(bili = log(bili), protime = log(protime))
head(pbcseq)
# id       = case number
# day      = days since regestration
# status   = 0=alive, 1=dead
# bili     = serum bilirubin in mg/dl
# protime  = prothrombin time in seconds

# below we combine the pbc and pbcseq data to produce pbc_ped, which contains
# all shared time points between the id's. This is a bit complicated, so here
# I calculate it for just two id's and investigate the output.
pbc_test <- pbc %>%
    filter(id == 1 | id == 2)

pbcseq_test <- pbcseq %>%
    filter(id == 1 | id == 2)

pbc_ped_test <- as_ped(
    data = list(pbc_test, pbcseq_test),
    formula =
        Surv(time, status) ~ . +
        concurrent(bili, protime, tz_var = "day"),
    id = "id")

# id 1 contains the time points:
# 0, 192, 400
unique(sort(filter(pbc_test, id == 1)$time))
unique(sort(filter(pbcseq_test, id == 1)$day))

# id 2 contains the time points:
# 0, 182, 365, 768, 1790, 2151, 2515, 2882, 3226, 4500
unique(sort(filter(pbc_test, id == 2)$time))
unique(sort(filter(pbcseq_test, id == 2)$day))

# the possible shared time points are 0, 182, 192, 365, 400

# so for id = 1 pbc_ped contains the time intervals
# 0-182, 182-192, 192-365, 365-400
unique(sort(filter(pbc_ped_test, id == 1)$tstart))
unique(sort(filter(pbc_ped_test, id == 1)$tend))

# so for id = 2 pbc_ped contains the same time intervals
# 0-182, 182-192, 192-365, 365-400
unique(sort(filter(pbc_ped_test, id == 2)$interval))

# tend, which is used to fit the model is the end point of these intervals
unique(sort(filter(pbc_ped_test, id == 2)$tend))
unique(sort(filter(pbc_ped_test, id == 1)$tend))

# now let's determine pbd_ped for the entire dataset
pbc_ped <- as_ped(
    data = list(pbc, pbcseq),
    formula =
        Surv(time, status) ~ . +
        concurrent(bili, protime, tz_var = "day"),
    id = "id")

# let's check how close the time points are, note there are some time points
# right next to eachother, 170, 171, 172, 173, 174, 175
# and some further apart, 41, 51, 71
unique(sort(pbc_ped$tend))

##################
#### Training ####
##################

pbc_pam <- gam(
    ped_status ~
    s(tend) +
    bili +
    protime,
    data = pbc_ped,
    family = poisson(),
    offset = offset)

cbind(pam = coef(pbc_pam)[2:3])

###################
#### Visualise ####
###################

# calculate average values as reference
reference <- sample_info(pbc_ped)

# predict effect of bili
bili_df <- pbc_ped %>%
    ungroup() %>%
    make_newdata(bili = seq_range(bili, n = 100)) %>%
    add_term(pbc_pam, term = "bili", reference = reference)

# predict effect of protime
protime_df <- pbc_ped %>%
    ungroup() %>%
    make_newdata(protime = seq_range(protime, n = 100)) %>%
    add_term(pbc_pam, term = "protime", reference = reference)

# plot results, remember that bili and protime are log transformed
p_term <- ggplot(data = NULL, aes(y = fit)) +
    geom_line(aes(col = "PAM")) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)
gridExtra::grid.arrange(
  p_term %+% bili_df + aes(x = exp(bili)),
  p_term %+% protime_df + aes(x = exp(protime)),
  nrow = 1L)
