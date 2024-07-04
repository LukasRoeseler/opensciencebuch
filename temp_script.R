# Note: This script uses renv (R dependency management)

# In order to reproduce our analysis with the exact same R environment that we
# used for our analysis, please proceed as follows:

# 1.) Download "02_analysis_for_TOTEtime_renv_files.zip" from OSF (https://osf.io/758zy) and unzip it 
# 2.) Put this script next to the "renv.lock" in the unzipped folder
# 3.) Open this script using R version 4.1.2 (64-bit) on Windows
#     [Note, we used R with RStudio, which automatically sets the working directory to the script
#      location and runs the contents of .Rprofile to activate the renv environment.
#      If you are using R differently, it might be necessary to perform those steps manually.]
# 4.) When using R on a different operating system, you can run "renv::restore()" to restore the packages from the renv.lock file



library(ggpubr)
library(sjstats)
library(pwr) # required by sjstats
library(tidyr)
library(dplyr)
library(Hmisc)
library(jtools)


# Read in data #
# data can be found on the OSF repository (https://osf.io/kcd8w/)

dat <- read.csv("https://osf.io/download/p5zyw", fileEncoding="UTF-8")


##############
### -- Outliers and Exclusions (see also preregistration)
### -- 1.) First, we include only the data of those participants into the analysis that both finish the experiment and provide consent for analyzing their data.
### -- 2.) Second, in the time pressure condition the participants have to provide their estimates within seven seconds after the appearance of the respective text input field. Participants failing to provide their estimates within this time period for at least one of the three experimental trials will be excluded from the analysis.
### -- 3.) Third, participants spending more than 60 seconds with any of the three experimental trials will be excluded from the analysis because this indicates that they might be distracted.
### -- 4.) Fourth, participants for whom the webpage loses focus (window.blur event) on any of the three experimental trials will be excluded from the analysis, as this indicates that they might be using external help (e.g. search on the internet).
### -- 5.) Fifth, participants that provide no numerical estimates for any of the three experimental trials will be excluded from the analysis. 
### -- 6.) Sixth, for the experimental trials we classify all responses outside the following ranges as outliers; Redwood: 5 to 500 m, United Nations: 10 to 300; Cat speed: 5 to 80 km/h. Thereafter, all participants that provided at least one response that was classified as outlier will be excluded from the analysis.
##############

N <- NULL
N[1] <- nrow(dat) + 63 #add 63 for participants that were excluded prior to uploading the data to OSF (see exclusion criertion 1 below)

### -- 1.) First, we include only the data of those participants into the analysis that both finish the experiment and provide consent for analyzing their data.

# This was already performed prior to uploading the data to OSF
# -> Thus, no additional exclusions due to those commands
dat <- dat[dat$FINISHED == 1,]
dat <- dat[dat$ET01 == 1,]
dat <- dat[dat$ET02 == 1,]

N[2] <- nrow(dat)


### -- 2.) Second, in the time pressure condition the participants have to provide their estimates within seven seconds after the appearance of the respective text input field. Participants failing to provide their estimates within this time period for at least one of the three experimental trials will be excluded from the analysis.

# NOTE: Variables are only present in the data set, if at least one participant responded too slow for the respective question
"QU03_03" %in% names(dat)
"QU04_03" %in% names(dat)
"QU05_03" %in% names(dat)
# --> thus, please remove those variables that are not present in the data set from the following command by hand (otherwise you will get an empty data.frame after executing the command)

dat <- dat[! (dat$QU03_03 == "too_slow" | dat$QU04_03 == "too_slow" | dat$QU05_03 == "too_slow"), ]

N[3] <- nrow(dat)


### -- 3.) Third, participants spending more than 60 seconds with any of the three experimental trials will be excluded from the analysis because this indicates that they might be distracted.

# Note: If you check the data before running this code, you will notice that for some reason
#       no response was recorded for a very low proportion of trials (e.g. sort by QU03_04 and you will notice the NAs)
#       Those cases will anyway be removed based on exclusion criterion 5, so it is no problem that some rows with NAs remain
#       after this step

dat <- dat[dat$QU03_04 <= 60000 & dat$QU04_04 <= 60000 & dat$QU05_04 <= 60000, ]

N[4] <- nrow(dat)


### -- 4.) Fourth, participants for whom the webpage loses focus (window.blur event) on any of the three experimental trials will be excluded from the analysis, as this indicates that they might be using external help (e.g. search on the internet).

# NOTE: Variables are only present in the data set, if at least one participant left the webpage for the respective question
"QU03_05" %in% names(dat)
"QU04_05" %in% names(dat)
"QU05_05" %in% names(dat)
# --> thus, please remove those variables that are not present in the data set from the following command by hand (otherwise you will get an empty data.frame after executing the command)

dat <- dat[! (dat$QU03_05 == "page_lost_focus" | dat$QU04_05 == "page_lost_focus" | dat$QU05_05 == "page_lost_focus"), ]

N[5] <- nrow(dat)


### -- 5.) Fifth, participants that provide no numerical estimates for any of the three experimental trials will be excluded from the analysis. 

dat <- dat[! (is.na(dat$QU03_02) | is.na(dat$QU04_02) | is.na(dat$QU05_02)), ]

N[6] <- nrow(dat)


### -- 6.) Sixth, for the experimental trials we classify all responses outside the following ranges as outliers; Redwood: 5 to 500 m, United Nations: 10 to 300; Cat speed: 5 to 80 km/h. Thereafter, all participants that provided at least one response that was classified as outlier will be excluded from the analysis.

dat$QU03_02[dat$QU03_02 < 5 | dat$QU03_02 > 500] <- NA
dat$QU04_02[dat$QU04_02 < 10 | dat$QU04_02 > 300] <- NA
dat$QU05_02[dat$QU05_02 < 5 | dat$QU05_02 > 80] <- NA

dat <- dat[! (is.na(dat$QU03_02) | is.na(dat$QU04_02) | is.na(dat$QU05_02)), ]

N[7] <- nrow(dat)


### Number of exclusions per criterion

N[1:length(N)-1] - N[2:length(N)]


##############
### -- Get a first impression of the data by visualizing the estimates entered by the participants
### -- (note: plot the data grouped according to our 2 (anchor: high vs. low) x 2 (time pressure: with vs. without) design.)
### -- 1.) Create a variable dat$anchor with the values "high" and "low" [Hint: see dat$EG01]
### -- 2.) Create a variable dat$time_pressure with the values "with" and "without" [Hint: see dat$EG01]
### -- 3.) Check how many participants there are left within each cell of our 2x2 design (following exclusions)
### -- 4.) visualization of data
##############

### -- 1.) Create a variable dat$anchor with the values "high" and "low" [Hint: see dat$EG01]

dat$anchor <- NA
dat$anchor[dat$EG01 == 1 | dat$EG01 == 2] <- "low"
dat$anchor[dat$EG01 == 3 | dat$EG01 == 4] <- "high"
sum(is.na(dat$anchor))

### -- 2.) Create a variable dat$time_pressure with the values "with" and "without" [Hint: see dat$EG01]

dat$time_pressure <- NA
dat$time_pressure[dat$EG01 == 1 | dat$EG01 == 3] <- "with"
dat$time_pressure[dat$EG01 == 2 | dat$EG01 == 4] <- "without"
sum(is.na(dat$time_pressure))

### -- 3.) Check how many participants there are left within each cell of our 2x2 design (following exclusions)

xtabs(~anchor+time_pressure,dat)

### -- 4.) visualization of data

# violin plots

ggviolin(data=dat, x="time_pressure", y="QU03_02", fill="anchor", add="mean_se", trim=TRUE)
ggviolin(data=dat, x="time_pressure", y="QU04_02", fill="anchor", add="mean_se", trim=TRUE)
ggviolin(data=dat, x="time_pressure", y="QU05_02", fill="anchor", add="mean_se", trim=TRUE)


##############
### -- 1.) Calculate the main dependent variable as dat$z (mean z-score, see preregistration)
### -- 2.) Visualize individual z-standardized items, i.e., dat$z_j (data grouped according to anchor-factor)
### -- 3.) Visualize dat$z (data grouped according to our design)
##############

### -- 1.) Calculate the main dependent variable as dat$z (mean z-score, see preregistration)

# From preregistration:
# Because our estimation tasks use different scales, we transform participants responses to each estimation task to z-scores prior to the analysis. Our main dependent measure for each participant is the participant’s mean z-score across the 3 estimation tasks that are used as experimental trials.

dat$QU03_02_z <- scale(dat$QU03_02)
dat$QU04_02_z <- scale(dat$QU04_02)
dat$QU05_02_z <- scale(dat$QU05_02)

dat$z <- (dat$QU03_02_z + dat$QU04_02_z + dat$QU05_02_z) / 3

aggregate(z ~ anchor, dat, mean)
aggregate(z ~ anchor, dat, sd)

#also scale anchors:
dat$QU03_01_z <- scale(dat$QU03_01, center = attr(dat$QU03_02_z, 'scaled:center'), scale = attr(dat$QU03_02_z, 'scaled:scale'))
dat$QU04_01_z <- scale(dat$QU04_01, center = attr(dat$QU04_02_z, 'scaled:center'), scale = attr(dat$QU04_02_z, 'scaled:scale'))
dat$QU05_01_z <- scale(dat$QU05_01, center = attr(dat$QU05_02_z, 'scaled:center'), scale = attr(dat$QU05_02_z, 'scaled:scale'))


### -- Visualize individual z-standardized items, i.e., dat$z_j (data grouped according to anchor-factor)

# violin plots

ggviolin(data=dat, x="time_pressure", y="QU03_02_z", fill="anchor", add="mean_se", trim=TRUE)
ggviolin(data=dat, x="time_pressure", y="QU04_02_z", fill="anchor", add="mean_se", trim=TRUE)
ggviolin(data=dat, x="time_pressure", y="QU05_02_z", fill="anchor", add="mean_se", trim=TRUE)


# bar plots

dat3 <- dat |> 
  pivot_longer(
    cols = c(QU03_02_z, QU04_02_z, QU05_02_z #estimates
             , QU03_01_z, QU04_01_z, QU05_01_z #anchors
    )
    , names_to = c('item', '.value')
    , names_pattern = '(.*?)_(.*)'
  ) |>
  rename(
    'z_02' = '02_z'
    , 'z_01' = '01_z'
  ) |>
  mutate(
    item = factor(substr(item, 4, 4))
  ) 
dat3_2 <- dat3 |>
  group_by(
    item
    , anchor
  ) |>
  summarise(
    mean_cl_normal(z_02)
  )

ggplot(dat3_2, aes(x=item, y=y, col=anchor)) +
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_point(data=dat3, aes(x=item, y=z_01), shape=3, size=2, col='black') +
  labs(y='z', x='item number') +
  jtools::theme_apa(legend.use.title = T)
ggsave("anchoring_items.png", width=16.5, height=8, units="cm")


### -- 3.) Visualize dat$z (data grouped according to our design)

# violin plots

ggviolin(data=dat, x="time_pressure", y="z", fill="anchor", add="mean_se", trim=TRUE)


# bar plots

dat2 <- dat |> 
  group_by(
    anchor, time_pressure
  ) |>
  summarise(
    mean_cl_normal(z)
  )

ggplot(dat2, aes(x=time_pressure, y=y, fill=anchor)) +
  geom_col(width = 0.5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1, position = position_dodge(0.5)) +
  scale_fill_grey(start=.4, end=.8) +
  labs(y='z', x='time pressure') +
  jtools::theme_apa(legend.use.title = T)
ggsave("interaction_bars.png", width=16.5, height=8, units="cm")



##############
### -- Main analysis
### -- 1.) Perform the main analysis as specified under "Analyses" in our preregistration
### -- 2.) Repeat the main analysis item-by-item (not preregistered)
### -- 3.) Perform the manipulation check as specified under "Other" in the preregistration
##############

# First, convert to factor
dat$anchor <- factor(dat$anchor)
dat$time_pressure <- factor(dat$time_pressure)

### -- 1.) Perform the main analysis as specified under "Analyses" in our preregistration

# From preregistration:
# We analyze the dependent measure mean z-score with a 2 (anchor: high vs. low) x 2 (time pressure: with vs. without) ANOVA.

aov_z <- aov(z ~ anchor * time_pressure, data=dat)
summary(aov_z)
sjstats::anova_stats(aov_z)
effectsize::eta_squared(aov_z, partial = TRUE, alternative = "two.sided")


### -- 2.) Repeat the main analysis item-by-item (not preregistered)

aov_03 <- aov(QU03_02_z ~ anchor * time_pressure, data=dat)
summary(aov_03)
sjstats::anova_stats(aov_03)
effectsize::eta_squared(aov_03, partial = TRUE, alternative = "two.sided")

aov_04 <- aov(QU04_02_z ~ anchor * time_pressure, data=dat)
summary(aov_04)
sjstats::anova_stats(aov_04)
effectsize::eta_squared(aov_04, partial = TRUE, alternative = "two.sided")

aov_05 <- aov(QU05_02_z ~ anchor * time_pressure, data=dat)
summary(aov_05)
sjstats::anova_stats(aov_05)
effectsize::eta_squared(aov_05, partial = TRUE, alternative = "two.sided")


### -- 3.) Perform the manipulation check as specified under "Other" in the preregistration

# From preregistration:
# As a manipulation check, we will also analyze participants' mean response times for the three experimental trials with a 2 (anchor: high vs. low) x 2 (time pressure: with vs. without) ANOVA. We predict a main effect of time pressure with participants responding faster with time pressure than without time pressure.

#QUXX_04 is sum of page loading time (5 s = 5000 ms) and actual reaction time (RT)
dat$QU03_04 <- dat$QU03_04 - 5000
dat$QU04_04 <- dat$QU04_04 - 5000
dat$QU05_04 <- dat$QU05_04 - 5000

dat$mean_time <- (dat$QU03_04 + dat$QU04_04 + dat$QU05_04) / 3

aggregate(mean_time ~ time_pressure, dat, mean)
aggregate(mean_time ~ time_pressure, dat, sd)

aov_mean_time <- aov(mean_time ~ anchor * time_pressure, data=dat)
summary(aov_mean_time)
sjstats::anova_stats(aov_mean_time)
effectsize::eta_squared(aov_mean_time, partial = TRUE, alternative = "two.sided")

ggviolin(data=dat, x="time_pressure", y="mean_time", fill="anchor", add="mean_se", trim=TRUE)



##############
### -- Get the descriptive statistics from our data set
### -- 1.) age
### -- 2.) sex
##############

### -- 1.) age
summary(dat$DE01_01)
sd(dat$DE01_01)

### -- 2.) sex
table(dat$DE02)