library(ggplot2)
library(openxlsx)
library(metafor)

set.seed(10)
k <- 15
cors <- data.frame("Stichprobenumfang" = round(rchisq(n = k, df = 2, ncp = 0)*5+10, digits = 0)
                   , "Korrelation" = rnorm(n = k, mean = .05, sd = .3)
                   , "Studie" = paste("Studie", toupper(letters[1:k]), sep = " ")
                   , "yi" = NA
                   , "vi" = NA
                   )

cors[, 4:5] <- metafor::escalc(ni = cors$Stichprobenumfang, ri = cors$Korrelation, measure = "COR")
cors$ucb <- cors$yi + qnorm(.975)*cors$vi
cors$lcb <- cors$yi - qnorm(.975)*cors$vi

cors

ggplot(cors, aes(x = Korrelation, y = reorder(Studie, Korrelation))) + geom_point() + geom_errorbar(xmin = cors$lcb, xmax = cors$ucb) + 
  geom_vline(xintercept = 0, lty = 2) + theme_classic() + ylab("") + xlim(c(-.4, .6))
