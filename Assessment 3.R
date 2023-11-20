setwd("E:\\Documents\\School\\PHD\\EPY 723")

library(vcd)
library(magrittr)
library(lavaan)
library(ggplot2)
library(tidySEM)

# Q2

# Read in the data

raterdata <- read.csv('HCrect-1.csv')

# Weighted Kappa with Equal-Spacing

Kappa(table(raterdata$human,raterdata$computer), weights='Equal-Spacing')

# Computer confidence intervals

confint(Kappa(table(raterdata$human,raterdata$computer), weights='Equal-Spacing'),level = 0.95)

# Q3

# Read in the data

cfadata <- read.csv('EclipseQuest.csv')

# Generate descriptives

cols <- c('astro1','astro2','astro3','nat1','nat2','nat3','people')

statmode <- function(someval) {
  
  uval <- unique(na.omit(someval))
  
  uval[which.max(tabulate(match(someval,uval)))]
  
}

descriptive <- function(somecol) {
  
  vector <- unname(
    c(sum(!is.na(somecol)),
      sum(is.na(somecol)),
      min(somecol,na.rm=TRUE),
      median(somecol,na.rm=TRUE),
      statmode(somecol),
      max(somecol,na.rm=TRUE)
    )
  )
  
  return(vector)
  
}

descriptives <- data.frame(descriptive(cfadata$astro1),
                           descriptive(cfadata$astro2),
                           descriptive(cfadata$astro3),
                           descriptive(cfadata$nat1),
                           descriptive(cfadata$nat2),
                           descriptive(cfadata$nat3),
                           descriptive(cfadata$people)
)
                           

write.table(t(descriptives),'descriptives.csv',row.names = c('astro1','astro2','astro3','nat1','nat2','nat3','people'),
            col.names =c('N','Missing','Min','Median','Mode','Max'), sep=",")

# Re-import corrected dataset.

cfadata <- read.csv('EclipseQuestCorrected.csv')

# Generate a correlation table

cfacortable <- round(cor(cfadata),2)

# TWo-factor correlated cfa, people loaded on lovenature

mod1f <- 'loveastro =~ astro1 + astro2 + astro3
          lovenature =~ nat1 + nat2 + nat3 + people
          '

fit1 <- cfa(mod1f, data=cfadata, std.lv=TRUE)

summary(fit1, fit.measures=TRUE, standardized=TRUE)

# TWo-factor uncorrelated cfa, people loaded on lovenature

mod2f <- 'loveastro =~ astro1 + astro2 + astro3
          lovenature =~ nat1 + nat2 + nat3 + people
          loveastro ~~ 0*lovenature'

fit2 <- cfa(mod2f, data=cfadata, std.lv=TRUE)

summary(fit2, fit.measures=TRUE, standardized=TRUE)

# TWo-factor correlated cfa, people loaded on loveastro

mod3f <- 'loveastro =~ astro1 + astro2 + astro3 + people
          lovenature =~ nat1 + nat2 + nat3
          '

fit3 <- cfa(mod3f, data=cfadata, std.lv=TRUE)

summary(fit3, fit.measures=TRUE, standardized=TRUE)

# Two-factor correlated cfa, people omitted

mod4f <- 'loveastro =~ astro1 + astro2 + astro3
          lovenature =~ nat1 + nat2 + nat3
          '

fit4 <- cfa(mod4f, data=cfadata, std.lv=TRUE)

summary(fit4, fit.measures=TRUE, standardized=TRUE)

# Two-factor uncorrelated cfa, people omitted

mod5f <- 'loveastro =~ astro1 + astro2 + astro3
          lovenature =~ nat1 + nat2 + nat3
          loveastro ~~ 0*lovenature'

fit5 <- cfa(mod5f, data=cfadata, std.lv=TRUE)

summary(fit5, fit.measures=TRUE, standardized=TRUE)

