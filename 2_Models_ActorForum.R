##############################
#### SCRIPT #2: Models ####
##############################

### Install Packages ###

library(network)
library(statnet)
library(ergm.userterms)

### Set working directory ###

setwd("~/Box Sync/Midwest CCA coupled networks/ActorForumAnalysis/Data") #Working Directory for Harrison

### read in RDS from Data script ###

af_network_full <- readRDS("af_network_full.rds")
PartnersInForum <- readRDS("PartnersInForum.rds")
IssueMatch <- readRDS("IssueMatch.rds")
four_cycle <- readRDS("four_cycle.rds")
interdependence_score <- readRDS("interdependence_score.rds")

### Set control and seed ###

cont <-
  control.ergm(
    MCMC.burnin = 5000,
    MCMC.samplesize = 1000,
    MCMC.interval = 1000,
    # parallel = 0,
    seed = 123
  )

### Model 1 ###

m1 <- ergm(af_network_full ~ edges
          + edgecov(PartnersInForum)
          + edgecov(IssueMatch)
          + edgecov(four_cycle)
          + edgecov(interdependence_score)
          # + b2cov("Interdependence")
          # + b2cov("UniSponsored")
          # + b2cov("GovSponsored")
          # + b2cov("EitherUniGov")
          + b2cov("forum_issues")
          + b1cov("actor_actors")
          + b1cov("actor_issues")
          + gwb1degree(.1, T)
          + gwb2degree(.2, T)
          # + b1factor("isolates", levels=-1)
          # + isolates()
           # org type and scope
           # custom ergm terms
           , control = cont
           , verbose = TRUE
)
summary(m1)
gof_m1 <- gof(m1)
# par(mfrow = c(2, 2)) 
plot(gof_m1)

m2 <- ergm(af_network_full ~ edges
           + edgecov(PartnersInForum)
           + edgecov(IssueMatch)
           + edgecov(four_cycle)
           + edgecov(interdependence_score)
           + b1factor("OrgType", 2) 
           # + b2cov("Interdependence")
           + b2cov("UniSponsored")
           + b2cov("GovSponsored")
           # + b2cov("EitherUniGov")
           + b2cov("forum_issues")
           + b1cov("actor_actors")
           + b1cov("actor_issues")
           + gwb1degree(.1, T)
           + gwb2degree(.2, T)
           # + isolates()
           # org type and scope
           # custom ergm terms
           , control = cont
           , verbose = TRUE
)
summary(m2)
### Reporting results of models ###

library(texreg)
screenreg(m1, single.row = T) 
screenreg(list(m1, m2), single.row = T)         

htmlreg(list(m1, m2, m3, m4), single.row = T, file = "../Output/testmodel.html")         
summary(m1)
summary(m2)
summary(m3)
summary(m4)

### Goodness of fit analyses ###

gofm1 <- gof(m1)
gofm2 <- gof(m2)
gofm3 <- gof(m3)
gofm1
gofm2
gofm3
plot(gofm1)
plot(gofm2)
plot(gofm3)



