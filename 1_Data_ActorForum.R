##############################
#### SCRIPT #1: Make Data ####
##############################

### Install Packages

library(network)
library(statnet)

### Set working directory

setwd("~/Box Sync/Midwest CCA coupled networks/ActorForumAnalysis/Data") #Working Directory for Harrison

#### Read in files for edgelist ####

af <- read.csv("actor-forum_edgelist.csv")
ai <- read.csv("ai_edgelist.csv")
aa <- read.csv("actor-actor_edgelist.csv")
fi <- read.csv("forum-issue_edgelist.csv")

#### Read in issue-issue adjacency matrix, turn into matrix object ####

ii_DirectedWeighted <- read.csv("../data/IssueMatrix_DirectedWeighted.csv", header = TRUE) #Issue-issue matrix
colnames(ii_DirectedWeighted)[1] <- "AirQuality" #Correct the column names
rownames(ii_DirectedWeighted) <- colnames(ii_DirectedWeighted) #Correct the row names
ii_DirectedWeighted <- as.matrix(ii_DirectedWeighted) 
colnames(ii_DirectedWeighted) <- rownames(ii_DirectedWeighted) <- paste0("i_", colnames(ii_DirectedWeighted))

#### Change headers for ai and fi and aa to match other data frames (a_ and f_ and i_) ####

ai$Actor <- paste0("a_", ai$Actor)
ai$Issue <- paste0("i_", ai$Issue)
fi$Forum <- paste0("f_", fi$forum)
fi$Issue <- paste0("i_", fi$issue)
aa$send <- paste0("a_", aa$send)
aa$receive <- paste0("a_", aa$receive)

#### Make the ai and fi edgelist objects #### 

ai_mode1 <- unique(ai[,1])
ai_network <- as.network(ai[,1:2], bipartite=length(ai_mode1), directed=FALSE)
ai_el <- reshape2::melt(as.sociomatrix(ai_network))
fi_mode1 <- unique(fi[,3])
fi_network <- as.network(fi[,3:4], bipartite=length(fi_mode1), directed=FALSE)
fi_el <- reshape2::melt(as.sociomatrix(fi_network))
  
#### Make actor-forum bipartite network object (without the isolates) ####

af$Actor <- paste0("a_", af$actor)
af$Forum <- paste0("f_", af$forum)
mode1 <- unique(af[,1])
mode2 <- unique(af[,2])
af_network_simple <- as.network(af[,3:4], bipartite=length(mode1), directed=FALSE)
af_network_simple

#### Add isolates (actors not tied to any forums) to the af bipartite network ####

isolate_list <- read.csv("isolates.csv")
isolate_list$Isolates <- paste0("a_", isolate_list$Isolates)
list.isolates <- as.list(isolate_list[,1])
isolatevector <- unlist(list.isolates)
af_network_full <- add.vertices.network(af_network_simple,length(list.isolates), last.mode=FALSE)
get.network.attribute(af_network_full,'bipartite')
vnames <- af_network_full%v%"vertex.names"
# vnames[316:642] <- isolatevector #hardcoding replaced with following line
vnames[which(is.na(vnames))] <- isolatevector
af_network_full%v%"vertex.names" <- vnames

####  Load in forum and actor attribute data and make them dfs ####

af_df <- data.frame(v=af_network_full%v%"vertex.names")

forum_attr <- read.csv("ForumSponsorship.csv") #Forum attribute file
forum_attr$match <- paste0("f_", forum_attr$forum) 
actor_attr <- read.csv("actor_orgtype.csv")
actor_attr$match <- paste0("a_", actor_attr$Actor)

#### Create Dummy Variable for Isolates *sorry for the hard-coding*  ####

isolatevector1 <- rep(0,315)
isolatevector2 <- rep(1,327)
dummy_isolatevec <- c(isolatevector1, isolatevector2)
isolate_attr <- data.frame(af_df$v[1:642], dummy_isolatevec)
isolate_attr$match <- isolate_attr$af_df.v.1.642.
isolate_attr$iso <- isolate_attr$dummy_isolatevec
af_df$isolate <- with(isolate_attr, iso[match(af_df$v, match)])
af_network_full%v%"isolates" <- af_df$isolate

#### Add main attributes to network #### 

af_df$OrgType <- with(actor_attr, OrgType[match(af_df$v, match)])
af_df$Scope <- with(actor_attr, Scope[match(af_df$v, match)])
af_df$GovSponsored <- with(forum_attr, GovSponsored[match(af_df$v, match)])
af_df$UniSponsored <- with(forum_attr, UniSponsored[match(af_df$v, match)])
af_df$EitherUniGov <- with(forum_attr, EitherUniGov[match(af_df$v, match)])
af_network_full%v%"OrgType" <- af_df$OrgType
af_network_full%v%"Scope" <- af_df$Scope
af_network_full%v%"GovSponsored" <- af_df$GovSponsored
af_network_full%v%"UniSponsored" <- af_df$UniSponsored
af_network_full%v%"EitherUniGov" <- af_df$EitherUniGov

#### Make af edgelist ####

af_el <- reshape2::melt(as.sociomatrix(af_network_full))

#### Make Blank Matrices for each edge covariate ####

PartnersInForum <- as.sociomatrix(af_network_full)
PartnersInForum[,] <- 0
IssueMatch <- as.sociomatrix(af_network_full)
IssueMatch[,] <- 0
four_cycle <- as.sociomatrix(af_network_full)
four_cycle[,] <- 0
interdependence_score <- as.sociomatrix(af_network_full)
interdependence_score[,] <- 0

#### Partners in Forum Baseline (COMPLETED) #### 
for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]
  a_partners <- aa$receive[which(aa$send==a)]
  f_partners <- af$Actor[which(af$Forum==f)]
  af_matches <- length(which(a_partners%in%f_partners))
  # if(af_matches > 0){
  #   print(i)
  # }
  PartnersInForum[which(rownames(PartnersInForum)==a),which(colnames(PartnersInForum)==f)] <- af_matches/length(a_partners)
}
PartnersInForum[which(is.na(PartnersInForum))] <- 0

#### Issues of Forum Match Issues of Actor (COMPLETED) ####
for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]
  a_issues <- ai$Issue[which(ai$Actor==a)]
  f_issues <- fi$Issue[which(fi$Forum==f)]
  issue_matches <- length(which(a_issues%in%f_issues))
  # if(issue_matches > 0){
  #   print(i)
  # }
  IssueMatch[which(rownames(IssueMatch)==a),which(colnames(IssueMatch)==f)] <- issue_matches/length(a_issues)
}

#### Number of similar forums based on issue overlap - forum four-cycles (COMPLETED) ####

for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]

  a_forums <- af$Forum[which(af$Actor==a)] #List of forums each actor participates in
  if(f%in%a_forums){
    a_forums <-a_forums[-which(a_forums==f)]
  }
  f_issues <- fi$Issue[which(fi$Forum==f)] #List of issues each forum focuses on
  ticker <- 0
  
  for(ii in 1:length(f_issues)){
    f_issues_ii <- f_issues[ii]
    i_forums <- fi$Forum[which(fi$Issue==f_issues_ii)] #List of issues each forum focuses on
    forum_matches <- length(which(i_forums%in%a_forums))
    ticker <- ticker + forum_matches
}
  four_cycle[which(rownames(four_cycle)==a),which(colnames(four_cycle)==f)] <- ticker/(length(a_forums)*length(f_issues))
}
four_cycle[which(is.na(four_cycle))] <- 0

#### Issue Interdependency Score for each Forum (COMPLETED) ####

for(i in 1:nrow(fi_el)){
  cat("\r", i)
  f <- fi_el$Var1[i]
  is <- fi_el$Var2[i]
  f_issues <- fi$Issue[which(fi$Forum==f)]
  ii_expand <- expand.grid(f_issues, f_issues)
  ii_expand <- ii_expand[-which(ii_expand$Var1==ii_expand$Var2),]
   issue_connectivity_vector <- c()
  for(j in 1:nrow(ii_expand)){
    f_issues[i]
    issue_connectivity_vector <- c(issue_connectivity_vector, 
                        ii_DirectedWeighted[which(colnames(ii_DirectedWeighted)==ii_expand$Var1[j]),
                        which(rownames(ii_DirectedWeighted)==ii_expand$Var2[j])])
  }

   interdependence_score[,which(colnames(interdependence_score)==f)] <- mean(issue_connectivity_vector)
}  
interdependence_score[which(is.na(interdependence_score))] <- 0

#### Convert interdependence score matrix to vector, then add it as a forum attribute to the network

names <- as.vector(colnames(interdependence_score))
inter_vector <- as.vector(interdependence_score[1,])
forum_interscore <- data.frame(names, inter_vector)
af_df$Interdependence <- with(forum_interscore, inter_vector[match(af_df$v, names)])
af_network_full%v%"Interdependence" <- af_df$Interdependence

#### Use for loops to count up various stats for each actor and forum ####

## Create Blank Vectors for which to input the data ##

actor_actors_vec <- actor_issues_vec <-  vector("integer", length = 642)
forum_issues_vec <- forum_issues_vec2 <- vector("integer", length = 393 )

## For loops for each statistic ## 

for(i in 1:642){
  cat("\r", i)
  a <- af_el$Var1[i]
  a_partners <- aa$receive[which(aa$send==a)]
  actor_actors_vec[i] <- length(a_partners)/641 #Proportion of all other actors who they partner with
}
for(i in 1:642){
  cat("\r", i)
  a <- ai_el$Var1[i]
  a_issues <- ai$Issue[which(ai$Actor==a)]
  actor_issues_vec[i] <- length(a_issues)/19 #Proportion of total issues the actor works on
}
for(i in 1:393){
  cat("\r", i)
  f <- fi_el$Var1[i]
  f_issues <- fi$Issue[which(fi$Forum==f)]
  forum_issues_vec[i] <- length(f_issues)/19 #Proportion of total issues the forum works on
  forum_issues_vec2[i] <- length(f_issues) ## Creating dummy variable used directly below
}

#### Create Dummy variable for forums only working on one issue, add to network ####
forum_issues_vec2[forum_issues_vec2 > 1] <- 0
forum_attr2 <- data.frame(af_df$v[643:1035], forum_issues_vec2)
forum_attr2$match <- forum_attr2$af_df.v.643.1035.
forum_attr2$one_issue <- forum_attr2$forum_issues_vec2
af_df$one_issue <- with(forum_attr2, one_issue[match(af_df$v, match)])
af_network_full%v%"one_issue" <- af_df$one_issue

#### Create new data frames to store the extra actor and forum statistics ####

new_actor_nums_df <- data.frame(actor_actors_vec, actor_issues_vec)
new_actor_nums_df$name <- af_df$v[1:642]
new_forum_nums_df <- data.frame(forum_issues_vec)
new_forum_nums_df$name <- af_df$v[643:1035]

#### Add the new actor and forum counts as node attributes to the network ####

af_df$actor_actors <- with(new_actor_nums_df, actor_actors_vec[match(af_df$v, name)])
af_network_full%v%"actor_actors" <- af_df$actor_actors
af_df$actor_issues <- with(new_actor_nums_df, actor_issues_vec[match(af_df$v, name)])
af_network_full%v%"actor_issues" <- af_df$actor_issues
af_df$forum_issues <- with(new_forum_nums_df, forum_issues_vec[match(af_df$v, name)])
af_network_full%v%"forum_issues" <- af_df$forum_issues

#### Save RDS files for separate modeling script #### 

saveRDS(af_network_full, "af_network_full.rds")
saveRDS(PartnersInForum, "PartnersInForum.rds")
saveRDS(IssueMatch, "IssueMatch.rds")
saveRDS(four_cycle, "four_cycle.rds")
saveRDS(interdependence_score, "interdependence_score.rds")





#### Extra adventures
#### PARTNERS/LIKELIHOOD OF TIE #### 
Partners <- Partners0 <- as.sociomatrix(af_network_full)

Partners[,] <- Partners0[,] <- 0
for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]
  a_partners <- aa$receive[which(aa$send==a)]
  f_partners <- af$Actor[which(af$Forum==f)]
  af_matches <- length(which(a_partners%in%f_partners))
  # if(af_matches > 0){
  #   print(i)
  # }
  Partners[which(rownames(Partners)==a),which(colnames(Partners)==f)] <- af_matches
}
Partners[which(is.na(Partners))] <- 0
PartnersVec <- as.vector(Partners)
af_el$Partners <- PartnersVec

#A way to get the unique integers
unique_int <- unique(c(Partners))

#Count
count <- sapply(unique_int, function(x) sum(Partners == x))
names(count) <- unique_int

count <- c(228041, 19581, 3385, 804, 295, 111, 44, 23, 8, 7, 4, 1, 1, 0, 0, 1)
plot(af_el$Partners, af_el$value)
af_el0 <- af_el[af_el$Partners==0,]
avg0 <- mean(af_el0$value)
af_el1 <- af_el[af_el$Partners==1,]
avg1 <- mean(af_el1$value)
af_el2 <- af_el[af_el$Partners==2,]
avg2 <- mean(af_el2$value)
af_el3 <- af_el[af_el$Partners==3,]
avg3 <- mean(af_el3$value)
af_el4 <- af_el[af_el$Partners==4,]
avg4 <- mean(af_el4$value)
af_el5 <- af_el[af_el$Partners==5,]
avg5 <- mean(af_el5$value)
af_el6 <- af_el[af_el$Partners==6,]
avg6 <- mean(af_el6$value)
af_el7 <- af_el[af_el$Partners==7,]
avg7 <- mean(af_el7$value)
af_el8 <- af_el[af_el$Partners==8,]
avg8 <- mean(af_el8$value)
af_el9 <- af_el[af_el$Partners==9,]
avg9 <- mean(af_el9$value)
af_el10 <- af_el[af_el$Partners==10,]
avg10 <- mean(af_el10$value)
af_el11 <- af_el[af_el$Partners==11,]
avg11 <- mean(af_el11$value)
af_el12 <- af_el[af_el$Partners==12,]
avg12 <- mean(af_el12$value)
af_el13 <- af_el[af_el$Partners==13,]
avg13 <- mean(af_el13$value)
af_el14 <- af_el[af_el$Partners==14,]
avg14 <- mean(af_el14$value)
af_el15 <- af_el[af_el$Partners==15,]
avg15 <- mean(af_el15$value)

tstdf$count <- count
tstdf <- data.frame(list(0:15), c(avg0, avg1, avg2, avg3, avg4, avg5, avg6, avg7, avg8, avg9, avg10, avg11, avg12, avg13, avg14, avg15))
tstdf$partners <- tstdf$X0.15
tstdf$likelihood <- tstdf$c.avg0..avg1..avg2..avg3..avg4..avg5..avg6..avg7..avg8..avg9..




#### ISSUES/LIKELIHOOD OF TIE #####
Issues <- as.sociomatrix(af_network_full)
Issues[,] <- 0
for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]
  a_issues <- ai$Issue[which(ai$Actor==a)]
  f_issues <- fi$Issue[which(fi$Forum==f)]
  issue_matches <- length(which(a_issues%in%f_issues))
  # if(issue_matches > 0){
  #   print(i)
  # }
  Issues[which(rownames(Issues)==a),which(colnames(Issues)==f)] <- issue_matches
}
IssueOverlapVector <- as.vector(Issues)
IssueOverlapTable <- data.frame(table(Issues))
af_el$IssueOverlap <- IssueOverlapVector

iaf_el0 <- af_el[af_el$IssueOverlap==0,]
iavg0 <- mean(iaf_el0$value)
iaf_el1 <- af_el[af_el$IssueOverlap==1,]
iavg1 <- mean(iaf_el1$value)
iaf_el2 <- af_el[af_el$IssueOverlap==2,]
iavg2 <- mean(iaf_el2$value)
iaf_el3 <- af_el[af_el$IssueOverlap==3,]
iavg3 <- mean(iaf_el3$value)
iaf_el4 <- af_el[af_el$IssueOverlap==4,]
iavg4 <- mean(iaf_el4$value)
iaf_el5 <- af_el[af_el$IssueOverlap==5,]
iavg5 <- mean(iaf_el5$value)
iaf_el6 <- af_el[af_el$IssueOverlap==6,]
iavg6 <- mean(iaf_el6$value)
iaf_el7 <- af_el[af_el$IssueOverlap==7,]
iavg7 <- mean(iaf_el7$value)
iaf_el8 <- af_el[af_el$IssueOverlap==8,]
iavg8 <- mean(iaf_el8$value)
iaf_el9 <- af_el[af_el$IssueOverlap==9,]
iavg9 <- mean(iaf_el9$value)
iaf_el10 <- af_el[af_el$IssueOverlap==10,]
iavg10 <- mean(iaf_el10$value)
iaf_el11 <- af_el[af_el$IssueOverlap==11,]
iavg11 <- mean(iaf_el11$value)
iaf_el12 <- af_el[af_el$IssueOverlap==12,]
iavg12 <- mean(iaf_el12$value)
iaf_el13 <- af_el[af_el$IssueOverlap==13,]
iavg13 <- mean(iaf_el13$value)
iaf_el14 <- af_el[af_el$IssueOverlap==14,]
iavg14 <- mean(iaf_el14$value)
iaf_el15 <- af_el[af_el$IssueOverlap==15,]
iavg15 <- mean(iaf_el15$value)
iaf_el16 <- af_el[af_el$IssueOverlap==16,]
iavg16 <- mean(iaf_el16$value)

IssueOverlapTable$Likelihood <- c(iavg0,iavg1,iavg2,iavg3,iavg4,iavg5,iavg6,iavg7,iavg8,iavg9,iavg10,iavg11,iavg12,iavg13,iavg14,iavg15,iavg16)

####Four-cycles/Tie Likelihood ####
cycles <- as.sociomatrix(af_network_full)
cycles[,] <- 0
for(i in 1:nrow(af_el)){
  cat("\r", i)
  a <- af_el$Var1[i]
  f <- af_el$Var2[i]
  
  a_forums <- af$Forum[which(af$Actor==a)] #List of forums each actor participates in
  if(f%in%a_forums){
    a_forums <-a_forums[-which(a_forums==f)]
  }
  f_issues <- fi$Issue[which(fi$Forum==f)] #List of issues each forum focuses on
  ticker <- 0
  
  for(ii in 1:length(f_issues)){
    f_issues_ii <- f_issues[ii]
    i_forums <- fi$Forum[which(fi$Issue==f_issues_ii)] #List of issues each forum focuses on
    forum_matches <- length(which(i_forums%in%a_forums))
    ticker <- ticker + forum_matches
  }
  cycles[which(rownames(cycles)==a),which(colnames(cycles)==f)] <- ticker
}
cycles[which(is.na(cycles))] <- 0
cycle_vector <- as.vector(cycles)
cycles_df <- data.frame(table(cycles))
cycles_df <- cycles_df[1:21,]
af_el$cycles <- cycle_vector

caf_el0 <- af_el[af_el$cycles==0,]
cavg0 <- mean(caf_el0$value)
caf_el1 <- af_el[af_el$cycles==1,]
cavg1 <- mean(caf_el1$value)
caf_el2 <- af_el[af_el$cycles==2,]
cavg2 <- mean(caf_el2$value)
caf_el3 <- af_el[af_el$cycles==3,]
cavg3 <- mean(caf_el3$value)
caf_el4 <- af_el[af_el$cycles==4,]
cavg4 <- mean(caf_el4$value)
caf_el5 <- af_el[af_el$cycles==5,]
cavg5 <- mean(caf_el5$value)
caf_el6 <- af_el[af_el$cycles==6,]
cavg6 <- mean(caf_el6$value)
caf_el7 <- af_el[af_el$cycles==7,]
cavg7 <- mean(caf_el7$value)
caf_el8 <- af_el[af_el$cycles==8,]
cavg8 <- mean(caf_el8$value)
caf_el9 <- af_el[af_el$cycles==9,]
cavg9 <- mean(caf_el9$value)
caf_el10 <- af_el[af_el$cycles==10,]
cavg10 <- mean(caf_el10$value)
caf_el11 <- af_el[af_el$cycles==11,]
cavg11 <- mean(caf_el11$value)
caf_el12 <- af_el[af_el$cycles==12,]
cavg12 <- mean(caf_el12$value)
caf_el13 <- af_el[af_el$cycles==13,]
cavg13 <- mean(caf_el13$value)
caf_el14 <- af_el[af_el$cycles==14,]
cavg14 <- mean(caf_el14$value)
caf_el15 <- af_el[af_el$cycles==15,]
cavg15 <- mean(caf_el15$value)
caf_el16 <- af_el[af_el$cycles==16,]
cavg16 <- mean(caf_el16$value)
caf_el17 <- af_el[af_el$cycles==17,]
cavg17 <- mean(caf_el17$value)
caf_el18 <- af_el[af_el$cycles==18,]
cavg18 <- mean(caf_el18$value)
caf_el19 <- af_el[af_el$cycles==19,]
cavg19 <- mean(caf_el19$value)
caf_el20 <- af_el[af_el$cycles==20,]
cavg20 <- mean(caf_el20$value)
cycles_df$Likelihood <- c(cavg0,cavg1,cavg2,cavg3,cavg4,cavg5,cavg6,cavg7,cavg8,cavg9,cavg10,cavg11,cavg12,cavg13,cavg14,cavg15,cavg16, cavg17, cavg18, cavg19, cavg20)

####PLOTS####
par(mfrow=c(1,3))
plot(tstdf$partners, tstdf$likelihood, xlab = "Partners in Forum", ylab = "Likelihood of Tie", )
plot(IssueOverlapTable$Issues, IssueOverlapTable$Likelihood, xlab="Number of Issues", ylab="Likelihood of Tie", )
plot(cycles_df$cycles, cycles_df$Likelihood, xlab="Number of Four Cycles", ylab="Likelihood of Tie", xlim=c(0,21))



# cex=sqrt(log(tstdf$count))
# cex=sqrt(log(IssueOverlapTable$Freq))
