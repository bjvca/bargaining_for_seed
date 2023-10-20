#
rm(list = ls())

library(ggplot2)
library(ggridges)
library(car)
library(plyr)

group.center <- function(var,grp) {
  return(var-tapply(var,grp,mean,na.rm=T)[grp])
}

##
path <- getwd()

path <- strsplit(path, "/bargaining_for_seed")[[1]]
dta <- read.csv(paste(path,"baseline/data/public/baseline.csv", sep="/"))
#These are the inital (random) prices offered:
#dta$P1_pric

#These are the prices offered by the enumerator
#dta$paid.P3_pric_2 -10

#answers to offers
#dta$Check2.check.maize.start_neg 

#these are the price bids by the farmers
##dta$Check2.check.maize.paid.P2_pric

##determine last ask price

##determine maximum bid price
dta$bid <- ifelse(!is.na(as.numeric(dta$paid.P2_pric_11)),as.numeric(dta$paid.P2_pric_11),
                  ifelse(!is.na(as.numeric(dta$paid.P2_pric_10)),as.numeric(dta$paid.P2_pric_10),
                         ifelse(!is.na(as.numeric(dta$paid.P2_pric_9)),as.numeric(dta$paid.P2_pric_9),
                                ifelse(!is.na(as.numeric(dta$paid.P2_pric_8)),as.numeric(dta$paid.P2_pric_8),
                                       ifelse(!is.na(as.numeric(dta$paid.P2_pric_7)),as.numeric(dta$paid.P2_pric_7),
                                              ifelse(!is.na(as.numeric(dta$paid.P2_pric_6)),as.numeric(dta$paid.P2_pric_6),
                                                     ifelse(!is.na(as.numeric(dta$paid.P2_pric_5)),as.numeric(dta$paid.P2_pric_5),
                                                            ifelse(!is.na(as.numeric(dta$paid.P2_pric_4)),as.numeric(dta$paid.P2_pric_4),
                                                                   ifelse(!is.na(as.numeric(dta$paid.P2_pric_3)),as.numeric(dta$paid.P2_pric_3),
                                                                          ifelse(!is.na(as.numeric(dta$paid.P2_pric_2)),as.numeric(dta$paid.P2_pric_2),
                                                                                 as.numeric(dta$paid.P2_pric)
                                                                          ))))))))))

dta$bid[dta$bid>20000] <- NA
##determine minimum ask price
dta$ask <-      ifelse(!is.na(as.numeric(dta$paid.P3_pric_10)),as.numeric(dta$paid.P3_pric_10),
                       ifelse(!is.na(as.numeric(dta$paid.P3_pric_9)),as.numeric(dta$paid.P3_pric_9),
                              ifelse(!is.na(as.numeric(dta$paid.P3_pric_8)),as.numeric(dta$paid.P3_pric_8),
                                     ifelse(!is.na(as.numeric(dta$paid.P3_pric_7)),as.numeric(dta$paid.P3_pric_7),
                                            ifelse(!is.na(as.numeric(dta$paid.P3_pric_6)),as.numeric(dta$paid.P3_pric_6),
                                                   ifelse(!is.na(as.numeric(dta$paid.P3_pric_5)),as.numeric(dta$paid.P3_pric_5),
                                                          ifelse(!is.na(as.numeric(dta$paid.P3_pric_4)),as.numeric(dta$paid.P3_pric_4),
                                                                 ifelse(!is.na(as.numeric(dta$paid.P3_pric_3)),as.numeric(dta$paid.P3_pric_3),
                                                                        ifelse(!is.na(as.numeric(dta$paid.P3_pric_2)),as.numeric(dta$paid.P3_pric_2),
                                                                               ifelse(!is.na(as.numeric(dta$paid.P3_pric)),as.numeric(dta$paid.P3_pric),
                                                                                      as.numeric(dta$P1_pric)
                                                                               ))))))))))

dta$ask[dta$ask>14000] <- NA



###number of rounds after which farmer agrees
dta$rounds <- 1
dta$rounds[ dta$paid.P3_pric!="n/a" ] <- 2
dta$rounds[ dta$paid.P3_pric_2!="n/a" ] <- 3
dta$rounds[ dta$paid.P3_pric_3!="n/a" ] <-  4
dta$rounds[dta$paid.P3_pric_4!="n/a" ] <-  5
dta$rounds[ dta$paid.P3_pric_5!="n/a" ] <-  6
dta$rounds[dta$paid.P3_pric_6!="n/a" ] <-  7
dta$rounds[dta$paid.P3_pric_7!="n/a" ] <-  8
dta$rounds[dta$paid.P3_pric_8!="n/a" ] <-  9
dta$rounds[dta$paid.P3_pric_9!="n/a" ] <-  10
dta$rounds[dta$paid.P3_pric_10!="n/a" ] <-  11
dta$rounds[dta$paid.P3_pric_11!="n/a" ] <-  12



###number of rounds after which bidder accepts

dta$accepts <- "seller"
dta$accepts[dta$paid.start_neg=="Yes"| dta$paid.start_neg_2=="Yes" | dta$paid.start_neg_3=="Yes"
| dta$paid.start_neg_4=="Yes" | dta$paid.start_neg_5=="Yes" | dta$paid.start_neg_6=="Yes"| dta$paid.start_neg_7=="Yes" 
| dta$paid.start_neg_8=="Yes" | dta$paid.start_neg_9=="Yes" | dta$paid.start_neg_10=="Yes" | dta$paid.start_neg_11=="Yes"] <- "buyer"

##determine willingness to pay
dta$final_price <- NA
### if buyer accepts, this is the last ask price
dta$final_price[dta$accepts=="buyer"] <- dta$ask[dta$accepts=="buyer"]
### if seller accepts this is the last bid price
dta$final_price[dta$accepts=="seller"] <- dta$bid[dta$accepts=="seller"]

###sticky prices determine if all prices are bid prices are the same
dta$sticky <- NA

dta$sticky <-((dta$paid.P2_pric_3 == dta$paid.P2_pric) | dta$paid.P2_pric_3 == "n/a") & ((dta$paid.P2_pric_4 == dta$paid.P2_pric) | dta$paid.P2_pric_4 == "n/a") & ((dta$paid.P2_pric_5 == dta$paid.P2_pric) | dta$paid.P2_pric_5 == "n/a") & ((dta$paid.P2_pric_6 == dta$paid.P2_pric) | dta$paid.P2_pric_6 == "n/a") & ((dta$paid.P2_pric_7 == dta$paid.P2_pric) | dta$paid.P2_pric_7 == "n/a") & ((dta$paid.P2_pric_8 == dta$paid.P2_pric) | dta$paid.P2_pric_8 == "n/a") & ((dta$paid.P2_pric_9 == dta$paid.P2_pric) | dta$paid.P2_pric_9 == "n/a") & ((dta$paid.P2_pric_10 == dta$paid.P2_pric) | dta$paid.P2_pric_10 == "n/a")

dta$sticky[dta$paid.P2_pric=="n/a"] <- NA

### keep only bargainers (those that get a initial price)
dta <- subset(dta, !is.na(P1_pric))
## drop that outlier

dta[c("P1_pric","bid","ask","rounds", "final_price", "accepts")]


###some cool graphs
dta$anchor <- as.factor(dta$P1_pric)

dta$starting_price <- as.numeric(as.character(dta$paid.P2_pric))
#set starting price to anchor if buyer accepts at first round
dta$starting_price[dta$paid.start_neg=="Yes"] <- as.numeric(as.character(dta$P1_pric))[dta$paid.start_neg=="Yes"]


plt1 <- dta[c("anchor","starting_price")]
plt1$time <- "initial price"
names(plt1) <- c("anchor","price","time")
plt2 <- dta[c("anchor","final_price")]
plt2$time <- "final price"
names(plt2) <- c("anchor","price","time")
plt <- rbind(plt1,plt2)

ggplot(plt, aes(x=price, fill=time)) +
  geom_density() + scale_fill_grey() + theme_classic() +
  geom_density(alpha=0.4) +
  scale_x_continuous(limits = c(1000, 13500))

ggplot(plt, aes(x = price, y = anchor)) + geom_density_ridges2(jittered_points = TRUE,
                                                                          position = position_points_jitter(width = 0.5, height = 0),
                                                                          point_shape = '|', point_size = 3, point_alpha = .5, alpha = 0.5)



ggplot(plt, aes(x = price, y = anchor, fill=time)) + geom_density_ridges2(jittered_points = TRUE,
                                                                                    position = position_points_jitter(width = 0.5, height = 0),
                                                                                    point_shape = '|', point_size = 3, point_alpha = .5, alpha = 0.5)

### analysis
##starting price on immediate acceptance
mean(dta$paid.start_neg=="Yes")*100
summary(lm(paid.start_neg=="Yes"~as.factor(anchor), dta))
summary(lm(starting_price~as.factor(anchor), dta))
summary(lm(final_price~as.factor(anchor), dta))

#iterate over outcomes
outcomes <- c("final_price","starting_price","rounds","paid.start_neg=='Yes'","sticky")
#matrix to store results
res_tab <-  array(NA,dim=c(13,3,length(outcomes)))
for (i in 1:length(outcomes)) {
  
  ### pooled regression (for marginal effects)
  ols <- lm(as.formula( paste(outcomes[i],"anchor",sep="~")), dta)
  res_tab[1:4,1,i] <- summary(ols)$coefficients[,1] 
  res_tab[1:4,2,i] <- summary(ols)$coefficients[,2] 
  res_tab[1:4,3,i] <- summary(ols)$coefficients[,4] 
  #p-values for test
  ftest <- linearHypothesis(ols,test="F", "anchor10000  = anchor11000")
  res_tab[5,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(ols,test="F", "anchor10000  = anchor12000")
  res_tab[6,3,i] <- ftest[["Pr(>F)"]][2]
  ftest <- linearHypothesis(ols,test="F", "anchor11000  = anchor12000")
  res_tab[7,3,i] <- ftest[["Pr(>F)"]][2]
  
  res_tab[8,1,i] <- nobs(ols)
  
  res_tab[9,1,i] <-  summary(ols)$adj.r.squared

  
  
}

##gender analysis starts here

summary(lm(final_price~gender, data=dta))

summary(lm(final_price~gender+anchor, data=dta))

summary(lm(final_price~enumerator_gender, data=dta))

summary(lm(final_price~enumerator_gender+anchor, data=dta))


#level 1 - gender of the buyer (controling for gender of the seller in regressions)
### look at gender balance for randomized offer price
summary(lm(gender=="Female" ~anchor, data=dta))
prop.table(table(dta$gender, dta$anchor), margin=2)
chisq.test(table(dta$gender, dta$anchor))




##question 1:do women bargain harder than nem
dta$gender[dta$resp_gender!= "n/a"] <- dta$resp_gender[dta$resp_gender!= "n/a"]

summary(lm(final_price~enumerator_gender*gender, data=dta))

summary(lm(starting_price~enumerator_gender*gender, data=dta))

summary(lm((paid.start_neg=='Yes')~enumerator_gender*gender, data=dta))

summary(lm(sticky~enumerator_gender*gender, data=dta))

summary(lm(final_price~enumerator_gender*gender*anchor, data=dta))

summary(lm(starting_price~enumerator_gender*gender*anchor, data=dta))

summary(lm((paid.start_neg=='Yes')~enumerator_gender*gender*anchor, data=dta))

summary(lm(sticky~enumerator_gender*gender*anchor, data=dta))


dta$initial_accept <- dta$paid.start_neg=='Yes'

###ethiopia
teff <- read.csv(paste(path,"bargaining_for_seed/ethiopia/data/Teff_BargainingExp.csv",sep="/"),na.strings=)
### only keep those that were part of the price expriment
teff <- subset(teff,paid_pac==TRUE)



wheat <- read.csv(paste(path,"bargaining_for_seed/ethiopia/data/Wheat_BargainingExp.csv",sep="/"))

teff$anchor <- as.factor(teff$p1_pric)
wheat$anchor <- as.factor(wheat$p1_pric)

wheat <- subset(wheat,paid_pac==TRUE)
##determine maximum bid price
teff$bid <- ifelse(!is.na(as.numeric(teff$p2_pric_11)),as.numeric(teff$p2_pric_11),
                  ifelse(!is.na(as.numeric(teff$p2_pric_10)),as.numeric(teff$p2_pric_10),
                         ifelse(!is.na(as.numeric(teff$p2_pric_9)),as.numeric(teff$p2_pric_9),
                                ifelse(!is.na(as.numeric(teff$p2_pric_8)),as.numeric(teff$p2_pric_8),
                                       ifelse(!is.na(as.numeric(teff$p2_pric_7)),as.numeric(teff$p2_pric_7),
                                              ifelse(!is.na(as.numeric(teff$p2_pric_6)),as.numeric(teff$p2_pric_6),
                                                     ifelse(!is.na(as.numeric(teff$p2_pric_5)),as.numeric(teff$p2_pric_5),
                                                            ifelse(!is.na(as.numeric(teff$p2_pric_4)),as.numeric(teff$p2_pric_4),
                                                                   ifelse(!is.na(as.numeric(teff$p2_pric_3)),as.numeric(teff$p2_pric_3),
                                                                          ifelse(!is.na(as.numeric(teff$p2_pric_2)),as.numeric(teff$p2_pric_2),
                                                                                 as.numeric(teff$p2_pric)
                                                                          ))))))))))

### max bid can not be higher that initial offer price
teff$bid[teff$bid>teff$p1_pric] <- NA

wheat$bid <- ifelse(!is.na(as.numeric(wheat$p2_pric_11)),as.numeric(wheat$p2_pric_11),
                   ifelse(!is.na(as.numeric(wheat$p2_pric_10)),as.numeric(wheat$p2_pric_10),
                          ifelse(!is.na(as.numeric(wheat$p2_pric_9)),as.numeric(wheat$p2_pric_9),
                                 ifelse(!is.na(as.numeric(wheat$p2_pric_8)),as.numeric(wheat$p2_pric_8),
                                        ifelse(!is.na(as.numeric(wheat$p2_pric_7)),as.numeric(wheat$p2_pric_7),
                                               ifelse(!is.na(as.numeric(wheat$p2_pric_6)),as.numeric(wheat$p2_pric_6),
                                                      ifelse(!is.na(as.numeric(wheat$p2_pric_5)),as.numeric(wheat$p2_pric_5),
                                                             ifelse(!is.na(as.numeric(wheat$p2_pric_4)),as.numeric(wheat$p2_pric_4),
                                                                    ifelse(!is.na(as.numeric(wheat$p2_pric_3)),as.numeric(wheat$p2_pric_3),
                                                                           ifelse(!is.na(as.numeric(wheat$p2_pric_2)),as.numeric(wheat$p2_pric_2),
                                                                                  as.numeric(wheat$p2_pric)
                                                                           ))))))))))

### max bid can not be higher that initial offer price
wheat$bid[wheat$bid>wheat$p1_pric] <- NA

###determine minimum ask price
teff$ask <-      
                                                 
                                                          ifelse(!is.na(as.numeric(teff$p3_pric_4)),as.numeric(teff$p3_pric_4),
                                                                 ifelse(!is.na(as.numeric(teff$p3_pric_3)),as.numeric(teff$p3_pric_3),
                                                                        ifelse(!is.na(as.numeric(teff$p3_pric_2)),as.numeric(teff$p3_pric_2),
                                                                               ifelse(!is.na(as.numeric(teff$p3_pric)),as.numeric(teff$p3_pric),
                                                                                      as.numeric(teff$p1_pric)
                                                                               ))))

wheat$ask <-      
  
  ifelse(!is.na(as.numeric(wheat$p3_pric_4)),as.numeric(wheat$p3_pric_4),
         ifelse(!is.na(as.numeric(wheat$p3_pric_3)),as.numeric(wheat$p3_pric_3),
                ifelse(!is.na(as.numeric(wheat$p3_pric_2)),as.numeric(wheat$p3_pric_2),
                       ifelse(!is.na(as.numeric(wheat$p3_pric)),as.numeric(wheat$p3_pric),
                              as.numeric(wheat$p1_pric)
                       ))))



teff$accepts <- "seller"
teff$accepts[teff$start_neg=="Yes"| teff$start_neg_2=="Yes" | teff$start_neg_3=="Yes"
            | teff$start_neg_4=="Yes" | teff$start_neg_5=="Yes" | teff$start_neg_6=="Yes"| teff$start_neg_7=="Yes" 
            | teff$start_neg_8=="Yes" | teff$start_neg_9=="Yes" | teff$start_neg_10=="Yes" | teff$start_neg_11=="Yes"] <- "buyer"

##determine willingness to pay
teff$final_price <- NA
### if buyer accepts, this is the last ask price
teff$final_price[teff$accepts=="buyer"] <- teff$ask[teff$accepts=="buyer"]
### if seller accepts this is the last bid price
teff$final_price[teff$accepts=="seller"] <- teff$bid[teff$accepts=="seller"]

wheat$accepts <- "seller"
wheat$accepts[wheat$start_neg=="Yes"| wheat$start_neg_2=="Yes" | wheat$start_neg_3=="Yes"
             | wheat$start_neg_4=="Yes" | wheat$start_neg_5=="Yes" | wheat$start_neg_6=="Yes"| wheat$start_neg_7=="Yes" 
             | wheat$start_neg_8=="Yes" | wheat$start_neg_9=="Yes" | wheat$start_neg_10=="Yes" | wheat$start_neg_11=="Yes"] <- "buyer"

##determine willingness to pay
wheat$final_price <- NA
### if buyer accepts, this is the last ask price
wheat$final_price[wheat$accepts=="buyer"] <- wheat$ask[wheat$accepts=="buyer"]
### if seller accepts this is the last bid price
wheat$final_price[wheat$accepts=="seller"] <- wheat$bid[wheat$accepts=="seller"]

### 

###sticky prices determine if all prices are bid prices are the same
teff$sticky <- NA

teff$sticky <-((teff$p2_pric_3 == teff$p2_pric) | is.na(teff$p2_pric_3)) & ((teff$p2_pric_4 == teff$p2_pric) | is.na(teff$p2_pric_4)) & ((teff$p2_pric_5 == teff$p2_pric) | is.na(teff$p2_pric_5) ) & ((teff$p2_pric_6 == teff$p2_pric) | is.na(teff$p2_pric_6)) & ((teff$p2_pric_7 == teff$p2_pric) | is.na(teff$p2_pric_7)) & ((teff$p2_pric_8 == teff$p2_pric) | is.na(teff$p2_pric_8)) & ((teff$p2_pric_9 == teff$p2_pric) | is.na(teff$p2_pric_9)) & ((teff$p2_pric_10 == teff$p2_pric) | is.na(teff$p2_pric_10 ))

teff$sticky[is.na(teff$p2_pric)] <- NA

###sticky prices determine if all prices are bid prices are the same
wheat$sticky <- NA

wheat$sticky <-((wheat$p2_pric_3 == wheat$p2_pric) | is.na(wheat$p2_pric_3)) & ((wheat$p2_pric_4 == wheat$p2_pric) | is.na(wheat$p2_pric_4)) & ((wheat$p2_pric_5 == wheat$p2_pric) | is.na(wheat$p2_pric_5) ) & ((wheat$p2_pric_6 == wheat$p2_pric) | is.na(wheat$p2_pric_6)) & ((wheat$p2_pric_7 == wheat$p2_pric) | is.na(wheat$p2_pric_7)) & ((wheat$p2_pric_8 == wheat$p2_pric) | is.na(wheat$p2_pric_8)) & ((wheat$p2_pric_9 == wheat$p2_pric) | is.na(wheat$p2_pric_9)) & ((wheat$p2_pric_10 == wheat$p2_pric) | is.na(wheat$p2_pric_10 ))

wheat$sticky[is.na(wheat$p2_pric)] <- NA



teff$initial_accept <- teff$start_neg == "Yes"
wheat$initial_accept <- wheat$start_neg == "Yes"
###
dta$starting_price <- as.numeric(as.character(dta$paid.P2_pric))
#set starting price to anchor if buyer accepts at first round
dta$starting_price[dta$start_neg=="Yes"] <- as.numeric(as.character(dta$P1_pric))[dta$start_neg=="Yes"]

teff$starting_price <- as.numeric(teff$p2_pric)
teff$starting_price[teff$start_neg=="Yes"] <- as.numeric(as.character(teff$p1_pric))[teff$start_neg=="Yes"]
teff$starting_price[teff$starting_price >  as.numeric(as.character(teff$p1_pric))] <- NA

wheat$starting_price <- as.numeric(wheat$p2_pric)
wheat$starting_price[wheat$start_neg=="Yes"] <- as.numeric(as.character(wheat$p1_pric))[wheat$start_neg=="Yes"]
wheat$starting_price[wheat$starting_price >  as.numeric(as.character(wheat$p1_pric))] <- NA

#bottom starting price
dta$bottom_price <- dta$starting_price == 3000 
teff$bottom_price <- teff$starting_price == 30 
wheat$bottom_price <- wheat$starting_price == 20


###standardize
dta$starting_price <- (dta$starting_price- mean(dta$starting_price, na.rm=T))/sd(dta$starting_price, na.rm=T)
teff$starting_price <- (teff$starting_price- mean(teff$starting_price, na.rm=T))/sd(teff$starting_price, na.rm=T)
wheat$starting_price <- (wheat$starting_price- mean(wheat$starting_price, na.rm=T))/sd(wheat$starting_price, na.rm=T)

#final price 
dta$final_price <-  (dta$final_price- mean(dta$final_price, na.rm=T))/sd(dta$final_price, na.rm=T)
teff$final_price <-  (teff$final_price- mean(teff$final_price, na.rm=T))/sd(teff$final_price, na.rm=T)
wheat$final_price <-  (wheat$final_price- mean(wheat$final_price, na.rm=T))/sd(wheat$final_price, na.rm=T)

teff$enumerator_gender <- "Male"
wheat$enumerator_gender <- "Male"

dta$gender <- relevel(factor(dta$gender), ref = "Male")
teff$gender <- relevel(factor(teff$gender), ref = "Male")
wheat$gender <- relevel(factor(wheat$gender), ref = "Male")

dta$enumerator_gender <- relevel(factor(dta$enumerator_gender), ref = "Male")
teff$enumerator_gender <- relevel(factor(teff$enumerator_gender), ref = "Male")
wheat$enumerator_gender <- relevel(factor(wheat$enumerator_gender), ref = "Male")

### anaysis
###collect results in matrix
outcomes <- c("initial_accept","starting_price", "bottom_price","sticky","final_price")
results <- array(NA,c(length(outcomes), 4,4))
t <- 1
for (i in outcomes) {
model <- lm(as.formula(paste(i,"gender", sep="~")), data=dta)
results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
results[t,1,3] <- summary(model)$coefficients[2,4]
results[t,1,4] <- nobs(model)

model <- lm(as.formula(paste(i,"gender", sep="~")), data=teff)
results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
results[t,2,3] <- summary(model)$coefficients[2,4]
results[t,2,4] <- nobs(model)

model <- lm(as.formula(paste(i,"gender", sep="~")), data=wheat)
results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
results[t,3,3] <- summary(model)$coefficients[2,4]
results[t,3,4] <- nobs(model)

first <- wheat[c("gender",i)]
first$crop <- "wheat"
second <- teff[c("gender",i)]
second$crop <- "teff"
third <-  dta[c("gender",i)]
third$crop <- "maize"

all <- rbind(first, second, third )


model <- lm(as.formula(paste(i,"gender+crop", sep="~")), data=all)
results[t,4,1:2] <- summary(model)$coefficients[2,1:2]
results[t,4,3] <- summary(model)$coefficients[2,4]
results[t,4,4] <- nobs(model)

t <- t + 1
}  

results_gender<- results


## anaysis for seller gende
###collect results in matrix
outcomes <- c("initial_accept","starting_price", "bottom_price","sticky","final_price")
results <- array(NA,c(length(outcomes), 4,4))
t <- 1
for (i in outcomes) {
  model <- lm(as.formula(paste(i,"enumerator_gender", sep="~")), data=dta)
  results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,1,3] <- summary(model)$coefficients[2,4]
  results[t,1,4] <- nobs(model)
  # 
  # model <- lm(as.formula(paste(i,"gender", sep="~")), data=teff)
  # results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
  # results[t,2,3] <- summary(model)$coefficients[2,4]
  # results[t,2,4] <- nobs(model)
  # 
  # model <- lm(as.formula(paste(i,"gender", sep="~")), data=wheat)
  # results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
  # results[t,3,3] <- summary(model)$coefficients[2,4]
  # results[t,3,4] <- nobs(model)
  
  first <- wheat[c("enumerator_gender",i)]
  first$crop <- "wheat"
  second <- teff[c("enumerator_gender",i)]
  second$crop <- "teff"
  third <-  dta[c("enumerator_gender",i)]
  third$crop <- "maize"
  
  all <- rbind(first, second, third )
  
  
  model <- lm(as.formula(paste(i,"enumerator_gender+crop", sep="~")), data=all)
  results[t,4,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,4,3] <- summary(model)$coefficients[2,4]
  results[t,4,4] <- nobs(model)
  
  t <- t + 1
} 

results_enumerator_gender <- results

### gender matching vs gender mixing

## anaysis
###collect results in matrix
outcomes <- c("initial_accept","starting_price", "bottom_price","sticky","final_price")
results <- array(NA,c(length(outcomes), 4,4))
t <- 1
for (i in outcomes) {
  model <- lm(as.formula(paste(i,"(gender==enumerator_gender)", sep="~")), data=dta)
  results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,1,3] <- summary(model)$coefficients[2,4]
  results[t,1,4] <- nobs(model)
  
  # model <- lm(as.formula(paste(i,"(gender==enumerator_gender)", sep="~")), data=teff)
  # results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
  # results[t,2,3] <- summary(model)$coefficients[2,4]
  # results[t,2,4] <- nobs(model)
  # 
  # model <- lm(as.formula(paste(i,"(gender==enumerator_gender)", sep="~")), data=wheat)
  # results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
  # results[t,3,3] <- summary(model)$coefficients[2,4]
  # results[t,3,4] <- nobs(model)
  
  first <- wheat[c("gender","enumerator_gender",i)]
  first$crop <- "wheat"
  second <- teff[c("gender","enumerator_gender",i)]
  second$crop <- "teff"
  third <-  dta[c("gender","enumerator_gender",i)]
  third$crop <- "maize"
  
  all <- rbind(first, second, third )
  
  
  model <- lm(as.formula(paste(i,"(gender==enumerator_gender)+crop", sep="~")), data=all)
  results[t,4,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,4,3] <- summary(model)$coefficients[2,4]
  results[t,4,4] <- nobs(model)
  
  t <- t + 1
}  


results_homophily_gender <- results

## adding controls

### household size
### gender of household head
### education level of household head
### age of household head
### starting price (categorical 1,2 or3)

dta$offer_price <- "base"
dta$offer_price[dta$anchor=="10000"] <- "cat1"
dta$offer_price[dta$anchor=="11000"] <- "cat2"
dta$offer_price[dta$anchor=="12000"] <- "cat3"

wheat$offer_price <- "base"
wheat$offer_price[wheat$anchor=="60"] <- "cat1"
wheat$offer_price[wheat$anchor=="70"] <- "cat2"
wheat$offer_price[wheat$anchor=="80"] <- "cat3"

teff$offer_price <- "base"
teff$offer_price[teff$anchor=="80"] <- "cat1"
teff$offer_price[teff$anchor=="95"] <- "cat2"
teff$offer_price[teff$anchor=="110"] <- "cat3"


## anaysis
###collect results in matrix
outcomes <- c("initial_accept","starting_price", "bottom_price","sticky","final_price")
results <- array(NA,c(length(outcomes), 4,4))
t <- 1
for (i in outcomes) {
  model <- lm(as.formula(paste(i,"gender+offer_price", sep="~")), data=dta)
  results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,1,3] <- summary(model)$coefficients[2,4]
  results[t,1,4] <- nobs(model)
  
  model <- lm(as.formula(paste(i,"gender+offer_price", sep="~")), data=teff)
  results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,2,3] <- summary(model)$coefficients[2,4]
  results[t,2,4] <- nobs(model)
  
  model <- lm(as.formula(paste(i,"gender+offer_price", sep="~")), data=wheat)
  results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,3,3] <- summary(model)$coefficients[2,4]
  results[t,3,4] <- nobs(model)
  
  first <- wheat[c("gender","offer_price",i)]
  first$crop <- "wheat"
  second <- teff[c("gender","offer_price",i)]
  second$crop <- "teff"
  third <-  dta[c("gender","offer_price",i)]
  third$crop <- "maize"
  
  all <- rbind(first, second, third )
  
  
  model <- lm(as.formula(paste(i,"gender+crop+offer_price", sep="~")), data=all)
  results[t,4,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,4,3] <- summary(model)$coefficients[2,4]
  results[t,4,4] <- nobs(model)
  
  t <- t + 1
}  
