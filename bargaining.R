#
rm(list = ls())
library(andersonTools)
library(ggplot2)
library(ggridges)
library(car)
library(plyr)
library(clubSandwich)
#library(ggpubr)

group.center <- function(var,grp) {
  return(var-tapply(var,grp,mean,na.rm=T)[grp])
}

##
path <- getwd()

#path <- strsplit(path, "/bargaining_for_seed")[[1]]
dta <- read.csv(paste(path,"data/baseline.csv", sep="/"))

###use Anderson sharpened q-values
sharp <- TRUE
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


bids <- cbind(as.numeric(dta$paid.P2_pric_2)- as.numeric(dta$paid.P2_pric), 
as.numeric(dta$paid.P2_pric_3)- as.numeric(dta$paid.P2_pric_2) ,
as.numeric(dta$paid.P2_pric_4)- as.numeric(dta$paid.P2_pric_3) ,
as.numeric(dta$paid.P2_pric_5)- as.numeric(dta$paid.P2_pric_4) ,
as.numeric(dta$paid.P2_pric_6)- as.numeric(dta$paid.P2_pric_5) ,
as.numeric(dta$paid.P2_pric_7)- as.numeric(dta$paid.P2_pric_6) ,
as.numeric(dta$paid.P2_pric_8)- as.numeric(dta$paid.P2_pric_7) ,
as.numeric(dta$paid.P2_pric_9)- as.numeric(dta$paid.P2_pric_8) ,
as.numeric(dta$paid.P2_pric_10)- as.numeric(dta$paid.P2_pric_9) ,
as.numeric(dta$paid.P2_pric_11)- as.numeric(dta$paid.P2_pric_10))

dta$av_bid_step <- rowMeans(bids, na.rm=T)
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

plt1 <- dta[c("starting_price","enumerator_gender")]
names(plt1) <- c("price","gender")

ggplot(plt1, aes(x = price, fill=gender)) +
  geom_density() + scale_fill_grey() + theme_classic() +
  geom_density(alpha=0.4) +
  scale_x_continuous(limits = c(1000, 13500))

plt2 <- dta[c("final_price","enumerator_gender")]
names(plt2) <- c("price","gender")

ggplot(plt1, aes(x = price, fill=gender)) +
  geom_density() + scale_fill_grey() + theme_classic() +
  geom_density(alpha=0.4) +
  scale_x_continuous(limits = c(1000, 13500))

plt1$who <- "starting"
plt2$who <- "final"

plt <- rbind(plt1,plt2)

ggplot(plt, aes(x = price, y = gender)) + geom_density_ridges2(jittered_points = TRUE,
                                                                          position = position_points_jitter(width = 0.5, height = 0),
                                                                          point_shape = '|', point_size = 3, point_alpha = .5, alpha = 0.5) 
p1 <-ggplot(plt1, aes(x=price, fill=gender)) +  geom_density(alpha=0.4) +   scale_x_continuous(limits = c(1000, 13500)) +   scale_y_continuous(limits = c(0, 0.000275))
p2 <-ggplot(plt2, aes(x=price, fill=gender)) +  geom_density(alpha=0.4) +   scale_x_continuous(limits = c(1000, 13500)) +   scale_y_continuous(limits = c(0, 0.000275))
#ggarrange(p2,p1, 
#          labels = c("final price", "first counter bid"),
#          ncol = 1, nrow = 2,heights = c(0.7, 0.7))


### question: is gender_enumerator orthogonal to farmer baseline outcomes
### start by most obvious: gender of the farmers
dta$anchor_high <- dta$anchor %in% c("11000","12000")

model <- lm(gender=="Female"~enumerator_gender=="Female",data=dta)
vcov_cluster <- vcovCR(model, cluster = dta$enumerator_gender, type = "CR0")
res <- coef_test(model, vcov_cluster)

model <- lm(enumerator_gender=="Female"~anchor,data=dta)

# Compute means and standard errors
library(dplyr)
summary_stats <- dta %>%
  group_by(enumerator_gender, anchor_high) %>%
  summarise(
    mean_price = mean(final_price, na.rm = TRUE),
    se = sd(final_price, na.rm = TRUE) / sqrt(sum(!is.na(final_price))),
    .groups = "drop"
  )

# Plot
ggplot(summary_stats, aes(x = interaction(enumerator_gender, anchor_high), y = mean_price)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(aes(ymin = mean_price - se, ymax = mean_price + se), width = 0.2) +
  labs(x = "Enumerator Gender × Anchor", y = "Mean Final Price", title = "Mean Final Price by Seller Gender and Anchor Level") +
  theme_minimal()

## 3 strategies to show that male and female sellers are equally likely to start off with higher vs lower offer prices
#first
table(dta$enumerator_gender, dta$anchor)

prop.table(table(dta$enumerator_gender, dta$anchor), margin = 2)
prop.table(table(dta$enumerator_gender, dta$anchor_high), margin = 2)

#second
chisq.test(table(dta$enumerator_gender, dta$anchor))
chisq.test(table(dta$enumerator_gender, dta$anchor_high))


library(ggplot2)

ggplot(dta, aes(x = factor(anchor), fill = enumerator_gender)) +
  geom_bar(position = "fill") +
  labs(x = "Initial Offer Price", y = "Proportion", fill = "Seller Gender") +
  theme_minimal()


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
dta$enumerator_gender <- relevel(factor(dta$enumerator_gender), ref = "Male")

##gender analysis starts here
dta$initial_accept <- dta$paid.start_neg == "Yes"
dta$bottom_price <- dta$starting_price==3000

dta$age[dta$age==999] <- NA

#level 1 - gender of the buyer (controling for gender of the seller in regressions)
### look at gender balance for randomized offer price
summary(lm(gender=="Female" ~anchor, data=dta))
prop.table(table(dta$gender, dta$anchor), margin=2)
chisq.test(table(dta$gender, dta$anchor))




# analysis for seller gender


# model <- lm(final_price~enumerator_gender, data=dta)
# vcov_cluster <- vcovCR(model, cluster = dta$enumerator_gender, type = "CR0")
# res <- coef_test(model, vcov_cluster)
dta$anchor_high <- dta$anchor %in% c("11000","12000")
dta$buyer_accepts <- dta$accepts=="buyer"
###collect results in matrix
outcomes <- c("initial_accept","starting_price", "bottom_price","buyer_accepts","sticky","rounds","final_price")
results <- array(NA,c(length(outcomes),13,4))
averages <- array(NA,c(length(outcomes),2))

dta$anchor_high_demeaned <- dta$anchor_high - mean(dta$anchor_high)
dta$enumerator_gender_female_demeaned <-  (dta$enumerator_gender == "Female")  - mean(dta$enumerator_gender == "Female")

t <- 1
for (i in outcomes) {
  
  averages[t,1] <- mean(dta[dta$anchor_high==FALSE & dta$enumerator_gender=="Male",i], na.rm=T)
  averages[t,2] <- sd(dta[dta$anchor_high==FALSE & dta$enumerator_gender=="Male",i], na.rm=T)
  
  model <- lm(as.formula(paste(i,"enumerator_gender+enumerator_gender*anchor_high_demeaned+gender+age", sep="~")), data=dta)
  
#  print(summary(model))
  results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,1,3] <- summary(model)$coefficients[2,4]
  results[t,9,1:2] <- summary(model)$coefficients[3,1:2]
  results[t,9,3] <- summary(model)$coefficients[3,4]
  results[t,10,1:2] <- summary(model)$coefficients[6,1:2]
  results[t,10,3] <- summary(model)$coefficients[6,4]
  results[t,11,1:2] <- summary(model)$coefficients[4,1:2] #respondent gender ctrl
  results[t,11,3] <- summary(model)$coefficients[4,4] 
  results[t,12,1:2] <- summary(model)$coefficients[5,1:2] #age ctrl
  results[t,12,3] <- summary(model)$coefficients[5,4]
  results[t,13,1] <- summary(model)$fstatistic["value"]
  results[t,13,2] <- pf(summary(model)$fstatistic["value"], summary(model)$fstatistic["numdf"], summary(model)$fstatistic["dendf"], lower.tail = FALSE)
 

  results[t,1,4] <- nobs(model)
  
  model <- lm(as.formula(paste(i,"anchor_high+anchor_high*dta$enumerator_gender_female_demeaned+gender+age", sep="~")), data=dta)
  
  results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,2,3] <- summary(model)$coefficients[2,4]
  results[t,2,4] <- nobs(model)
  model <- lm(as.formula(paste(i,"enumerator_gender*anchor_high+gender+age", sep="~")), data=dta)
  
  results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,3,3] <- summary(model)$coefficients[2,4]
  results[t,4,1:2] <- summary(model)$coefficients[3,1:2]
  results[t,4,3] <- summary(model)$coefficients[3,4]
  results[t,5,1:2] <- summary(model)$coefficients[6,1:2]
  results[t,5,3] <- summary(model)$coefficients[6,4]
  results[t,6,1:2] <- summary(model)$coefficients[4,1:2] #respondent gender ctrl
  results[t,6,3] <- summary(model)$coefficients[4,4] 
  results[t,7,1:2] <- summary(model)$coefficients[5,1:2] #age ctrl
  results[t,7,3] <- summary(model)$coefficients[5,4]
  results[t,8,1] <- summary(model)$fstatistic["value"]
  results[t,8,2] <- pf(summary(model)$fstatistic["value"], summary(model)$fstatistic["numdf"], summary(model)$fstatistic["dendf"], lower.tail = FALSE)
 

  results[t,3,4] <- nobs(model)

  t <- t + 1
}

if (sharp == TRUE){
	results[1:(length(outcomes)),1,3] <- anderson_sharp_q(results[1:(length(outcomes)),1,3] ) 
	a_sharp <- anderson_sharp_q(c(results[,3,3],results[,4,3],results[,5,3]))

	results[1:(length(outcomes)),3,3] <- a_sharp[1:(length(outcomes))] 
	results[1:(length(outcomes)),4,3] <- a_sharp[(length(outcomes)+1):(2*length(outcomes))]  
	results[1:(length(outcomes)),5,3] <- a_sharp[(2*length(outcomes)+1):(3*length(outcomes))]
}


results_enumerator_gender <- results 
averages_enumerator_gender <- averages



summary(dta$age)
prop.table(table(dta$quality_use))
prop.table(table(dta$bazo_use))


dta$plot_size[dta$plot_size == 999] <- NA
dta$bag_harv[dta$bag_harv==999] <- NA
dta$bag_kg[dta$bag_kg==999] <- NA

dta$yield <- dta$bag_harv*dta$bag_kg/dta$plot_size
dta$yield[dta$yield>20000] <- NA

prop.table(table(dta$knw_bazo=="Yes"))
prop.table(table(dta$tried_bazo=="Yes"))
#balance table
dta$gender <- dta$gender == "Male"
dta$quality_use <- dta$quality_use == "Yes"
dta$bazo_use <- dta$bazo_use == "Yes"
dta$knw_bazo <- dta$knw_bazo == "Yes"
dta$tried_bazo <- dta$tried_bazo == "Yes"
outcomes <- c("gender","age","hh_size","plot_size","yield","quality_use","bazo_use")
results <- array(NA,c(length(outcomes), 6,4))
averages <- array(NA,c(length(outcomes),2))

dta$anchor_high_demeaned <- dta$anchor_high - mean(dta$anchor_high)
dta$enumerator_gender_female_demeaned <-  (dta$enumerator_gender == "Female")  - mean(dta$enumerator_gender == "Female")

t <- 1
for (i in outcomes) {
 
  averages[t,1] <- mean(dta[dta$anchor_high==FALSE & dta$enumerator_gender=="Male",i], na.rm=T)
  averages[t,2] <- sd(dta[dta$anchor_high==FALSE & dta$enumerator_gender=="Male",i], na.rm=T)
  
  model <- lm(as.formula(paste(i,"enumerator_gender+enumerator_gender*anchor_high_demeaned", sep="~")), data=dta)
  
#  print(summary(model))
  results[t,1,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,1,3] <- summary(model)$coefficients[2,4]
  results[t,1,4] <- nobs(model)
  
  model <- lm(as.formula(paste(i,"anchor_high+anchor_high*dta$enumerator_gender_female_demeaned", sep="~")), data=dta)
  
  results[t,2,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,2,3] <- summary(model)$coefficients[2,4]
  results[t,2,4] <- nobs(model)
  ###F test that all treatment cells are jointly zero
  model <- lm(as.formula(paste(i,"enumerator_gender*anchor_high", sep="~")), data=dta)
  
  results[t,3,1:2] <- summary(model)$coefficients[2,1:2]
  results[t,3,3] <- summary(model)$coefficients[2,4]

  results[t,4,3] <- summary(model)$coefficients[3,4]
  results[t,4,1:2] <- summary(model)$coefficients[3,1:2]
  
  results[t,5,1:2] <- summary(model)$coefficients[4,1:2]
  results[t,5,3] <- summary(model)$coefficients[4,4]
  results[t,6,1] <- summary(model)$fstatistic["value"]
  results[t,6,2] <- pf(summary(model)$fstatistic["value"], summary(model)$fstatistic["numdf"], summary(model)$fstatistic["dendf"], lower.tail = FALSE)
  results[t,3,4] <- nobs(model)

  t <- t + 1
} 


#if (sharp == TRUE){
#	a_sharp <- anderson_sharp_q(c(results[,3,3],results[,4,3],results[,5,3]))
#
#	results[1:(length(outcomes)),3,3] <- a_sharp[1:(length(outcomes))] 
#	results[1:(length(outcomes)),4,3] <- a_sharp[(length(outcomes)+1):(2*length(outcomes))]  
#	results[1:(length(outcomes)),5,3] <- a_sharp[(2*length(outcomes)+1):(3*length(outcomes))]
#}



balance_table <- results 
averages_balance <- averages


