sewtd('/Users/mamane/Desktop/untitled folder/Project.2 STAT0023')

# read datasets 
products <- read.csv("groceryProducts.csv")
stores <- read.csv("groceryStores.csv")
transactions <- read.csv("groceryTransactions.csv")


# compile the three given datasets 
newdata <- merge(transactions, products,by = "UPC")
newdata <- merge(newdata, stores, by="STORE_NUM")


# separate into training set and testing set
test <- newdata[newdata$UNITS==-1,]
train1 <- newdata[newdata$UNITS!=-1,]


# inclusion of new covariates 
FEATUREmonth <- round(train1$FEATURE *train1$NWEEKS,1)
DISPLAYmonth <- round(train1$DISPLAY *train1$NWEEKS,1)
TPRmonth <- round(train1$TPR_ONLY *train1$NWEEKS,1)
discount <- (train1$BASE_PRICE-train1$PRICE)/train1$BASE_PRICE
train1 <- cbind(train1, discount, TPRmonth, DISPLAYmonth, FEATUREmonth)



# Plots for EDA, and autosave 

pdf("Figure 1.pdf")
boxplot(train1$UNITS ~ factor(train1$MANUFACTURER), xlab = "Product Manufacturer", ylab = "Units Sold", cex.axis=0.75,main = "Box Plot of Units Sold For Each Manufacturer",sub="Figure 1: Boxplot showing the number of units of cereal sold for each manufacturer.") 
dev.off()

pdf("Figure 2.pdf")
boxplot(train1$UNITS ~ factor(train1$SUB_CATEGORY), xlab = "Sub Category Of Product", ylab = "Units Sold", main = "Box Plot of Units Sold For Each Product Subcategory",sub="Figure 2: Boxplot showing the # of units of cereal sold for each sub category.")
dev.off()
  
pdf("Figure 3.pdf")
boxplot(train1$UNITS ~ train1$NWEEKS, xlab = "Number of Weeks Available", ylab="Units Sold", ylim=range(train1$UNITS), main ="Box Plot of Units Sold Based on Number of Weeks Available",sub="Figure 3: Boxplot showing # of units of cereal sold for # of available weeks in a month.")
dev.off()

pdf("Figure 4-6.pdf")  
par(mfrow=c(3,1))
boxplot(train1$UNITS ~ train1$FEATUREmonth, xlab = "Number of Weeks on Feature", ylab = "Units Sold", main = "Box Plot of Units Sold for each number of weeks on Feature") 
boxplot(train1$UNITS ~ train1$DISPLAYmonth, xlab = "Number of Weeks on Display", ylab = "Units Sold", main = "Box Plot of Units Sold for each number of weeks on Display") 
boxplot(train1$UNITS ~ train1$TPRmonth, xlab = "Number of Weeks on TPR only", ylab = "Units Sold", main = "Box Plot of Units Sold for each number of weeks on TPR only",sub="Figures 4-6: Boxplots showing the number of units of cereal sold based on the number of weeks using each promotional method. ") 
dev.off()

pdf("Figure 7-9.pdf") 
par(mfrow = c(1,3))
plot(train1$PRICE, train1$UNITS, xlab = "Selling Price", ylab = "Units Sold", main = "Selling Price Against Units",sub="Figure 7")
plot(train1$BASE_PRICE, train1$UNITS, xlab = "Base Price", ylab = "Units Sold", main = "Base Price Against Units",sub="Figure 8")
plot(train1$discount, train1$UNITS, xlab = "Discount of Price", ylab = "Units Sold", main = "Price Discount Against Units",sub="Figure 9")
dev.off()

pdf("Figure 10.pdf")
boxplot(train1$UNITS ~ train1$STATE, xlab = "US State", ylab = "Units Sold", main = "Box Plot of Units Sold For Each State",sub="Figure 10: Boxplot showing number of units sold for each state") 
dev.off()

pdf("Figure 11.pdf")
boxplot(train1$UNITS ~ train1$AREA_CODE, xlab = "Area Code", ylab = "Units Sold", main = "Box Plot of Units Sold For Each Area Code", sub="Figure 11: Boxplot showing number of units sold for each area") 
dev.off()

pdf("Figure 12.pdf")
boxplot(train1$UNITS ~ train1$STORE_TYPE, xlab = "Store Type", ylab = "Units Sold", main = "Box Plot of Units Sold For Each Store Type",sub="Figure 12: Boxplot showing the number of units of cereal sold for each type of store.") 
dev.off()

pdf("Figure 13.pdf")
plot(train1$AVG_WEEKLY_BASKETS, train1$UNITS, xlab = "Average Weekly Baskets", ylab = "Units Sold", main = "Plot of Weekly Baskets Against Units Sold",sub="Figure 13: Plot showing relationship between # of units sold and avg. weekly baskets.")
dev.off()

pdf("Figure 14-16.pdf")
par(mfrow=c(3,1))
boxplot(train1$UNITS[train1$YEAR =="2009"] ~ train1$MONTH[train1$YEAR =="2009"], 
        main="Units Sold In 2009 across different Months", xlab = "Months", ylab="Units Sold", ylim=range(train1$UNITS))

boxplot(train1$UNITS[train1$YEAR =="2010"] ~ train1$MONTH[train1$YEAR =="2010"], 
        main="Units Sold In 2010 across different Months", xlab = "Months", ylab="Units Sold", ylim=range(train1$UNITS))

boxplot(train1$UNITS[train1$YEAR =="2011"] ~ train1$MONTH[train1$YEAR =="2011"], 
        main="Units Sold In 2011 across different Months", xlab = "Months", ylab="Units Sold", ylim=range(train1$UNITS),sub="Figure 14-16: Boxplots showing the number of units of cereal sold in each year across the months.")
dev.off()





# Codes for Model Building


# clustering of city, based on store types and avg weekly baskets

city_clus <- data.frame(stores$CITY,stores$STORE_TYPE,stores$AVG_WEEKLY_BASKETS)

# count the number of each store type in each city, and sum up the baskets
MAINSTREAM <- c()
UPSCALE <- c()
VALUE <- c()
BASKETS <- c()

for (i in unique(stores$CITY)){
  MAINSTREAM <- c(MAINSTREAM,sum(city_clus[city_clus$stores.CITY==i,2]=="MAINSTREAM"))
  UPSCALE <- c(UPSCALE,sum(city_clus[city_clus$stores.CITY==i,2]=="UPSCALE"))
  VALUE <- c(VALUE,sum(city_clus[city_clus$stores.CITY==i,2]=="VALUE"))
  BASKETS <- c(BASKETS, sum(city_clus[city_clus$stores.CITY==i,3]))
}

CITY <- unique(stores$CITY)
city_clus <- data.frame(CITY ,MAINSTREAM, UPSCALE, VALUE,BASKETS) 

# start hierachical clustering, codes heavily adapted from the ones provided in the notes
NumVars <- (sapply(city_clus, is.numeric) & # Columns containing numeric
              !names(city_clus) %in% c(CITY))
CityMeans <-  aggregate(city_clus[,NumVars], # Means of all numeric covariates
                        by=list(city_clus$CITY), FUN=mean) 
rownames(CityMeans) <- CityMeans[,1]
CityMeans <- scale(CityMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(CityMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering

# assign groupings
NewGroups <- cutree(ClusTree, k=4)

grp1 <- names(NewGroups[NewGroups==1])
grp2 <- names(NewGroups[NewGroups==2])
grp3 <- names(NewGroups[NewGroups==3])
grp4 <- names(NewGroups[NewGroups==4])

Grp_CITY <- rep('constant', nrow(train1))
Grp_CITY[which(train1$CITY %in% grp1)] <- "1"
Grp_CITY[which(train1$CITY %in% grp2)] <- "2"
Grp_CITY[which(train1$CITY %in% grp3)] <- "3"
Grp_CITY[which(train1$CITY %in% grp4)] <- "4"

train1 <- cbind(train1,Grp_CITY) 

# summary for the groupings, compute means of each variables in each group
grp1_sum <- c()
for (i in 2:5){
  grp1_sum <- c(grp1_sum,mean(city_clus[city_clus$CITY %in% grp1,i]))
}

grp2_sum <- c()
for (i in 2:5){
  grp2_sum <- c(grp2_sum,mean(city_clus[city_clus$CITY %in% grp2,i]))
}

grp3_sum <- c()
for (i in 2:5){
  grp3_sum <- c(grp3_sum,mean(city_clus[city_clus$CITY %in% grp3,i]))
}

grp4_sum <- c()
for (i in 2:5){
  grp4_sum <- c(grp4_sum,mean(city_clus[city_clus$CITY %in% grp4,i]))
}

# change absolute values into proportions 
grp1_sum <- c(grp1_sum[1:3]/sum(grp1_sum[1:3]),grp1_sum[4])
grp2_sum <- c(grp2_sum[1:3]/sum(grp2_sum[1:3]),grp2_sum[4])
grp3_sum <- c(grp3_sum[1:3]/sum(grp3_sum[1:3]),grp3_sum[4])
grp4_sum <- c(grp4_sum[1:3]/sum(grp4_sum[1:3]),grp4_sum[4])

grp_summary <- round(rbind(grp1_sum,grp2_sum,grp3_sum,grp4_sum),2)
colnames(grp_summary) <- colnames(city_clus)[2:5]
rownames(grp_summary) <- c("Group 1", "Group 2", "Group 3", "Group 4")

grp_summary # table for summary of CITY groups




# clustering of store number, based on TPRmonth, DISPLAYmonth, FEATUREmonth

# convert TPR, DISPLAY, and FEATURE from by week to by month. 
STORE_NUM <- train1$STORE_NUM
TPRmonth <- round(train1$TPR_ONLY * train1$NWEEKS)
DISPLAYmonth <- round(train1$DISPLAY * train1$NWEEKS)
FEATUREmonth <- round(train1$FEATURE * train1$NWEEKS)

store_num_clus1 <- data.frame(STORE_NUM, TPRmonth, DISPLAYmonth, FEATUREmonth)

# start hierachical clustering, codes heavily adapted from the ones provided in the notes
NumVars <- (sapply(store_num_clus1, is.numeric) & # Columns containing numeric
              !names(store_num_clus1) %in% c(STORE_NUM))
CityMeans <-  aggregate(store_num_clus1[,NumVars], # Means of all numeric covariates
                        by=list(store_num_clus1$STORE_NUM), FUN=mean) 
rownames(CityMeans) <- CityMeans[,1]
CityMeans <- scale(CityMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(CityMeans) # Pairwise distances
ClusTree1 <- hclust(Distances, method="complete") # Do the clustering

#assign groupings
NewGroups1 <- cutree(ClusTree1, k=4)

grp1_1 <- names(NewGroups1[NewGroups1==1])
grp1_2 <- names(NewGroups1[NewGroups1==2])
grp1_3 <- names(NewGroups1[NewGroups1==3])
grp1_4 <- names(NewGroups1[NewGroups1==4])

Grp_STORE_NUM1 <- rep('constant', nrow(train1))
Grp_STORE_NUM1[which(train1$STORE_NUM %in% grp1_1)] <- "1"
Grp_STORE_NUM1[which(train1$STORE_NUM %in% grp1_2)] <- "2"
Grp_STORE_NUM1[which(train1$STORE_NUM %in% grp1_3)] <- "3"
Grp_STORE_NUM1[which(train1$STORE_NUM %in% grp1_4)] <- "4"

#include clustered store number, TPRmonth, DISPLAYmonth, FEATUREmonth
train1 <- cbind(train1,Grp_STORE_NUM1)
train1 <- cbind(train1,TPRmonth,DISPLAYmonth,FEATUREmonth)

#summary of groupings, compute mean of each variables in each group

grp1_1_sum <- c()
for (i in 2:4){
  grp1_1_sum <- c(grp1_1_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_1,i]))
}

grp1_2_sum <- c()
for (i in 2:4){
  grp1_2_sum <- c(grp1_2_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_2,i]))
}

grp1_3_sum <- c()
for (i in 2:4){
  grp1_3_sum <- c(grp1_3_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_3,i]))
}

grp1_4_sum <- c()
for (i in 2:4){
  grp1_4_sum <- c(grp1_4_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_4,i]))
}

grp_summary1 <- rbind(round(grp1_1_sum,3),round(grp1_2_sum,3),round(grp1_3_sum,3),round(grp1_4_sum,3))
colnames(grp_summary1) <- colnames(store_num_clus1)[2:4]
rownames(grp_summary1) <- c("Group 1", "Group 2", "Group 3", "Group 4")
grp_summary # table for summary of CITY groups


# start with building of models 

# linear model 
linear_model <- lm(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1)

# diagnostic plots of linear model
pdf("Figure 17.pdf")
par(mfrow=c(2,2))
plot(linear_model)
dev.off()


# Poisson GLM model 
model0 <- glm(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1, family=poisson)

sum( resid(model0,type="pearson")^2 ) / model0$df.residual


# QuasiPoisson GLM model
model1 <- glm(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1, family=quasipoisson)

summary(model1)

# negative binomial GLM
library(MASS)
model2 <- glm.nb(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1)

summary(model2)

# comparison of QQ plots of Poisson GLM and Negative Binomial GLM
pdf("Figure 18-19.pdf")
par(mfrow=c(1,2))
plot(model0,which=c(2)) 
plot(model2,which=c(2))
dev.off()



# removal of covariates based on ANOVA test

# remove YEAR
model3 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1)

anova(model2,model3) #big p, so remove 


# remove Grp Store Num

model4 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY)  + factor(MONTH), data= train1)

anova(model4,model3) #small p, no remove


# remove store type
model5 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE  + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY)  + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1)

anova(model3,model5) #small p, no remove


# remove STATE
model6 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY  + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH), data= train1)

anova(model3,model6) #small p, so remove 


# consider interaction
model7 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY  + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH) + discount*MANUFACTURER, data= train1)

summary(model7)

sum(rstandard(model7)<1.96 & rstandard(model7)>-1.96)/length(rstandard(model7)) # percentage of residuals within +- 1.96

# diagnostic plots of final model
pdf("Figure 20.pdf")
par(mfrow=c(2,2))
plot(model7,which=1)
plot(model7,which=2)
plot(model7,which=3)
plot(cooks.distance(model7),ylab="Cooks Distance",main="Cooks Distance")
dev.off()

# finalise model
model <- model7






# prediction and output


# include added covariates to testing set
TPRmonth <- round(test$TPR_ONLY * test$NWEEKS)
DISPLAYmonth <- round(test$DISPLAY * test$NWEEKS)
FEATUREmonth <- round(test$FEATURE * test$NWEEKS)
discount <- round((test$BASE_PRICE - test$PRICE)/test$BASE_PRICE,3) 

Grp_CITY <- rep('constant', nrow(test))
Grp_CITY[which(test$CITY %in% grp1)] <- "1"
Grp_CITY[which(test$CITY %in% grp2)] <- "2"
Grp_CITY[which(test$CITY %in% grp3)] <- "3"
Grp_CITY[which(test$CITY %in% grp4)] <- "4"

Grp_STORE_NUM1 <- rep('constant', nrow(test))
Grp_STORE_NUM1[which(test$STORE_NUM %in% grp1_1)] <- "1"
Grp_STORE_NUM1[which(test$STORE_NUM %in% grp1_2)] <- "2"
Grp_STORE_NUM1[which(test$STORE_NUM %in% grp1_3)] <- "3"
Grp_STORE_NUM1[which(test$STORE_NUM %in% grp1_4)] <- "4"

test <- cbind(test, TPRmonth, DISPLAYmonth, FEATUREmonth, discount, Grp_CITY, Grp_STORE_NUM1)


# make predictions and output it
predict <- predict(model, test, type="response", se.fit = T)
predict_sd <- sqrt((predict$fit + predict$fit^2 * (1/model$theta)) + predict$se.fit^2)  # compute standard deviation using formula provided in notes
prediction <- matrix(c(test$ID,predict$fit, predict_sd),ncol=3) 
write(t(prediction),file="pred.dat",ncolumns=3,sep = "\t") # generate prediction output


