# read datasets 
products <- read.csv("groceryProducts.csv")
stores <- read.csv("groceryStores.csv")
transactions <- read.csv("groceryTransactions.csv")


# compile the three given datasets 
newdata <- merge(transactions, products,by = "UPC")
newdata <- merge(newdata, stores, by="STORE_NUM")


# separate into training set and testing set
test <- newdata[newdata$UNITS==-1,]
train <- newdata[newdata$UNITS!=-1,]






#include discount in training data set

discount <- round((train$BASE_PRICE - train$PRICE)/train$BASE_PRICE,3) 
train1 <- cbind(train,discount)




#include season in training data set

SEASON <- rep('constant', nrow(train1))
SEASON[which(train1$MONTH %in% c(11,12,1))] <- "WINTER"
SEASON[which(train1$MONTH %in% c(2,3,4))] <- "SPRING"
SEASON[which(train1$MONTH %in% c(5,6,7))] <- "SUMMER"
SEASON[which(train1$MONTH %in% c(8,9,10))] <- "AUTUMN"
train1 <- cbind(train1,SEASON)







# clustering of city, based on store types and avg weekly baskets

city_clus <- data.frame(stores$CITY,stores$STORE_TYPE,stores$AVG_WEEKLY_BASKETS)

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

NumVars <- (sapply(city_clus, is.numeric) & # Columns containing numeric
              !names(city_clus) %in% c(CITY))
CityMeans <-  aggregate(city_clus[,NumVars], # Means of all numeric covariates
                        by=list(city_clus$CITY), FUN=mean) # by rural / urban group
rownames(CityMeans) <- CityMeans[,1]
CityMeans <- scale(CityMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(CityMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
par(mar=c(3,3,3,1), mgp=c(2,0.75,0)) # Set plot margins
plot(ClusTree, xlab="City", ylab="Separation", cex=0.7)

NewGroups <- cutree(ClusTree, k=4)
print(NewGroups[1:8])

NewGroups[NewGroups==1]
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





# clustering of store number, based on tprweek, displayweek, featureweek, discount, baskets

STORE_NUM <- train1$STORE_NUM
TPRweek <- round(train1$TPR_ONLY * train1$NWEEKS)
DISPLAYweek <- round(train1$DISPLAY * train1$NWEEKS)
FEATUREweek <- round(train1$FEATURE * train1$NWEEKS)

store_num_clus1 <- data.frame(STORE_NUM, TPRweek, DISPLAYweek, FEATUREweek)


NumVars <- (sapply(store_num_clus1, is.numeric) & # Columns containing numeric
              !names(store_num_clus1) %in% c(STORE_NUM))
CityMeans <-  aggregate(store_num_clus1[,NumVars], # Means of all numeric covariates
                        by=list(store_num_clus1$STORE_NUM), FUN=mean) # by rural / urban group
rownames(CityMeans) <- CityMeans[,1]
CityMeans <- scale(CityMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(CityMeans) # Pairwise distances
ClusTree1 <- hclust(Distances, method="complete") # Do the clustering
par(mar=c(3,3,3,1), mgp=c(2,0.75,0)) # Set plot margins
plot(ClusTree1, xlab="Store Number", ylab="Separation", cex=0.7)

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


grp1_1_sum <- c()
for (i in 2:4){
  grp1_1_sum <- c(grp1_1_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_1,i]))
}
grp1_1_sum <- round(grp1_1_sum,3)

grp1_2_sum <- c()
for (i in 2:4){
  grp1_2_sum <- c(grp1_2_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_2,i]))
}
grp1_2_sum <- round(grp1_2_sum,3)

grp1_3_sum <- c()
for (i in 2:4){
  grp1_3_sum <- c(grp1_3_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_3,i]))
}
grp1_3_sum <- round(grp1_3_sum,3)

grp1_4_sum <- c()
for (i in 2:4){
  grp1_4_sum <- c(grp1_4_sum,mean(store_num_clus1[store_num_clus1$STORE_NUM %in% grp1_4,i]))
}
grp1_4_sum <- round(grp1_4_sum,3)







grp_summary1 <- rbind(grp1_1_sum,grp1_2_sum,grp1_3_sum,grp1_4_sum)
colnames(grp_summary1) <- colnames(store_num_clus1)[2:4]
grp_summary1

UNITS <- train1$UNITS
boxplot(UNITS ~ Grp_STORE_NUM1,ylim=c(0,700))
boxplot(UNITS ~ Grp_CITY)


#include clustered store number, TPRweek, DISPLAYweek, FEATUREweek
train1 <- cbind(train1,Grp_STORE_NUM1)
train1 <- cbind(train1,TPRweek,DISPLAYweek,FEATUREweek)







model0 <- glm(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRweek) + factor(DISPLAYweek) + factor(FEATUREweek) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + SEASON, data= train1, family=poisson(link="log"))

summary(model0)

model1 <- glm(UNITS ~ factor(YEAR) + factor(NWEEKS) + factor(TPRweek) + factor(DISPLAYweek) + factor(FEATUREweek) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY + STATE + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + SEASON, data= train1, family=quasipoisson(link="log"))

summary(model1)

model6 <- glm.nb(UNITS ~ factor(NWEEKS) + factor(TPRmonth) + factor(DISPLAYmonth) + factor(FEATUREmonth) + PRICE + BASE_PRICE + MANUFACTURER + SUB_CATEGORY  + STORE_TYPE + AVG_WEEKLY_BASKETS + discount + factor(Grp_CITY) + factor(Grp_STORE_NUM1) + factor(MONTH) + discount*MANUFACTURER, data= train1)








