library(tidyverse)
library(dslabs)
data(murders)
mydata <- read.csv(file.choose())
install.packages("ggplot2")
mydata %>% ggplot(aes(carat,price, colour = clarity)) +
  geom_point(alpha = 0.1) + 
  geom_smooth() +
  scale_x_continuous(trans="log")
  
#Confirming the Law of Large Numbers using R to show that Mean(-1<=X<=1) tends to 68.2% 

#my own Solution (attempt1)
N <- seq(50,20000, length.out=50)
sim <- function (x) {
  simulated <- rnorm(x)
  e <- simulated >=-1 & simulated <= 1
  mean(e)
} 
res <- sapply(N,sim)
resd <- data.frame(N,res)
plot(N,res)
resd %>% ggplot(aes(N,res)) + 
  geom_smooth() + 
  scale_x_continuous(trans = "log10")

#based on the hint of the course
N <- 10000000
counter <- 0
for (i in rnorm(N)) {
  if (i <=1 & i>=-1) {
    counter <- counter +1 
  }
}
answer <- counter / N
answer

#Financial statement analysis
#data set available at superdatascience.com/rcourse

#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

mprop <- revenue - expenses
profit_at<- mprop-mprop*0.3
margin <- profit_at/revenue*100
index <- profit_at - mean(profit_at)
good_month <- which(index >=0)
bad_month <- which(index <0)
best_month <- which.max(profit_at)
worst_month <- which.min(profit_at)

#Results
round(mprop)
round(profit_at)
round(margin)
good_month
bad_month
best_month
worst_month

#Section 4 - Homework - Baseketball 
#Seasons
Seasons <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

#Players
Players <- c("KobeBryant","JoeJohnson","LeBronJames","CarmeloAnthony","DwightHoward","ChrisBosh","ChrisPaul","KevinDurant","DerrickRose","DwayneWade")

#Free Throws
KobeBryant_FT <- c(696,667,623,483,439,483,381,525,18,196)
JoeJohnson_FT <- c(261,235,316,299,220,195,158,132,159,141)
LeBronJames_FT <- c(601,489,549,594,593,503,387,403,439,375)
CarmeloAnthony_FT <- c(573,459,464,371,508,507,295,425,459,189)
DwightHoward_FT <- c(356,390,529,504,483,546,281,355,349,143)
ChrisBosh_FT <- c(474,463,472,504,470,384,229,241,223,179)
ChrisPaul_FT <- c(394,292,332,455,161,337,260,286,295,289)
KevinDurant_FT <- c(209,209,391,452,756,594,431,679,703,146)
DerrickRose_FT <- c(146,146,146,197,259,476,194,0,27,152)
DwayneWade_FT <- c(629,432,354,590,534,494,235,308,189,284)
#Matrix
FT <- rbind(KobeBryant_FT, JoeJohnson_FT, LeBronJames_FT, CarmeloAnthony_FT, 
                    DwightHoward_FT,ChrisPaul_FT,ChrisBosh_FT,KevinDurant_FT,
                    DerrickRose_FT,DwayneWade_FT)
rownames(FT) <- Players
colnames(FT) <- Seasons
FT <- as.data.frame(FT)
FT <- rownames_to_column(FT, var="Player")
FT_long <- FT %>% gather(Year, FT, -Player)
FT_long$Year <- as.numeric(FT_long$Year)
FT_long %>% ggplot(aes(Year, FT, color = Player)) + geom_line()

#Free Throw Attempts
KobeBryant_FTA <- c(819,768,742,564,541,583,451,626,21,241)
JoeJohnson_FTA <- c(330,314,379,362,269,243,186,161,195,176)
LeBronJames_FTA <- c(814,701,771,762,773,663,502,535,585,528)
CarmeloAnthony_FTA <- c(709,568,590,468,612,605,367,512,541,237)
DwightHoward_FTA <- c(598,666,897,849,816,916,572,721,638,271)
ChrisBosh_FTA <- c(581,590,559,617,590,471,279,302,272,232)
ChrisPaul_FTA <- c(465,357,390,524,190,384,302,323,345,321)
KevinDurant_FTA <- c(256,256,448,524,840,675,501,750,805,171)
DerrickRose_FTA <- c(205,205,205,250,338,555,239,0,32,187)
DwayneWade_FTA <- c(803,535,467,771,702,652,297,425,258,370)

library(tibble)
FTA <- rbind(KobeBryant_FTA, JoeJohnson_FTA, LeBronJames_FTA, CarmeloAnthony_FTA, 
             DwightHoward_FTA,ChrisPaul_FTA,ChrisBosh_FTA,KevinDurant_FTA,
             DerrickRose_FTA,DwayneWade_FTA)
rownames(FTA) <- Players
colnames(FTA) <- Seasons
FTA <- as.data.frame(FTA) #converting matrix into data.frame
FTA <- rownames_to_column(FTA, var = "Player") #convert into a tibble 
FTA_long <- FTA %>% gather (Year, FTA, -Player) #convert into a long format 
FTA_long$Year <- as.factor(FTA_long$Year) #if year is "char", then ggplot2 does not know how to order the levels to connect them with lines
FTA_long %>% ggplot(aes(Year, FTA, color = Player)) + geom_line()
FTA_long_1 <- FTA_long %>% .$FTA #remember that the select function returns a data frame instead of a vector

#Accuracy of Free Throws
df <- FT_long %>% mutate(FTA_c = FTA_long_1, hits = FT/FTA_c) # Merge the data frames by the common column "Player"
head(df)
str(df)
df %>% ggplot(aes(Year,hits, color = Player)) + 
  geom_line()
#Points
KobeBryant_PTS <- c(2832,2430,2323,2201,1970,2078,1616,2133,83,782)
JoeJohnson_PTS <- c(1653,1426,1779,1688,1619,1312,1129,1170,1245,1154)
LeBronJames_PTS <- c(2478,2132,2250,2304,2258,2111,1683,2036,2089,1743)
CarmeloAnthony_PTS <- c(2122,1881,1978,1504,1943,1970,1245,1920,2112,966)
DwightHoward_PTS <- c(1292,1443,1695,1624,1503,1784,1113,1296,1297,646)
ChrisBosh_PTS <- c(1572,1561,1496,1746,1678,1438,1025,1232,1281,928)
ChrisPaul_PTS <- c(1258,1104,1684,1781,841,1268,1189,1186,1185,1564)
KevinDurant_PTS <- c(903,903,1624,1871,2472,2161,1850,2280,2593,686)
DerrickRose_PTS <- c(597,597,597,1361,1619,2026,852,0,159,904)
DwayneWade_PTS <- c(2040,1397,1254,2386,2045,1941,1082,1463,1028,1331)
#Field Goal Attempts
KobeBryant_FGA <- c(2173,1757,1690,1712,1569,1639,1336,1595,73,713)
JoeJohnson_FGA <- c(1395,1139,1497,1420,1386,1161,931,1052,1018,1025)
LeBronJames_FGA <- c(1823,1621,1642,1613,1528,1485,1169,1354,1353,1279)
CarmeloAnthony_FGA <- c(1572,1453,1481,1207,1502,1503,1025,1489,1643,806)
DwightHoward_FGA <- c(881,873,974,979,834,1044,726,813,800,423)
ChrisBosh_FGA <- c(1087,1094,1027,1263,1158,1056,807,907,953,745)
ChrisPaul_FGA <- c(947,871,1291,1255,637,928,890,856,870,1170)
KevinDurant_FGA <- c(647,647,1366,1390,1668,1538,1297,1433,1688,467)
DerrickRose_FGA <- c(436,436,436,1208,1373,1597,695,0,164,835)
DwayneWade_FGA <- c(1413,962,937,1739,1511,1384,837,1093,761,1084)
#Matrix
FieldGoalAttempts <- rbind(KobeBryant_FGA, JoeJohnson_FGA, LeBronJames_FGA, CarmeloAnthony_FGA, DwightHoward_FGA, ChrisBosh_FGA, ChrisPaul_FGA, KevinDurant_FGA, DerrickRose_FGA, DwayneWade_FGA)
rm(KobeBryant_FGA, JoeJohnson_FGA, LeBronJames_FGA, CarmeloAnthony_FGA, DwightHoward_FGA, ChrisBosh_FGA, ChrisPaul_FGA, KevinDurant_FGA, DerrickRose_FGA, DwayneWade_FGA)
colnames(FieldGoalAttempts) <- Seasons
rownames(FieldGoalAttempts) <- Players

#Field Goals
KobeBryant_FG <- c(978,813,775,800,716,740,574,738,31,266)
JoeJohnson_FG <- c(632,536,647,620,635,514,423,445,462,446)
LeBronJames_FG <- c(875,772,794,789,768,758,621,765,767,624)
CarmeloAnthony_FG <- c(756,691,728,535,688,684,441,669,743,358)
DwightHoward_FG <- c(468,526,583,560,510,619,416,470,473,251)
ChrisBosh_FG <- c(549,543,507,615,600,524,393,485,492,343)
ChrisPaul_FG <- c(407,381,630,631,314,430,425,412,406,568)
KevinDurant_FG <- c(306,306,587,661,794,711,643,731,849,238)
DerrickRose_FG <- c(208,208,208,574,672,711,302,0,58,338)
DwayneWade_FG <- c(699,472,439,854,719,692,416,569,415,509)
#Matrix
FieldGoals <- rbind(KobeBryant_FG, JoeJohnson_FG, LeBronJames_FG, CarmeloAnthony_FG, DwightHoward_FG, ChrisBosh_FG, ChrisPaul_FG, KevinDurant_FG, DerrickRose_FG, DwayneWade_FG)
rm(KobeBryant_FG, JoeJohnson_FG, LeBronJames_FG, CarmeloAnthony_FG, DwightHoward_FG, ChrisBosh_FG, ChrisPaul_FG, KevinDurant_FG, DerrickRose_FG, DwayneWade_FG)
colnames(FieldGoals) <- Seasons
rownames(FieldGoals) <- Players
#Convert to data frame
FieldGoals <- as.data.frame(FieldGoals)
FieldGoals <- rownames_to_column(FieldGoals, var = "Player")
FGA <- gather(FieldGoals, Year, Goals, -Player)
ind1 <- FGA$Goals

#Plot for playing style (2 vs 3 points)
Points <- rbind(KobeBryant_PTS, JoeJohnson_PTS, LeBronJames_PTS, CarmeloAnthony_PTS, DwightHoward_PTS, ChrisBosh_PTS, ChrisPaul_PTS, KevinDurant_PTS, DerrickRose_PTS, DwayneWade_PTS)
rm(KobeBryant_PTS, JoeJohnson_PTS, LeBronJames_PTS, CarmeloAnthony_PTS, DwightHoward_PTS, ChrisBosh_PTS, ChrisPaul_PTS, KevinDurant_PTS, DerrickRose_PTS, DwayneWade_PTS)
colnames(Points) <- Seasons
rownames(Points) <- Players
Points <- as.data.frame(Points)
Points <- rownames_to_column(Points, var = "Player")
Points_long <- gather(Points, Year, Pts, -Player)
ind <- Points_long$Pts
df <- df %>% mutate(Pts = ind, Goals = ind1, Excl = Pts - FT, Style = Excl/Goals) 
df %>% ggplot(aes(Year, Style, color = Player)) + 
  geom_line()
str(df)
head(df)

#can i go directly from the vectors to a data.frame?
# Try on FieldGoals
df2 <- data.frame(Players, Seasons, KobeBryant_FG, JoeJohnson_FG, LeBronJames_FG, CarmeloAnthony_FG, DwightHoward_FG, ChrisBosh_FG, ChrisPaul_FG, KevinDurant_FG, DerrickRose_FG, DwayneWade_FG)
#the above line does not seem to be what I want. Are there any other way that I do not have to go through creating a matrix?

