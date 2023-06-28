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

#financial statement analysis
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
