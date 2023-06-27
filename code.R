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