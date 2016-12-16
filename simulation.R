# ======================= 1 ========================


#cumulative means of exp dist., of size 40
library(ggplot2)

mns <- NULL
vars <- NULL
lambda <- 0.2

## get the several means
for (i in 1:1000){
  mns <- c(mns, mean(rexp(40, lambda)))
  vars <- c(vars, var(rexp(40, lambda)))
}
cummeans <- cumsum(mns)/(1:1000)
cumvar <- cumsum(vars)/(1:1000)

## theoretical values
mean.theor <- 1/lambda
var.theor <- (1/lambda)^2

## the distribution of means converging to a normal
hist(rexp(1000, 0.2)) #distribution of 1000 random exps; not gaussian

hist(mns) # 1000 means distributed
abline(v = mean.theor, col = "red", lwd= 2) #mark theoretical mean
abline(v = mean(mns), col = "blue", lwd= 2) #mark mean of sample mean


plot(1:1000, cummeans, type = "l") #show of convergence
abline(h= mean.theor) #mark theoretical mean

## the distribution of variances converging to normal
hist(vars) # 1000 variances obeyin CLT
abline(v = var.theor, col= "red", lwd= 2) # mark theoretical mean
abline(v= mean(vars), col= "blue", lwd= 2)  # mark mean of sample variances

plot(1:1000, cumvar, type = "l") # show of convergence
abline(h= var.theor, col= "red") #mark theoretical variance

# ======================== 1 ggplot equiv ======================
## the distribution of means converging to a normal
set.seed(100)
#sample normal distribution
p1 <- ggplot(mapping = aes(rnorm(1000, mean = mean.theor)))+
  geom_histogram(binwidth = 0.2, col="red", fill= "blue", alpha=0.2)+
  labs(x = "", title= "Sample Normal Distribution")

#distribution of 1000 random exps; not gaussian
p2 <- ggplot(data.frame(val = rexp(1000, 0.2)), aes(val))+
  geom_histogram(binwidth = 0.8, col="red", fill= "blue", alpha=0.2)+
  labs(x="", title= "Dist. of 1000 random Samples")
  

# distribution of means
mns.data <- data.frame(mns)
p3 <- ggplot(mns.data, aes(mns))+ 
  geom_histogram(binwidth = 0.2, col="red", fill= "blue", alpha=0.2)+
  geom_vline(xintercept = c(mean.theor, mean(mns)), col= c("blue", "red"), 
                            size = 0.5)+
  labs(x="", title= "Dist. of 1000 random Sample means")

# arranging plots
set.seed(100)
lay <- rbind(c(1,2),
             c(3,4))
grid.arrange(p1, p1, p2, p3, layout_matrix= lay,
             top = "checking obedience of the central limit theorem of the mean")

#convergence
ggplot(mapping = aes(1:1000, cummeans))+
  geom_line()+
  geom_hline(yintercept = mean.theor, col= "red")+
  labs(x = "number of outcomes", y = "mean of the outcome",
       title= "convergence of the mean of exponential dist. to the theoretical value")
  

## the distribution of variances converging to normal
#sample normal distribution
p5 <- ggplot(mapping = aes(rnorm(1000, mean = var.theor)))+
  geom_histogram(binwidth = 0.2, col="red", fill= "blue", alpha=0.2)+
  labs(x = "", title= "Sample Normal Distribution")

# distribution of variances
p6 <- ggplot(mapping= aes(vars))+
  geom_histogram(binwidth = 0.2, col="red", fill= "blue", alpha=0.2)+
  geom_vline(xintercept = c(var.theor, mean(vars)), col= c("blue", "red"), 
             size = 0.5)+
  lab(title= "Dist. of 1000 random Sample means", x= "")

set.seed(100)
lay2 <- rbind(c(1, 2),
              c(3, 4))
grid.arrange(p5,p5, p2, p6, layout_matrix= lay2,
             top= "checking obedience of the central limit theorem")

#convergence
p7 <- ggplot(mapping = aes(1:1000, cumvar))+
  geom_line()+
  geom_hline(yintercept = var.theor, col= "red")+
  labs(x = "number of outcomes", y = "variance of the outcomes",
       title= "convergence of the variance of exponential dist. to the theoretical value")


# END SECTION



# ======================= 2 ===========================
## assuming paired observation
data("ToothGrowth")
n <- 30

#divison along supp
g1 <- ToothGrowth$len[1:30]
g2 <- ToothGrowth$len[31:60]

## t-tests

# assuming paired obs.
mn + c(-1, 1)*qt(0.975, n-1)*s/sqrt(n)
mn <- mean(g2-g1); s <- sd(g2-g1)
### note the interval does not contain zero, 
### therefore the difference not zero, with 95% confidence

# Assuming unpaired obs.
t.test(x= g2, y = g1, paired = FALSE)
### we see that the interval contains zero, 
### so we can't rule out zero assuming unpaired observation



## some plots by category (useless)
ggplot(ToothGrowth)+
  geom_histogram(mapping = aes(len, fill= factor(dose)), binwidth = 0.2)+
  facet_grid(supp~.)
## not paired

## implement the permutation test
cal.diff <- function(value, group){
  ## function to get the mean different for a list
  ## differentiated by groups (another column)
  divs <- levels(group)
  meandiff <- mean(value[group == divs[1] ]) - mean(value[group == divs[2]])
  meandiff
}


observation <- cal.diff(ToothGrowth$len, ToothGrowth$supp)
permutations <- NULL
for (i in 1:1000){
  permutations <- c(permutations, cal.diff(ToothGrowth$len, 
                                           sample(ToothGrowth$supp)))
}
observation


## check fraction of permutation greater than the observation
mean(permutations > observation)

## the 95 percentile of the permutation
quantile(permutations, 0.95)


## Assumming paired observation
len <- nrow(ToothGrowth)
g1 <- ToothGrowth$len[1:(len/2)]
g2 <- ToothGrowth$len[(len/2+1):len]
g1_map <- data.frame(x= rep(1, len/2), y= g1, dose = ToothGrowth$dose[1:(len/2)])
g2_map <- data.frame(x= rep(2, len/2), y= g2, dose = ToothGrowth$dose[(len/2+1):len])

ggplot(g1_map, mapping= aes(x, y, col= factor(dose)))+
  geom_point()+
  geom_point(data = g2_map, aes(x,y, col=factor(dose)))+
  labs(x="", y="", title= "showing the changes in the tooth growth for the 
       two supplement (assuming the obbservations are paired)")+
  geom_segment(aes(x = g1_map$x, y = g1_map$y, 
                   xend = g2_map$x, yend = g2_map$y, colour = "red"))

## showing the distributions for the two supplements
ggplot(data = ToothGrowth)+
  geom_boxplot(mapping = aes(x=supp, y= len) ) +
  labs(title= "comparing the distributions for the two supplements disregarding the dose")

## checking the relationship between len and dose
g1_VC <- subset(ToothGrowth, supp = "VC")
g2_OJ <- subset(ToothGrowth, supp = "OJ")

# var explained by dose, grid by supp
ggplot(ToothGrowth)+
  geom_point(mapping = aes(x=dose, y=len, col= factor(dose)))+
  facet_grid(supp~.)+
  labs(title= "variatons explained by the dosage for each of the supplement")

#exploring variations due to all factors
ggplot(ToothGrowth)+
  geom_point(mapping = aes(x=factor(dose), y=len, col= supp))+
  labs(title= "variatons explained by the dose and supplement")

#
ggplot(ToothGrowth)+
  geom_point(mapping = aes(x=supp, y=len, col= factor(dose)))+
  labs(title= "variatons explained by the supp and sode")


  
