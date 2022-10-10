# Peer-graded-Assignment-Statistical-Inference-Course-Project
Resolution Part 1
We start by running a 1000 simulations of 40 exponentials.

## Required libraries
library(ggplot2)
library(knitr)
## setting seed
set.seed(1)
#No. of values (n) = 40, lambda = 0.2, No.of iterations, at least 1000, numsim=2000,Theoretical mean =1 / lambda or 1 / 0.2 
lambda <- 0.2 
nosim <- 1:1000 # Number of Simulations/rows
n <- 40 
Generating data using rexp
Use the rexp function to develop a dataset with the mean and lambda specified above.

#sd(apply(matrix(rnorm(nosim*n), nosim), 1, mean))
#Create a matrix of simulated values:
e_matrix <- data.frame(x = sapply(nosim, function(x) {mean(rexp(n, lambda))}))
head(e_matrix)
 
 
x
<dbl>
1	4.172102
2	5.066588
3	4.784848
4	5.710865
5	6.740959
6	4.696177
6 rows
1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.

sim_mean <- apply(e_matrix, 2, mean)
sim_mean
       x 
5.047502 
Which is very close to the expected theoretical center of the distribution:

th_mean <- 1/lambda
th_mean
[1] 5
2.Show how variable it is and compare it to the theoretical variance of the distribution. .

sim_SD <- sd((e_matrix$x)) 
sim_SD
[1] 0.7960988
sim_Var <- var(e_matrix$x)
sim_Var
[1] 0.6337733
Let's compare, the expected theretical SD and Variance are:

th_SD <- (1/lambda)/sqrt(n)
th_SD
[1] 0.7905694
th_Var <- th_SD^2
th_Var
[1] 0.625
Comparing Theoretical and actual Values of mean,Standard deviation and variance Table

Variable	Theoretical val	Actual Val
Mean	5	5.048
SD	0.791	0.796
Var	0.625	0.634
We can verify that the differences are minimal, as expected.

3. Show that the distribution is approximately normal.

plot <- ggplot(data = e_matrix, aes(x = x)) + 
    geom_histogram(aes(y=..density..), binwidth = 0.20, fill="slategray3", col="black")
plot <- plot + labs(title="Density of 40 Numbers from Exponential Distribution", x="Mean of 40 Selections", y="Density")
plot <- plot + geom_vline(xintercept=sim_mean,size=1.0, color="black")
plot <- plot + stat_function(fun=dnorm,args=list(mean=sim_mean, sd=sim_SD),color = "dodgerblue4", size = 1.0)
plot <- plot+ geom_vline(xintercept=th_mean,size=1.0,color="indianred4",linetype = "longdash")
plot <- plot + stat_function(fun=dnorm,args=list(mean=th_mean, sd=th_SD),color = "darkmagenta", size = 1.0)
plot


we conclude that the function appears to aproximate to nearly Normal.

Resolution Part 2
1. We load the ToothGrowth data in the R datasets package and perform some basic exploratory data analyses

library(datasets)
data(ToothGrowth)
head(ToothGrowth)
 
 
len
<dbl>
supp
<fctr>
dose
<dbl>
1	4.2	VC	0.5
2	11.5	VC	0.5
3	7.3	VC	0.5
4	5.8	VC	0.5
5	6.4	VC	0.5
6	10.0	VC	0.5
6 rows
EDA
Data: The Effect of Vitamin C on Tooth Growth in Guinea Pigs

Description:

The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC).

Format

A data frame with 60 observations on 3 variables.

[,1]	len	numeric	Tooth length
[,2]	supp	factor	Supplement type (VC or OJ)
[,3]	dose	numeric	Dose in milligrams/day
We can visualize the dataset easily with a boxplot and coplot:

boxplot(len ~ supp * dose, data=ToothGrowth, ylab="Tooth Length", main="Comparing Tooth Growth between different supplements and different dosis", col=c("brown4", "cornflowerblue", "brown4", "cornflowerblue", "brown4", "cornflowerblue"))


require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,xlab = "Comparing Tooth Growth between different supplements and different dosis", col=c("royalblue4"), pch=21)


2. Provide a basic summary of the data.

As we can see in the plots, the average of the tooth length seems to increase with the supplement dosis. this can mean that it might be a relationship between applying a supplement dosis and the tooth growth.

Further data summaries:

# basic info
head(ToothGrowth)
 
 
len
<dbl>
supp
<fctr>
dose
<dbl>
1	4.2	VC	0.5
2	11.5	VC	0.5
3	7.3	VC	0.5
4	5.8	VC	0.5
5	6.4	VC	0.5
6	10.0	VC	0.5
6 rows
nrow(ToothGrowth)
[1] 60
summary(ToothGrowth)
      len        supp         dose      
 Min.   : 4.20   OJ:30   Min.   :0.500  
 1st Qu.:13.07   VC:30   1st Qu.:0.500  
 Median :19.25           Median :1.000  
 Mean   :18.81           Mean   :1.167  
 3rd Qu.:25.27           3rd Qu.:2.000  
 Max.   :33.90           Max.   :2.000  
table(ToothGrowth$supp,ToothGrowth$dose)
    
     0.5  1  2
  OJ  10 10 10
  VC  10 10 10
3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)

Sample size is not too big, we suggest T distribution

t.test(len ~ supp, data = ToothGrowth)

    Welch Two Sample t-test

data:  len by supp
t = 1.9153, df = 55.309, p-value = 0.06063
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.1710156  7.5710156
sample estimates:
mean in group OJ mean in group VC 
        20.66333         16.96333 
Comparing the difference between the two supplements shows no strong evidence to reject the null hypothesis, since the p-value is bigger than the 5% significance level.

However, for this dataset it is important to also compare the differences between the the different dosis level, since bigger dosis may yield contradicting evidence.

t.test(ToothGrowth$len, ToothGrowth$dose)

    Welch Two Sample t-test

data:  ToothGrowth$len and ToothGrowth$dose
t = 17.81, df = 59.798, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 15.66453 19.62881
sample estimates:
mean of x mean of y 
18.813333  1.166667 
Comparing the difference between the two supplements shows enough evidence to reject the null hypothesis, since the p-value aproximates to 0.

4. State your conclusions and the assumptions needed for your conclusions.

As already stated, there is no convincing evidence that there is a difference between the two type of supplements based on the existing datasets and T statistics. meaning we fail to reject the Null hypothesis (H0).

However, there is convincing evidence that there is a difference between the dosis level, and the growth. meaning we reject the Null hypothesis (H0) in favour of the alternative hypothesis (Ha).
