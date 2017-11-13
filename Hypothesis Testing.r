#Lower Tail Test of Population Proportion
#The null hypothesis of the lower tail test about population proportion can be expressed as follows:
#p ≥ p0
#where p0 is a hypothesized lower bound of the true population proportion p.
#The null hypothesis is that p ≥ 0.6. We begin with computing the test statistic.

pbar = 85/148          # sample proportion 
p0 = .6                # hypothesized value 
n = 148                # sample size 
z = (pbar - p0)/sqrt(p0* (1 - p0)/n) 
z                      # test statistic 
alpha = .05
z.alpha = qnorm(1 - alpha)
- z.alpha               # critical value
#Answer: The test statistic -0.6376 is not less than the critical value of -1.6449. Hence, at .05 significance level, we do not reject the null hypothesis that the proportion of voters in the population is above 60% this year.


#Alternative solution1
pval = pnorm(z) 
pval                   # lower tail p−value


#Alternative Soltution2
prop.test(85, 148, p=.6, alt="less", correct=FALSE)

#Upper Tail Test of Population Mean with Known Variance
#The null hypothesis of the upper tail test of the population mean can be expressed as follows:
#μ ≤ μ0
#where μ0 is a hypothesized upper bound of the true population mean μ.
#The null hypothesis is that μ ≤ 2. We begin with computing the test statistic.
xbar = 2.1             # sample mean 
mu0 = 2                # hypothesized value 
sigma = 0.25           # population standard deviation 
n = 35                 # sample size 
z = (xbar - mu0)/(sigma/sqrt(n)) 
z                      # test statistic 
#We then compute the critical value at .05 significance level.
alpha = .05 
z.alpha = qnorm(1 - alpha) 
z.alpha                # critical value 

#The test statistic 2.3664 is greater than the critical value of 1.6449. Hence, at .05 significance level, we reject the claim that there is at most 2 grams of saturated fat in a cookie.

#Alternative Solution
pval = pnorm(z, lower.tail=FALSE) 
pval                   # upper tail p−value 


#Upper Tail Test of Population Mean with Unknown Variance
#The null hypothesis of the upper tail test of the population mean can be expressed as follows:
#μ ≤ μ0
#where μ0 is a hypothesized upper bound of the true population mean μ.
#The null hypothesis is that μ ≤ 2. We begin with computing the test statistic.
xbar = 2.1             # sample mean 
mu0 = 2                # hypothesized value 
s = 0.3                # sample standard deviation 
n = 35                 # sample size 
t = (xbar - mu0)/(s/sqrt(n)) 
t                      # test statistic 

#We then compute the critical value at .05 significance level.
alpha = .05 
t.alpha = qt(1-alpha, df=n-1) 
t.alpha                # critical value 

#The test statistic 1.9720 is greater than the critical value of 1.6991. Hence, at .05 significance level, we can reject the claim that there is at most 2 grams of saturated fat in a cookie.

#Alternative Solution
pval = pt(t, df=n-1, lower.tail=FALSE) 
pval                   # upper tail p−value 