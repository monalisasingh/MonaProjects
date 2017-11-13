#We begin with computing the standard deviation of the mean, sem.
n = 30                # sample size 
sigma = 120           # population standard deviation 
sem = sigma/sqrt(n); sem   # standard error 

#We next compute the lower bound of sample means for which the null hypothesis μ ≥ 10000 would not be rejected.
alpha = .05           # significance level 
mu0 = 10000           # hypothetical lower bound 
q = qnorm(alpha, mean=mu0, sd=sem); q 

# we can compute the probability of the sample mean above 9964, and thus found the probability of type II error.
mu = 9950             # assumed actual mean 
pnorm(q, mean=mu, sd=sem, lower.tail=FALSE) 