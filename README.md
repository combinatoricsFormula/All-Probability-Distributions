________________________________________
I. DISCRETE DISTRIBUTIONS
________________________________________
	Description: 
		Used for countable outcomes (e.g., number of successes, failures, occurrences).
________________________________________
	Python (SciPy & NumPy)
		Distribution	Description	Python Function
		Bernoulli	Single-trial binary outcome	scipy.stats.bernoulli.pmf(x, p)
		Binomial	Number of successes in n trials	scipy.stats.binom.pmf(x, n, p)
		Poisson	Count of events in fixed time/space	scipy.stats.poisson.pmf(x, lambda_)
		Geometric	First success in Bernoulli trials	scipy.stats.geom.pmf(x, p)
		Negative Binomial	Number of failures before r successes	scipy.stats.nbinom.pmf(x, r, p)
		Hypergeometric	Successes without replacement	scipy.stats.hypergeom.pmf(x, M, n, N)
		Discrete Uniform	Equal probability for k outcomes	scipy.stats.randint.pmf(x, a, b)
		Zipf	Power-law distributed rank data	scipy.stats.zipf.pmf(x, a)

________________________________________
	R (Base & Extra Packages)
		Distribution	Description	R Function
		Bernoulli	Single-trial binary outcome	rbinom(n, 1, p)
		Binomial	Number of successes in n trials	rbinom(n, size, prob)
		Poisson	Count of events in fixed time/space	rpois(n, lambda)
		Geometric	First success in Bernoulli trials	rgeom(n, prob)
		Negative Binomial	Number of failures before r successes	rnbinom(n, size, prob)
		Hypergeometric	Successes without replacement	rhyper(nn, m, n, k)
		Discrete Uniform	Equal probability for k outcomes	sample(1:k, n, replace=TRUE)
		Zipf	Rank data power law	VGAM::rzipf(n, shape)
________________________________________

	SQL (Approximation Using Built-in Functions)
		Distribution	SQL Implementation
		Binomial	RAND() in loops for n trials
		Poisson	EXP(-lambda) * POWER(lambda, x) / FACTORIAL(x)
		Geometric	CEIL(LOG(1 - RAND()) / LOG(1 - p))
		Negative Binomial	Simulation with CASE conditions
		Hypergeometric	Using NTILE() over grouped draws
________________________________________
II. CONTINUOUS DISTRIBUTIONS
________________________________________
	Discription:
 		Used for modeling continuous values like heights, weights, time intervals, etc.
________________________________________
	Python (SciPy & NumPy)
		Distribution	Description	Python Function
		Uniform	Equal probability in range [a, b]	scipy.stats.uniform.pdf(x, a, b)
		Normal (Gaussian)	Bell curve distribution	scipy.stats.norm.pdf(x, mu, sigma)
		Log-Normal	Logarithmically normal	scipy.stats.lognorm.pdf(x, s)
		Exponential	Time until next event in Poisson process	scipy.stats.expon.pdf(x, lambda_)
		Gamma	Waiting times of k events	scipy.stats.gamma.pdf(x, k, theta)
		Beta	Probabilities between [0,1]	scipy.stats.beta.pdf(x, alpha, beta)
		Cauchy	Heavy-tailed distribution	scipy.stats.cauchy.pdf(x, loc, scale)
		Weibull	Reliability & survival analysis	scipy.stats.weibull_min.pdf(x, c)
		Pareto	Power-law distribution	scipy.stats.pareto.pdf(x, b)
		Chi-Square	Sum of squared standard normal variables	scipy.stats.chi2.pdf(x, df)
		Student's t	Small sample mean distribution	scipy.stats.t.pdf(x, df)
		F-Distribution	Ratio of variances	scipy.stats.f.pdf(x, dfn, dfd)
________________________________________
	R (Base & Extra Packages)
		Distribution	Description	R Function
		Uniform	Equal probability in [a,b]	runif(n, min, max)
		Normal	Bell curve distribution	rnorm(n, mean, sd)
		Log-Normal	Logarithmic normal	rlnorm(n, meanlog, sdlog)
		Exponential	Time intervals in Poisson	rexp(n, rate)
		Gamma	Waiting times for k events	rgamma(n, shape, scale)
		Beta	Probabilities [0,1]	rbeta(n, shape1, shape2)
		Cauchy	Heavy tails	rcauchy(n, location, scale)
		Weibull	Survival models	rweibull(n, shape, scale)
		Pareto	Power law	VGAM::rpareto(n, shape, scale)
		Chi-Square	Sum of squared normal variables	rchisq(n, df)
		Student's t	Small sample mean inference	rt(n, df)
		F-Distribution	Ratio of variances	rf(n, df1, df2)
________________________________________
	SQL (Approximation Using Built-in Functions)
		Distribution	SQL Implementation
		Uniform	RAND()
		Normal	NORMINV(RAND(), mu, sigma)
		Exponential	-LN(RAND()) / lambda
		Chi-Square	SUM(POWER(NORMINV(RAND(), 0, 1), 2))
		Student's t	NORMINV(RAND(), 0, 1) / SQRT(CHISQ_INV(RAND(), df) / df)
		F-Distribution	((CHISQ_INV(RAND(), df1) / df1) / (CHISQ_INV(RAND(), df2) / df2))
________________________________________
III. MULTIVARIATE & JOINT DISTRIBUTIONS
________________________________________
	Python
		Distribution	Description	Python Function
		Multivariate Normal	Generalized normal distribution	scipy.stats.multivariate_normal.pdf(x, mean, cov)
		Dirichlet	Multivariate Beta	scipy.stats.dirichlet.pdf(x, alpha)
		Wishart	Covariance matrix distribution	scipy.stats.wishart.pdf(x, df, scale)
	R
		Distribution	Description	R Function
		Multivariate Normal	Normal in multiple dimensions	MASS::mvrnorm(n, mu, Sigma)
		Dirichlet	Multivariate Beta	gtools::rdirichlet(n, alpha)
		Wishart	Covariance matrix	rWishart(n, df, Sigma)
