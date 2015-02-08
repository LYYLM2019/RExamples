If you are looking to fit a [log-logistic distribution][1] to your data, it is fairly straightforward to do so. In the example below, I am using the function `dllog` to get at the density of the log-logistic for a given set of values of the shape and scale parameter, but it is no trouble to write the PDF code yourself as well.

----

(Log-)Likelihood & MLE
====
The density of log-logistic distributed a random variable has the probability density function [PDF]:
$$
f(X_i; \alpha, \beta)  = \dfrac{\left(\tfrac{\beta}{\alpha}\right)\left(\tfrac{X_i}{\alpha}\right)^{\beta - 1}}{\left(1+ \left(\tfrac{X_i}{\alpha}\right)^{\beta}\right)^2}
$$
where $\alpha$ and $\beta$ are the scale and shape parameters respectively. 

For a given sample of data $X_1, \ldots, X_N$, this implies that the log-likelihood of the sample is:
$$
\ell_N(\alpha, \beta \mid X_1, \ldots, X_N) = \sum_{i=1}^N \log f(X_i; \alpha, \beta) 
$$

The MLE of the parameters given the sample of the data, is given by the maximizer of the log-likelihood:
$$
\hat{\alpha}_{MLE}, \hat{\beta}_{MLE} = \arg\max_{\alpha, \beta}\ell_N(\alpha, \beta \mid X_1, \ldots, X_N)
$$

----

Computing & optimizing the log-likelihood
====
In the code below:  
0. I have used the function `rllog` to generate a random sample from a log-logistic distribution with parameters `c(5, 6)`.  
1. The function `fnLLLL` computes the (negative) log-likelihood of the data.  
2. The function `fnLLLL` uses the function `dllog` from the `FAdist` package to compute the PDF of the log-logistic distribution, $f$.  
3. `optim` computes the values of $\alpha$, and $\beta$ that minimize the negative log-likelihood, and the values `c(2, 3)` are the intial values for the optimizer. 
Those optimized values are $5.132758$ & $5.654340$, and the optimized value of the negative log-likelihood function is $9239.179$.

    # simulate some log-logistic data
    library(FAdist)
    vY = rllog(n = 1000, shape = 5, scale = 6)
    
    # log-likelihood function
    fnLLLL = function(vParams, vData) {
      # uses the density function of the log-logistic function from FAdist
      return(-sum(log(dllog(vData, shape = vParams[1], scale = vParams[2]))))
    }
    
    # optimize it
    optim(c(2, 3), fnLLLL, vData = vY)

This gives:

    > optim(c(2, 3), fnLLLL, vData = vY)
    $par
    [1] 5.132758 5.654340
    
    $value
    [1] 9239.179
    
    $counts
    function gradient 
          57       NA 
    
    $convergence
    [1] 0
    
    $message
    NULL

  [1]: http://en.wikipedia.org/wiki/Log-logistic_distribution
