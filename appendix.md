# Appendix A: Distribution families for model checks in EVM

Our aim in enabling users to choose between distribution families when specifying model checks was to provide ways to approximate the shape of the data distribution, including boundaries.
For example, if the user sees that the distribution of the outcome variable is skewed or has a lower bound at zero---common distribution shapes---they should be able to specify a model that accounts for those details of the apparent data generating process. 
We implemented model checking in EVM with the following **distribution families**:

- *Normal (a.k.a. Gaussian)*: a traditional linear model for continuous outcomes, estimating the conditional mean, with the option to specify separate sub-models for the Gaussian distribution's location and scale.
- *Log normal:* a transformed linear model for continuous outcomes in the domain $(0, +\infty)$, estimating the conditional mean in log units, with the option to specify separate sub-models for the Gaussian distribution's location and scale on a log scale.
- *Logit normal*: a transformed linear model for continuous outcomes in the domain $(0, 1)$ such as proportions, estimating the conditional mean in log odds units, with the option to specify separate sub-models for the Gaussian distribution's location and scale on a log odds scale.
- *Logistic*: a generalized linear model for binary outcomes, estimating the conditional log odds of outcomes coded as one vs zero.
- *Poisson*: a generalized linear model for count outcomes, estimating the conditional rate of events.
- *Negative binomial*: a generalized linear model for over-dispersed count outcomes, estimating the conditional rate of events, with the option to specify separate sub-models for the location and scale of rates. In EVM, we approach the negative binomial as a mixture of Poisson distributions with rates drawn from a Gamma distribution, a common parameterization of the negative binomial used when counts have heterogeneous
 variance.

We chose these distribution families to cover as many common distributions of outcome variables as we could without including models that are redundant in terms of the shape of distribution they support. 
For example, we avoid redundancy by not including the Beta distribution family since the logit normal will suffice for outcomes on the domain $(0, 1)$. 

Additionally, EVM does not support distributions for ordinal and multinomial outcome variables.
These kinds of outcomes lack support in the `gamlss` R package, which we relied on for all model checks in EVM due to its consistency of syntax and speed at runtime.
As mentioned in the paper, models need to be able to fit relatively quickly to support and interactive system with minimal latency.
One option we explored to support ordinal and multinomial outcome variables was to use the `brms` R package, which has robust support for these types of outcomes.
However, similar to other Bayesian implementations, `brms` models rely on a fitting procedure that is far too slow to support low latency model checking.
Future work should explore whether a system like EVM could support usable interactions with Bayesian regression models by adopting a modeling queue interface to increase user tolerance for latency and a parallelization scheme to maximize the speed of fitting.
