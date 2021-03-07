//sim1test.stan
data {
  // number of partitipants
  int<lower=1> P;
  // total number of measured response variables
  int<lower=1> N;
  // number of columns in design matrix X (= unique vertex IDs)
  int<lower=1> K;
  // response variables in vector
  int<lower=0> y[N];
  // design matrix of nodeIDs
  row_vector[K] x[N];
  // vector of participant IDX
  int<lower=1> pn[N];
  // vector of stepsleft IDX
  int<lower=0> sl[N];
}
parameters {
  real mu[K];             // Hierarchical constraint on coefficients per unique vertex ID per participant (of true interest after inverse logit transform!)
  vector[K] beta[P];      // Coefficient per unique vertex ID
  real<lower=0> sigma[K]; // Variance of participant coefficients drawn from mu[K]
}
transformed parameters{
  real pi[K];
  pi = inv_logit(mu);
}
model {
  // vector used for bernoulli_logit
  vector[N] x_beta_ll;
  
  for(k in 1:K){
    mu[k] ~ normal(0,100); // compare mean of current node to set prior
    for(p in 1:P){
      beta[p,k] ~ normal(mu[k], sigma[k]); // compare all participant coefficients to hierarchical node coefficient
    }
  }
  for(n in 1:N){
    x_beta_ll[n] = x[n] * beta[pn[n]];
  }
  y ~ bernoulli_logit(x_beta_ll);
}
