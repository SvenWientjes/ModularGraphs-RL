//sim1test4.stan
data{
  // number of columns in design matrix X (= unique vertex IDs)
  int<lower=1> K;
  // Number of actual choices 
  int<lower=1> M;
  // Data of choices
  int<lower=0> y[M];
  // Index for vertex id per data point
  int<lower=1> Vx[M];
}
parameters{
  real beta[K];
}
model{
  for(k in 1:K){
    beta[k] ~ normal(0, 10);
  }
  for(n in 1:M){
    y[n] ~ bernoulli_logit(beta[Vx[n]]);
  }
}
generated quantities{
  real theta[K];
  for(k in 1:K){
    theta[k] = inv_logit(beta[k]);
  }
}