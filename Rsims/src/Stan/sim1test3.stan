//sim1test3.stan
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
  real<lower=0, upper=1> theta[K];
}
model{
  for(k in 1:K){
    theta[k] ~ beta(1, 1);
  }
  for(n in 1:M){
    y[n] ~ bernoulli(theta[Vx[n]]);
  }
}