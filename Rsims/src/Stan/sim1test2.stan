//sim1test2.stan
data{
  // number of columns in design matrix X (= unique vertex IDs)
  int<lower=1> K;
  // Number of actual measurements (some might not occur naturally?)
  int<lower=1> M;
  // Number of made choices per participant per vertex ID
  int<lower=1> N[M];
  // Total number of betted choices ( < N)
  int<lower=0> B[M];
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
    B[n] ~ binomial(N[n], theta[Vx[n]]);
  }
}