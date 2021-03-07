//sim1test5.stan
data{
  // number of participants
  int<lower=1> P;
  // number of columns in design matrix X (= unique vertex IDs)
  int<lower=1> K;
  // Number of actual choices 
  int<lower=1> M;
  // Data of choices
  int<lower=0> y[M];
  // Index for vertex id per data point
  int<lower=1> Vx[M];
  // Index for participant number per data point
  int<lower=1> Pn[M];
}
parameters{
  real beta[K];
  row_vector[K] param[P];
  real<lower=0> sigma;
}
model{
  for(k in 1:K){
    beta[k] ~ normal(0, 10);
  }
  for(p in 1:P){
    for(k in 1:K){
      param[p,k] ~ normal(beta[k], sigma);
    }
  }
  for(n in 1:M){
    y[n] ~ bernoulli_logit(param[Pn[n],Vx[n]]);
  }
}
generated quantities{
  real theta[K];
  for(k in 1:K){
    theta[k] = inv_logit(beta[k]);
  }
}