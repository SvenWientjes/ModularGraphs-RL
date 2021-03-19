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
  real beta[K];             // Population level effects
  row_vector[K] z_param[P]; // Standardized participant level effects
  real<lower=0> sigma;      // Rescaler of participant level effects
}
transformed parameters{
  row_vector[K] param[P];   // Proper participant level effects
  real theta[K];            // Effect rescaled from logodds to prob
  for(p in 1:P){
    param[p] = z_param[p] * sigma;
  }
  theta = inv_logit(beta);
}
model{
  target += normal_lpdf(sigma|0,1) - normal_lccdf(0|0,1);
  for(k in 1:K){
    beta[k] ~ normal(0, 10);
  }
  for(p in 1:P){
    target += std_normal_lpdf(to_vector(z_param[p]));
  }
  
  for(n in 1:M){
    y[n] ~ bernoulli_logit(beta[Vx[n]] + param[Pn[n],Vx[n]]);
  }
}
