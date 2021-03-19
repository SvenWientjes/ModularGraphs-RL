//sim1test6.stan
data{
  // number of participants
  int<lower=1> P;
  // number of columns in design matrix X (= unique vertex IDs)
  int<lower=1> K;
  // Number of actual choices 
  int<lower=1> M;
  // Number of different stepsleft values
  int<lower=0> S;
  // Data of choices
  int<lower=0> y[M];
  // Index for vertex id per data point
  int<lower=1> Vx[M];
  // Index for participant number per data point
  int<lower=1> Pn[M];
  // Regressor of stepsleft
  int<lower=0> Sl[M];
}
parameters{
  real apop[K];         // Population level intercept
  real bpop[K];         // Population level regressor
  row_vector[K] z_a[P]; // Z-scored individual deviance intercept
  row_vector[K] z_b[P]; // Z-scored individual deviance regressor
  vector<lower=0>[2] sigma;   // Rescaler individual deviance 
}
transformed parameters{
  row_vector[K] a[P];
  row_vector[K] b[P];
  vector[M] lohat;
  vector[M] theta;
  for(p in 1:P){
    a[p] = z_a[p]*sigma[1];
    b[p] = z_b[p]*sigma[2];
  }
  for(n in 1:M){
    lohat[n] = apop[Vx[n]] + a[Pn[n], Vx[n]] + (bpop[Vx[n]] + b[Pn[n], Vx[n]]) * Sl[n];
  }
  theta = inv_logit(lohat);
}
model{
  target += normal_lpdf(sigma|0,1) - 2*normal_lccdf(0|0,1);
  for(k in 1:K){
    apop[k] ~ normal(0, 10);
    bpop[k] ~ normal(0, 10);
  }
  for(p in 1:P){
    target += std_normal_lpdf(z_a[p]);
    target += std_normal_lpdf(z_b[p]);
  }
  for(n in 1:M){
    y[n] ~ bernoulli_logit(lohat[n]);
  }
}