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
  real apop[K];
  real bpop[K];
  row_vector[K] a[P];
  row_vector[K] b[P];

  real<lower=0> asig;
  real<lower=0> bsig;
}
model{
  for(k in 1:K){
    apop[k] ~ normal(0, 10);
    bpop[k] ~ normal(0, 10);
  }
  for(p in 1:P){
    for(k in 1:K){
      a[p,k] ~ normal(apop[k], asig);
      b[p,k] ~ normal(bpop[k], bsig);
    }
  }
  for(n in 1:M){
    y[n] ~ bernoulli_logit(a[Pn[n], Vx[n]] + b[Pn[n], Vx[n]] * Sl[n]);
  }
}
generated quantities{
  // Initialize theta as probability of bet for different node IDs (K) at each stepsleft (S)
  row_vector[S] theta[K];
  row_vector[S] oddreg[K];
  int steps[S] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14};
  for(k in 1:K){
    for(s in 1:S){
      oddreg[k,s] = apop[k]+bpop[k]*steps[s];
      theta[k,s] = inv_logit(apop[k]+bpop[k]*steps[s]);
    }
  }
}