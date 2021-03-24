//opt_regress.stan
data{
  // number of participants
  int<lower=1> P;
  // Number of actual choices 
  int<lower=1> M;
  // Data of choices
  int<lower=0> y[M];
  // EV regressor per data point
  real EV[M];
  // Index for participant number per data point
  int<lower=1> Pn[M];
}
parameters{
  real alpha;                // Population level intercept
  real beta;                 // Population level slope
  vector[P] z_par_alpha;     // Standardized participant level intercept
  vector[P] z_par_beta;      // Standardized participant level slope
  vector<lower=0>[2] sigma;  // Rescaler of participant level effects
}
transformed parameters{
  vector[P] par_alpha;    // Proper participant level intercept
  vector[P] par_beta;     // Proper participant level slope
  vector[M] theta_hat;    // Predicted choices per trial
  
  par_alpha = z_par_alpha * sigma[1];
  par_beta  = z_par_beta * sigma[2];
  for(n in 1:M){
    theta_hat[n] = inv_logit(alpha + par_alpha[Pn[n]] + (beta + par_beta[Pn[n]])*EV[n]);
  }
}
model{
  target += normal_lpdf(sigma|0,1) - 2*normal_lccdf(0|0,1);
  
  target += normal_lpdf(alpha|0,10);
  target += normal_lpdf(beta|0,10);
  
  target += std_normal_lpdf(z_par_alpha);
  target += std_normal_lpdf(z_par_beta);
  
  for(n in 1:M){
    target += bernoulli_logit_lpmf(y[n]|alpha + par_alpha[Pn[n]] + (beta + par_beta[Pn[n]])*EV[n]);
  }
}
