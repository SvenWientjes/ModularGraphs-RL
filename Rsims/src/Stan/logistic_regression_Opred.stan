//Hierarchical logistic regression for any number of independent variables
data{
  // number of participants
  int<lower=1> P;
  // Number of actual choices 
  int<lower=1> M;
  // Number of regressors to enter the model
  int<lower=1> O;
  // Matrix of regressors (MxO)
  vector[O] Reg[M];
  // Index for participant number per data point
  int<lower=1> Pn[M];
  // Data of choices
  int<lower=0> y[M];
}
parameters{
  real alpha;                   // Population level intercept
  row_vector[O] beta;               // Population level slopes
  vector[P] z_par_alpha;        // Standardized participant level intercept
  row_vector[O] z_par_beta[P];  // Standardized participant level slope
  real<lower=0> sigma_a;        // Rescaler of participant level intercept
  row_vector<lower=0>[O] sigma_b;     // Rescaler of participant level slopes
}
transformed parameters{
  vector[P] par_alpha;          // Proper participant level intercept
  row_vector[O] par_beta[P];    // Proper participant level slope
  vector[M] theta_hat;          // Predicted choices per trial
  
  par_alpha = z_par_alpha * sigma_a;
  for(p in 1:P){
    par_beta[p] = z_par_beta[p] .* sigma_b;
  }
  for(n in 1:M){
    theta_hat[n] = inv_logit(alpha + par_alpha[Pn[n]] + (beta + par_beta[Pn[n]])*Reg[n]);
  }
}
model{
  target += normal_lpdf(sigma_a|0,1) - normal_lccdf(0|0,1);
  target += normal_lpdf(sigma_b|0,1) - O*normal_lccdf(0|0,1);
  
  target += normal_lpdf(alpha|0,10);
  target += normal_lpdf(beta|0,10);
  
  target += std_normal_lpdf(z_par_alpha);
  for(p in 1:P){
    target += std_normal_lpdf(z_par_beta[p]);
  }
  
  for(n in 1:M){
    target += bernoulli_logit_lpmf(y[n]|alpha + par_alpha[Pn[n]] + (beta + par_beta[Pn[n]])*Reg[n]);
  }
}