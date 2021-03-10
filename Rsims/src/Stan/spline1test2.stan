functions {
  vector build_b_spline(int[] t, real[] ext_knots, int ind, int order);
  vector build_b_spline(int[] t, real[] ext_knots, int ind, int order) {
    // INPUTS:
      //    t:          the points at which the b_spline is calculated
    //    ext_knots:  the set of extended knots
    //    ind:        the index of the b_spline
    //    order:      the order of the b-spline
    vector[size(t)] b_spline;
    vector[size(t)] w1 = rep_vector(0, size(t));
    vector[size(t)] w2 = rep_vector(0, size(t));
    if (order==1)
      for (i in 1:size(t)) // B-splines of order 1 are piece-wise constant
    b_spline[i] = (ext_knots[ind] <= t[i]) && (t[i] < ext_knots[ind+1]);
    else {
      if (ext_knots[ind] != ext_knots[ind+order-1])
        w1 = (to_vector(t) - rep_vector(ext_knots[ind], size(t))) /
          (ext_knots[ind+order-1] - ext_knots[ind]);
      if (ext_knots[ind+1] != ext_knots[ind+order])
        w2 = 1 - (to_vector(t) - rep_vector(ext_knots[ind+1], size(t))) /
          (ext_knots[ind+order] - ext_knots[ind+1]);
      // Calculating the B-spline recursively as linear interpolation of two lower-order splines
      b_spline = w1 .* build_b_spline(t, ext_knots, ind, order-1) +
        w2 .* build_b_spline(t, ext_knots, ind+1, order-1);
    }
    return b_spline;
  }
}

data{
  // Number of participants
  int<lower=1> P;
  // Number of columns in design matrix X (= unique vertex IDs)
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
  
  // Number of knots
  int num_knots;
  // Indices of knots
  vector[num_knots] knots;
  // Degree of spline
  int spline_degree;
  
}
transformed data {
  int num_basis = num_knots + spline_degree - 1; // total number of B-splines
  matrix[num_basis, M] B;  // matrix of B-splines
  vector[spline_degree + num_knots] ext_knots_temp;
  vector[2*spline_degree + num_knots] ext_knots; // set of extended knots
  ext_knots_temp = append_row(rep_vector(knots[1], spline_degree), knots);
  ext_knots = append_row(ext_knots_temp, rep_vector(knots[num_knots], spline_degree));
  for (ind in 1:num_basis)
    B[ind,:] = to_row_vector(build_b_spline(Sl, to_array_1d(ext_knots), ind, spline_degree + 1));
  B[num_knots + spline_degree - 1, M] = 1;
}
parameters{
  row_vector[num_basis] a_raw[K];
  matrix[K, num_basis] a_raw_pp[P];
  real a0[K];  // intercept
  vector[K] a0_pp[P];
  real<lower=0> tau;
  real<lower=0> sigma_raw;
  real<lower=0> sigma_0;
}
transformed parameters {
  matrix[K, num_basis] a[P]; // spline coefficients
  vector[M] Y_hat;
  for(p in 1:P){
    for(k in 1:K){
    a[p][k] = a_raw_pp[p][k]*tau;
    }
  }
  for(n in 1:M){
    Y_hat[n] = a0_pp[Pn[n],Vx[n]]*Sl[n] + a[Pn[n]][Vx[n]]*B[:,n];
  }
}
model{
  // Priors
  for(k in 1:K){
    a_raw[k] ~ normal(0,10);
  }
  a0 ~ normal(0, 10);
  tau ~ normal(0, 10);
  for(p in 1:P){
    for(k in 1:K){
      a_raw_pp[p][k] ~ normal(a_raw[k], sigma_raw);
      a0_pp[p][k] ~ normal(a0[k], sigma_0);
    }
  }
  for(n in 1:M){
    y[n] ~ bernoulli_logit(Y_hat[n]);
  }
}
//generated quantities{
  // Initialize theta as probability of bet for different node IDs (K) at each stepsleft (S)
  //  row_vector[S] theta[K];
  //row_vector[S] oddreg[K];
  //  int steps[S] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  //  for(k in 1:K){
    //    for(s in 1:S){
      //      oddreg[k,s] = a0[k]*steps[s] + a[k]*build_b_spline(steps[s], to_array_1d(ext_knots), ind, spline_degree + 1);
      //      theta[k,s] = inv_logit(oddreg[k,s]);
      //    }
    //  }
  //}