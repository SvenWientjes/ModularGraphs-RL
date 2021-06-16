gen.ham.schap <- function(){
  # path of sequence -> will be started at random points and sometimes reversed
  ham.seq <- c(1, sample(2:4), 5, 6, sample(7:9), 10, 11, sample(12:14), 15)
  return(ham.seq)
}
