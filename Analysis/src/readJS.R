################################################################################
####### Schapiro - Bet recover trajectories from js file used online ###########
################################################################################
read.JSarray <- function(fileloc, nPP, nTrs){
  ## Reads specifically Array of Arrays of Arrays
  ## corresponding to Experiment - Participant - Miniblock
  ## entries are individual trials
  #-------------------------------------------------------
  # Load in as text variable
  JStxt <- readChar(fileloc, file.info(fileloc)$size)
  JStxt <- gsub('[\r\n]', '',JStxt)
  vecStr <- str_match_all(JStxt, regex('(?<=\\[{1})([^A-Za-z\\[]*?)(?=\\]{1})', dotall=T))[[1]][,1]
  egI = expand.grid(tr=1:nTrs, pp=1:nPP)
  trTab <- foreach(st = vecStr, tr = egI$tr, pp=egI$pp, .combine=rbind) %do% {
    data.table(pp=pp, tr=tr, v=as.numeric(strsplit(st, split=", ",fixed=T)[[1]]))
  }
  return(trTab)
}




