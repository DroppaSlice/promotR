```{r Function:promotR_grab}
promotR_grab <- function(x, seq, upstream = 40, downstream = 0){
  require(Biostrings)
  result <- vector(mode="character", length=3)
  names(result) <- c("unique.id", "downstream.gene", "promoter.sequence")
  x <- as.character(as.vector(x))
  result[1] <- as.character(x[1])
  result[2] <- as.character(x[5])
  genome.position <- as.numeric(x[3])
  comp <- complement(seq)
  if(x[2]=="+"){
    result[3] <- toString(subseq(seq, start=(genome.position - upstream), end=(genome.position + downstream)))
  }
  else{
    result[3] <- toString(subseq(comp, start=(genome.position - downstream), end=(genome.position + upstream)))
  }
  return(result)
}
