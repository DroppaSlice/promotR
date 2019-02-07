promotR_closest <- function(TSS_list){
  require(dplyr)
  TSS_list<- arrange(TSS_list, downstream.gene)
  n_orig <- nrow(TSS_list)
  keep <- c()
  for(i in 1:n_orig){
    #special conditions for the first & last row of the df
    if(i == 1){
      #skip if not a duplicate
      if(TSS_list[i,"downstream.gene"]!=TSS_list[i+1,"downstream.gene"]){
        keep <- append(keep,T)
      }
      #the meat of what to do; if further remove, else keep
      else{
        if(abs(TSS_list[i,"pos.relative.to.downstream.gene"]) > abs(TSS_list[i+1, "pos.relative.to.downstream.gene"])){
          keep <- append(keep,F)
        }
        else{
          keep <-  append(keep,T)
        }
      }
    }
    else if(i==n_orig){
      if(TSS_list[i,"downstream.gene"]!=TSS_list[i-1,"downstream.gene"]){
        keep <- append(keep,T)
      }
      else{
        if(abs(TSS_list[i,"pos.relative.to.downstream.gene"])>abs(TSS_list[i-1,"pos.relative.to.downstream.gene"])){
          keep <- append(keep,F)
        }
        else{
          keep <- append(keep,T)
        }
      }
    }
    else{
      if(TSS_list[i,"downstream.gene"]!=TSS_list[i+1,"downstream.gene"] & TSS_list[i,"downstream.gene"]!=TSS_list[i-1,"downstream.gene"]){
        keep<-append(keep,T)
      }
      else{
        if(abs(TSS_list[i,"pos.relative.to.downstream.gene"])>abs(TSS_list[i+1,"pos.relative.to.downstream.gene"]) & TSS_list[i,"downstream.gene"]==TSS_list[i+1,"downstream.gene"] | abs(TSS_list[i,"pos.relative.to.downstream.gene"])>abs(TSS_list[i-1,"pos.relative.to.downstream.gene"]) & TSS_list[i,"downstream.gene"]==TSS_list[i-1,"downstream.gene"]){
          keep<-append(keep,F)
        }
        else{
          keep<-append(keep,T)
        }
      }
    }
  }
  new_df <- TSS_list[keep,]
  row.names(new_df) <- new_df[,"downstream.gene"]
  print(paste(nrow(TSS_list)-nrow(new_df),"duplicate TSS removed from table"))
  print(length(keep))
  print(head(keep,10))
  new_df
}