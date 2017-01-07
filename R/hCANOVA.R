
library(parallel)
setClass("CANOVA",

  contains = "GeneralTest"

)

setValidity("CANOVA", function(object){
  if(object@p.opt == "dist")
    stop('No "dist" option for CANOVA, please use "MC".')
  if(object@p.opt == "table")
    stop('No "table" option for CANOVA, please use "MC".')

})


setMethod("test", signature(object = "CANOVA"), function(object){

  p = object@pdata
  data = p[[ls(p)]]
  n = nrow(data)


  #test statistic
  if(length(unique(data[,1])) != n || length(unique(data[,2])) != n){
    if(object@set.seed){set.seed(1)}
    W = W_ties(data[,1], data[,2], 2)
  }
  else
    W = W_no_ties(data[,1], data[,2], 2)

  #MC
  if(object@set.seed){set.seed(1)}
  pv = MC_p(data[,1], data[,2], W, object@num.MC, 2)


  #BS.CI
  if(object@BS.CI == 0){
    return (new("testforDEP_result", TS = W, p_value = pv))
  }

  times = 1000

  BS = function(i){
    if(object@set.seed){set.seed(i)}
    BSdata = data[sample(1:n, n, replace = TRUE),]
    return(W_ties(BSdata[,1], BSdata[,2], 2))
  }

  cl <- parallel::makeCluster(8)
  parallel::clusterExport(cl, "BS", envir = environment())
  parallel::clusterExport(cl, "n", envir = environment())
  parallel::clusterExport(cl, "data", envir = environment())
  results <- parallel::parLapply(cl, 1:times, BS)
  Ws <- as.vector(do.call('rbind',results))
  parallel::stopCluster(cl)

  CI = getCI(W, Ws, object@BS.CI)

  return (new("testforDEP_result", TS = W, p_value = pv, CI = CI))



})
