#' Create data.frame of prediction accuracy.
#' 
#' @param vec.pred
#' @param vec.correct
#'
#' @export
#' 

df.accuracy_predict <- 
  function(
    vec.pred, 
    vec.correct
    ){
  df.accuracy_predict.var.test <- 
    data.frame(
      Specificity = 
        table(vec.pred, vec.correct)[1,1]/
        sum(table(vec.pred, vec.correct)[,1]),
      Sensitivity = 
        table(vec.pred, vec.correct)[2,2]/
        sum(table(vec.pred, vec.correct)[,2]),
      NPV = 
        table(vec.pred, vec.correct)[1,1]/
        sum(table(vec.pred, vec.correct)[1,]),
      PPV = 
        table(vec.pred, vec.correct)[2,2]/
        sum(table(vec.pred, vec.correct)[2,])
    )
  return(df.accuracy_predict.var.test)
}