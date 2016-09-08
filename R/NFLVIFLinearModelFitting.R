#' NFL VIF fitting
#'
#' @param lmodel 
#'
#' @return VIFS Linear models
#' @export
#'
#' @examples ...
vifStepLinearModelNFL <- function(lmodel = lm1){
all_vifs <- car::vif(lm1)
print(all_vifs)

signif_all <- names(all_vifs)
# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("dataResults~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  lm1 <- lm(myForm, data=dataModelPredictors1)  # re-build model with new formula
  all_vifs <- car::vif(lm1)
}
summary(lm1)
car::vif(lm1)        
######Recursively Remove nonsignificant variables#######
all_vars <- names(lm1[[1]])[-1]  # names of all X variables
# Get the non-significant vars
summ <- summary(lm1)  # model summary
pvals <- summ[[4]][, 4]  # get all p values
not_significant <- character()  # init variables that aren't statsitically significant
not_significant <- names(which(pvals > 0.1))
not_significant <- not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!

# If there are any non-significant variables, 
while(length(not_significant) > 0){
  all_vars <- all_vars[!all_vars %in% not_significant[1]]
  myForm <- as.formula(paste("dataResults ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
  lm1 <- lm(myForm, data=dataModelPredictors1)  # re-build model with new formula
  
  # Get the non-significant vars.
  summ <- summary(lm1)
  pvals <- summ[[4]][, 4]
  not_significant <- character()
  not_significant <- names(which(pvals > 0.1))
  not_significant <- not_significant[!not_significant %in% "(Intercept)"]
}
summary(lm1)

return(lm1)
}