#-------------------------------------------------------------------------------------------------------------------
#                         The function 'ROC_plot' takes in 2 parameters &  plots a ROC Curve 
#-------------------------------------------------------------------------------------------------------------------
# actual - a vector of actual classes
# predicted_prob - a vector of predicted probabilities
# example - > ROC_plot(actual = test$Death, predicted_prob = pred)
#-------------------------------------------------------------------------------------------------------------------

ROC_plot <- function(actual,predicted_prob)
{
  threshold = seq(1,0,-0.025)
  sensitivity <- c()
  specificity <- c()
  fpr <- c()
  count <- 0
  
  for (i in threshold)
  {
    count = count + 1
    pred_class = ifelse(predicted_prob > i, 1, 0)
    Actual = actual
    confusion_matrix=(table(Actual,pred_class))
    if(i == 1)
    {
      sensitivity[count] <- 0
      specificity[count] <- confusion_matrix[1][1]/(confusion_matrix[1][1]+0)
      fpr[count] <- 1-specificity[count]
    }
    else if(i == 0)
    {
      sensitivity[count] <- confusion_matrix[2][1]/(confusion_matrix[2][1]+0)
      specificity[count] <- 0
      fpr[count] <- 1-specificity[count]
    }
    else
    {
      sensitivity[count] <- confusion_matrix[2,2]/(confusion_matrix[2,2] + confusion_matrix[2,1])
      specificity[count] <- confusion_matrix[1,1]/(confusion_matrix[1,1] + confusion_matrix[1,2])
      fpr[count] <- 1-specificity[count]
    }
  }
  plot(fpr, sensitivity, type="l", main='ROC Curve', col = 'dark green') 
}














