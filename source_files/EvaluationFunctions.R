# Evaluation Functions:
#   A set of evaluation functions.
# Author: Samantha Piatt, Marek Petrik


# Returns evaluation as a data frame.
evaluate_data_asFrame <- function(data, prediction){
  if(!("X" %in% colnames(data))) data$X <- 1:length(data$Time)
  predictions <- data.frame(ObservationId = data$X, Selected = c(prediction))
  return(core_evaluation(data, predictions))
}

# Evaluates target and predictions. To be used with evaluate_file or evaluate_data
#
# Outputs performance data.
core_evaluation <- function(mms.target, predictions){
  # from: https://gitlab.com/marekpetrik/ML-Spring2018/blob/master/project/evaluation.pdf
  #sort them by prediction weight
  predictions <- predictions[order(predictions$Selected, decreasing = TRUE),]
  
  # use MITL to select data points
  true.predictions.count <- sum(mms.target$Selected == T)
  predictions$MITL_Selected <- F
  predictions$MITL_Selected[1:true.predictions.count] <- T
  
  # select the predictions
  predictions_comparison <- inner_join(predictions %>% select(-Selected),
                                       mms.target, by=c("ObservationId" = "X"))
  
  # computed metrics
  found <- predictions_comparison %>% filter(Selected == T & MITL_Selected == T)
  missed <- predictions_comparison %>% filter(Selected == T & MITL_Selected == F)
  
  return(data.frame("Total SITL" = true.predictions.count, 
                    "Found SITL" = nrow(found), "Missed SITL" = nrow(missed), 
                    "Class Error" = with(predictions_comparison,{mean(MITL_Selected != Selected)}),
                    "ERROR" = sum(missed$Priority^2) / true.predictions.count))
}

eval.method <- function(target, prediction, threshold){
  t.seg = segments(target)
  p.seg = segments(prediction)
  
  t.count = 0
  p.count = 0
  
  for(s in 1:dim(t.seg)[1]){
    t.sum = sum(target[t.seg[s,1]:t.seg[s,2]], na.rm = TRUE)
    p.sum = sum(prediction[t.seg[s,1]:t.seg[s,2]], na.rm = TRUE)
    if((p.sum / t.sum) > threshold) t.count = t.count + 1
  }
  for(s in 1:dim(p.seg)[1]){
    t.sum = sum(target[p.seg[s,1]:p.seg[s,2]], na.rm = TRUE)
    p.sum = sum(prediction[p.seg[s,1]:p.seg[s,2]], na.rm = TRUE)
    if((t.sum / p.sum) > threshold) p.count = p.count + 1
  }
  
  c(Target_Ratio = t.count / dim(t.seg)[1], Prediction_Ratio = p.count / dim(p.seg)[1])
}

segments <- function(input){
  time <- c(1:length(input))
  tmp <- c(FALSE, abs(diff(input)) == 1)
  colors <- ifelse(input == 1, TRUE, FALSE)
  s <- c(time[1], time[tmp])
  e <- c(time[tmp]-1, time[length(time)])
  col <- c(colors[1], colors[tmp])
  selections <- data.frame(start = s, end = e, Selection = col)
  return(selections[selections$Selection==TRUE,1:2])
}

