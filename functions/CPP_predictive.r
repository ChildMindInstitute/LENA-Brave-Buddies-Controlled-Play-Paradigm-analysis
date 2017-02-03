library("randomForest")
# function to widen CPP LENA data for predictive models
CPP_aggregate <- function(data){
  CPP_agg <- aggregate(data, by=list(URSI.g=data$URSI, Session.g=data$Session), FUN="mean")
  CPP_agg$URSI <- CPP_agg$URSI.g
  CPP_agg$URSI.g <- NULL
  CPP_agg$Session <- CPP_agg$Session.g
  CPP_agg$Session.g <- NULL
  CPP_agg <- sort_df(CPP_agg, vars=c("URSI", "Session"))
  return(CPP_agg)
}

# function to collapse wide CPP LENA dataframe to one row per URSI
CPP_collapse_wide <- function(data){
  if(length(levels(data$Session)) == 2){
    new_data <- data[0,]
    new_data$Session <- NULL
    features <- c("Turn_Count", "Child_Voc_Count", "Child_Voc_Duration", "Child_NonVoc_Duration", "Average_SignalLevel", "Peak_SignalLevel")
    for (row in 1:nrow(data)){
      if (row %% 2 == 0)
        new_data <- rbind(new_data, data.frame(c(data[row, c("URSI", "SM_dx")], (data[row - 1, features] - data[row, features]))))
        
    }
    data <- new_data
  } else {
        data$Session <- NULL
        data <- aggregate(data, by=list(URSI.g=data$URSI), FUN="mean")
        data$URSI <- data$URSI.g
        data$URSI.g <- NULL
  }
  return(data)
}

# function to run Random Forests on CPP LENA data
# returns a randomForest model
CPP_random_forest <- function(data, plot_title){
   rf <- randomForest(SM_dx ~ Turn_Count + Child_Voc_Count +
         Child_Voc_Duration + Child_NonVoc_Duration + Average_SignalLevel +
         Peak_SignalLevel, data=data, importance=TRUE, ntree=2000)
   print(importance(rf))
   print(rf)
   varImpPlot(rf, color="#0067a0", main = plot_title)
   return(rf)
}

# Get CPP data
CPP_data <- read.csv("../data/CPP_data_all.csv")

# Build dataframe for A - non-A
CPP_A_v_nonA <- CPP_data
levels(CPP_A_v_nonA$Session) <- c("A", "non-A", "non-A")

# Build dataframe for B - C
CPP_B_v_C <- CPP_data[!(CPP_data$Session == "A"),]
levels(CPP_B_v_C$Session) <- droplevels(CPP_B_v_C$Session)

# Widen CPP data
CPP_A_B_C <- CPP_aggregate(CPP_data)
CPP_A_v_nonA <- CPP_aggregate(CPP_A_v_nonA)
CPP_B_v_C <- CPP_aggregate(CPP_B_v_C)

# Collapse into one row per URSI in each wide frame
CPP_A_B_C <- CPP_collapse_wide(CPP_data)
CPP_A_v_nonA <- CPP_collapse_wide(CPP_A_v_nonA)
CPP_B_v_C <- CPP_collapse_wide(CPP_B_v_C)

# Random Forests
rf_CPP_A_B_C <- CPP_random_forest(CPP_A_B_C, "mean of all blocks")
rf_CPP_A_v_nonA <- CPP_random_forest(CPP_A_v_nonA, "A - non-A")
rf_CPP_B_v_C <- CPP_random_forest(CPP_B_v_C, "B - C")
