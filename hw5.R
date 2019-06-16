data_set <-  read.csv("C:\\Users\\Goko\\Desktop\\hw5\\hw05_data_set.csv", header=TRUE)

train_set <- data_set[1:100,]

test_set <- data_set[101:133,]

x <- data_set$x
y <- data_set$y 

x_train <- train_set$x
y_train <- train_set$y

x_test <- test_set$x
y_test <- test_set$y




N <- length(x)
N_train <- length(y_train)
N_test <- length(y_test)



train <- function(pre_prun){
  
node_indices <- list()
is_terminal <- c()
need_split <- c()

node_features <- c()
node_splits <- c()
node_prediction <- list()

node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)



while (1) {
  # find nodes that need splitting
  split_nodes <- which(need_split)
  
  # check whether we reach all terminal nodes
  if (length(split_nodes) == 0) {
    break
  }
  # find best split positions for all nodes
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    node_prediction[[split_node]] <- sum(y_train[data_indices])/length(data_indices)
    # check whether node is pure
    if (length(data_indices) <= pre_prun || length(unique(x_train[data_indices])) == 1) {
      is_terminal[split_node] <- TRUE
    } else {
      is_terminal[split_node] <- FALSE
      
      best_score <- 0
      best_split <- 0
      
        
        unique_values <- sort(unique(x_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          
          left_indices <- data_indices[which(x_train[data_indices] < split_positions[s])]
          right_indices <- data_indices[which(x_train[data_indices] >= split_positions[s])]
          split_scores[s] <- sum(sapply(list(left_indices, right_indices),
                                        function(indices){ sum((y_train[indices] - sum(y_train[indices])/length(indices))^2) })) / length(data_indices)
        }
        best_score <- min(split_scores)
        best_split <- split_positions[which.min(split_scores)]
      
      # decide where to split on which feature
      node_splits[split_node] <- best_split
      
      # create left node using the selected split
      left_indices <- data_indices[which(x_train[data_indices] < best_split)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      # create left node using the selected split
      right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}
return( function(x){
  
  index <- 1
  repeat {
    if (is_terminal[index] == TRUE) {
      return( node_prediction[[index]] )
    } else {
      if (x <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
})
}
p <- 10

tree <- train(p)
y_predicted <- rep(0, N_test)

y_predicted <- sapply(x_test, tree)

  
  rmse <- sqrt(mean((y_test - y_predicted)^2))
  print(sprintf("RMSE is %.4f when P is %g", rmse, p))
  
  
  minimum_value <- 0
  maximum_value <- 60
  data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)
  

    plot(x_train, y_train, type = "p", pch = 19, col = "blue",
         ylim = c(min(y), max(y)), xlim = c(minimum_value, maximum_value),
         ylab = "y", xlab = "x", las = 1)
    points(x_test, y_test, type = "p", pch = 19, col = "red")
    lines(data_interval, sapply(data_interval, tree), type = "l", lwd = 2, col = "black")
    
p_testing <- sapply(1:20, function(pre_prun){
  tree <- train(pre_prun)
  y_predicted <- sapply(x_test, tree)
  return (sqrt (mean((y_test - y_predicted)^2)))
  
  
})
    
    plot(1:length(p_testing), p_testing, type="b", ylab = "RMSE", xlab = "p")