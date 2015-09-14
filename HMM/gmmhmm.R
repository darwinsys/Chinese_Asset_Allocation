
gmmhmm_training <- function(data_training) {
  data_training <- dataset[(i - n_start + 1):i,];
  output <- list();
  
  #### Using Mixture Model to determine the optimal number of regimes and the settings under 
  #### each regime
  mm_model <- Mclust(data_training);
  mm_output <- summary(mm_model);
  output$gmm <- mm_model;
  
  #### creating the HMM model
  J <- mm_output$G
  print(paste("Nr of Regimes ", J))
  initial <- rep(1/J, J)
  P <- matrix(rep(1/J, J*J), nrow=J)
  mean <- list()
  vcv <- list()
  for (j in 1:J){
    mean[[j]] <- mm_output$mean[, j]
    vcv[[j]] <- mm_output$variance[,,j]
  }
  b <- list()
  b$mu <- mean
  b$sigma <- vcv
  
  #### training HMM model
  hmm_model <- hmmspec(init=initial, trans=P, parms.emission = b, dens.emission =dmvnorm.hsmm)
  hmm_fitted <- hmmfit(data_training, hmm_model, mstep = mstep.mvnorm)
  print("hmm fitting")
  #### Predict future regime
  regime <- tail(hmm_fitted$yhat, 1);
  
  output$hmm <- hmm_fitted
  
  ############################################################
  #### In the training set, the regimes and returns
  yhat_train <- as.xts(hmm_fitted$yhat, order.by = index(data_training), tzone=tzone(data_training))
  ret_training_regime <- list()
  for (k in 1:J) {
    ret_training_regime[[k]] <- data_training[,1] * (yhat_train == k)
  }
  ret_training_regime <- do.call(cbind, ret_training_regime)
  
  output$hmm_yhat <- yhat_train
  output$hmm_ret_regime <- ret_training_regime
  
  
  ### calculate the risk measures 
  sharpe_training_regime_vol <- SharpeRatio(ret_training_regime)[1,]
  max_sharpe_regime <- match(max(sharpe_training_regime_vol), sharpe_training_regime_vol)
  calmar_training_regime <- CalmarRatio(ret_training_regime)
  max_calmar_regime <- match(max(calmar_training_regime), calmar_training_regime)
  sortino_training_regime <- SortinoRatio(ret_training_regime)
  max_sortino_regime <- match(max(sortino_training_regime), sortino_training_regime)
  ret_training_regime <- Return.annualized(ret_training_regime)
  
  max_order = sort(sharpe_training_regime_vol, index.return=TRUE, decreasing=TRUE)
  max_order = max_order$ix
  
  top_regime1 <- max_order[1];
  top_regime2 <- max_order[2];
  
  ret_avg_regime1 <- ret_training_regime[top_regime1]
  ret_avg_regime2 <- ret_training_regime[top_regime2]
  
  
  ##################################
  #signal <- gmm_hmm_strategy(data_training, data_test, ret_target);
  
  
  last_ret <- ret_target[i];
  next_ret <- ret_target[i+1];
  
  #selected_ret <- next_ret * (sharpe_training_regime_vol[regime] > 0)
  
  selected_ret <- next_ret * ((regime == top_regime1 & ret_avg_regime1 > 0) | 
                                (regime == top_regime2 & ret_avg_regime2 > 0))
  #selected_ret <- next_ret * ((regime == top_regime1 & ret_avg_regime1 > 0) )
  ret_s1[i+1] <- selected_ret
  
  print(paste("target regime = ", top_regime1, ": ret = ", ret_avg_regime1));
  print(paste("target regime = ", top_regime2, ": ret = ", ret_avg_regime2));
  print(paste("current regime = ", regime, ": date = ", as.character(index(ret_target[i]))));
  print(paste("prev ret =", last_ret, 
              ": next ret =", next_ret, ": selected_ret = ", selected_ret));
  print(paste("cumulative ret=", sum(ret_s1)))
  
  if (i >= (n_start+10)){
    ret_c <- cbind(ret_s1[(n_start):(i+1)], ret_target[(n_start):(i+1)])
    charts.PerformanceSummary(ret_c)
    
  }
}

gmmhmm <- function(data, n_start) {
  n <- nrow(dataset) 
  ret_s1 <- ret_target * 0;
  for (i in n_start:(n)) {
    data_training <- dataset[(i - n_start + 1):i,];
    
    #### Using Mixture Model to determine the optimal number of regimes and the settings under 
    #### each regime
    mm_model <- Mclust(data_training);
    mm_output <- summary(mm_model);
    
    #### creating the HMM model
    J <- mm_output$G
    print(paste("Nr of Regimes ", J))
    initial <- rep(1/J, J)
    P <- matrix(rep(1/J, J*J), nrow=J)
    mean <- list()
    vcv <- list()
    for (j in 1:J){
      mean[[j]] <- mm_output$mean[, j]
      vcv[[j]] <- mm_output$variance[,,j]
    }
    b <- list()
    b$mu <- mean
    b$sigma <- vcv
    
    #### training HMM model
    hmm_model <- hmmspec(init=initial, trans=P, parms.emission = b, dens.emission =dmvnorm.hsmm)
    hmm_fitted <- hmmfit(data_training, hmm_model, mstep = mstep.mvnorm)
    print("hmm fitting")
    #### Predict future regime
    regime <- tail(hmm_fitted$yhat, 1);
    
    
    ############################################################
    #### In the training set, the regimes and returns
    yhat_train <- as.xts(hmm_fitted$yhat, order.by = index(data_training), tzone=tzone(data_training))
    ret_training_regime <- list()
    for (k in 1:J) {
      ret_training_regime[[k]] <- data_training[,1] * (yhat_train == k)
    }
    ret_training_regime <- do.call(cbind, ret_training_regime)
    
    ### calculate the risk measures 
    sharpe_training_regime_vol <- SharpeRatio(ret_training_regime)[1,]
    max_sharpe_regime <- match(max(sharpe_training_regime_vol), sharpe_training_regime_vol)
    calmar_training_regime <- CalmarRatio(ret_training_regime)
    max_calmar_regime <- match(max(calmar_training_regime), calmar_training_regime)
    sortino_training_regime <- SortinoRatio(ret_training_regime)
    max_sortino_regime <- match(max(sortino_training_regime), sortino_training_regime)
    ret_training_regime <- Return.annualized(ret_training_regime)
    
    max_order = sort(sharpe_training_regime_vol, index.return=TRUE, decreasing=TRUE)
    max_order = max_order$ix
    
    top_regime1 <- max_order[1];
    top_regime2 <- max_order[2];
    
    ret_avg_regime1 <- ret_training_regime[top_regime1]
    ret_avg_regime2 <- ret_training_regime[top_regime2]
    
    
    ##################################
    #signal <- gmm_hmm_strategy(data_training, data_test, ret_target);
    
    
    last_ret <- ret_target[i];
    next_ret <- ret_target[i+1];
    
    #selected_ret <- next_ret * (sharpe_training_regime_vol[regime] > 0)
    
    selected_ret <- next_ret * ((regime == top_regime1 & ret_avg_regime1 > 0) | 
                                  (regime == top_regime2 & ret_avg_regime2 > 0))
    #selected_ret <- next_ret * ((regime == top_regime1 & ret_avg_regime1 > 0) )
    ret_s1[i+1] <- selected_ret
    
    print(paste("target regime = ", top_regime1, ": ret = ", ret_avg_regime1));
    print(paste("target regime = ", top_regime2, ": ret = ", ret_avg_regime2));
    print(paste("current regime = ", regime, ": date = ", as.character(index(ret_target[i]))));
    print(paste("prev ret =", last_ret, 
                ": next ret =", next_ret, ": selected_ret = ", selected_ret));
    print(paste("cumulative ret=", sum(ret_s1)))
    
    if (i >= (n_start+10)){
      ret_c <- cbind(ret_s1[(n_start):(i+1)], ret_target[(n_start):(i+1)])
      charts.PerformanceSummary(ret_c)
      
    }
    
  }
  
  ret_c <- cbind(ret_s1[n_start:n,], ret_target[n_start:n,])
  charts.PerformanceSummary(ret_c)
  rbind(table.AnnualizedReturns(ret_c), maxDrawdown(ret_c), CalmarRatio(ret_c))
  return(ret_c)
  
}

