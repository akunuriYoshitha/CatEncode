#' Detects categorical variables in a given dataset and encodes it to numeric values
#'
#' Takes in a dataset, detects categorical variables and encodes the data using multiple encoding techniques:
#'
#' \strong{Contrast Encoding}
#' \itemize{
#' \item Dummy Coding
#' \item Simple Coding
#' \item Deviation Coding
#' \item Polynomial Coding
#' \item Reverse Helmert Coding
#' \item Helmert Coding
#' \item Forward Difference Coding
#' \item Backward Difference Coding
#' }
#'
#' \strong{Generic Encoders}
#' \itemize{
#' \item Label/Integer Encoding
#' \item One-Hot Encoding
#' \item Binary Encoding
#' \item Frequency Encoding
#' \item Hashing Encoding
#' \item Probability Ratio Encoding
#' }
#'
#' \strong{Bayesian Encoders}
#' \itemize{
#' \item Mean Target Encoding
#' \item Leave One Out Encoding
#' \item Weight Of Evidence Encoding
#' \item James Stein Encoding
#' \item M-Estimator Encoding
#' }
#' Each of these 19 datasets are passed to a logistic model to choose the best possible dataset.
#' @param data Any dataset with atleast one categorical field that is to be encoded
#' @param dv Dependent variable in the given dataset
#' @return Returns a list with an encoded dataset, performance metrics and a fit file to fit the test data.
#' @export
#' @usage BestCatEncode(data, dv)
BestCatEncode <- function(data, dv)
{
  # library(BestTransform)
  # library(data.table)
  dist <- data_distribution(data, dv)
  cat_cols <- dist[(dist$distribution == "Categorical" & dist$is_dv == FALSE),]$names
  if (length(cat_cols) == 0)
    return(list(encoded_data = NULL, encoding_fit_model = NULL, model_perf_metrics = NULL))
  dvcol <- data[,dv]
  {
    dummy_encoded <- data.table(data)[,lapply(.SD,function(x){encode_dummy(x, dvcol)$encoded}),.SDcols=cat_cols]
    dummy_encoded <- cbind(dummy_encoded, dvcol)
    # dummy_fit <- data.table(data)[,lapply(.SD,function(x){encode_dummy(x, dvcol)$fit}),.SDcols=cat_cols]
    dummy_fit <- list()
    for(i in cat_cols)
    {
      dummy_fit[[i]] <- encode_dummy(data[,i], dvcol)$fit
    }

    simple_encoded <- data.table(data)[,lapply(.SD,function(x){encode_simple(x, dvcol)$encoded}),.SDcols=cat_cols]
    simple_encoded <- cbind(simple_encoded, dvcol)
    # simple_fit <- data.table(data)[,lapply(.SD,function(x){encode_simple(x, dvcol)$fit}),.SDcols=cat_cols]
    simple_fit <- list()
    for(i in cat_cols)
    {
      simple_fit[[i]] <- encode_simple(data[,i], dvcol)$fit
    }

    deviation_encoded <- data.table(data)[,lapply(.SD,function(x){encode_deviation(x, dvcol)$encoded}),.SDcols=cat_cols]
    deviation_encoded <- cbind(deviation_encoded, dvcol)
    # deviation_fit <- data.table(data)[,lapply(.SD,function(x){encode_deviation(x, dvcol)$fit}),.SDcols=cat_cols]
    deviation_fit <- list()
    for(i in cat_cols)
    {
      deviation_fit[[i]] <- encode_deviation(data[,i], dvcol)$fit
    }

    poly_encoded <- data.table(data)[,lapply(.SD,function(x){encode_poly(x, dvcol)$encoded}),.SDcols=cat_cols]
    poly_encoded <- cbind(poly_encoded, dvcol)
    # poly_fit <- data.table(data)[,lapply(.SD,function(x){encode_poly(x, dvcol)$fit}),.SDcols=cat_cols]
    poly_fit <- list()
    for(i in cat_cols)
    {
      poly_fit[[i]] <- encode_poly(data[,i], dvcol)$fit
    }

    rev_helmert_encoded <- data.table(data)[,lapply(.SD,function(x){encode_rev_helmert(x, dvcol)$encoded}),.SDcols=cat_cols]
    rev_helmert_encoded <- cbind(rev_helmert_encoded, dvcol)
    # rev_helmert_fit <- data.table(data)[,lapply(.SD,function(x){encode_rev_helmert(x, dvcol)$fit}),.SDcols=cat_cols]
    rev_helmert_fit <- list()
    for(i in cat_cols)
    {
      rev_helmert_fit[[i]] <- encode_rev_helmert(data[,i], dvcol)$fit
    }

    helmert_encoded <- data.table(data)[,lapply(.SD,function(x){encode_helmert(x, dvcol)$encoded}),.SDcols=cat_cols]
    helmert_encoded <- cbind(helmert_encoded, dvcol)
    # helmert_fit <- data.table(data)[,lapply(.SD,function(x){encode_helmert(x, dvcol)$fit}),.SDcols=cat_cols]
    helmert_fit <- list()
    for(i in cat_cols)
    {
      helmert_fit[[i]] <- encode_helmert(data[,i], dvcol)$fit
    }

    fwd_diff_encoded <- data.table(data)[,lapply(.SD,function(x){encode_fwd_diff(x, dvcol)$encoded}),.SDcols=cat_cols]
    fwd_diff_encoded <- cbind(fwd_diff_encoded, dvcol)
    # fwd_diff_fit <- data.table(data)[,lapply(.SD,function(x){encode_fwd_diff(x, dvcol)$fit}),.SDcols=cat_cols]
    fwd_diff_fit <- list()
    for(i in cat_cols)
    {
      fwd_diff_fit[[i]] <- encode_fwd_diff(data[,i], dvcol)$fit
    }

    bwd_diff_encoded <- data.table(data)[,lapply(.SD,function(x){encode_bwd_diff(x, dvcol)$encoded}),.SDcols=cat_cols]
    bwd_diff_encoded <- cbind(bwd_diff_encoded, dvcol)
    # bwd_diff_fit <- data.table(data)[,lapply(.SD,function(x){encode_bwd_diff(x, dvcol)$fit}),.SDcols=cat_cols]
    bwd_diff_fit <- list()
    for(i in cat_cols)
    {
      bwd_diff_fit[[i]] <- encode_bwd_diff(data[,i], dvcol)$fit
    }

    label_encoded <- data.table(data)[,lapply(.SD,function(x){encode_label(x)$encoded}),.SDcols=cat_cols]
    label_encoded <- cbind(label_encoded, dvcol)
    # label_fit <- data.table(data)[,lapply(.SD,function(x){encode_label(x)$fit}),.SDcols=cat_cols]
    label_fit <- list()
    for(i in cat_cols)
    {
      label_fit[[i]] <- encode_label(data[,i])$fit
    }

    one_hot_encoded <- data.frame()
    one_hot_fit <- list()
    for (i in cat_cols)
    {
      # print(i)
      one_hot <- encode_onehot(data[, cat_cols], i)
      ifelse(length(one_hot_encoded) == 0, one_hot_encoded <- one_hot$encoded, one_hot_encoded <- cbind(one_hot_encoded, one_hot$encoded))
      one_hot_fit[[i]] <- one_hot$fit
    }
    one_hot_encoded <- cbind(one_hot_encoded, dvcol)

    binary_encoded <- data.frame(mapply(encode_binary, data.table(data[, cat_cols]), cat_cols))
    binary_encoded <- cbind(binary_encoded, dvcol)
    binary_fit <- mapply(encode_binary, data.table(data[, cat_cols]), cat_cols, T)

    freq_encoded <- data.table(data)[,lapply(.SD,function(x){encode_freq(x)$encoded}),.SDcols=cat_cols]
    freq_encoded <- cbind(freq_encoded, dvcol)
    # freq_fit <- data.table(data)[,lapply(.SD,function(x){encode_freq(x)$fit}),.SDcols=cat_cols]
    freq_fit <- list()
    for(i in cat_cols)
    {
      freq_fit[[i]] <- encode_freq(data[,i])$fit
    }

    hash_encoded <- data.frame()
    hash_fit <- list()
    for (i in cat_cols)
    {
      hash <- encode_hash(data[, cat_cols], i)
      ifelse(length(hash_encoded) == 0, hash_encoded <- hash$encoded, hash_encoded <- cbind(hash_encoded, hash$encoded))
      hash_fit[[i]] <- hash$fit
    }
    colnames(hash_encoded) <- cat_cols
    hash_encoded <- cbind(hash_encoded, dvcol)

    prob_ratio_encoded <- data.table(data)[,lapply(.SD,function(x){encode_prob_ratio(x, dvcol)$encoded}),.SDcols=cat_cols]
    prob_ratio_encoded <- cbind(prob_ratio_encoded, dvcol)
    # prob_ratio_fit <- data.table(data)[,lapply(.SD,function(x){encode_prob_ratio(x, dvcol)$fit}),.SDcols=cat_cols]
    prob_ratio_fit <- list()
    for(i in cat_cols)
    {
      prob_ratio_fit[[i]] <- encode_prob_ratio(data[,i], dvcol)$fit
    }

    target_encoded <- data.table(data)[,lapply(.SD,function(x){encode_target(x, dvcol)$encoded}),.SDcols=cat_cols]
    target_encoded <- cbind(target_encoded, dvcol)
    # target_fit <- data.table(data)[,lapply(.SD,function(x){encode_target(x, dvcol)$fit}),.SDcols=cat_cols]
    target_fit <- list()
    for(i in cat_cols)
    {
      target_fit[[i]] <- encode_target(data[,i], dvcol)$fit
    }

    leave_one_out_encoded <- data.table(data)[,lapply(.SD,function(x){encode_leave_one_out(x, dvcol)$encoded}),.SDcols=cat_cols]
    leave_one_out_encoded <- cbind(leave_one_out_encoded, dvcol)
    leave_one_out_fit <- data.table(data)[,lapply(.SD,function(x){encode_leave_one_out(x, dvcol)$fit}),.SDcols=cat_cols]

    woe_encoded <- data.table(data)[,lapply(.SD,function(x){encode_woe(x, dvcol)$encoded}),.SDcols=cat_cols]
    woe_encoded <- cbind(woe_encoded, dvcol)
    # woe_fit <- data.table(data)[,lapply(.SD,function(x){encode_woe(x, dvcol)$fit}),.SDcols=cat_cols]
    woe_fit <- list()
    for(i in cat_cols)
    {
      woe_fit[[i]] <- encode_woe(data[,i], dvcol)$fit
    }

    james_stein_encoded <- data.table(data)[,lapply(.SD,function(x){encode_james_stein(x, dvcol)$encoded}),.SDcols=cat_cols]
    james_stein_encoded <- cbind(james_stein_encoded, dvcol)
    # james_stein_fit <- data.table(data)[,lapply(.SD,function(x){encode_james_stein(x, dvcol)$fit}),.SDcols=cat_cols]
    james_stein_fit <- list()
    for(i in cat_cols)
    {
      james_stein_fit[[i]] <- encode_james_stein(data[,i], dvcol)$fit
    }

    m_estimator_encoded <- data.table(data)[,lapply(.SD,function(x){encode_m_estimator(x, dvcol)$encoded}),.SDcols=cat_cols]
    m_estimator_encoded <- cbind(m_estimator_encoded, dvcol)
    # m_estimator_fit <- data.table(data)[,lapply(.SD,function(x){encode_m_estimator(x, dvcol)$fit}),.SDcols=cat_cols]
    m_estimator_fit <- list()
    for(i in cat_cols)
    {
      m_estimator_fit[[i]] <- encode_m_estimator(data[,i], dvcol)$fit
    }

  }



  total_matrix_all <- list("Dummy Encoding" = dummy_encoded,
                           "Simple Encoding" = simple_encoded,
                           "Deviation Coding" = deviation_encoded,
                           "Polynomial Encoding" = poly_encoded,
                           "Reverse Helmert Encoding" = rev_helmert_encoded,
                           "Helmert Encoding" = helmert_encoded,
                           "Forward Difference Encoding" = fwd_diff_encoded,
                           "Backward Difference Encoding" = bwd_diff_encoded,
                           "Label Encoding" = label_encoded,
                           "One-Hot Encoding" = one_hot_encoded,
                           "Binary Encoding" = binary_encoded,
                           "Frequency Encoding" = freq_encoded,
                           "Hash Encoding" = hash_encoded,
                           "Probability Ratio Encoding" = prob_ratio_encoded,
                           "Target Encoding" = target_encoded,
                           "Leave One Out Encoding" = leave_one_out_encoded,
                           "Weight Of Evidence Encoding" = woe_encoded,
                           "James Stein Encoding" = james_stein_encoded,
                           "M-Estimator Encoding" = m_estimator_encoded)
  encoding_fits <- list(dummy_fit, simple_fit, deviation_fit, poly_fit, rev_helmert_fit, helmert_fit, fwd_diff_fit, bwd_diff_fit, label_fit,one_hot_fit, binary_fit, freq_fit, hash_fit, prob_ratio_fit, target_fit, leave_one_out_fit, woe_fit, james_stein_fit, m_estimator_fit)

  for (i in names(total_matrix_all))
  {
    # print(i)
    if (anyNA(total_matrix_all[[i]]))
      total_matrix_all[[i]] <- NULL
  }

  # library(pbmcapply)
  output<-do.call(rbind,pbmclapply(seq(1:length(total_matrix_all)), model_my_data,
                                   data = total_matrix_all,
                                   mc.cores = 1))

  if (dist[dist$is_dv == T, ]$distribution == "Continous")
  {
    output <- data.frame(output[, c("Rsquared", "MAE", "RMSE")])
    var <- c("Rsquared", "MAE", "RMSE")
  } else
  {
    if (length(unique(data.frame(data)[, dv])) == 2)
    {
      output <- data.frame(output[, c("F", "Precision", "Recall")])
      var <- c("F", "Precision", "Recall")
    } else
    {
      output <- data.frame(output[, c("Mean_F1", "Mean_Precision", "Mean_Recall")])
      var <- c("Mean_F1", "Mean_Precision", "Mean_Recall")
    }
  }

  output$Method <- names(total_matrix_all)
  best_encoder <- names(total_matrix_all[which(output[,1] == max(output[,1]))[1]])
  encoded_data <- total_matrix_all[[best_encoder]]
  encoding_fit_model <- list("Method" = best_encoder, "fit" = encoding_fits[[which(output[,1] == max(output[,1]))[1]]])

  output <- output[,c("Method", var)]

  return(list(encoded_data = encoded_data, encoding_fit_model = encoding_fit_model, model_perf_metrics = output))

}
