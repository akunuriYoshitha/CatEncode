#' Contrast Encoding using Dummy Coding
#'
#' Takes in a vector and encodes using Dummy Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_dummy(x, dv)
encode_dummy <- function(x, dv)
{
  x <- factor(x)
  df <- data.frame(x, dv)
  contrasts(x) = contr.treatment(length(unique(x)))
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Simple Coding
#'
#' Takes in a vector and encodes using Simple Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_simple(x, dv)
encode_simple <- function(x, dv)
{
  x <- factor(x)
  df <- data.frame(x, dv)
  c<-contr.treatment(length(unique(x)))
  my.coding<-matrix(rep(1/length(unique(x)), length(unique(x)) * (length(unique(x)) - 1)), ncol=length(unique(x)) - 1)
  my.simple<-c-my.coding
  my.simple
  contrasts(x)<-my.simple
  ss <- summary(lm(dv~x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Deviation Coding
#'
#' Takes in a vector and encodes using Deviation Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_deviation(x, dv)
encode_deviation <- function(x, dv)
{
  x <- factor(x)
  df <- data.frame(x, dv)
  contrasts(x) = contr.sum(length(unique(x)))
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Polynomial Coding
#'
#' Takes in a vector and encodes using Polynomial Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_poly(x, dv)
encode_poly <- function(x, dv)
{
  x <- factor(x)
  df <- data.frame(x, dv)
  contrasts(x) = contr.poly(length(unique(x)))
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Reverse Helmert Coding
#'
#' Takes in a vector and encodes using Reverse Helmert Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_rev_helmert(x, dv)
encode_rev_helmert <- function(x, dv)
{
  x <- factor(x)
  df <- data.frame(x, dv)
  contrasts(x) = contr.helmert(length(unique(x)))
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Helmert Coding
#'
#' Takes in a vector and encodes using Helmert Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_helmert(x, dv)
encode_helmert <- function(x, dv)
{
  # require(rlist)
  x <- factor(x)
  df <- data.frame(x, dv)
  k <- length(unique(x))
  cont_list <- list()
  for (i in 1:(k-1))
  {
    for (j in 1:i)
    {
      ifelse(j < i, cont_list <- list.append(cont_list, 0), cont_list <- list.append(cont_list, (k - i)/(k - i + 1)))
    }
    for (h in (j+1):k)
    {
      cont_list <- list.append(cont_list, -1/(k - i + 1))
    }
  }
  unlist(cont_list)
  helmert.contrasts <- matrix(unlist(cont_list), ncol = k - 1)
  rownames(helmert.contrasts) <- rownames(contrasts(x))
  colnames(helmert.contrasts) <- colnames(contrasts(x))
  contrasts(x) = helmert.contrasts
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Forward Difference Coding
#'
#' Takes in a vector and encodes using Forward Difference Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_fwd_diff(x, dv)
encode_fwd_diff <- function(x, dv)
{
  # require(rlist)
  x <- factor(x)
  df <- data.frame(x, dv)
  k <- length(unique(x))
  cont_list <- list()
  for (i in 1:(k-1))
  {
    for (j in 1:i)
    {
      cont_list <- list.append(cont_list, (k - i)/k)
    }
    for (h in (j+1):k)
    {
      cont_list <- list.append(cont_list, -i/k)
    }
  }
  my.forward.diff <- matrix(unlist(cont_list), ncol = k - 1)
  x <- factor(x)
  contrasts(x) = my.forward.diff
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Contrast Encoding using Backward Difference Coding
#'
#' Takes in a vector and encodes using Backward Difference Coding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_bwd_diff(x, dv)
encode_bwd_diff <- function(x, dv)
{
  # require(rlist)
  x <- factor(x)
  df <- data.frame(x, dv)
  k <- length(unique(x))
  cont_list <- list()
  for (i in 1:(k-1))
  {
    for (j in 1:i)
    {
      cont_list <- list.append(cont_list, (k - i)/k)
    }
    for (h in (j+1):k)
    {
      cont_list <- list.append(cont_list, -i/k)
    }
  }
  my.backward.diff <- matrix(unlist(lapply( cont_list, FUN= function(x) x*-1)), ncol = k - 1)
  x <- factor(x)
  contrasts(x) = my.backward.diff
  ss <- summary(lm(dv ~ x, df))
  estimates <- data.frame(ss$coefficients)$Estimate
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Label/Integer Encoding
#'
#' Takes in a vector and encodes using Label/Integer Encoding
#' @param x Any categorical vector which needs to be encoded
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_label(x)
encode_label <- function(x)
{
  x <- factor(x)
  estimates <- seq(length(levels(x)))
  names(estimates) <- levels(x)
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' One-Hot Encoding
#'
#' Takes in a vector and encodes using One-Hot Encoding
#' @param df A dataset with atleast one categorical field
#' @param colname A string with field name of categorical field
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_onehot(df, colname)
encode_onehot <- function(df, colname)
{
  # # library(caret)
  # df <- data.frame(df)
  # df[, c(colname)] <- factor(df[, c(colname)])
  # df_dummy <- df
  # vars <- dummyVars(paste(" ~ ", colname), data = df)
  # new_df <- data.frame(predict(vars, newdata = df_dummy))
  # return (list("encoded" = new_df, "fit" = colnames(new_df)))

  # library(mltools)
  df <- data.frame(df)
  df[, c(colname)] <- factor(df[, c(colname)])
  new_df <- one_hot(data.table(df), colname, sparsifyNAs=TRUE)
  new_df <- data.frame(new_df)[, !colnames(new_df) %in% colnames(df)]
  return (list("encoded" = new_df, "fit" = colnames(new_df)))
}

#' Binary Encoding
#'
#' Takes in a vector and encodes using Binary Encoding
#' @param x A vector that needs to be encoded
#' @param name A string which needs to given as a field name after encoding
#' @return Returns a vector with encoded data
#' @export
#' @usage binary_encoding(x, name)
binary_encoding <- function(x, name = "v_")
{
  # library(binaryLogic)
  x <- as.numeric(factor(x, levels = unique(x), exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  rownames(d) <- NULL
  colnames(d) <- paste0(name, 1:maxlen)
  d
}

#' Binary Encoding - Supporting Function
#'
#' Takes in a vector and encodes using Integer Encoding and then passes the result to Binery Encoding
#' @param x A categorical field that needs to be Binary encoded
#' @param colname A string with field name of categorical field
#' @param returnFit A boolean indicating if the result should be a fit or the encoded values
#' @return Returns encoded data when returnFit parameter is FALSE and fit file to fit the test data otherwise
#' @export
#' @usage encode_binary(x, colname, returnFit)
encode_binary <- function(x, colname, returnFit = FALSE)
{

  x <- factor(x)
  estimates <- seq(length(levels(x)))
  names(estimates) <- levels(x)
  if (returnFit == T)
    return(estimates)
  encoded <- unname(estimates[x])
  return (binary_encoding(encoded, name = paste0(colname, "_")))
}

#' Frequency Encoding
#'
#' Takes in a vector and encodes using Frequency Encoding
#' @param x Any categorical vector which needs to be encoded
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_freq(x)
encode_freq <- function(x)
{
  x <- factor(x)
  freq_table <- data.frame(table(x))
  estimates <- freq_table$Freq
  names(estimates) <- freq_table[[1]]
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Hashing Encoding
#'
#' Takes in a vector and encodes using Hashing Encoding
#' @param df A dataset with atleast one categorical field
#' @param colname A string with field name of categorical field
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_hash(df, colname)
encode_hash <- function(df, colname)
{
  df[, c(colname)] <- factor(df[, c(colname)])
  # library(FeatureHashing)
  m.mat <- hashed.model.matrix(c(colname), df, hash.size = 2 ^ 10, create.mapping = TRUE)
  estimates <- hash.mapping(m.mat)
  encoded <- unname(estimates[df[, c(colname)]])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Probability Ratio Encoding
#'
#' Takes in a vector and encodes using Probability Ratio
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_prob_ratio(x, dv)
encode_prob_ratio <- function(x, dv)
{
  target_mean <- aggregate(dv, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  colnames(target_mean) <- c("label", "dv_1")
  target_mean$dv_0 <- 1-target_mean$dv_1
  if (length(target_mean[target_mean$dv_0 == 0, ]$dv_0) != 0)
    target_mean[target_mean$dv_0 == 0, ]$dv_0 <- 0.000001
  target_mean$PR <- target_mean$dv_1 / target_mean$dv_0
  estimates <- target_mean$PR
  names(estimates) <- target_mean$label
  encoded <- unname(estimates[x])
  return (list("encoded" = encoded, "fit" = estimates))
}

#' Mean Target Encoding
#'
#' Takes in a vector and encodes using Mean target encoding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_target(x, dv)
encode_target <- function(x, dv)
{
  d <- aggregate(dv, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  m <- d[is.na(as.character(d[, 1])), 2]
  l <- d[, 2]
  names(l) <- d[, 1]
  fit <- l
  l <- l[x]
  l[is.na(l)] <- m
  encoded <- unlist(l)
  return (list("encoded" = unname(encoded), "fit" = fit))
}

#' Leave One Out Encoding
#'
#' Takes in a vector and encodes using Leave One Out encoding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_leave_one_out(x, dv)
encode_leave_one_out <- function(x, dv)
{
  n <- length(x)
  x <- as.numeric(x)
  x[is.na(x)] <- "__MISSING"
  x <- factor(x)
  x2 <- vapply(1:n, function(i) {
    xval <- x[i]
    yloo <- dv[-i]
    xloo <- x[-i]
    yloo <- yloo[xloo == xval]
    mean(yloo, na.rm = TRUE)
  }, numeric(1))

  return (list("encoded" = x2))
}

#' Weight Of Evidence Encoding
#'
#' Takes in a vector and encodes using Weight Of Evidence encoding
#' @param x Any categorical vector which needs to be encoded
#' @param dv Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_woe(x, dv)
encode_woe <- function(x, dv)
{
  d <- aggregate(dv, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  d[["woe"]] <- log(((1 / d[, 2]) - 1) *
                      (sum(dv) / sum(1-dv)))
  m <- d[is.na(as.character(d[, 1])), 3]
  l <- d[, 3]
  names(l) <- d[, 1]
  fit <- l
  l <- l[x]
  l[is.na(l)] <- m

  return (list("encoded" = unname(l), "fit" = fit))
}

#' James Stein Encoding
#'
#' Takes in a vector and encodes using James Stein encoding
#' @param x Any categorical vector which needs to be encoded
#' @param y Dependent variable of the dataset
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_james_stein(x, y)
encode_james_stein <- function(x, y)
{
  n_all <- length(y)
  p_all <- mean(y)
  var_all <- (p_all * (1 - p_all)) / n_all

  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  d2 <- aggregate(y, list(factor(x, exclude = NULL)), length)
  g <- names(d)[1]
  d <- merge(d, d2, by = g, all = TRUE)
  d[, 4] <- (d[, 2] * (1 - d[, 2])) / d[, 3]
  d[, 5] <- d[, 4] / (d[, 4] + var_all)
  d[, 6] <- (1 - d[, 5]) * d[, 2] + d[, 5] * p_all

  m <- d[is.na(as.character(d[, 1])), 6]
  l <- d[, 6]
  names(l) <- d[, 1]
  fit  <- l
  l <- l[x]
  l[is.na(l)] <- m

  return (list("encoded" = unname(l), "fit" = fit))
}

#' M-Estimator Encoding
#'
#' Takes in a vector and encodes using M-Estimator encoding
#' @param x Any categorical vector which needs to be encoded
#' @param y Dependent variable of the dataset
#' @param m M value defaulted to 1
#' @return Returns a list with encoded data and fit file to fit the test data
#' @export
#' @usage encode_m_estimator(x, y, m)
encode_m_estimator <- function(x, y, m = 1)
{
  p_all <- mean(y)

  d <- aggregate(y, list(factor(x, exclude = NULL)), sum, na.rm = TRUE)
  d2 <- aggregate(y, list(factor(x, exclude = NULL)), length)
  g <- names(d)[1]
  d <- merge(d, d2, by = g, all = TRUE)
  d[, 4] <- (d[, 2] + p_all * m) / (d[, 3] + m)

  m <- d[is.na(as.character(d[, 1])), 4]
  l <- d[, 4]
  names(l) <- d[, 1]
  fit <- l
  l <- l[x]
  l[is.na(l)] <- m

  return (list("encoded" = unname(l), "fit" = fit))
}
