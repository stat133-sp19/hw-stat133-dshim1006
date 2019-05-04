#1 check_prob
# @title check proability
# @description check if an input is a valid probability
# @param prob numeric 0 <= p <= 1
# @return logical values
check_prob <- function(prob) {
  if (0 <= prob & prob <=1) {
    return(TRUE)
  } else {
    stop("invalid prob value")
  }
}

#2 check_trials
# @title check trials
# @description check if an input trials is a valid value for number of trials
# @param trials numeric greater than or equal to 0
# @return logical values
check_trials <- function(trials) {
  if (trials >= 0) {
    return(TRUE)
  } else {
    stop("invalid trials value")
  }
}

#3 check_success
# @title check success
# @description check if an input success is a valid value for number of successes
# @param trials numeric 0 <= k <= n
# @param success numeric 0 <= k <= n
# @return logical values
check_success <- function(success, trials) {
  for (i in 1:length(success)) {
    if (0 > success[i]  | success[i] > trials) {
      stop("'invalid success value")
    }
  }
  return(TRUE)
}

#4 aux_mean
# @title aux mean
# @description expected number of successes in n trials
# @param trials n
# @param prob p
# @return expected mean
aux_mean <- function(n, p) {
  return(n * p)
}


#5 aux_variance
# @title aux variance
# @description expected variance
# @param trials n
# @param prob p
# @return expected variance
aux_variance <- function(n, p) {
  return((n * p * (1 - p)))
}



#6 aux_mode
# @title aux mode
# @description expected mode
# @param trials n
# @param prob p
# @return expected mode
aux_mode <- function(n, p) {
  m = n * p + p
  if(m %% 1 == 0) {
    return(c(m, m-1))
  } else {
    return(as.integer(m))
  }
}



#7 aux_skewness
# @title aux skewness
# @description expected skewness
# @param trials n
# @param prob p
# @return expected skewness
aux_skewness <- function(n, p) {
  skewness = (1 - 2*p) / ((n * p * (1 - p)) ** (1/2))
  return(skewness)
}


#8 aux_kurtosis
# @title aux kurtosis
# @description expected kurtosis
# @param trials n
# @param prob p
# @return expected kurtosis
aux_kurtosis <- function(n, p) {
  kurtosis = ((1 - 6*p) * (1 - p)) / (n*p * (1 - p))
  return(kurtosis)
}

#1.3 bin_choose
#' @title bin choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n trials
#' @param k success
#' @return n choose k
#' @examples
#' bin_choose(5, 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
#' @export
bin_choose <- function(n, k) {
  if (check_trials(n) == TRUE & check_success(k, n) == TRUE) {
    return(factorial(n) / (factorial(k) * factorial(n - k)))
  }
}

#1.4 bin_probability
#' @title bin probability
#' @description calculates the probability
#' @param  n trials
#' @param k success
#' @param p prob
#' @return probability
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#' @export
bin_probability <- function(k, n, p) {
  if (check_trials(n) == TRUE & check_success(k, n) == TRUE & check_prob(p)) {
    choose0 <- bin_choose(n, k)
    return(choose0 * (p^k) * (1-p)^(n-k))
  }
}

#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_distribution <- function(n, p){
  success = c(0:n)
  df <- data.frame("success" = success, "probability" = bin_probability(success, n, p))
  class(df) <- c("bindis", "data.frame")
  return(df)
}




#' @export
plot.bindis <- function(b) {
  barplot(b$probability, ylab="Probability",
          xlab="Success", names.arg = b$success)
}

#1.6 bin_cumulative
#' @title bin cumulative
#' @description calculates the cumulation
#' @param n trials
#' @param p prob
#' @return cumulative
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' @export
bin_cumulative <- function(n, p){
  success = c(0:n)
  probability = bin_probability(success, n, p)
  df <- data.frame("success" = success, "probability" = probability, "cumulative" = cumsum(probability))
  class(df) <- c("bincum", "data.frame")
  return(df)
}

#' @export
plot.bincum <- function(c) {
  plot(c$success, c$cumulative, type="o", ylab="Probability",xlab="Success")
}



#1.7 bin_variable
#' @title bin variable
#' @description calculates the variable
#' @param n trials
#' @param p prob
#' @return object of class "binvar"
#' @export
bin_variable <- function(n, p){
  if (check_trials(n) & check_prob(p)) {
    var <- list(trials = n, prob = p)
    class(var) <- "binvar"
    return(var)
  }
}
#' @export
print.binvar <- function(v) {
  cat('"Binomial variable"\n',
      '\n',
      'Parameters\n',
      '- number of trials:', v[[1]],
      '- prob of success : ', v[[2]]
      )
}

#' @export
summary.binvar <- function(v) {
  n <- v[[1]]
  p <- v[[2]]
  a_mean <- aux_mean(n, p)
  a_var <- aux_variance(n, p)
  a_mode <- aux_mode(n, p)
  a_skew <- aux_skewness(n, p)
  a_kurt <- aux_kurtosis(n, p)
  all <- list(trials = n, prob = p, a_mean, a_var, a_mode,  a_skew, a_kurt)
  class(all) <- "binvar"
  return(all)
}

#' @export
print.summary.binvar <- function(v) {
  cat('"Summaary Binomial"\n',
      '\n',
      'Parameters\n',
      '- number of trials:', v[[1]],
      '- prob of success : ', v[[2]],
      '\n',
      '"Measures"',
      '- mean    :', a_mean,
      '- variance:', a_var,
      '- mode    :', a_mode,
      '- skewness:', a_skew,
      '- kurtosis:', a_kurt)
}




#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_mean <- function(n, p) {
  if (check_trials(n) & check_prob(p)) {
    return(aux_mean(n,p))
  }
}




#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_variance <- function(n, p) {
  if (check_trials(n) & check_prob(p)) {
    return(aux_variance(n, p))
  }
}



#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_mode <- function(n, p) {
  if (check_trials(n) & check_prob(p)) {
    return(aux_mode(n, p))
  }
}



#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_skewness <- function(n, p) {
  if (check_trials(n) & check_prob(p)) {
    return(aux_skewness(n, p))
  }
}



#1.5 bin_distribution
#' @title bin distribution
#' @description calculates the distribution
#' @param n trials
#' @param p prob
#' @return probability
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' @export
bin_kurtosis <- function(n, p) {
  if (check_trials(n) & check_prob(p)) {
    return(aux_kurtosis(n, p))
  }
}









