#1.1
#Check Prob
#to check if the probability is correct (between 0 and 1)
check_prob <- function(prob){
  if (prob<0 | prob >1) {
    stop("p has to be a number betwen 0 and 1")
  }
  TRUE
}

#Check trials
#to check if the trials variable is correct (equal or larger than zero)
check_trials <- function(trials){
  if (trials<0){
    stop("invalid trials value")
  }
  TRUE
}

#Check success
#to check if the success and trials variables are correct (0<= k <= n)
check_success <- function(trials,success){
  for(i in 1:length(success)){
    if (success[i]>trials){
      stop("success cannot be greater than trials")
    } else if (success[i]<0){
      stop("invalid success value")
    } else if (trials<0){
      stop("invalid trials value")
    }
  }
  TRUE
}

#1.2
#aux mean
#a function to get the mean
aux_mean <- function(trials,prob){
  mean <- trials*prob
  return(mean)
}

#aux_variance
#a function to get the variance
aux_variance <- function(trials,prob){
  mean <- trials*prob
  variance <- mean*(1-prob)
  return(variance)
}

#aux_mode
#a function to get the mode
aux_mode <- function(trials,prob){
  mean <- trials*prob
  if (round(mean+prob)!=(mean+prob)){
    mode <- round(mean+prob)
    return(mode)} else if (round(mean+prob)==(mean+prob)){
      a <- mean+prob
      b <- (mean+prob-1)
      mode1 <- c(a,b)
      return(mode1)
    }
}
aux_mode(5,0.5)
#aux_skewness
#a function to get the skewness
aux_skewness <- function(trials,prob){
  mean <- trials*prob
  variance <- mean*(1-prob)
  skewness <- (1-2*prob)/(sqrt(variance))
  return(skewness)
}

#aux_kurtosis
#a function to get the kurtosis
aux_kurtosis <- function(trials,prob){
  mean <- trials*prob
  variance <- mean*(1-prob)
  kurtosis <- (1-6*prob*(1-prob))/variance
  return(kurtosis)
}

#1.3
#bin_choose
#' @title n choose k
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param trials(n), success(k)
#' @return choose
#' @export
#' @examples bin_choose(n = 5, k = 2) = 10
bin_choose <-  function(n,k){
  check_trials(n)
  check_success(n,k)
  choose = factorial(n)/(factorial(n-k)*factorial(k))
  return(choose)
}

#1.4
#bin_probability
#' @title binomial probability
#' @description to calculate probability of getting ? successes in ? trials, assuming prob of success = ?
#' @param success, trials, prob
#' @return bin_prob
#' @export
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5) = 0.3125
bin_probability <- function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(trials,success)
  bin_prob <- bin_choose(trials,success)*(prob^success)*((1-prob)^(trials-success))
  return(bin_prob)
}

#1.5
#bin_distribution
#' @title binomial distribution
#' @description to get a dataframe of binomial probability distribution for each trial
#' @param trials, prob
#' @return a data.frame with two classes: c("bindis", "data.frame")
#' @export
#' @examples bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob){
  success <- c()
  probability <- c()
  for (i in 0:trials){
    success=as.numeric(c(success,i))
    probability=as.numeric(c(probability,bin_probability(i,trials,prob)))
  }
  table <- data.frame(success,probability)
  class(table) <- c("bindis","data.frame")
  return(table)
}

#plot.bindis
#' @export
#' @examples dis1 <- bin_distribution(trials = 5, prob = 0.5) plot(dis1)
plot.bindis <- function(x,...){
  barplot(x$probability,xlab="success",ylab="probability")
}

#1.6
#bin_cumulative
#' @title binomial cumulative distribution
#' @description to get a dataframe of binomial cumulative distribution for each trial
#' @param trials, prob
#' @return  a data.frame with two classes: c("bincum", "data.frame")
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials,prob){
  success <- c()
  probability <- c()
  for (i in 0:trials){
    success=as.numeric(c(success,i))
    probability=as.numeric(c(probability,bin_probability(i,trials,prob)))
  }
  cumulative <- cumsum(probability)
  table <- data.frame(success,probability,cumulative)
  class(table) <- c("bincum","data.frame")
  return(table)
}

#plot.bincum
#' @export
#' @examples
#' cum1 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(cum1)
plot.bincum <- function(x,...){
  plot(x$cumulative,xlab="success",ylab="probability")+lines(x$cumulative)
}

#1.7
#bin_variable
#' @title binomial variable
#' @description a list that displaces the number of trials and the probability of success
#' @param trials, prob
#' @return an object of class "binvar", that is, a binomial random variable object
#' @export
#' @examples bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  list <- list(trials,prob)
  class(list)=c("binvar")
  return(list)
}

#print binvar
#' @export
print.binvar <- function(x,...){
  cat('Binomial variable \n\n')
  cat("Parameters","\n")
  cat("- number of trials:", x[[1]], "\n")
  cat("- prob of success:", x[[2]], "\n")
  invisible(x)
}

#summary
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' binsum1 <- summary(bin1)
#' binsum1
summary.binvar <- function(x,...){
  trials <- x[[1]]
  prob <- x[[2]]
  mean <- aux_mean(x[[1]],x[[2]])
  variance <- aux_variance(x[[1]],x[[2]])
  mode <- aux_mode(x[[1]],x[[2]])
  skewness <- aux_skewness(x[[1]],x[[2]])
  kurtosis <- aux_kurtosis(x[[1]],x[[2]])
  list <- list(trials,prob,mean,variance,mode,skewness,kurtosis)
  class(list) <- c("summary.binvar")
  return(list)
}


#print summary binvar
#' @export
#' @examples
#'  bin1 <- bin_variable(trials = 10, p = 0.3)
#'  print(bin1)
print.summary.binvar <- function(x,...){
  cat('Summary Binomial \n\n')
  cat("Parameters","\n")
  cat("- number of trials:", x[[1]], "\n")
  cat("- prob of success:", x[[2]], "\n\n")
  cat('Measures \n')
  cat("- mean:", x[[3]], "\n")
  cat("- variance:", x[[4]], "\n")
  cat("- mode:", x[[5]], "\n")
  cat("- skewness:", x[[6]], "\n")
  cat("- kurtosis:", x[[7]], "\n")
  invisible(x)
}

#1.8
#functions of measures
#bin mean
#' @title mean of the binomial distribution
#' @description calculate the mean
#' @param trials, prob
#' @return mean
#' @export
#' @examples bin_mean(10, 0.3)
bin_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  mean <- aux_mean(trials,prob)
  return(mean)
}

#bin_variance
#' @title variance of the binomial distribution
#' @description calculate the variance
#' @param trials, prob, bbb
#' @return variance
#' @export
#' @examples bin_variance(10, 0.3)
bin_variance <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  variance <- aux_variance(trials,prob)
  return(variance)
}

#bin_mode
#' @title mode of the binomial distribution
#' @description calculate the mode
#' @param trials, prob
#' @return mode
#' @export
#' @examples bin_mode(10, 0.3)
bin_mode <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  mode <- aux_mode(trials,prob)
  return(mode)
}

#bin_skewness
#' @title skewness of the binomial distribution
#' @description calculate the skewness
#' @param trials, prob
#' @return skewness
#' @export
#' @examples bin_skewness(10, 0.3)
bin_skewness <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  skewness <- aux_skewness(trials,prob)
  return(skewness)
}

#bin_kurtosis
#' @title kurtosis of the binomial distribution
#' @description calculate the kurtosis
#' @param trials, prob
#' @return kurtosis
#' @export
#' @examples bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  kurtosis <- aux_kurtosis(trials,prob)
  return(kurtosis)
}

