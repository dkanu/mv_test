###############################
## Helpers
###############################
RBetaWrapper <- function(x, r){
  rbeta(r, (x[[2]] + 1), (x[[1]] - x[[2]]) + 1)
  }
QBetaWrapper <- function(x, i){
  qbeta(i, (x[[2]] + 1), (x[[1]] - x[[2]]) + 1)
  }
###############################
## MultiVariant Testing
###############################
MVTest <- function(r = 10^5, n = c(100, 100), x = c(5, 7), i = c(0.025, 0.975), rnd = 6){
  # MultiVaraint testing using bayesian sampling from beta distributions
  # Args:
  #   r: number of simulated draws from beta distribution
  #   n: trials vector
  #   x: successes vector
  #   i: crediblity interval range
  #   rnd: integer specifying number decmimal places to round credibility interval endpoints
  # Returns: Trail and Success input information, comparative predicited long run performance probabilities, credibility intervals
  set.seed(777)
  nx.list <- split(cbind(n,x), seq(nrow(cbind(n,x))))
  samples <- lapply(nx.list, RBetaWrapper, r = r)
  intervals <- (lapply(nx.list, QBetaWrapper, i = i))
  bindedcols <- Reduce(cbind, samples)
  maxlist <- lapply(split(bindedcols, seq(nrow(bindedcols))), which.max)
  
  data.in <- (t(unname(data.frame(nx.list))))
  colnames(data.in) <- c('trials', 'successes')
  
  out <- list(data.in,
              table(unlist(maxlist))/sum(table(unlist(maxlist))),
              apply(Reduce(rbind,intervals), c(1,2), round, rnd)
              )
  names(out) <- c("input", "probs", "cred.int")
  out
}

MVTest()
