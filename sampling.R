sampleBinary <- function(n = 50, J = 1, b = rep(log(1), J), alpha = NULL, col.names = NULL, prob.x = .5) {
  J <- length(b)
  if (is.null(alpha)) alpha <- -log(sqrt(prod(exp(b))))
  if (is.null(col.names)) col.names <- c("Y", paste("X", 1:J, sep = ""))
  coefs <- c(alpha, b)
  x  <- cbind(1, matrix(rbinom(n * J, size = 1, prob = prob.x), nrow = n, ncol = J))
  lp <- coefs %*% t(x)
  p  <- metamisc:::inv.logit(lp)
  y  <- stats::rbinom(length(lp), size = 1, prob = p)
  
  out <- data.frame(cbind(y,x[ , -1]))
  colnames(out) <- col.names
  out
}

sampleBinaryIPDMA <- function(n = 50, J = 1, 
                              b = rep(log(1), J), k = 10, b.sd = 1, alpha.sd = 1, 
                              prob.x = .5, prob.x.logit.sd = 1, ...) {
  J <- length(b)
  bb <- matrix(rnorm(k * length(b), b, .1), ncol = k, nrow = length(b))
  aa <- -log(sqrt(prod(exp(as.vector(b))))) + rnorm(k, 0, alpha.sd)
  prob.xx <- metamisc:::inv.logit(metamisc:::logit(prob.x) + rnorm(k, 0, prob.x.logit.sd))
  
  dd <- list()
  for (i in seq_len(k)) {
    dd[[length(dd) + 1]] <- cbind(sampleBinary(n = n, J = J, b = bb[ , i], alpha = aa[i], prob.x = prob.xx[i], ...), cluster = i)
  }

  dd
  do.call(rbind, dd)
}

# b <- c(1,-1, 0)
# k <- 10
# 
# 
# sampleBinaryIPDMA(b = b, k = k, n = 3)
# sampleBinary(b = c(1,2), n = 3, J =2, alpha = 2, prob.x = .2)
