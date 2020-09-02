library(metamisc)
source("sampling.R")

nsim <- 1000
n <- 250
n_v <- 10000
b <- c(1,-1, 0)
k <- 10


set.seed(2020)
seed <- coef0 <- coef0.5 <- coef1 <- coef0_bw <- coef0.5_bw <- coef1_bw <- list()
begin <- proc.time()

# nsim <- 3
for (sim in seq_len(nsim)) {
  # seed[[length(seed) + 1]] <- .Random.seed
  
  d <- sampleBinaryIPDMA(b = b, k = k, n = n, b.sd = 1, alpha.sd = 1, prob.x = .5, prob.x.logit.sd = 1)
  
  m0 <- metapred(data = d, strata = "cluster", formula = Y ~ 1, scope = Y ~ X1 + X2 + X3, estFUN = "logistfirth",
           center = TRUE, meta.method = "DL", genFUN = "rema", lambda = 0)
  m0.5 <- update(m0, lambda = 1/2)
  m1   <- update(m0, lambda = 1)
  
  m0_bw <- update(m0, formula = Y ~ X1 + X2 + X3, scope = Y ~ 1)
  m0.5_bw <- update(m0_bw, lambda = 1/2)
  m1_bw <- update(m0_bw, lambda = 1)
  
  coef0[[length(coef0) + 1]]     <- coef(m0)
  coef0.5[[length(coef0.5) + 1]] <- coef(m0.5)
  coef1[[length(coef1) + 1]]     <- coef(m1)
  
  coef0_bw[[length(coef0_bw) + 1]]     <- coef(m0_bw)
  coef0.5_bw[[length(coef0.5_bw) + 1]] <- coef(m0.5_bw)
  coef1_bw[[length(coef1_bw) + 1]]     <- coef(m1_bw)
  save.image("SIECV-sim-date_20200817.RData")
  print(sim)
}
end <- proc.time()
end - begin

all.equal(lapply(coef0, sort), lapply(coef0_bw, sort))
all.equal(lapply(coef0.5, sort), lapply(coef0.5_bw, sort))
all.equal(lapply(coef1, sort), lapply(coef1_bw, sort))

 
lapply(coef0, sort)[[52]]
lapply(coef0_bw, sort)[[52]]

lapply(coef0, sort)[[671]]
lapply(coef0_bw, sort)[[671]]



list.items.equal <- function(x, y) {
  id <- rep(NA, length(x))
  for (i in seq_along(x)) {
    id[i] <- isTRUE(all.equal(sort(x[[i]]), sort(y[[i]])))
  }
  id
}

mean_eq <- mean(list.items.equal(coef0, coef0_bw))
mean_eq 

save.image("SIECV-sim-date_20200817.RData")
