load("SIECV-sim-date_20200817.RData")

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


