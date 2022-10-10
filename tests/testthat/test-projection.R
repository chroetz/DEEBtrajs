testRandomProj <- function(d, targetDim, n) {
  y <- matrix(rnorm(n*d), nrow=n)
  proj <- calculateProjection(y, targetDim)
  reembedded <- proj$embed(proj$project(y))
  rank <- sum(stats::prcomp(reembedded)$sdev > sqrt(.Machine$double.eps))
  expect_equal(rank, min(targetDim, d, n))
  totalVar <- sum((y-rep(colMeans(y), each=nrow(y)))^2)
  remainingVar <- sum((y-reembedded)^2)
  expect_lt(remainingVar, totalVar*targetDim/min(n,d))
}

test_that("random", {
 testRandomProj(2, 2, 4)
 testRandomProj(3, 3, 4)
 testRandomProj(3, 2, 4)
 testRandomProj(4, 2, 4)
 testRandomProj(4, 3, 4)
 testRandomProj(2, 1, 4)
 testRandomProj(1, 1, 4)
 testRandomProj(2, 1, 2)
 testRandomProj(3, 1, 2)
 testRandomProj(3, 2, 20)
})
