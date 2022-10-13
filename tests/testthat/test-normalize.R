randomTrajs <- function(d, n) {
  makeTrajs(
    time = 0,
    state = matrix(rnorm(n*d), ncol=d, nrow=n))
}

testRandom <- function(d, n) {
  trajs <- randomTrajs(d, n)
  norm <- calculateNormalization(trajs)
  normedTrajs <- norm$normalize(trajs)
  normedMean <- colMeans(normedTrajs$state)
  normedCov <- cov(normedTrajs$state)
  expect_equal(normedMean, rep(0, length(normedMean)))
  if (nrow(trajs$state) >= 2) {
    expect_equal(normedCov, diag(nrow=nrow(normedCov)))
  }
  expect_equal(norm$denormalize(normedTrajs), trajs)
}

testSingularRandom <- function(d, n) {
  trajs <- randomTrajs(d, n)
  norm <- calculateNormalization(trajs)
  normedTrajs <- norm$normalize(trajs)
  normedMean <- colMeans(normedTrajs$state)
  expect_equal(normedMean, rep(0, length(normedMean)))
  expect_equal(norm$denormalize(normedTrajs), trajs)
  if (n >= 2) expect_equal(apply(normedTrajs$state, 2, var), rep(1, d))
}

testRandomDeriv <- function(d, n) {
  d <- 2
  n <- 100
  trajs <- makeDerivTrajs(
    state = diag(d)
  )
  trajsNew <- makeDerivTrajs(
    state = rbind(diag(d), matrix(0, nrow=n, ncol=d)),
    deriv = rbind(diag(d), matrix(rnorm(n*d), nrow=n, ncol=d))
  )
  nrm <- calculateNormalization(trajs)
  trajsNewNormed <- nrm$normalize(trajsNew)
  A <- trajsNewNormed$deriv[1:d,]
  expect_equal(trajsNewNormed$deriv, trajsNew$deriv %*% A)
  expect_equal(nrm$denormalize(trajsNewNormed), trajsNew)
}

testSingular <- function(d) {
  d <- 2
  trajs <- makeDerivTrajs(
    state = diag(d)
  )
  nrm <- calculateNormalization(trajs)
  trajsNormed <- nrm$normalize(trajs)
  expect_equal(apply(trajsNormed$state, 2, var), rep(1, d))
}

test_that("random", {
  testRandom(1, 2)
  testRandom(2, 3)
  testRandom(1, 2)
  testRandom(2, 3)
  testRandom(3, 4)
  testRandom(1, 3)
  testRandom(3, 4)
  testRandom(2, 3)
  testRandom(3, 4)
  testRandom(3, 10)
  testRandom(10, 12)
})

test_that("deriv", {
  testRandomDeriv(1, 1)
  testRandomDeriv(2, 1)
  testRandomDeriv(1, 2)
  testRandomDeriv(2, 2)
  testRandomDeriv(3, 1)
  testRandomDeriv(1, 3)
  testRandomDeriv(3, 3)
  testRandomDeriv(2, 3)
  testRandomDeriv(3, 2)
  testRandomDeriv(3, 10)
  testRandomDeriv(10, 3)
})

test_that("singular", {
  testSingular(1)
  testSingular(2)
  testSingular(3)
  testSingularRandom(1, 1)
  testSingularRandom(2, 2)
  testSingularRandom(3, 3)
  testSingularRandom(1, 2)
  testSingularRandom(1, 3)
  testSingularRandom(2, 3)
})
