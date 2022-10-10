test_that("single trajId 1D", {
  traj <- makeTrajs(time = c(0,2), state = c(0,2))
  target <- makeTrajs(time = 0:2, state = 0:2)
  trajIp <- interpolateTrajs(traj, target$time)
  expect_identical(trajIp, target)
})

test_that("single trajId 3D", {
  traj <- makeTrajs(time = c(0,2), state = matrix(c(0,2, 0,4, 1,3), ncol=3))
  target <- makeTrajs(time = 0:2, state = matrix(c(0:2, 0,2,4, 1:3), ncol=3))
  trajIp <- interpolateTrajs(traj, target$time)
  expect_identical(trajIp, target)
})

test_that("multi trajId", {
  traj <- makeTrajs(trajId = c(1,1,2), time = c(0,2,1), state = c(0,2,1))
  target <- makeTrajs(
    trajId = rep(1:2, each=3),
    time = rep(0:2, times=2),
    state = c(0:2, rep(1,3)))
  trajIp <- interpolateTrajs(traj, 0:2)
  expect_identical(trajIp, target)
})
