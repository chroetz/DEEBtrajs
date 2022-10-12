test_that("single trajId 1D targetTime", {
  traj <- makeTrajs(time = c(0,2), state = c(0,2))
  target <- makeTrajs(time = 0:2, state = 0:2)
  trajIp <- interpolateTrajs(traj, target$time)
  expect_identical(trajIp, target)
})

test_that("single trajId 1D intersteps", {
  traj <- makeTrajs(time = c(0,2,6), state = c(0,2,6))
  trajIp <- interpolateTrajs(traj, interSteps=1)
  expect_identical(traj, trajIp)

  traj <- makeTrajs(time = c(0,2), state = c(0,2))
  target <- makeTrajs(time = 0:2, state = 0:2)
  trajIp <- interpolateTrajs(traj, interSteps=2)
  expect_identical(trajIp, target)

  traj <- makeTrajs(time = c(2,4,8), state = c(2,4,8))
  target <- makeTrajs(time = c(2:4,6,8), state = c(2:4,6,8))
  trajIp <- interpolateTrajs(traj, interSteps=2)
  expect_identical(trajIp, target)

  traj <- makeTrajs(time = c(0,3,9), state = c(0,3,9))
  target <- makeTrajs(time = c(0:3,5,7,9), state = c(0:3,5,7,9))
  trajIp <- interpolateTrajs(traj, interSteps=3)
  expect_identical(trajIp, target)
})

test_that("single trajId 3D targetTime", {
  traj <- makeTrajs(time = c(0,2), state = matrix(c(0,2, 0,4, 1,3), ncol=3))
  target <- makeTrajs(time = 0:2, state = matrix(c(0:2, 0,2,4, 1:3), ncol=3))
  trajIp <- interpolateTrajs(traj, target$time)
  expect_identical(trajIp, target)
})

test_that("single trajId 3D intersteps", {
  traj <- makeTrajs(time = c(0,2), state = matrix(c(0,2, 0,4, 1,3), ncol=3))
  target <- makeTrajs(time = 0:2, state = matrix(c(0:2, 0,2,4, 1:3), ncol=3))
  trajIp <- interpolateTrajs(traj, interSteps=2)
  expect_identical(trajIp, target)
})

test_that("multi trajId targetTime", {
  traj <- makeTrajs(trajId = c(1,1,2), time = c(0,2,1), state = c(0,2,1))
  target <- makeTrajs(
    trajId = rep(1:2, each=3),
    time = rep(0:2, times=2),
    state = c(0:2, rep(1,3)))
  trajIp <- interpolateTrajs(traj, 0:2)
  expect_identical(trajIp, target)
})
