context("get.armour")


dummy_data <- matrix(NA, ncol = 22, nrow = 10)
dummy_data[,11] <- rep(1, 10) # All have a head
dummy_data[,11+1] <- c(rep(0, 5), rep(1, 5))
dummy_data[,11+2] <- rep(c(0,1), 5)
dummy_data[,15] <- c(rep(0, 5), rep(1, 5)) # top half has no body
dummy_data[,15+1] <- c(rep(0, 9), 1)
dummy_data[,15+2] <- c(rep(0, 9), 1)
dummy_data[,19] <- rep(c(0,1), 5) # Half have a tail
dummy_data[,19+1] <- c(0,1,0,0,0,1,0,0,0,1)
dummy_data[,19+2] <- c(0,1,0,0,0,0,0,0,0,0)

test_that("get.armour works", {

    ## Errors handling
    expect_error(get.armour("dummy_data"))
    expect_error(get.armour(dummy_data, body.part = "a"))
    expect_error(get.armour(dummy_data, armour.type = "a"))
    expect_error(get.armour(dummy_data, body.part = 4))
    expect_error(get.armour(dummy_data, armour.type = 3))

    ## Expected outputs
    expected <- list(
        c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE), # All
        c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE), # Head surface
        c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE), # Head projection
        c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE), # Head + Body surface
        c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE) # Tail surface + projection
        )

    ## Correct outputs
    expect_true(all(get.armour(dummy_data) == expected[[1]]))
    expect_true(all(get.armour(dummy_data, 1, 1) == expected[[2]]))
    expect_true(all(get.armour(dummy_data, 1, 2) == expected[[3]]))
    expect_true(all(get.armour(dummy_data, c(1,2), 2) == expected[[4]]))
    expect_true(all(get.armour(dummy_data, 3) == expected[[5]]))
})