# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

set.seed(123)
rm(list = ls())
swirl_options(swirl_logging = TRUE)
swirl_options(swirl_is_fun = FALSE)


v <- sample(100, 20, TRUE)

my_logi_all <- c(TRUE, TRUE, TRUE)
my_logi_any <- c(FALSE, FALSE, TRUE, FALSE)
my_logi_none <- c(FALSE, FALSE, FALSE)

v1 <- seq(pi, length.out = 4)
v2 <- 4:1
v3 <- seq(-1.5, length.out = 4)
v4 <- rep(3,3)
v5 <- 1:5