# test-inference.R
test_that("simulated events with controls can be fitted by GAM correctly", {
    skip_if_not_installed("mgcv")
    library(mgcv)

    set.seed(42)
    p <- 20
    n_sims <- 10 # Reduced n_sims for fast testing, 100 in issue script was too slow for unit tests
    n <- 500 # Reduced event count for fast testing
    b1 <- 1 # True parameter

    actors <- as.character(seq_len(p))

    # Simulate static sender/receiver covariates, but we only use 'x' which is a dyadic covariate.
    # The issue script used an exogenous dyadic covariate x ~ N(0, 1).
    # We can mimic this by generating dyadic logits directly using a covariate matrix

    # Run a basic loop over simulations
    gam_coefs <- sapply(1:n_sims, function(i) {
        x <- matrix(rnorm(p^2), ncol = p, nrow = p)
        baseline <- b1 * x

        events <- simulate_relational_events(
            n_events = n,
            senders = actors,
            receivers = actors,
            baseline_logits = baseline,
            allow_loops = FALSE,
            n_controls = 1
        )

        # Extract x values for realized events and controls
        get_x <- function(s, r) {
            x[cbind(as.numeric(s), as.numeric(r))]
        }

        events$x_val <- mapply(get_x, events$sender, events$receiver)

        # Prepare difference in covariates delta_x = x_event - x_control
        dat <- events[events$event == 1, ]
        nondat <- events[events$event == 0, ]

        # Make sure they are aligned by stratum
        dat <- dat[order(dat$stratum), ]
        nondat <- nondat[order(nondat$stratum), ]

        fit_df <- data.frame(
            one = 1,
            delta.x = dat$x_val - nondat$x_val
        )

        fit <- gam(one ~ delta.x - 1, family = "binomial", data = fit_df)
        fit$coefficients[[1]]
    })

    # The mean coefficient over simulations should be around b1 = 1
    mean_coef <- mean(gam_coefs)
    expect_true(mean_coef > 0.8 && mean_coef < 1.2)
})
