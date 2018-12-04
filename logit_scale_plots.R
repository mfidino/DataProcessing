
y <- rbinom(500, 1,prob = 0.2)

Roughness = runif(500, 0,100)
NDVI = runif(500, -10, 10)

my_y <- data.frame(y = y, Roughness = Roughness, NDVI = NDVI )

m1 <- glm(y ~ Roughness + NDVI, data = my_y, family = binomial)

new_df <- data.frame(Roughness= seq(min(my_y$Roughness), 100, 0.5),
                     NDVI = median(my_y$NDVI))


my_pred <- predict(m1, newdata = new_df,
                   type = "link",
                   se.fit = TRUE)

low_bounds <- qnorm(c(0.025), my_pred$fit, my_pred$se.fit)
hi_bounds <- qnorm(c(0.975), my_pred$fit, my_pred$se.fit)

predicted <- data.frame(fit = my_pred$fit, low = low_bounds, 
                        hi = hi_bounds)

prob_predicted <- data.frame(apply(predicted, 2, plogis))

plot(prob_predicted$fit ~ new_df$Roughness, type = 'l', bty = 'l',
     ylim = c(0,1))
lines(prob_predicted$low ~ new_df$Roughness, lty = 2)
lines(prob_predicted$hi ~ new_df$Roughness, lty = 2)
