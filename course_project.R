data(mtcars)
require(dplyr); require(GGally); require(ggplot2)

cars <- mutate(mtcars,
               cyl = as.factor(cyl),
               vs = as.factor(vs),
               am = as.factor(am),
               gear = as.factor(gear),
               carb = as.factor(carb))
cars_a <- select(filter(cars, am == 0), -am)
cars_m <- select(filter(cars, am == 1), -am)

round(t.test(cars_m$mpg, cars_a$mpg, var.equal = FALSE, paired = FALSE, alternative = "greater")$conf.int[1:2], 3)
round(t.test(cars_m$mpg, cars_a$mpg, var.equal = FALSE, paired = FALSE, alternative = "greater")$p.value, 6)

fit1 <- lm(mpg ~ am, cars)
confint(fit1)

mdl1 <- lm(mpg ~ ., data = cars)
mdl1_a <- lm(mpg ~ ., data = cars_a)
mdl1_m <- lm(mpg ~ ., data = cars_m)

plot(c(max(max(cars_a$wt), max(cars_m$wt)) + 2, min(min(cars_a$wt), min(cars_m$wt))) - 1,
     c(max(max(cars_a$mpg), max(cars_m$mpg)) + 5, min(min(cars_a$mpg), min(cars_m$mpg))) - 5,
     type = "n", frame = FALSE, ylab = "MPG", xlab = "Weight")
points(cars_a$wt, cars_a$mpg, bg = "darkblue", pch = 21, cex = 2)
points(cars_m$wt, cars_m$mpg, bg = "forestgreen", pch = 21, cex = 2)

abline(lm(mpg ~ wt, cars_a), lw = 2, col = "darkblue")
abline(lm(mpg ~ wt, cars_m), lw = 2, col = "forestgreen")
abline(lm(mpg ~ wt, cars), lw = 2, col = "red", lty = 3)

x <- mutate(mtcars, mpg = mpg, cyl = as.factor(cyl), disp = disp, hp = hp, wt = wt, am = as.factor(am)) %>%
        select(mpg, cyl, disp, hp, wt, am)

g<-ggpairs(cars[, c('mpg', 'am', 'wt')], 
        upper = list(continuous = "cor"), 
        lower = list(continuous = "smooth", combo = "box", discrete = "ratio"))
g<- g + ggplot2::theme(plot.title = "The Title")
g<- g + ggplot2::theme(plot.title = ggplot2::element_text(face="bold"))



