library(survival)
library(survminer)

mean <- 5
rate_time <- 1/mean

time_one <- rexp(200, rate = rate_time)
time_two <- rexp(200, rate = rate_time)
times <- as.data.frame(cbind(time_one,time_two))

times$event_time <- NA
times$event_time <- pmin(times$time_one,times$time_two)

times$event <- "Censored"
times$event[times$time_one<times$time_two] <- "One"
times$event[times$time_one>times$time_two] <- "Two"
times$event[times$event_time>10] <- "Censored"
times$event <- as.factor(times$event)

crfit <- survfit(Surv(event_time, event) ~ 1, data=times)
plot(crfit,col=c("Red3","Blue"))
ggcompetingrisks(crfit)