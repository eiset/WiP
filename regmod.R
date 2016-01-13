# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Author:   Andreas Halgreen Eiset
# Title:    Regression model for crowding
# Purpose:  R code for the regression model building upon previous findings
# Licensed under GNU GENERAL PUBLIC LICENSE, Version 2, June 1991
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# The following code presupposes the generated data set of and builds upon the
# findings in "RCode_analysing_crowding.R" also available in this reposetory.


# Data set ----------------------------------------------------------------

# The final data set
test <- left_join(dttmSft, qblack, by = "period.id") %>%
        left_join(., sumad, by = "period.id") %>% 
        mutate(dy.sft = weekdays(dte.sft)) %>%
        unite(intrm, shift, dy.sft, sep = "_", remove = FALSE) %>%
        mutate(dicdy.sft = ifelse(dy.sft %in% c("Saturday", "Sunday"),
                                  "weekend", "weekday")) %>%
        mutate(dicdy.sft = ifelse(intrm == "night_Friday", "weekend",
                                  ifelse(intrm == "night_Sunday", "weekday",
                                         ifelse(dicdy.sft == "weekday", "weekday",
                                                "weekend")))) %>%
        mutate(mnth.sft = months(as.Date(as.character(dte.sft)))) %>%
        mutate(season = ifelse(
                mnth.sft %in% c("April", "May", "June", "July", "August", "September"),
                "summer",
                "winter")) %>%
        select(-intrm) %>%
        ungroup() %>%
        mutate(lag.arr = Lag(arrivals, shift = 1)) %>% 
        mutate(lag.dep = Lag(departures, shift = 1)) %>% 
        mutate(lag.queue = Lag(queue, shift = 1)) %>%
        mutate(N = arrivals + queue) %>% 
        filter(!dte.sft == "9999-01-01")
#group_by(dte.sft, shift) %>% 
#filter(queue == max(queue))

u <- glm(cbind(departures, N - departures) ~ log(arrivals + 1) + log(queue + 1), data = test,
    family = binomial(link = "logit"))
plot(u)


ggplot(test, aes(, fill = camper)) +
        geom_histogram() +
        scale_x_log10() +
        facet_grid(camper ~ ., margins=TRUE, scales="free_y")



library(pscl)

m1 <- hurdle(departures ~ arrivals + queue | arrivals + queue, 
             data = test, dist = "negbin")
summary(m1)

m2 <- zeroinfl(departures ~ arrivals + queue | arrivals + queue
               data = test, dist = "negbin")
summary(m2)

library(MASS)

m3 <- glm.nb(departures ~ arrivals + queue, data = test)
summary(m3)

# Test modellerne
vuong(m1, m3)
vuong(m2, m3)
