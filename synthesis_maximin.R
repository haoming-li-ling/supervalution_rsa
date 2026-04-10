source("synthesis.R")

U1 <- function() {
  L0() %>%
    mutate(Qw = Q_equiv(QuD, world)) %>%
    group_by(inter, QuD, message, Qw) %>%
    mutate(sum_equiv = sum(prob)) %>%
    group_by(world, QuD, message) %>%
    summarise(util = min(log(sum_equiv) - cost(message))) %>%
    ungroup()
}