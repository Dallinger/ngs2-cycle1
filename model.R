# number of groups (2)
# whether you can change groups (no vs. yes)
# between group temperature (1.0 vs. 2.0)
# within group temperature (1.0 vs. 2.0)
# level of inequality (1.0 vs. 0.1 to worst off group)
# players (8 vs. 3)
# rounds (10 vs. 1)
# that’s (control vs. experimental)

# predictions:
# of the form: [% donate to anyone, % of donations to ingroup]
# control: [14, 52]
# you can change groups: [13, 50]
# hot between group temperature: [15, 55]
# hot within group temperature: [11, 54]
# level of inequality: [16, 52]
# players: [36, 56] <-- this is likely due to small populations disrupting the efficiency of selection
# rounds: [50, 50] NA depends on assumed starting behavior of participants for which we have little data

# predictions assuming a larger population (N=100):
# of the form: [% donate to anyone, % of donations to ingroup]
# control: [9, 49]
# you can change groups: [9, 53]
# hot between group temperature: [9, 49] <-- no effect because changing temp to 2 while intra-temp remains at 1 is too weak
# hot within group temperature: [7, 52]
# level of inequality: [9, 51]
# players: NA, assumed to be 100
# rounds: [50, 50] NA depends on assumed starting behavior of participants for which we have little data

# population size
N <- 100
# number of rounds
rounds <- 200
# number of rounds of donation
c_rounds <- 1
# number of repeats
repeats <- 200
# number of groups
groups <- 2
# migration probability
p_migrate <- 0
# is group membership heritable
group_h <- 0
# is strategy heritable
c_h <- 1.0
# intergroup competition
inter_temp <- 10
intra_temp <- 1


# cooperation genes
initial_c1 <- function(val) {
  return(runif(N, 0, 1))
}
initial_c2 <- function() {
  return(runif(N, 0, 1))
}
# starting group markers
initial_groups <- function() {
  # return(c(1, 1, 2))
  dum <- rep(c(1:groups), N/2)
  return(sample(dum,size=N, replace=FALSE))
}
# points given to agents each round
initial_points <- function() {
  p <- round(rgamma(N, 1.5, 0.2))
  #p <- rnorm(N, 8, 0.5)
  equality <- c(1, 1)
  p <- p*equality[g]
  return(p)
}
# pot multiplier
m <- 1.4
# var of normal mutations
q <- 0.05


# data lists
# stores mean c1
data <- list()
# stores mean c2
data2 <- list()
# stores biggest group size
data3 <- list()
# stores smallest group size
data4 <- list()

for (rep in 1:repeats) {
  # print(rep)
  # set up genes and groups
  c1 <- initial_c1()
  c2 <- initial_c2()
  g <- initial_groups()
  
  # set up data vectors
  mean_c1 <- vector()
  mean_c2 <- vector()
  max_g <- vector()
  min_g <- vector()

  for (round in 1:rounds) {
    points <- rep(0, N)
    for (c_round in 1:c_rounds) {
      # distribute points
      points <- points + initial_points()
    }
    # contribute
    contributions <- points * c1
    group_contributions <- contributions * c2
    global_contributions <- contributions * (1-c2)
    
    # calculate pot sizes
    total_global_contributions <- sum(global_contributions)
    total_group_contributions <- rep(0, groups)
    for (i in 1:groups) {
      total_group_contributions[i] <- sum(group_contributions[g==i])
    }
    
    global_pot <- total_global_contributions*m
    group_pots <- total_group_contributions*m
    
    # distribute payoffs
    group_sizes = tabulate(g, groups)
    
    payoff <- pmax(points - contributions + global_pot/N + group_pots[g]/group_sizes[g], 0.0001)
    # implement inter-group and intragroup competetion
      
    # work out total payoff to each group
    group_payoff <- c(1:groups)
    for (group in 1:groups) {
      group_payoff[group] <- sum(payoff[g==group])
    }
    
    # shift payoffs between groups according to inter_temp
    group_payoff_hot <- (group_payoff/N)^inter_temp
    group_payoff_hot <- (group_payoff_hot/sum(group_payoff_hot))*sum(group_payoff)
    
    # reculculate each individuals payoff
    payoff_hot <- (payoff/group_payoff[g])*group_payoff_hot[g]
      
    # shift payoffs within groups according to intra_temp
    payoff_hotter <- payoff_hot^intra_temp
    group_payoff_hotter <- c(1:groups)
    for (group in 1:groups) {
      group_payoff_hotter[group] <- sum(payoff_hotter[g==group])
    }
    payoff_hotter <- (payoff_hotter/group_payoff_hotter[g])*group_payoff_hot[g]
    
    # calculate fitness and reproduce
    # choose parents weighted by fitness
    fitness <- payoff_hotter/sum(payoff_hotter)
    parents <- sample(c(1:N), size=N, prob=fitness, replace=TRUE)
    
    # inherit their cooperativeness + mutation
    dum <- runif(N, 0, 1) < c_h
    c1 <- pmin(pmax(c1[parents] + rnorm(N, 0, q), 0), 1)*dum + pmin(pmax(c1 + rnorm(N, 0, q), 0), 1)*(1-dum)
    mean_c1 <- c(mean_c1, mean(c1))
    c2 <- pmin(pmax(c2[parents] + rnorm(N, 0, q), 0), 1)*dum + pmin(pmax(c2 + rnorm(N, 0, q), 0), 1)*(1-dum)
    mean_c2 <- c(mean_c2, mean(c2))
    # manage groups and migration
    # optionally migrate to your parents group
    dum <- runif(N, 0, 1) < group_h
    g <- g[parents]*dum + g*(1-dum)
    # optionally migrate at random
    dum <- runif(N, 0, 1) < p_migrate
    g <- (1-dum)*g + 1*dum*initial_groups()
    # identify the biggest and smallest groups
    max_g <- c(max_g, max(tabulate(g, groups))/(N/groups))
    min_g <- c(min_g, min(tabulate(g, groups))/(N/groups))
  }
  data[[length(data) + 1]] <- mean_c1
  data2[[length(data2) + 1]] <- mean_c2
  data3[[length(data3) + 1]] <- max_g
  data4[[length(data4) + 1]] <- min_g
}

# plot the c graph
plot_data <- data[[1]]
if (length(data) > 1) {
  for (i in 2:length(data)) {
    plot_data <- plot_data + data[[i]]
  }
}
plot_data <- plot_data/length(data)
plot(plot_data, type="l", ylim=c(0,1), ylab="contributions", xlab="round", lwd=2)

plot_data2 <- data2[[1]]
if (length(data2) > 1) {
  for (i in 2:length(data2)) {
    plot_data2 <- plot_data2 + data2[[i]]
  }
}
plot_data2 <- plot_data2/length(data2)
lines(plot_data2, type="l", lwd=2, lty=2)
legend(c(0, 1), c("c1", "c2"), lty=c(1, 2), lwd=c(2, 2), bty="n")
# 
# plot_data <- data3[[1]]
# plot_data2 <- data4[[1]]
# if (length(data3) > 1) {
#   for (i in 2:length(data3)) {
#     plot_data <- plot_data + data3[[i]]
#     plot_data2 <- plot_data2 + data4[[i]]
#   }
# }
# plot_data <- plot_data/length(data3)
# plot_data2 <- plot_data2/length(data4)
# 
# plot(plot_data, type="l", ylim=c(0, groups), ylab="biggest group size", xlab="trial", lwd=2)
# lines(plot_data2, type="l")

# dum <- initial_points()
# for (i in 1:100) {
#   dum <- dum + initial_points()
# }
# hist(dum)