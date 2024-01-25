library("idefix")

al <- list(
  c("90k", "110k", "140k", "190k"), # wage
  c("10 min", "30 min", "60 min"), # commute time
  c("State-owned", "Private", "Non-commerical"), # sectors
  c("Full-time", "Hybrid", "Online"), # work mode
  c("Formal", "Intermediate", "Informal"), # corporate culture
  c("Good", "Intermediate", "Bad"), # Office
  c("yes", "no"), # promotion criterion
  c("less than 25", "25-35", "35-45", 'greater than 45'), # average age of the team
  c("Interesting", "Intermediate", "Boring") # task
)

mu <- c(70, # no.choice (constant utility)
        1, # wage (constant utility)
        -1/3,
        +30, +10,
        +10, +20,
        +10, +20,
        -10, -20,
        -10,
        0, -5, -15,
        0, 0)

v <- diag(length(mu[c(-1, -2)])) # Prior variance.
set.seed(123)
pd <- MASS::mvrnorm(n = 10, mu = mu[c(-1, -2)], Sigma = v) # 10 draws.
no.choice.pd <- matrix(rep(mu[1], 10))
wage.pd <- matrix(rep(mu[2], 10))
pd <- cbind(wage.pd, pd)
ps <- list(no.choice.pd, pd)
coded <- c("C", "C", "D", "D", "D", "D", "D", "D", "D")
alt <- c(0, 0, 0, 1)

exp <- CEA(lvls = c(4, 3, 3, 3, 3, 3, 2, 4, 3),
    coding = coded,
    par.draws = ps,
    c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)),
    n.alts = 4, n.sets = 25,
    alt.cte = alt,
    no.choice = TRUE)

exp$decoded <- Decode(des = exp$design, n.alts = 4, lvl.names = al, coding = coded, alt.cte = alt,
       c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)),
       no.choice = 4)

saveRDS(exp, file="design.RData")
# write.csv(exp$decoded[1]$design, "design.csv")


# bullshit
des <- exp$design
set.seed(123)
true_par <- rnorm(17)
RespondMNL(par = true_par, des = exp$design, n.alts = 4)

# Survey App
n.sets <- 25
alternatives <- c("Alternative A", "Alternative B", "Alternative C", "None")
attributes <- c('wage', 'commute_time', 'sectors', 'work_mode', 'corporate_culture',
                'office', 'promotion', 'team_age', 'task')

labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("90k", "110k", "140k", "190k") # wage
labels[[2]] <- c("10 min", "30 min", "60 min") # commute time
labels[[3]] <- c("State-owned", "Private", "Non-commerical") # sectors
labels[[4]] <- c("Full-time", "Hybrid", "Online") # work mode
labels[[5]] <- c("Formal", "Intermediate", "Informal") # corporate culture
labels[[6]] <- c("Good", "Intermediate", "Bad") # Office
labels[[7]] <- c("yes", "no") # promotion criterion
labels[[8]] <- c("less than 25", "25-35", "35-45", 'greater than 45') # average age of the team
labels[[9]] <- c("Interesting", "Intermediate", "Boring") # task

code <- coded

b.text <- "Please choose the alternative you prefer"
i.text <- "Welcome, here are some instructions ... good luck!"
e.text <- "Thanks for taking the survey"

SurveyApp(des = exp$design, n.total = n.sets, alts = alternatives,
          atts = attributes, lvl.names = labels, coding = code,
          alt.cte = alt, no.choice = 4, buttons.text = b.text,
          c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)),
          intro.text = i.text, end.text = e.text,  data.dir = ".")


cand <- Profiles(lvls = c(4, 3, 3, 3, 3, 3, 2, 4, 3),
                 coding = coded,
                 c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)))


