library("idefix")

al <- list(
  c("90k", "110k", "140k", "190k"), # wage
  c("10 мин", "30 мин", "60 мин"), # commute time
  c("Full-time", "Гибрид", "Полностью удаленный"), # work mode
  c("Формальная", "Промежуточная", "Неформальная"), # corporate culture
  c("Хорошо обустроенный", "Средне обустроенный", "Плохо обустроенный"), # Office
  c("В течение 1 года", "В течение 3 лет", "В течение 5 лет"), # promotion criterion
  c("< 25", "25 - 35", "35 - 45", '> 45'), # average age of the team
  c("20%", "50%", "70%") # task
)

mu <- c(70, # no.choice (constant utility)
        1, # wage (constant utility)
        -1/3, # commute time
        -20, -10, # work mode
        -20, -10, # corporate culture
        +20, +10, # Office
        +20, +10, # promotion criterion
        +5, +5, -5, # average age of the team
        0, -5 # task
)

v <- diag(length(mu[c(-1, -2)])) # Prior variance.
set.seed(123)
pd <- MASS::mvrnorm(n = 10, mu = mu[c(-1, -2)], Sigma = v) # 10 draws.
no.choice.pd <- matrix(rep(mu[1], 10))
wage.pd <- matrix(rep(mu[2], 10))
pd <- cbind(wage.pd, pd)
ps <- list(no.choice.pd, pd)
coded <- c("C", "C", "D", "D", "D", "D", "D", "D")
alt <- c(0, 0, 0, 1)

exp <- CEA(lvls = c(4, 3, 3, 3, 3, 3, 4, 3),
           coding = coded,
           par.draws = ps,
           c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)),
           n.alts = 4, n.sets = 25,
           alt.cte = alt,
           no.choice = TRUE)

exp$decoded <- Decode(des = exp$design, n.alts = 4, lvl.names = al, coding = coded, alt.cte = alt,
       c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60)),
       no.choice = 4)

# saveRDS(exp, file="design.RData")
# write.csv(exp$decoded[1]$design, "design.csv")

# Survey App
n.sets <- 25
alternatives <- c("Alternative A", "Alternative B", "Alternative C", "None")
attributes <- c('wage', 'commute_time', 'work_mode', 'corporate_culture',
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

alternatives_rus <- c("Работа A", "Работа B", "Работа C", "Отказаться от всех")
attributes_rus <- c('Зарплата', 'Время в пути', 'График работы', 'Корпоративная культура',
                    'Офис', 'Потенциал повышения', 'Средний возраст коллектива', 'Доля неитересных задач')

labels_rus <- vector(mode = "list", length(attributes))
labels_rus[[1]] <- c("90k", "110k", "140k", "190k") # wage
labels_rus[[2]] <- c("10 мин", "30 мин", "60 мин") # commute time
labels_rus[[3]] <- c("Full-time", "Гибрид", "Полностью удаленный") # work mode
labels_rus[[4]] <- c("Формальная", "Промежуточная", "Неформальная") # corporate culture
labels_rus[[5]] <- c("Хорошо обустроенный", "Средне обустроенный", "Плохо обустроенный") # Office
labels_rus[[6]] <- c("В течение 1 года", "В течение 3 лет", "В течение 5 лет") # promotion criterion
labels_rus[[7]] <- c("< 25", "25 - 35", "35 - 45", '> 45') # average age of the team
labels_rus[[8]] <- c("20%", "50%", "70%") # task

code <- coded

# Russian lang selected
alternatives <- alternatives_rus
labels <- labels_rus
attributes <- attributes_rus

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


