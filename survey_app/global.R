library("idefix")

al <- list(
  c("70k", "110k", "150k", "200k"), # wage
  c("10 мин", "30 мин", "60 мин"), # commute time
  c("Full-time", "Гибрид", "Полностью удаленный"), # work mode
  c("Формальная", "Промежуточная", "Неформальная"), # corporate culture
  c("Хорошо обустроенный", "Средне обустроенный", "Плохо обустроенный"), # Office
  c("В течение 1 года", "В течение 3 лет", "В течение 5 лет"), # promotion criterion
  c("< 25 лет", "25 - 35 лет", "35 - 45 лет", '> 45 лет'), # average age of the team
  c("20%", "50%", "70%") # task
)

mu <- c(70, # no.choice (constant utility)
        1, # wage (constant utility)
        -1/3, # commute time
        +10, +20, # work mode
        +10, +20, # corporate culture
        -10, -20, # Office
        -10, -20, # promotion criterion
        +5, -5, -10, # average age of the team
        0, -10 # task
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

exp <- readRDS("design_new.RData")

des <- exp$design

# Survey App
n.sets <- 25
alternatives_eng <- c("Alternative A", "Alternative B", "Alternative C", "None")
attributes_eng <- c('wage', 'commute_time', 'sectors', 'work_mode', 'corporate_culture',
                'office', 'promotion', 'team_age', 'task')

labels_eng <- vector(mode = "list", length(attributes))
labels_eng[[1]] <- c("70k", "110k", "150k", "200k") # wage
labels_eng[[2]] <- c("10 min", "30 min", "60 min") # commute time
labels_eng[[3]] <- c("State-owned", "Private", "Non-commerical") # sectors
labels_eng[[4]] <- c("Full-time", "Hybrid", "Online") # work mode
labels_eng[[5]] <- c("Formal", "Intermediate", "Informal") # corporate culture
labels_eng[[6]] <- c("Good", "Intermediate", "Bad") # Office
labels_eng[[7]] <- c("yes", "no") # promotion criterion
labels_eng[[8]] <- c("< 25", "25 - 35", "35 - 45", '> 45') # average age of the team
labels_eng[[9]] <- c("20%", "50%", "70%") # task

alternatives_rus <- c("Работа A", "Работа B", "Работа C", "Отказаться от всех")
attributes_rus <- c('Зарплата', 'Время в пути', 'График работы', 'Корпоративная культура',
                    'Офис', 'Потенциал повышения', 'Средний возраст команды', 'Доля неитересных задач')

labels_rus <- vector(mode = "list", length(attributes))
labels_rus[[1]] <- c("70k", "110k", "150k", "200k") # wage
labels_rus[[2]] <- c("10 мин", "30 мин", "60 мин") # commute time
labels_rus[[3]] <- c("Full-time", "Гибрид", "Полностью удаленный") # work mode
labels_rus[[4]] <- c("Формальная", "Промежуточная", "Неформальная") # corporate culture
labels_rus[[5]] <- c("Хорошо обустроенный", "Средне обустроенный", "Плохо обустроенный") # Office
labels_rus[[6]] <- c("В течение 1 года", "В течение 3 лет", "В течение 5 лет") # promotion criterion
labels_rus[[7]] <- c("< 25 лет", "25 - 35 лет", "35 - 45 лет", '> 45 лет') # average age of the team
labels_rus[[8]] <- c("20%", "50%", "70%") # task

code <- coded

# Russian lang selected
alternatives <- alternatives_rus
labels <- labels_rus
attributes <- attributes_rus

b.text <- "Выберете наиболее привлекательное для вас предложение:"
i.text <- readChar('intro_1.txt', file.info('intro_1.txt')$size)
e.text <- "Спасибо за прохождение первой части опроса! Нажмите ОК, чтобы продолжить, и заполните небольшую анкету!"

