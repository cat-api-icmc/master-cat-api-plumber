# API functions
check_health <- function(base_url) {
  url <- paste0(base_url, "/hc")
  response <- httr::GET(url)
  return(httr::content(response))
}

start_assessment_api <- function(base_url, questions, pattern_theta, criteria, start_item, design) {
  url <- paste0(base_url, "/start-assessment")
  body <- list(
    questions = questions,
    pattern_theta = pattern_theta,
    criteria = criteria,
    start_item = start_item,
    design = design
  )
  response <- httr::POST(url, body = body, encode = "json")
  return(httr::content(response))
}

get_next_item_api <- function(base_url, design, answer, previous_index) {
  url <- paste0(base_url, "/next-item")
  body <- list(
    design = design,
    answer = answer,
    previous_index = previous_index
  )
  response <- httr::POST(url, body = body, encode = "json")
  return(httr::content(response))
}

get_design_data_api <- function(base_url, design) {
  url <- paste0(base_url, "/get-design-data")
  body <- list(design = design)
  response <- httr::POST(url, body = body, encode = "json")
  return(httr::content(response))
}

# Simulation functions
sim_next_answer <- function(theta, a, b, c){
  
  n <- length(theta)
  d <- -a*b # Intercept
  eta <- theta %*% t(a) +  matrix(d,n,1,byrow=TRUE) # n x I (a'theta+d)
  P <- c + (1-c)/(1+exp(-eta)) # n x I
  X <- runif(n*1) # Uniform(0,1)
  sim_resp <- 1*(X<P)
  
  return(sim_resp)
}

sim_cat_api <- function(
    base_url = base_url,
    questions = questions,
    real_theta = 0,
    pattern_theta = 0,
    criteria = "MI",
    start_item = "random",
    design = list(min_SEM = 0.3)
){
  cat(" ------------------ Starting simulation ------------------ \n")
  # start assessment
  assessment_design <- start_assessment_api(
    base_url = base_url, 
    questions = questions, 
    pattern_theta = pattern_theta, 
    criteria = criteria, 
    start_item = start_item, 
    design = design
  )
  
  # simulate assessment 
  while(!assessment_design$stop){
    
    # Get next answer 
    cur_resp <- sim_next_answer(
      theta = real_theta,
      a = as.numeric(questions$discrimination[as.numeric(assessment_design$next_index)]),
      b = as.numeric(questions$difficulty[as.numeric(assessment_design$next_index)]),
      c = as.numeric(questions$guess[as.numeric(assessment_design$next_index)])
    )
    
    cat("Item: ", assessment_design$next_index, " Response: ", as.numeric(cur_resp), "\n")
    
    # Get next item
    assessment_design <- get_next_item_api(
      base_url, 
      design = assessment_design$design, 
      answer = as.numeric(cur_resp), 
      previous_index = as.numeric(assessment_design$next_index)
    )
  }
  
  # Get design data
  assessment <- get_design_data_api(
    base_url, 
    design = assessment_design$design
  )
  
  cat(" ------------------ Ending simulation ------------------ \n")
  
  return(assessment)
}


# ---- Simulating one assessment ---- 
# Create 50 questions with random IRT parameters
set.seed(123)  # For reproducibility
questions <- list(
  discrimination = runif(50, 0.5, 2),  # Discrimination between 0.5 and 2
  difficulty = rnorm(50, 0, 1),        # Difficulty from a normal distribution with mean 0, sd 1
  guess = runif(50, 0, 0.3)            # Guessing parameters between 0 and 0.3
)

set.seed(1994)  # For reproducibility
base_url <- "http://127.0.0.1:8080"
simulation <- sim_cat_api(
  base_url = base_url,
  questions = questions,
  real_theta = -3,
  criteria = "MI",
  start_item = "random",
  design = list(min_SEM = 0.4)
)

library(tidyverse)

plot_simulation <- function(simulation){
  assessment_history <- data.frame(
    item_history = c(NA,na.exclude(as.numeric(simulation$item_history))),
    theta_history = as.numeric(simulation$theta_history),
    standard_error_history = as.numeric(simulation$standard_error_history)
  )
  
  answer_history <- data.frame(
    item = 1:length(as.numeric(simulation$response_history)),
    answer = as.numeric(simulation$response_history)
  )
  
  history <- assessment_history |> 
    left_join(answer_history, by = c("item_history" = "item")) |> 
    mutate(ordenation = 0:(n()-1))
  
  history |> 
    ggplot(aes(x = ordenation, y = theta_history)) +
    geom_line(size = 0.6) +
    geom_point(aes(color = factor(answer)),size = 3) +
    geom_text(aes(label = item_history), vjust = -1) +
    geom_ribbon(aes(ymin = theta_history - standard_error_history ,
                    ymax = theta_history + standard_error_history ), alpha = 0.1) +
    ylim(-4,4) +
    ylab("Theta") +
    xlab("Items ordenation") +
    ggtitle(paste0("Itens Answered: ", last(history$ordenation),"  |  ",
                   "Theta Estimated: ", last(history$theta_history),"  |  ",
                   "Standard Error: ", last(history$standard_error_history)
    )) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
}


plot_simulation(simulation)


# ---- Simulating more than one assessment ---- 
# Create 50 questions with random IRT parameters
library(furrr)

set.seed(123)  # For reproducibility
questions <- list(
  discrimination = runif(50, 0.5, 2),  # Discrimination between 0.5 and 2
  difficulty = rnorm(50, 0, 1),        # Difficulty from a normal distribution with mean 0, sd 1
  guess = runif(50, 0, 0.3)            # Guessing parameters between 0 and 0.3
)

base_url <- "http://127.0.0.1:8080"

sim_design <- expand_grid(
  design = list(list(min_SEM = 0.5)),
  replica = 1:10,
  real_theta = seq(-3, 3, by = 0.2),
  criteria = "MI",
  start_item = "MI",
)

set.seed(1994)
full_simulation <- pmap(
  .l = list(
    base_url   = list(base_url),
    questions  = list(questions),
    real_theta = sim_design$real_theta,
    criteria   = sim_design$criteria,
    start_item = sim_design$start_item,
    design     = sim_design$design
  ),
  .f = sim_cat_api,
  .progress = T
)


extract_assessment_data <- function(simulation) {
  n_itens <- length(simulation$theta_history) - 1
  theta_est <- last(simulation$theta_history)
  se_est <- last(simulation$standard_error_history)
  
  assessment_data <- data.frame(n_itens, theta_est, se_est)
  
  return(assessment_data)
}

full_simulation_data <- pmap_df(
  .l = list(simulation = full_simulation),
  .f = extract_assessment_data
)

sim_design |> 
  bind_cols(full_simulation_data)





