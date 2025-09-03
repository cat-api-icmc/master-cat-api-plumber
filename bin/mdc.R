# DINA improvement
generate_fake_mirt_pars <- function(q_matrix){
    n_itens <- nrow(q_matrix)
    n_skills <- ncol(q_matrix)
    mirt_pars <- matrix(0, nrow = n_itens, ncol = n_skills + 2)
    colnames(mirt_pars) <- c(paste0("a", 1:n_skills), "d", "g")
    return(mirt_pars)
}

rowProds <- function(x) apply(x, 1, prod)
get_prob_matrix <- function(Q, parameters, skill_patterns, model) {
    J <- nrow(Q)
    n_alpha <- nrow(skill_patterns)
    K <- ncol(Q)
    
    if (model %in% c("DINA", "DINO")) {

      slip <- unname(unlist(lapply(parameters, function(row) unlist(row["slip"]))))
      guess <- unname(unlist(lapply(parameters, function(row) unlist(row["guess"]))))

      # Matrix of required skill counts per item
      required_matrix <- t(skill_patterns %*% t(Q))   
      n_required_skills <- rowSums(Q)
      
      
      # required_matrix[9,13] # item 9, alpha 13 requires 2
      # mastered[9,13] # FALSE: alpha 13 does not match DINA requirements for item 9
      # 
      # required_matrix[2,5] # item 2, alpha 5 requires 1
      # mastered[2,5] # TRUE: alpha 5 matches DINA requirements for item 2
      
      
      if (model == "DINA") {
        # Mastery only if all required skills are present
        mastered <- required_matrix == n_required_skills  
        prob_matrix <- (mastered * (1 - slip)) + ((1 - mastered) * guess)
        
      } else if (model == "DINO") {
        # Mastery if at least one required skill is present
        mastered <- t(t(required_matrix) > 0) 
        prob_matrix <- (mastered * (1 - slip)) + ((1 - mastered) * guess)
      }
      
      return(prob_matrix) 
    }
    
    else if (model == "GDINA") {
      prob_matrix <- matrix(NA, ncol = n_alpha, nrow = J)
      
      for (j in 1:J) {
        required <- Q[j, ] == 1
        for (a in 1:n_alpha) {
          pattern <- skill_patterns[a, required]
          key <- paste0(pattern, collapse = "")
          
          # The parameter list must contain keys like "00", "01", ..., depending on the item
          prob_matrix[j, a] <- parameters[[j]][[key]]
        }
      }
      
      return(prob_matrix) 
    }
    
    else {
      stop("Model must be 'DINA', 'DINO', or 'GDINA'")
    }
}

get_likelihood <- function(responses, prob_matrix) {
    p <- t(prob_matrix)

    likelihoods <- matrix(1, nrow = nrow(responses), ncol = nrow(p))
    
    for(r in 1:nrow(responses)){
      temp_resp <- matrix(responses[r, ], ncol=ncol(p), nrow=nrow(p), byrow = T)
      
      #print(dim(p))
      #print(dim(temp_resp))
      
      likelihoods[r, ] <- rowProds((p^temp_resp) * ((1 - p)^(1 - temp_resp)))  
    }
    
    return(likelihoods)
}

# Posterior function
get_posterior <- function(likelihoods, prior) {
    posterior <- likelihoods * matrix(prior, nrow = nrow(likelihoods), ncol = ncol(likelihoods), byrow = TRUE)
    
    posterior <- posterior / rowSums(posterior)
    return(posterior)
}

estimateSkills <- function(response, Q, parameters, model = "DINA", method = "EAP", prior = NULL) {
  # response: matrix (respondents x items)
  # Q: Q-matrix (items x attributes)
  # parameters: list with slip/guess (DINA/DINO) or list of lists (GDINA)
  # model: "DINA", "DINO", "GDINA"
  # method: "MLE", "MAP", "EAP"
  # prior: vector of size 2^K or NULL
  
  # Inputs
  J <- nrow(Q)
  K <- ncol(Q)
  response <- if (is.matrix(response)) response else matrix(response, nrow = 1)
  n_resp <- nrow(response)
  
  # Generate possible skill profiles
  skill_patterns <- as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
  colnames(skill_patterns) <- paste0("A", 1:K)
  n_alpha <- nrow(skill_patterns)
  
  # Uniform prior (Bayesian cases)
  if (is.null(prior)) prior <- rep(1 / n_alpha, n_alpha)
  
  prob_matrix <- get_prob_matrix(Q, parameters, skill_patterns, model)
  likelihoods <- get_likelihood(response, prob_matrix)
  
  if (method == "MLE") {
    idx <- apply(likelihoods, 1, which.max)
    #print(head(likelihoods))
    MLE <- skill_patterns[idx, , drop = FALSE]
  
    # Variância aproximada para MLE baseada na dispersão das verossimilhanças
    
    posterior_like <- likelihoods / rowSums(likelihoods)
    E_alpha <- posterior_like %*% skill_patterns
    E_alpha2 <- posterior_like %*% (skill_patterns^2)
    SE_MLE <- sqrt(E_alpha2 - E_alpha^2)
    
    colnames(MLE) <- paste0("F",1:K)
    colnames(SE_MLE) <- paste0("F",1:K,"_SE")
    
    return(cbind(MLE = MLE, SE = SE_MLE))
    
    
  } else if (method == "MAP") {
    posteriors <- get_posterior(likelihoods, prior)
    idx <- apply(posteriors, 1, which.max)
    MAP <- skill_patterns[idx, , drop = FALSE]
    
    E_alpha <- posteriors %*% skill_patterns
    E_alpha2 <- posteriors %*% (skill_patterns^2)
    SE_MAP <- sqrt(E_alpha2 - E_alpha^2)
    
    colnames(MAP) <- paste0("F",1:K)
    colnames(SE_MAP) <- paste0("F",1:K,"_SE")
    
    return(cbind(MAP = MAP, SE = SE_MAP))
    
  } else if (method == "EAP") {
    posteriors <- get_posterior(likelihoods, prior)
    EAP <- posteriors %*% skill_patterns  # n_resp x K
    
    E_alpha <- posteriors %*% skill_patterns
    E_alpha2 <- posteriors %*% (skill_patterns^2)
    SE_EAP <- sqrt(E_alpha2 - E_alpha^2)
    
    colnames(EAP) <- paste0("F",1:K)
    colnames(SE_EAP) <- paste0("F",1:K,"_SE")

    return(cbind(EAP = EAP, SE = SE_EAP))
    
  } else {
    stop("Unknown estimation method.")
  }
  
}

#  If response variability method = 'ML' else 'EAP'
customUpdateThetas <- function(design, person, test){
    
    # from global environment:
    model <- model
    q_matrix <- q_matrix
    parameters <- cdm_parameters

    # from design:
    # mo <- extract.mirtCAT(test, 'mo')
    method <- design@method

    # cat("Estimating", model, "using", method, "method ")

    responses <- extract.mirtCAT(person, 'responses')
    itens_non_answered <- which(is.na(responses))
    n_itens <- length(responses)
    n_itens_non_answered <- length(itens_non_answered)
    n_itens_answered <- n_itens - n_itens_non_answered

    responses_temp <- responses[-itens_non_answered]
    parameters_temp <- parameters[-itens_non_answered]
    q_matrix_temp <- q_matrix[-itens_non_answered,]
    
    theta_colnames <- paste0("F", 1:ncol(q_matrix))
    theta_se_colnames <- paste0("F", 1:ncol(q_matrix), "_SE")


    # pelo menos 1 item respondido
    if(n_itens_answered == 1){
        start_skill <- 0
        start_skill_SE <- 1
        person$Update_thetas(theta=rep(start_skill,ncol(q_matrix)), theta_SE=rep(start_skill_SE,ncol(q_matrix)))
        # print("skipped")
        
    # mais de 1 item respondido
    }else if(n_itens_answered < n_itens){
        # tmp <- dina_estimate(parameters_temp, responses_temp, q_matrix_temp, est = "MAP")
        tmp <- estimateSkills(responses_temp, q_matrix_temp, parameters_temp, model = model, method = method)
        person$Update_thetas(theta=tmp[,theta_colnames], theta_SE=tmp[,theta_se_colnames, drop=FALSE])

    # todos os itens respondidos
    }else if(n_itens_answered == n_itens){
        # tmp <- dina_estimate(parameters, responses, q_matrix, est = "MAP")
        tmp <- estimateSkills(responses, q_matrix, parameters, model = model, method = method)
        person$Update_thetas(theta=tmp[,theta_colnames], theta_SE=tmp[,theta_se_colnames, drop=FALSE])
    }

    # cat(" | Updated thetas:", person$thetas, "\n")
    invisible()
}

KL_criteria <- function(item_index, alpha_hat_index, prob_matrix) {
  # prob_matrix: matriz J x n_profiles com P(X = 1 | alpha)
  n_profiles <- ncol(prob_matrix)
  p_hat <- prob_matrix[item_index, alpha_hat_index]
  nz <- 1e-10
  
  kl_sum <- 0
  for (c in 1:n_profiles) {
    p_c <- prob_matrix[item_index, c]
    
    kl <- p_hat * log((p_hat + nz) / (p_c + nz)) + 
      (1 - p_hat) * log((1 - p_hat + nz) / (1 - p_c + nz))
    
    kl_sum <- kl_sum + kl
  }
  return(kl_sum)
}
PWKL_criteria <- function(item_index, alpha_hat_index, prob_matrix, posterior) {
  # prob_matrix: matriz J x n_profiles com P(X = 1 | alpha)
  # posterior: vetor de posteriori atual (tamanho = n_profiles)
  n_profiles <- ncol(prob_matrix)
  p_hat <- prob_matrix[item_index, alpha_hat_index]
  nz <- 1e-10
  
  pwkl_sum <- 0
  for (c in 1:n_profiles) {
    p_c <- prob_matrix[item_index, c]
    post_c <- posterior[c]
    
    kl <- p_hat * log((p_hat + nz) / (p_c + nz)) + 
      (1 - p_hat) * log((1 - p_hat + nz) / (1 - p_c + nz))
    
    pwkl_sum <- pwkl_sum + kl * post_c
  }
  return(pwkl_sum)
}
MPWKL_criteria <- function(item_index, prob_matrix, posterior) {
  n_profiles <- length(posterior)
  p <- prob_matrix[item_index, ]
  nz <- 1e-10
  mpwkl <- 0
  
  for (d in 1:n_profiles) {
    for (c in 1:n_profiles) {
      p_d <- p[d]
      p_c <- p[c]
      
      kl <- p_d * log((p_d + nz) / (p_c + nz)) +
        (1 - p_d) * log((1 - p_d + nz) / (1 - p_c + nz))
      
      mpwkl <- mpwkl + kl * posterior[d] * posterior[c]
    }
  }
  return(mpwkl)
}
SHE_criteria <- function(item_index, prob_matrix, posterior) {
  # item_index: índice do item que está sendo avaliado
  # prob_matrix: matriz J x n_profiles com P(X = 1 | alpha)
  # posterior: vetor de tamanho n_profiles com as posterioris atuais
  n_profiles <- length(posterior)
  p_success <- prob_matrix[item_index, ]
  nz <- 1e-10  # Para evitar log(0)
  
  # Entropia condicional esperada após a resposta ao item (x = 1 e x = 0)
  expected_entropy <- 0
  
  for (x in c(0, 1)) {
    # Probabilidade de observar a resposta x para o item
    px_given_alpha <- if (x == 1) p_success else (1 - p_success)
    prob_x <- sum(posterior * px_given_alpha)
    
    # Atualiza posterior dado a resposta x
    posterior_x <- (posterior * px_given_alpha) / (prob_x + nz)
    
    # Entropia de Shannon da posterior atualizada
    entropy_x <- -sum(posterior_x * log(posterior_x + nz))
    
    # Entropia esperada ponderada pela probabilidade de x
    expected_entropy <- expected_entropy + prob_x * entropy_x
  }
  
  return(expected_entropy)
}


select_next_item <- function(
  responses,
  Q,
  parameters,
  model = "DINA",
  criteria = "seq",  # "KL", "PWKL", "MPWKL", "SHE"
  method = "MAP",
  prior = NULL
) {
  
  answered <- !is.na(responses)
  candidate_items <- which(!answered)
  
  # Selection of the criterion
  if (criteria == "seq") {
    selected <- candidate_items[1]
    return(selected)
  } else if (criteria == "random") {
    selected <- sample(candidate_items, 1)
    if(length(candidate_items) == 1) selected <- candidate_items
    return(selected)
  } 
  
  
  K <- ncol(Q)
  skill_patterns <- as.matrix(expand.grid(replicate(K, 0:1, simplify = FALSE)))
  n_profiles <- nrow(skill_patterns)

  if (is.null(prior)) prior <- rep(1 / n_profiles, n_profiles)
  
  prob_matrix <- get_prob_matrix(Q, parameters, skill_patterns, model)

  observed_responses <- matrix(ifelse(is.na(responses), 0, responses), nrow = 1)
  likelihood <- get_likelihood(observed_responses, prob_matrix)
  posterior <- get_posterior(likelihood, prior)[1, ]
 
  if(method %in% c("EAP", "MAP")){
    alpha_hat_index <- which.max(posterior) # MAP
  } else {
    alpha_hat_index <- which.max(likelihood[1, ])
  }
  

  if (criteria == "KL") {
    scores <- sapply(candidate_items, function(j) KL_criteria(j, alpha_hat_index, prob_matrix))
    selected <- candidate_items[which.max(scores)]

  } else if (criteria == "PWKL") {
    scores <- sapply(candidate_items, function(j) PWKL_criteria(j, alpha_hat_index, prob_matrix, posterior))
    selected <- candidate_items[which.max(scores)]

  } else if (criteria == "MPWKL") {
    scores <- sapply(candidate_items, function(j) MPWKL_criteria(j, prob_matrix, posterior))
    selected <- candidate_items[which.max(scores)]

  } else if (criteria == "SHE") {
    scores <- sapply(candidate_items, function(j) SHE_criteria(j, prob_matrix, posterior))
    selected <- candidate_items[which.min(scores)]

  } else {
    stop("Unknown criteria. Use: 'seq', 'random', 'KL', 'PWKL', 'MPWKL' or 'SHE'.")
  }

  return(selected)
}

customNextItem <- function(person, design, test) {

  # from global environment:
  model <- model
  q_matrix <- q_matrix
  parameters <- cdm_parameters
  criteria <- criteria
  
  # from design:
  # criteria <- "seq" #design@method
  method <- design@method
  
  # Dados do teste
  responses <- extract.mirtCAT(person, 'raw_responses')
  
  responses <- as.integer(responses)
  responses[responses == 1] <- 0
  responses[responses == 2] <- 1
  
  best_item <- select_next_item(response = responses, Q=q_matrix, parameters = parameters, model = model, criteria = criteria, method = method)
  cat("Next item selected:", best_item, "-", criteria, "\n")
  return(best_item)
}


# customNextItem <- function(person, design, test){
#     # print(slotNames(person))
#     # print(slotNames(design))
#     # print(slotNames(test))

#     # CAT_criteria <- design@criteria # precisa fazer uma gambs para criterios CDM, o mirt nao aceita novos
#     # cat("Next item criteria:", CAT_criteria, "\n")

#     items_in_bank <- extract.mirtCAT(person, 'items_in_bank')
#     items_answered <- extract.mirtCAT(person, 'items_answered')
    

#     ret = items_in_bank[1]

#     # objective <- computeCriteria(person=person, design=design, test=test, criteria = 'seq')
#     # ret <- findNextItem(person=person, test=test, design=design, objective=objective)
    

#     cat("Next CDM item:", ret, "\n")
#     ret
# }