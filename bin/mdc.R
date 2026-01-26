# --- CDM parameter building function ---
build_cdm_parameters <- function(questions) {

  if (is.null(questions$params))
    stop("questions$params is missing.")

  slip  <- questions$params$cdm_slipping
  guess <- questions$params$cdm_guessing
  cdm_qmatrix <- questions$params$cdm_qmatrix

  if (is.null(slip) || is.null(guess))
    stop("Slip and Guess parameters must be provided for CDM models.")

  if (!is.list(cdm_qmatrix) || length(cdm_qmatrix) == 0)
    stop("cdm_qmatrix must be a non-empty list.")

  max_index <- max(sapply(cdm_qmatrix, max))
  qmatrix <- matrix(0, nrow = length(cdm_qmatrix), ncol = max_index)

  for (i in seq_along(cdm_qmatrix)) {
    if (any(cdm_qmatrix[[i]] <= 0))
      stop("Q-matrix indices must be positive integers.")
    qmatrix[i, cdm_qmatrix[[i]]] <- 1
  }

  if (length(slip) != nrow(qmatrix) || length(guess) != nrow(qmatrix))
    stop("Slip and guess must have length equal to number of items.")

  parameters <- lapply(seq_len(nrow(qmatrix)), function(i) {
    list(slip = slip[[i]], guess = guess[[i]])
  })

  list(
    parameters = parameters,
    qmatrix    = qmatrix,
    n_skills   = max_index
  )
}

generate_fake_mirt_pars <- function(q_matrix){
    n_itens <- nrow(q_matrix)
    n_skills <- ncol(q_matrix)
    mirt_pars <- matrix(0, nrow = n_itens, ncol = n_skills + 2)
    colnames(mirt_pars) <- c(paste0("a", 1:n_skills), "d", "g")
    return(mirt_pars)
}

# --- CDM estimation functions ---
rowProds <- function(x) apply(x, 1, prod)
get_prob_matrix <- function(Q, parameters, skill_patterns, model) {
  
  # -----------------------------
  # Basic checks
  # -----------------------------
  if (!is.matrix(Q))
    stop("Q must be a matrix (items x attributes).")
  
  if (!is.matrix(skill_patterns))
    stop("skill_patterns must be a matrix (patterns x attributes).")
  
  model <- toupper(model)
  if (!model %in% c("DINA", "DINO", "GDINA"))
    stop("Model must be 'DINA', 'DINO', or 'GDINA'.")
  
  J <- nrow(Q)
  K <- ncol(Q)
  n_alpha <- nrow(skill_patterns)
  
  if (ncol(skill_patterns) != K)
    stop("Number of attributes in Q and skill_patterns do not match.")
  
  # -----------------------------
  # Check Q-matrix validity
  # -----------------------------
  if (any(rowSums(Q) == 0)) {
    stop("Q-matrix contains items with no required attributes (rowSums(Q) == 0).")
  }
  
  # -----------------------------
  # DINA / DINO
  # -----------------------------
  if (model %in% c("DINA", "DINO")) {
    
    # Extract parameters safely
    slip  <- sapply(parameters, function(x) x$slip)
    guess <- sapply(parameters, function(x) x$guess)
    
    if (length(slip) != J || length(guess) != J)
      stop("Length of slip/guess parameters must match number of items.")
    
    if (any(is.na(slip)) || any(is.na(guess)))
      stop("Missing slip or guess parameters.")
    
    if (any(slip <= 0 | slip >= 1 | guess <= 0 | guess >= 1))
      stop("Slip and guess parameters must be in (0,1).")
    
    # Required skill counts: J x n_alpha
    required_matrix <- Q %*% t(skill_patterns)
    n_required_skills <- rowSums(Q)
    
    if (model == "DINA") {
      # Conjunctive rule
      mastered <- required_matrix == n_required_skills
      
    } else {
      # DINO: disjunctive rule
      mastered <- required_matrix > 0
    }
    
    # Probability matrix: J x n_alpha
    prob_matrix <- mastered * (1 - slip) + (!mastered) * guess
    
    return(prob_matrix)
  }
  
  # -----------------------------
  # G-DINA
  # -----------------------------
  if (model == "GDINA") {
    
    if (length(parameters) != J)
      stop("For GDINA, parameters must be a list of length J (one per item).")
    
    prob_matrix <- matrix(NA_real_, nrow = J, ncol = n_alpha)
    
    for (j in seq_len(J)) {
      
      required <- Q[j, ] == 1
      
      if (sum(required) == 0)
        stop(paste("Item", j, "has no required attributes in Q-matrix."))
      
      for (a in seq_len(n_alpha)) {
        
        pattern <- skill_patterns[a, required]
        key <- paste0(pattern, collapse = "")
        
        if (!key %in% names(parameters[[j]])) {
          stop(
            paste(
              "Missing GDINA parameter for item", j,
              "and latent pattern", key
            )
          )
        }
        
        prob_matrix[j, a] <- parameters[[j]][[key]]
      }
      
      if (any(prob_matrix[j, ] <= 0 | prob_matrix[j, ] >= 1)) {
        stop(paste("Invalid GDINA probabilities for item", j, 
                   "(must be in (0,1))."))
      }
    }
    
    return(prob_matrix)
  }
}

get_likelihood <- function(responses, prob_matrix) {
  p <- t(prob_matrix)
  
  likelihoods <- matrix(1, nrow = nrow(responses), ncol = nrow(p))
  
  for(r in 1:nrow(responses)){
    temp_resp <- matrix(responses[r, ], ncol=ncol(p), nrow=nrow(p), byrow = T)
    
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

estimate_alpha <- function(
  responses,
  Q,
  parameters,
  model = "DINA",
  method = c("MLE", "MAP", "EAP"),
  prior = NULL
) {

  method <- match.arg(method)

  responses <- as.matrix(responses)
  N <- nrow(responses)
  K <- ncol(Q)

  skill_patterns <- as.matrix(
    expand.grid(replicate(K, 0:1, simplify = FALSE))
  )
  n_profiles <- nrow(skill_patterns)

  if (is.null(prior))
    prior <- rep(1 / n_profiles, n_profiles)

  prob_matrix <- get_prob_matrix(Q, parameters, skill_patterns, model)

  likelihood <- matrix(NA_real_, N, n_profiles)
  posterior  <- matrix(NA_real_, N, n_profiles)
  alpha_hat  <- matrix(NA_real_, N, K)
  alpha_hat_index <- rep(NA_integer_, N)

  for (i in seq_len(N)) {

    answered_idx <- which(!is.na(responses[i, ]))

    # Nenhuma resposta
    if (length(answered_idx) == 0) {
      posterior[i, ] <- prior
      alpha_hat_index[i] <- which.max(prior)

      if (method %in% c("MLE", "MAP")) {
        alpha_hat[i, ] <- skill_patterns[alpha_hat_index[i], ]
      } else {
        alpha_hat[i, ] <- as.vector(prior %*% skill_patterns)
      }
      next
    }

    resp_obs <- matrix(responses[i, answered_idx], nrow = 1)
    prob_obs <- prob_matrix[answered_idx, , drop = FALSE]

    like_i <- get_likelihood(resp_obs, prob_obs)[1, ]
    likelihood[i, ] <- like_i

    post_i <- get_posterior(matrix(like_i, nrow = 1), prior)[1, ]
    posterior[i, ] <- post_i

    if (method == "MLE") {
      alpha_hat_index[i] <- which.max(like_i)
      alpha_hat[i, ] <- skill_patterns[alpha_hat_index[i], ]
    } else if (method == "MAP") {
      alpha_hat_index[i] <- which.max(post_i)
      alpha_hat[i, ] <- skill_patterns[alpha_hat_index[i], ]
    } else {
      alpha_hat_index[i] <- which.max(post_i)
      alpha_hat[i, ] <- as.vector(post_i %*% skill_patterns)
    }
  }

  colnames(alpha_hat) <- colnames(Q)

  list(
    alpha_hat        = alpha_hat,
    alpha_hat_index  = alpha_hat_index,
    likelihood       = likelihood,
    posterior        = posterior,
    prob_matrix      = prob_matrix
  )
}

customUpdateSkills <- function(design, person, test){

    
    # Globais esperadas
    model <- model
    q_matrix <- q_matrix
    parameters <- cdm_parameters
    prior <- NULL

    # Respostas do respondente (0/1/NA)
    responses <- extract.mirtCAT(person, "responses")
    
    # Método de estimação (MLE / MAP / EAP)
    method <- design@method

    K <- ncol(q_matrix)

    theta_colnames <- paste0("F", 1:K)
    theta_se_colnames <- paste0("F", 1:K, "_SE")

    # -----------------------------
    # Estima habilidades
    # -----------------------------
    est <- estimate_alpha(
        responses = matrix(responses, nrow = 1),
        Q = q_matrix,
        parameters = parameters,
        model = model,
        method = method,
        prior = prior
    )

    theta_hat <- est$alpha_hat[1, , drop = FALSE]

    theta_SE <- matrix(
        NA_real_,
        nrow = 1,
        ncol = K,
        dimnames = list(NULL, theta_se_colnames)
    )

    # -----------------------------
    # Atualiza no mirtCAT
    # -----------------------------
    person$Update_thetas(
        theta    = theta_hat,
        theta_SE = theta_SE
    )

    person$clientData$est <- est
    

    invisible()
}

# --- Item selection criteria functions ---
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
  person,
  responses,
  administered,
  Q,
  parameters,
  prior = NULL,
  model = "DINA",
  criterion,
  estimation_method = c("MAP"),
  use_constraints = FALSE
) {

  estimation_method <- match.arg(estimation_method)

  if (!is.character(criterion) || length(criterion) != 1)
    stop("criterion must be a single character string.")

  if (any(!is.na(responses) & !responses %in% c(0, 1)))
    stop("Responses must be coded as 0/1/NA.")

  if (sum(administered) == 0 && criterion %in% c("KL", "PWKL", "SHE")) {
    warning(
      "Criterion '", criterion,
      "' used with no responses. Posterior equals prior."
    )
  }

  if (is.null(person$clientData$est)) {
    est <- estimate_alpha(
      responses = matrix(responses, nrow = 1),
      Q = Q,
      parameters = parameters,
      model = model,
      method = estimation_method,
      prior = prior
    )
  } else {
    est <- person$clientData$est
  }

  posterior <- est$posterior[1, ]
  alpha_hat_index <- est$alpha_hat_index[1]
  prob_matrix <- est$prob_matrix

  candidate_items <- if (use_constraints) {
    seq_len(nrow(Q))
  } else {
    which(!administered)
  }

  if (length(candidate_items) == 0)
    stop("No candidate items available.")

  if (criterion == "seq") {
    return(list(item = candidate_items[1], scores = NULL))
  }

  if (criterion == "random") {
    return(list(item = sample(candidate_items, 1), scores = NULL))
  }

  scores <- sapply(candidate_items, function(j) {
    switch(
      criterion,
      KL    = KL_criteria(j, alpha_hat_index, prob_matrix),
      PWKL  = PWKL_criteria(j, alpha_hat_index, prob_matrix, posterior),
      MPWKL = MPWKL_criteria(j, prob_matrix, posterior),
      SHE   = -SHE_criteria(j, prob_matrix, posterior),
      stop("Invalid criterion.")
    )
  })

  list(
    item = candidate_items[which.max(scores)],
    scores = scores,
    candidate_items = candidate_items
  )
}

customNextItemCDM <- function(
  person,
  design,
  test,
  model,
  q_matrix,
  parameters,
  criteria,
  prior,
  start_item
) {
  cat('Selecting next item\n')

  # --------------------------------------------------
  # Respostas do respondente (0/1/NA)
  # --------------------------------------------------
  responses <- extract.mirtCAT(person, "responses")
  # person$clientData$posterior <- 'Oi thithi'#est$posterior[1, ]
  # print(person$clientData$posterior)

  # Sanity check
  if (any(!is.na(responses) & !responses %in% c(0, 1))) {
    stop("Responses must be coded as 0/1/NA for CDM.")
  }

  administered <- !is.na(responses)

  # --------------------------------------------------
  # Verifica uso de restrições (mesma lógica antiga)
  # --------------------------------------------------
  constr_fun <- design@constr_fun
  use_constraints <- length(body(constr_fun)) > 1

  # --------------------------------------------------
  # Caso especial: primeiro item
  # --------------------------------------------------
  if (all(!administered) && !is.null(start_item) && !is.na(start_item)) {

    # 1) start_item numérico → item fixo
    if (is.numeric(start_item)) {

      item <- as.integer(start_item)

      if (length(item) != 1)
        stop("start_item numeric must be a single item index.")

      if (item < 1 || item > length(responses))
        stop("start_item numeric is out of item range.")

      return(item)
    }

    # 2) start_item como string → critério especial
    effective_criterion <- as.character(start_item)

  } else {
    # Critério padrão do CAT
    effective_criterion <- criteria
  }

  # --------------------------------------------------
  # Seleção do próximo item (CD-CAT)
  # --------------------------------------------------
  sel <- select_next_item(
    person            = person,
    responses         = responses,
    administered      = administered,
    Q                 = q_matrix,
    parameters        = parameters,
    prior             = prior,
    model             = model,
    criterion         = effective_criterion,
    estimation_method = design@method,
    use_constraints   = use_constraints   # 🔑 CRÍTICO
  )

  # --------------------------------------------------
  # Sem restrições → seleção direta
  # --------------------------------------------------
  if (!use_constraints) {
    return(sel$item)
  }

  # --------------------------------------------------
  # Com restrições → otimização via mirtCAT
  # --------------------------------------------------
  if (is.null(sel$scores)) {
    stop("Scores must be provided when constraints are active.")
  }

  names(sel$scores) <- sel$candidate_items

  best_item <- findNextItem(
    person    = person,
    design    = design,
    test      = test,
    objective = sel$scores
  )

  return(best_item)
}

# --- Termination functions ---
stop_cdcat <- function(
  person,
  administered,
  min_items,
  threshold
) {

  n_administered <- sum(administered)
  if (n_administered < min_items) {
    return(FALSE)
  }

  # -----------------------------
  # Usa posterior já estimada
  # -----------------------------
  est <- person$clientData$est
  if (is.null(est) || is.null(est$posterior)) {
    stop("Posterior not found in person$clientData$est.")
  }

  posterior <- est$posterior[1, ]

  ord <- sort(posterior, decreasing = TRUE)
  cat("Posterior top probabilities:", ord[1:2], "\n")

  # -----------------------------
  # Regras de parada
  # -----------------------------
  if (length(threshold) == 1) {
    # Max posterior rule
    return(ord[1] >= threshold[1])

  } else if (length(threshold) == 2) {
    # Dual rule
    return(
      ord[1] >= threshold[1] &&
      ord[2] <= threshold[2]
    )

  } else {
    stop("threshold must be length 1 (max) or 2 (dual)")
  }
}

customStopCDM <- function(person, design, test) {

  # Globais esperadas
  threshold <- threshold

  # -----------------------------
  # Itens administrados
  # -----------------------------
  responses <- extract.mirtCAT(person, "responses")
  administered <- !is.na(responses)

  # -----------------------------
  # Parâmetros do desenho
  # -----------------------------
  min_items <- extract.mirtCAT(design, "min_items")

  # -----------------------------
  # Decisão de parada
  # -----------------------------
  stop_cdcat(
    person       = person,
    administered = administered,
    min_items    = min_items,
    threshold    = threshold
  )
}


