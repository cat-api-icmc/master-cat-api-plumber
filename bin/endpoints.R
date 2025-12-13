#* @get /hc
#* @description
#* Endpoint de verificação de saúde (Health Check) da API.
#*
#* Retorna:
#* - `status`: mensagem indicando que a API está saudável, incluindo versões de R, Plumber e da aplicação embutidas no texto.
#*
#* Em caso de erro, retorna:
#* - `error`: mensagem de erro
#* - `trace`: rastreamento da pilha de execução
function(res) {
  tryCatch({

    # Variáveis globais esperadas
    r_version <- get0("r_version", ifnotfound = "unknown", inherits = TRUE)
    plumber_version <- get0("plumber_version", ifnotfound = "unknown", inherits = TRUE)
    app_version <- get0("app_version", ifnotfound = "unknown", inherits = TRUE)

    res$status <- 200
    return(list(
      status = jsonlite::unbox(
        sprintf("Healthy! %s | plumber %s | api %s", r_version, plumber_version, app_version)
      )))

  }, error = function(e) {
    # Captura e retorna erro detalhado
    res$status <- 500
    list(
      error = e$message,
      trace = paste(capture.output(traceback()), collapse = "\n")
    )
  })
}


#* @apiTitle IRT Assessment API
#* @apiDescription API responsável por iniciar um teste adaptativo computadorizado (CAT)
#* baseado em modelos de Resposta ao Item (IRT), como 1PL, 2PL e 3PL.
#* 
#* O endpoint `/irt/start-assessment` recebe um conjunto de parâmetros
#* de itens e de configuração do teste, cria o objeto IRT e o design do CAT,
#* e retorna o próximo item a ser apresentado ao participante.
#*
#* @param req Corpo da requisição contendo:
#*   - `questions`: lista com parâmetros IRT em `questions$params`, contendo:
#*       - `irt_discrimination`: vetor de discriminações
#*       - `irt_difficulty`: vetor de dificuldades
#*       - `irt_guess`: vetor de parâmetros de acerto ao acaso
#*   - `config`: configurações gerais do teste adaptativo, incluindo:
#*       - `model_type`: tipo de modelo IRT (1PL, 2PL ou 3PL)
#*       - `criteria`: critério de seleção do próximo item
#*       - `start_item`: índice do primeiro item
#*       - `thetas_start`: valores iniciais de theta
#*       - `pattern_theta`: padrão de theta em avaliações multidimensionais
#*       - `design`: critérios de parada (`min_sem`, `delta_thetas`, `min_items`, `max_items`, `max_time`)
#*
#* @return JSON com:
#*   - `status`: "success" em caso de sucesso
#*   - `next_index`: índice do próximo item
#*   - `stop`: flag indicando se o teste deve parar
#*   - `design`: design serializado do CAT
#*   - Em caso de erro: `status = "error"`, `message`, `call` e `traceback`
#*
#* @post /irt/start-assessment
function(req, res) {
  tryCatch({

    # ===============================
    # 🔹 INPUT PARSING
    # ===============================
    questions <- req$body$questions
    config <- req$body$config

    model <- config$model_type # irt model
    start_item <- config$start_item
    criteria <- config$criteria

    thetas_start <- config$thetas_start
    pattern_theta <- config$pattern_theta

    # stopping criteria
    min_sem <- config$min_sem #config$design$min_sem
    delta_thetas <- config$delta_thetas
    min_items <- config$min_items
    max_items <- config$max_items
    max_time <- ifelse(
      !is.null(config$max_time),
      config$max_time,
      Inf
    )

    # print all request parameters for debugging
    cat("Starting IRT assessment with parameters:\n"
        , paste("Model:", model, "\n")
        , paste("Start Item:", start_item, "\n")
        , paste("Criteria:", criteria, "\n")
        , paste("Thetas Start:", toString(thetas_start), "\n")
        , paste("Pattern Theta:", toString(pattern_theta), "\n")
        , paste("Design - min_sem:", toString(min_sem), "\n")
        , paste("Design - delta_thetas:", toString(delta_thetas), "\n")
        , paste("Design - min_items:", min_items, "\n")
        , paste("Design - max_items:", max_items, "\n")
        , paste("Design - max_time:", max_time, "\n")
    )

    design <- list(
      min_SEM = min_sem,
      delta_thetas = delta_thetas,
      thetas.start = thetas_start,
      min_items = min_items,
      max_items = max_items,
      max_time = max_time #,
      # customNextItem = customNextItemIRT # flexibilixa o uso de critérios customizados
    )

    # ===============================
    # 🔹 IRT OBJECT CREATION
    # ===============================
    irt_params <- build_irt_parameters(
      discrimination_list = questions$params$irt_discrimination,
      difficulty_list = questions$params$irt_difficulty,
      guessing_list = questions$params$irt_guess
      # upper_asymptote_list = questions$params$irt_upper_asymptote
    )

    mo <- create_mirt_object(
      item_type = ifelse(
        model %in% list("3PL", "2PL", "1PL"),
        model,
        "3PL"
      ),
      parameters = irt_params,
      latent_covariance = matrix(2)
    )

    # ===============================
    # 🔹 CAT DESIGN
    # ===============================
    cat_design <- create_cat_design(
      mo,
      pattern_theta = pattern_theta,
      start_item = start_item,
      criteria = criteria,
      method = "EAP", # default method
      design = design
    )
    
    # cat("Selecting next item using criteria:", cat_design$design@criteria, "\n")

    cat_design$item_time_history <- list()
    cat_design$last_answer_time <- Sys.time()

    # Próximo item
    # next_index <- mirtCAT::findNextItem(cat_design)
    # next_index <- cat_design$design@start_item  
    next_index <- customNextItemIRT(
        person = cat_design$person,
        design = cat_design$design,
        test = cat_design$test,
        criteria = criteria
      )
    cat("First item selected:", next_index, "-", criteria, "\n")

    # ===============================
    # 🔹 RETURN SUCCESS
    # ===============================
    res$status <- 200
    return(list(
      status = "success",
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  },
  error = function(e) {
    # ===============================
    # 🔹 ERROR HANDLING
    # ===============================
    trace <- paste(capture.output(traceback()), collapse = "\n")

    # Loga o erro no console (ou arquivo)
    cat(sprintf(
      "[%s] ERRO em /irt/start-assessment: %s\nCALL: %s\nTRACE:\n%s\n\n",
      Sys.time(),
      e$message,
      deparse(e$call),
      trace
    ))

    res$status <- 500
    list(
      status = "error",
      message = e$message,
      call = deparse(e$call),
      traceback = trace
    )
  })
}


#* @apiTitle CDM Assessment API
#* @apiDescription
#* Inicia um teste adaptativo computadorizado (CAT) baseado em Modelos de Diagnóstico Cognitivo (CDM),
#* como DINA, DINO e GDINA.
#*
#* O endpoint `/cdm/start-assessment` recebe os parâmetros dos itens (Q-matrix e parâmetros do modelo),
#* as configurações do teste e retorna o índice do próximo item a ser administrado,
#* juntamente com o design serializado do CAT.
#*
#* @param req Corpo da requisição contendo:
#*   - `questions`: lista de questões com parâmetros e Q-matrix (utilizada por `build_cdm_parameters`)
#*   - `config`: lista com parâmetros de configuração, incluindo:
#*       - `model_type`: tipo de modelo CDM (DINA, DINO, GDINA)
#*       - `start_item`: índice do primeiro item a ser aplicado
#*       - `criteria`: critério de seleção (validado)
#*       - `method`: método de estimação (EAP, MAP, MLE)
#*       - `thetas_start`: vetor inicial de habilidades (opcional)
#*       - `pattern_theta`: padrão de habilidades multidimensionais (opcional)
#*       - `design`: critérios de parada (`min_sem`, `min_items`, `max_items`, `max_time`)
#*
#* Observações:
#* - O design interno utiliza `min_SEM`, `thetas.start` e funções customizadas (`customUpdateThetas`, `customNextItemCDM`).
#* - Na inicialização é usado `criteria = "custom"` internamente; o próximo item inicial é `start_item`.
#*
#* @return JSON com:
#*   - `status`: "success" em caso de sucesso
#*   - `next_index`: índice do próximo item (igual a `start_item` na inicialização)
#*   - `stop`: flag indicando se o teste deve parar
#*   - `model_type`: modelo utilizado (serializado)
#*   - `questions`: lista de questões (serializada)
#*   - `q_matrix`: matriz Q (serializada)
#*   - `design`: design do CAT (serializado)
#*   - `criteria`: critério de seleção informado na configuração
#*
#* Em caso de erro, retorna:
#*   - `status = "error"`
#*   - `message`: mensagem do erro
#*   - `call`: expressão que causou o erro
#*   - `traceback`: pilha de chamadas
#*
#* @post /cdm/start-assessment
function(req, res) {
  tryCatch({

    # ===============================
    # 🔹 PARSING DA REQUISIÇÃO
    # ===============================
    questions <- req$body$questions
    config <- req$body$config

    qmatrix_values <- build_cdm_parameters(questions)
    cdm_parameters <- qmatrix_values$parameters  # não usado diretamente
    q_matrix <- qmatrix_values$qmatrix
    n_skills <- qmatrix_values$n_skills

    # ===============================
    # 🔹 CONFIGURAÇÕES DO TESTE
    # ===============================
    model <- config$model_type # cdm model
    criteria <- config$criteria
    start_item <- config$start_item
    thetas_start <- config$thetas_start
    pattern_theta <- config$pattern_theta
    method <- config$method

    # critérios de parada
    min_sem <- config$min_sem
    delta_thetas <- config$delta_thetas
    min_items <- max(config$min_items, 2)
    max_items <- config$max_items
    max_time <- ifelse(
      !is.null(config$max_time),
      config$max_time,
      Inf
    )

    # print all request parameters for debugging
    cat("Starting CMD assessment with parameters:\n"
        , paste("Model:", model, "\n")
        , paste("Start Item:", start_item, "\n")
        , paste("Criteria:", criteria, "\n")
        , paste("Thetas Start:", toString(thetas_start), "\n")
        , paste("Pattern Theta:", toString(pattern_theta), "\n")
        , paste("Design - min_sem:", toString(min_sem), "\n")
        , paste("Design - delta_thetas:", toString(delta_thetas), "\n")
        , paste("Design - min_items:", min_items, "\n")
        , paste("Design - max_items:", max_items, "\n")
        , paste("Design - max_time:", max_time, "\n")
    )

    allowed_methods <- c("EAP", "MAP", "MLE")
    allowed_models <- c("DINA", "DINO", "GDINA")
    allowed_criteria <- c("seq", "random", "SHE", "KL", "PWKL", "MPWKL")

    if (!method %in% allowed_methods) {
      stop("Método não permitido, escolha entre: ", paste(allowed_methods, collapse = ", "), ".")
    }
    if (!model %in% allowed_models) {
      stop("Modelo não permitido, escolha entre: ", paste(allowed_models, collapse = ", "), ".")
    }
    if (!criteria %in% allowed_criteria) {
      stop("Critério não permitido, escolha entre: ", paste(allowed_criteria, collapse = ", "), ".")
    }

    
    # ===============================
    # 🔹 DESIGN DO CAT
    # ===============================
    design <- list(
      min_SEM = 0.3,
      thetas.start = thetas_start,
      min_items = min_items, # garante pelo menos 2 items
      max_items = max_items,
      max_time = max_time,
      customUpdateThetas = customUpdateThetas,
      customNextItem = customNextItemCDM # flexibilixa o uso de critérios customizados
    )

    # ===============================
    # 🔹 OBJETO MIRT
    # ===============================
    params <- generate_fake_mirt_pars(q_matrix)
    trait_cov <- diag(ncol(q_matrix))

    mo <- create_mirt_object(
      item_type = "3PL", # placeholder (model),
      parameters = params,
      latent_covariance = trait_cov
    )

    # ===============================
    # 🔹 CRIAÇÃO DO DESIGN CAT
    # ===============================
    cat_design <- create_cat_design(
      mo,
      pattern_theta = rep(0, n_skills),
      method = method,
      start_item = start_item,
      design = design
    )
  
    cat_design$item_time_history <- list()
    cat_design$last_answer_time <- Sys.time()

    next_index <- customNextItemCDM(
        person = cat_design$person,
        design = cat_design$design,
        test = cat_design$test,
        model = model, 
        q_matrix = q_matrix, 
        parameters = cdm_parameters, 
        criteria = criteria
      )
    cat("First item selected:", next_index, "-", criteria, "\n")

    # ===============================
    # 🔹 RETORNO DE SUCESSO
    # ===============================
    res$status <- 200
    return(list(
      status = "success",
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      model_type = jsonlite::unbox(serialize_design(model)),
      questions = jsonlite::unbox(serialize_design(questions)),
      q_matrix = jsonlite::unbox(serialize_design(q_matrix)),
      design = jsonlite::unbox(serialize_design(cat_design)),
      criteria = jsonlite::unbox(config$criteria)
    ))

  },
  error = function(e) {
    # ===============================
    # 🔹 TRATAMENTO DE ERROS
    # ===============================
    trace <- paste(capture.output(traceback(2)), collapse = "\n")

    # cat(sprintf(
    #   "[%s] ERRO em /cdm/start-assessment: %s\nCALL: %s\nTRACE:\n%s\n\n",
    #   Sys.time(),
    #   e$message,
    #   deparse(e$call),
    #   trace
    # ))

    res$status <- 500
    list(
      status = "error",
      message = e$message,
      call = deparse(e$call),
      traceback = trace
    )
  })
}



#* @post /irt/next-item
#* @description
#* Atualiza o estado do teste adaptativo IRT após o envio de uma nova resposta.
#* 
#* Este endpoint recebe:
#* - `design`: o objeto de design serializado que representa o estado atual do teste
#* - `answer`: a resposta dada pelo participante à questão anterior
#* - `previous_index`: o índice do item previamente respondido
#*
#* O endpoint:
#* 1. Desserializa o design do teste (`deserialize_design`)
#* 2. Atualiza os parâmetros de estimativa do participante (`updateDesign`)
#* 3. Registra o tempo de resposta
#* 4. Seleciona o próximo item a ser administrado usando `mirtCAT::findNextItem`
#*
#* Retorna:
#* - `next_index`: índice do próximo item (ou `0` se `stop = TRUE`)
#* - `stop`: indica se o teste deve parar
#* - `design`: novo design serializado
#*
#* Em caso de erro, retorna:
#* - `error`: mensagem de erro
#* - `trace`: rastreamento do erro
function(req, res) {
  tryCatch({

    e_design <- req$body$design
    answer <- req$body$answer
    prev_item <- req$body$previous_index

    # config <- req$body$config # nao é mais necessário
    # criteria <- config$criteria # nao é mais necessário

    # desserializa e atualiza o design
    cat_design <- mirtCAT::updateDesign(
      deserialize_design(e_design),
      new_item = prev_item,
      new_response = answer,
      updateTheta = TRUE
    )

    criteria <- cat_design$design@criteria

    # registra o tempo da resposta
    now <- Sys.time()
    cat_design$item_time_history <- append(
      cat_design$item_time_history,
      as.numeric(difftime(
        now, cat_design$last_answer_time,
        units = "secs"
      ))
    )
    cat_design$last_answer_time <- now
    
    # obtém o próximo item
    # next_index <- ifelse(
    #   !cat_design$design@stop_now,
    #   mirtCAT::findNextItem(cat_design),
    #   0
    # )
    if (cat_design$design@stop_now) {
      next_index <- 0
    } else {
      next_index <- customNextItemIRT(
        person = cat_design$person,
        design = cat_design$design,
        test = cat_design$test,
        criteria = criteria
      )
    }

    if(!cat_design$design@stop_now) cat("Next item selected:", next_index, "-", criteria, "\n")
    if(cat_design$design@stop_now) cat("Test stopped.\n")
    
    res$status <- 200
    return(list(
      status = "success",
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  }, error = function(e) {
    # captura e retorna erro detalhado
    res$status <- 500
    list(
      error = e$message,
      trace = paste(capture.output(traceback(2)), collapse = "\n")
    )
  })
}



#* @post /cdm/next-item
#* @description
#* Atualiza o estado do teste adaptativo CDM (Cognitive Diagnostic Model) após a resposta do participante.
#*
#* Este endpoint recebe:
#* - `design`: o objeto de design serializado que representa o estado atual do teste
#* - `answer`: a resposta dada pelo participante à questão anterior
#* - `previous_index`: o índice do item anteriormente respondido
#* - `questions`: lista de questões com parâmetros CDM
#* - `config`: configurações do teste, incluindo:
#*     - `model_type`: tipo de modelo CDM (DINA, DINO, GDINA)
#*     - `criteria`: critério de seleção (seq, random, SHE, KL, PWKL, MPWKL)
#*     - `method`: método de estimação (EAP, MAP, MLE)
#*
#* O endpoint:
#* 1. Reconstrói os parâmetros CDM (`build_cdm_parameters`)
#* 2. Define variáveis globais de modelo e matriz Q
#* 3. Atualiza o design do teste (`updateDesign`)
#* 4. Registra o tempo de resposta
#* 5. Calcula o próximo item a ser apresentado usando `customNextItemCDM`
#*
#* Retorna:
#* - `next_index`: índice do próximo item (ou `0` se `stop = TRUE`)
#* - `stop`: indica se o teste deve parar
#* - `criteria`: critério de seleção utilizado
#* - `model_type`, `questions`, `q_matrix`, `design`: elementos serializados do estado atual
#*
#* Em caso de erro, retorna:
#* - `error`: mensagem de erro
#* - `trace`: rastreamento completo da pilha de execução
function(req, res) {
  tryCatch({
    e_design <- req$body$design
    answer <- req$body$answer
    prev_item <- req$body$previous_index

    questions <- req$body$questions
    qmatrix_values <- build_cdm_parameters(questions)
    cdm_parameters <- qmatrix_values$parameters
    q_matrix <- qmatrix_values$qmatrix
    n_skills <- qmatrix_values$n_skills

    config <- req$body$config
    model_type <- config$model_type
    criteria <- config$criteria
    method <- config$method

    allowed_methods <- c("EAP", "MAP", "MLE")
    allowed_models <- c("DINA", "DINO", "GDINA")
    allowed_criteria <- c("seq", "random", "SHE", "KL", "PWKL", "MPWKL")

    if (!method %in% allowed_methods) {
      stop("Método não permitido, escolha entre: ", paste(allowed_methods, collapse = ", "), ".")
    }
    if (!model_type %in% allowed_models) {
      stop("Modelo não permitido, escolha entre: ", paste(allowed_models, collapse = ", "), ".")
    }
    if (!criteria %in% allowed_criteria) {
      stop("Critério não permitido, escolha entre: ", paste(allowed_criteria, collapse = ", "), ".")
    }

    # define variáveis globais necessárias para funções auxiliares
    model <<- model_type
    criteria <<- criteria
    cdm_parameters <<- cdm_parameters
    q_matrix <<- q_matrix

    # desserializa e atualiza o design
    cat_design <- mirtCAT::updateDesign(
      deserialize_design(e_design),
      new_item = prev_item,
      new_response = answer,
      updateTheta = TRUE
    )

    # registra tempo de resposta
    now <- Sys.time()
    cat_design$item_time_history <- append(
      cat_design$item_time_history,
      as.numeric(difftime(
        now, cat_design$last_answer_time,
        units = "secs"
      ))
    )
    cat_design$last_answer_time <- now

    # determina o próximo item
    if (cat_design$design@stop_now) {
      next_index <- 0
    } else {
      next_index <- customNextItemCDM(
        person = cat_design$person,
        design = cat_design$design,
        test = cat_design$test,
        model = model, 
        q_matrix = q_matrix, 
        parameters = cdm_parameters, 
        criteria = criteria
      )
    }
    if(!cat_design$design@stop_now) cat("Next item selected:", next_index, "-", criteria, "\n")
    if(cat_design$design@stop_now) cat("Test stopped.\n")

    res$status <- 200
    return(list(
      status = "success",
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      criteria = jsonlite::unbox(criteria),
      model_type = jsonlite::unbox(serialize_design(model_type)),
      questions = jsonlite::unbox(serialize_design(questions)),
      q_matrix = jsonlite::unbox(serialize_design(q_matrix)),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  }, error = function(e) {
    # captura e retorna erro detalhado
    res$status <- 500
    list(
      error = e$message,
      trace = paste(capture.output(traceback()), collapse = "\n")
    )
  })
}



#* @post /get-design-data
#* @description
#* Retorna dados resumidos do estado atual do teste adaptativo (IRT ou CDM).
#*
#* Este endpoint recebe:
#* - `design`: objeto de design serializado representando o estado atual do teste.
#*
#* O endpoint:
#* 1. Desserializa o objeto `design` para restaurar o estado interno do teste.
#* 2. Extrai informações sobre:
#*    - histórico de itens respondidos
#*    - respostas dadas
#*    - tempos de resposta
#*    - histórico de estimativas de habilidade (theta)
#*    - erro padrão das estimativas
#*
#* Retorna:
#* - `item_history`: índices dos itens respondidos
#* - `response_history`: respostas fornecidas pelo participante
#* - `item_time_history`: tempo de resposta para cada item
#* - `last_answer_time`: horário da última resposta
#* - `theta_history`: histórico das estimativas de theta
#* - `standard_error_history`: histórico dos erros-padrão associados a cada theta
#*
#* Em caso de erro, retorna:
#* - `error`: mensagem de erro
#* - `trace`: rastreamento completo da pilha de execução
function(req, res) {
  tryCatch({

    e_design <- req$body$design
    cat_design <- deserialize_design(e_design)

    item_history <- cat_design$person$items_answered
    response_history <- cat_design$person$responses
    last_answer_time <- jsonlite::unbox(cat_design$last_answer_time)

    item_time_history <- cat_design$item_time_history

    theta_history <- cat_design$person$thetas_history

    standard_error_history <- cat_design$person$thetas_SE_history

    res$status <- 200

    response <- list(
      item_history = item_history,
      response_history = response_history,
      item_time_history = item_time_history,
      last_answer_time = last_answer_time,
      theta_history = theta_history,
      standard_error_history = standard_error_history
    )


    return(response)

  }, error = function(e) {
    # Captura e retorna erro detalhado
    res$status <- 500
    list(
      error = e$message,
      trace = paste(capture.output(traceback()), collapse = "\n")
    )
  })
}
