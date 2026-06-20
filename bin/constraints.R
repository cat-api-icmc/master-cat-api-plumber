# ===============================
# SHADOW CAT CONSTRAINTS
# ===============================
#
# Gera a função `constr_fun(person, test, design)` consumida pelo mirtCAT
# a partir de restrições no formato de triplas (query, op, value).
#
# Cada tripla vira uma linha de restrição de cardinalidade:
#     Σ_{j ∈ query} x_j (op) value
#
# - query: índices de itens (base-1). Lista por vírgula ("1, 5, 6"),
#          intervalos a:b ("1, 3:7, 9") ou "ALL" (todos os itens do banco).
# - op:    "<=", ">=" ou "==" (também aceita "=" e rótulos em português).
# - value: a cota (número escalar).
#
# Entra no pipeline via `resolve_constr_fun`, chamado por
# `irt_start_assessment` (bin/tri-cat.R) e `cdm_start_assessment` (bin/mdc-cat.R).

# -------------------------------------------------
# Normaliza o operador para "==", "<=" ou ">=".
# -------------------------------------------------
normalize_op <- function(op) {
  key <- tolower(trimws(as.character(op)))
  if (key %in% c("=", "==", "igual a", "igual"))                         return("==")
  if (key %in% c("<=", "=<", "≤", "menor ou igual a", "menor ou igual")) return("<=")
  if (key %in% c(">=", "=>", "≥", "maior ou igual a", "maior ou igual")) return(">=")
  abort_unprocessable(
    sprintf("Operador não reconhecido: '%s'. Use <=, >= ou ==.", op)
  )
}

# -------------------------------------------------
# Converte a consulta em um vetor de índices de itens.
# Aceita "ALL", índices soltos e intervalos a:b. Espaços são ignorados.
# -------------------------------------------------
parse_item_query <- function(query, nitems, idx = NA) {
  q <- trimws(as.character(query))
  if (!nzchar(q))
    abort_unprocessable(sprintf("Consulta vazia na restrição %s.", idx))
  if (toupper(q) == "ALL")
    return(seq_len(nitems))

  q    <- gsub("[[:space:]]+", "", q)             # remove espaços internos
  toks <- strsplit(q, ",", fixed = TRUE)[[1]]
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0L)
    abort_unprocessable(
      sprintf("Consulta '%s' (restrição %s): nenhum índice informado.", query, idx)
    )

  ids <- integer(0)
  for (tk in toks) {
    if (grepl(":", tk, fixed = TRUE)) {           # intervalo a:b
      ab <- strsplit(tk, ":", fixed = TRUE)[[1]]
      if (length(ab) != 2L || any(!nzchar(ab)))
        abort_unprocessable(
          sprintf("Intervalo inválido '%s' (restrição %s): use a:b.", tk, idx)
        )
      a <- suppressWarnings(as.integer(ab[1]))
      b <- suppressWarnings(as.integer(ab[2]))
      if (anyNA(c(a, b)))
        abort_unprocessable(
          sprintf("Intervalo inválido '%s' (restrição %s): use inteiros a:b.", tk, idx)
        )
      ids <- c(ids, a:b)
    } else {                                      # índice único
      v <- suppressWarnings(as.integer(tk))
      if (is.na(v))
        abort_unprocessable(
          sprintf("Índice inválido '%s' (restrição %s).", tk, idx)
        )
      ids <- c(ids, v)
    }
  }

  ids <- sort(unique(ids))
  if (any(ids < 1L | ids > nitems))
    abort_unprocessable(
      sprintf("Consulta '%s' (restrição %s): há índices fora do intervalo 1..%d.",
              query, idx, nitems)
    )
  ids
}

# -------------------------------------------------
# Monta o data.frame(lhs, dirs, rhs) consumido pelo mirtCAT.
# Função pura (não depende do mirt) — facilita o teste.
# -------------------------------------------------
build_constr_matrix <- function(constraints, nitems) {
  n_constr <- length(constraints)
  lhs  <- matrix(0, nrow = n_constr, ncol = nitems)
  dirs <- character(n_constr)
  rhs  <- numeric(n_constr)

  for (i in seq_along(constraints)) {
    ids <- parse_item_query(constraints[[i]]$query, nitems, idx = i)
    lhs[i, ids] <- 1
    dirs[i]     <- normalize_op(constraints[[i]]$op)
    rhs[i]      <- as.numeric(unlist(constraints[[i]]$value))
  }

  data.frame(lhs, dirs, rhs)   # formato consumido pelo mirtCAT
}

# -------------------------------------------------
# Fábrica: lista de triplas -> constr_fun(person, test, design).
# Validação ansiosa (op e value) no momento da configuração; a validação
# dos índices depende de nitems e ocorre na primeira seleção de item.
# -------------------------------------------------
make_shadow_constr_fun <- function(constraints) {
  force(constraints)

  for (i in seq_along(constraints)) {
    normalize_op(constraints[[i]]$op)             # valida o operador agora
    v <- suppressWarnings(as.numeric(unlist(constraints[[i]]$value)))
    if (length(v) != 1L || is.na(v))
      abort_unprocessable(
        sprintf("A restrição %d precisa de um 'value' numérico único.", i)
      )
  }

  function(person, test, design) {
    mo     <- extract.mirtCAT(test, "mo")
    nitems <- extract.mirt(mo, "nitems")
    build_constr_matrix(constraints, nitems)
  }
}

# -------------------------------------------------
# Ponte: configs em data.frame (colunas query/op/value) -> lista de triplas.
# Útil quando o JSON é simplificado pelo jsonlite/plumber em data.frame.
# -------------------------------------------------
rows_to_constraints <- function(df,
                                query_col = "query",
                                op_col    = "op",
                                value_col = "value") {
  lapply(seq_len(nrow(df)), function(i) {
    list(query = df[[query_col]][i],
         op    = df[[op_col]][i],
         value = as.numeric(df[[value_col]][i]))
  })
}

# -------------------------------------------------
# Normaliza o que chega em config$shadow_test_config para uma lista de triplas.
# Aceita: data.frame, lista de triplas, ou uma única tripla nomeada.
# -------------------------------------------------
normalize_shadow_constraints <- function(shadow_test_config) {
  if (is.null(shadow_test_config))
    return(list())

  if (is.data.frame(shadow_test_config))
    return(rows_to_constraints(shadow_test_config))

  if (!is.list(shadow_test_config))
    abort_unprocessable(
      "shadow_test_config deve ser uma lista de objetos {query, op, value}."
    )

  # Uma única tripla enviada como objeto (não dentro de um array)
  if (all(c("query", "op", "value") %in% names(shadow_test_config)))
    return(list(shadow_test_config))

  shadow_test_config
}

# -------------------------------------------------
# Constrói o constr_fun a partir de config$shadow_test_config.
# Retorna NULL quando não há restrições estruturadas (permite o fallback
# para o constr_fun string legado).
# -------------------------------------------------
build_shadow_constr_fun <- function(shadow_test_config) {
  constraints <- normalize_shadow_constraints(shadow_test_config)

  if (length(constraints) == 0L)
    return(NULL)

  for (i in seq_along(constraints)) {
    s <- constraints[[i]]
    if (!is.list(s) || !all(c("query", "op", "value") %in% names(s)))
      abort_bad_request(
        sprintf("A restrição %d deve conter os campos 'query', 'op' e 'value'.", i)
      )
  }

  make_shadow_constr_fun(constraints)
}

# -------------------------------------------------
# Dispatcher: prioriza shadow_test_config (triplas); na ausência dele,
# mantém o comportamento legado de constr_fun como string de função R.
# -------------------------------------------------
resolve_constr_fun <- function(shadow_test_config = NULL, constr_fun_string = NULL) {
  shadow <- build_shadow_constr_fun(shadow_test_config)
  if (!is.null(shadow))
    return(shadow)

  build_constr_fun(constr_fun_string)
}
