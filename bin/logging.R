# ===============================
# ACCESS LOG (um arquivo por request)
# ===============================
#
# Log estruturado de request/response das rotas da API, via hooks do plumber.
# Cada requisição vira um arquivo JSON (pretty) em
# logs/requests/<endpoint>_<timestamp>_<seq>.json, com método, rota, status,
# duração, corpo do request e corpo da response. Pasta montada por volume no
# docker-compose.
#
# Registrado em bin/plumber.R com register_access_log(root).

.ACCESS_LOG_DIR <- "logs"
.REDACT_FIELDS  <- c("design")   # campos volumosos redigidos no log (blob base64)

# Parseia o corpo (string JSON) para objeto R; mantém como string se não for JSON.
.parse_json <- function(raw) {
  if (is.null(raw)) return(NULL)
  raw <- paste(as.character(raw), collapse = "")
  if (!nzchar(raw)) return(NULL)
  tryCatch(jsonlite::fromJSON(raw, simplifyVector = TRUE),
           error = function(e) raw)
}

# Redige campos volumosos (ex.: 'design') de um corpo parseado.
.redact <- function(x) {
  if (is.list(x))
    for (f in .REDACT_FIELDS)
      if (!is.null(x[[f]])) x[[f]] <- "<omitido>"
  x
}

register_access_log <- function(pr) {

  # Caminho absoluto resolvido na inicialização: o cwd do processo pode mudar
  # em runtime, então não dependemos de caminho relativo na hora de gravar.
  # Um arquivo por request em logs/requests/.
  log_dir <- normalizePath(file.path(getwd(), .ACCESS_LOG_DIR, "requests"), mustWork = FALSE)
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  seq_counter <- 0L  # garante unicidade de nome no mesmo milissegundo

  # Marca o início para medir a duração.
  pr$registerHook("preroute", function(data, req, res) {
    data$start <- Sys.time()
  })

  # Após serializar a resposta: monta a linha JSON e grava.
  pr$registerHook("postserialize", function(data, req, res, value) {

    req_body <- tryCatch({
      raw <- if (!is.null(req$postBody)) paste(as.character(req$postBody), collapse = "") else ""
      obj <- if (nzchar(raw)) .parse_json(raw) else req$body
      if (is.list(obj) && length(obj) == 0) obj <- NULL
      .redact(obj)
    }, error = function(e) NULL)

    res_body <- tryCatch({
      b <- res$body
      if (is.raw(b)) b <- rawToChar(b)
      .redact(.parse_json(b))
    }, error = function(e) NULL)

    dur_ms <- tryCatch(
      round(as.numeric(difftime(Sys.time(), data$start, units = "secs")) * 1000, 1),
      error = function(e) NA_real_
    )

    entry <- list(
      ts          = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
      method      = req$REQUEST_METHOD,
      path        = req$PATH_INFO,
      query       = if (nzchar(req$QUERY_STRING %||% "")) req$QUERY_STRING else NULL,
      status      = res$status %||% 200L,
      duration_ms = dur_ms,
      request     = req_body,
      response    = res_body
    )

    # Nome do arquivo: <endpoint>_<timestamp>_<seq>.json
    endpoint <- gsub("^/", "", req$PATH_INFO %||% "")
    endpoint <- gsub("[^A-Za-z0-9._-]", "_", endpoint)
    if (!nzchar(endpoint)) endpoint <- "root"
    ts <- format(Sys.time(), "%Y%m%d-%H%M%OS3")
    seq_counter <<- seq_counter + 1L
    fname <- sprintf("%s_%s_%03d.json", endpoint, ts, seq_counter)

    line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null",
                             na = "null", pretty = TRUE)
    tryCatch(
      writeLines(line, file.path(log_dir, fname)),
      error = function(e) NULL   # log nunca pode quebrar a resposta
    )

    value
  })
}
