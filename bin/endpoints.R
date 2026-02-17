#* @get /hc
health_check <- function(res) {
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
      trace = paste(rev(sapply(sys.calls(), deparse)), collapse = "\n")
    )
  })
}

#* @post /irt/start-assessment
irt_start_assessment

#* @post /cdm/start-assessment
cdm_start_assessment

#* @post /irt/next-item
irt_next_item

#* @post /cdm/next-item
cdm_next_item

#* @post /get-design-data
get_assessment_data
