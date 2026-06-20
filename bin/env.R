library(dotenv)

cat("Loading .env file...", append = TRUE)

load_dot_env(file = ".env")

debug <- as.logical(Sys.getenv("DEBUG"))
plumber_host <- Sys.getenv("PLUMBER_HOST")
plumber_port <- as.numeric(Sys.getenv("PLUMBER_PORT"))
django_api_url <- Sys.getenv("DJANGO_API_URL")

# Log estruturado de request/response (logs/requests.jsonl). Opcional — ligue
# em desenvolvimento com PLUMBER_ACCESS_LOG=TRUE. Default: desligado.
access_log <- tolower(Sys.getenv("PLUMBER_ACCESS_LOG")) %in% c("true", "1", "yes", "on", "t")

cat("DONE!\n")
