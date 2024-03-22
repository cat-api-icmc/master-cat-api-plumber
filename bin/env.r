cat("Loading .env file...", append = TRUE)

load_dot_env(file = ".env")

debug <- as.logical(Sys.getenv("DEBUG"))
plumber_host <- Sys.getenv("PLUMBER_HOST")
plumber_port <- as.numeric(Sys.getenv("PLUMBER_PORT"))
django_api_url <- Sys.getenv("DJANGO_API_URL")

cat("DONE!\n")
