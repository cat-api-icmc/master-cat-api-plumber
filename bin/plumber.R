library(plumber)

cat("Registering endpoints...")
root <- pr("bin/endpoints.R")
cat("DONE!\n")

if (access_log) {
  register_access_log(root)
  cat("Access log: ON (logs/requests.jsonl)\n")
} else {
  cat("Access log: OFF\n")
}

cat("Starting plumber API...\n")
root$run(host = plumber_host, port = plumber_port)
