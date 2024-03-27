library(plumber)

cat("Registering endpoints...")
root <- pr("bin/endpoints.R")
cat("DONE!\n")

cat("Starting plumber API...\n")
root$run(host = plumber_host, port = plumber_port)
