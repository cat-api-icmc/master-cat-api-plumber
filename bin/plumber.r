library(plumber)

cat("Registering endpoints...")
root <- pr("bin/endpoints.R")
cat("DONE!\n")

cat("Starting plumber API...\n")
root$run(port = plumber_port)
