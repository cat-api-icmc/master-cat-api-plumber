
#* @get /hc
function() {
  return(list(status = jsonlite::unbox("Healthy!")))
}

#* @get /echo
function(msg = "Hello, world!") {
  return(response(msg))
}

#* @get /test/serialize_design
function() {
  a <- c(1, 1.2, 0.9, 0.8, 1.1, 1.2, 0.8, 0.7, 0.5, 1)
  d <- c(-1, 1.5, 0, 0.5, -0.5, -1, 0, 0.1, 1.1, -0.2)
  g <- rep(0.2, 10)
  pars <- data.frame(a1 = a, d = d, g = g)
  lc <- matrix(2)

  mo <- create_mirt_object(
    parameters = pars,
    latent_covariance = lc,
  )
  design <- create_design(mo, pattern_theta = 0.1)

  e <- serialize_design(design)
  return(response(e))
}

#* @post /test/serialize_design
function(req) {
  e <- req$body$encoded
  design <- deserialize_design(e)

  print(design)
  print(class(design))
  print(typeof(design))

  return("DONE!")
}

