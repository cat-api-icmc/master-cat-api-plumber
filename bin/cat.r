library(mirtCAT)

create_mirt_object <- function(
    pars,
    itemtype = "3PL",
    latent_covariance = matrix(2)) {
  mirt_object <- mirtCAT::generate.mirt_object(
    pars,
    itemtype = itemtype, latent_covariance = latent_covariance
  )
  return(mirt_object)
}

generate_pattern <- function(mirt_object, theta) {
  pattern <- mirtCAT::generate_pattern(mirt_object, Theta = theta)
  return(pattern)
}

create_design <- function(
    mirt_object,
    pattern_theta,
    start_item = "MI",
    criteria = "MI") {
  pattern <- generate_pattern(mirt_object, Theta = pattern_theta)

  design <- mirtCAT(
    mo = mirt_object,
    local_pattern = pattern,
    start_item = start_item,
    criteria = criteria
  )

  return(design)
}
