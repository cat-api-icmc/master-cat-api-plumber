library(mirtCAT)

build_irt_parameters <- function(
    discrimination_list,
    difficulty_list,
    guessing_list
    # upper_asymptote_list
) {
  df <- data.frame(
    a1 = discrimination_list,
    d = -discrimination_list * difficulty_list,
    g = guessing_list
    # u = upper_asymptote_list
  )
  return(df)
}

customNextItemIRT <- function(design, person, test, criteria){
  best_item <- findNextItem(person=person, design=design, test=test, criteria=criteria)
  best_item
}

create_mirt_object <- function(
    parameters,
    item_type = "3PL",
    latent_means = NULL,
    latent_covariance = NULL,
    key = NULL,
    min_category = 0) {
  mirt_object <- mirtCAT::generate.mirt_object(
    parameters = parameters,
    itemtype = item_type,
    latent_means = latent_means,
    latent_covariance = latent_covariance,
    key = key,
    min_category = min_category
  )
  return(mirt_object)
}

generate_pattern <- function(
    mirt_object,
    theta,
    dataframe = NULL) {
  pattern <- mirtCAT::generate_pattern(
    mo = mirt_object,
    Theta = theta,
    df = dataframe
  )
  return(pattern)
}

create_cat_design <- function(
    mirt_object,
    dataframe = NULL,
    method = "MAP",
    criteria = "seq",
    start_item = 1,
    pattern_theta,
    pattern_dataframe = NULL,
    design = design,
    ...) {
  pattern <- generate_pattern(
    mirt_object,
    theta = pattern_theta, dataframe = pattern_dataframe
  )

  cat_design <- mirtCAT(
    mo = mirt_object,
    dataframe = dataframe,
    local_pattern = pattern,
    start_item = start_item,
    criteria = criteria,
    design_elements = TRUE,
    method = method,
    design = design,
    ...
  )

  return(cat_design)
}
