library(base64enc)

serialize_object <- function(object) {
  return(base64encode(serialize(object, NULL)))
}

deserialize_object <- function(encoded_object) {
  return(unserialize(base64decode(encoded_object)))
}

serialize_design <- function(design) {
  return(serialize_object(design))
}

deserialize_design <- function(encoded_design) {
  return(deserialize_object(encoded_design))
}
