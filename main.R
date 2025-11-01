r_version <- R.version$version.string
plumber_version <- as.character(packageVersion("plumber"))
app_version <- "1.0.0"

source("utils/serializer.R")
source("bin/env.R")
source("bin/cat.R")
source("bin/mdc.R")
source("bin/plumber.R")
