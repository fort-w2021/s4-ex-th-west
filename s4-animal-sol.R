library(methods)

# make-name function
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

validate_animal <- function(object, sub_class = is(object)[[1]]) {
  min_hide <- numeric(0)
  max_hide <- numeric(0)
  min_weight <- numeric(0)
  max_weight <- numeric(0)
  min_seek <- numeric(0)
  max_seek <- numeric(0)
  sub_class <- match.arg(sub_class)
  switch(sub_class,
    mouse = {
      min_hide <- 0.6
      max_hide <- 1
      min_weight <- 0.5
      max_weight <- 1
    },
    rabbit = {
      min_hide <- 0.3
      max_hide <- 0.8
      min_weight <- 1
      max_weight <- 5
    },
    deer = {
      min_hide <- 0.2
      max_hide <- 0.7
      min_weight <- 15
      max_weight <- 30
    },
    hawk = {
      min_seek <- 0.6
      max_seek <- 1
      min_weight <- 3
      max_weight <- 8
    },
    lynx = {
      min_seek <- 0.5
      max_seek <- 0.9
      min_weight <- 20
      max_weight <- 60
    },
    prey = {
      min_hide <- 0
      max_hide <- 1
    },
    predator = {
      min_seek <- 0
      max_seek <- 1
    }
  )

  invalids <- character(0)
  if (is(object)[[1]] %in% c("mouse", "rabbit", "deer", "hawk", "lynx")) {
    wrong_weight <- !(object@weight >= min_weight && object@weight <= max_weight)
    if (wrong_weight) invalids <- paste0("weight must be in [", min_weight, ", ", max_weight, "]")
  }
  if (is.element("prey", is(object))) {
    wrong_hide <- !(object@hide >= min_hide && object@hide <= max_hide)
    if (wrong_hide) invalids <- paste0("hide factor must be in [", min_hide, ", ", max_hide, "]")
  }

  if (is.element("predator", is(object))) {
    wrong_seek <- !(object@seek >= min_seek && object@seek <= max_seek)
    if (wrong_seek) invalids <- paste0("seek factor must be in [", min_seek, ", ", max_seek, "]")
  }

  if (length(invalids)) invalids else TRUE
}

## animal
setClass(
  Class = "animal",
  slots = list(
    name = "character",
    weight = "numeric",
    female = "logical"
  ),
  prototype = list(
    name = make_name(),
    weight = 1,
    female = FALSE
  ),
  validity = function(object) {
    invalids <- character(0)
    no_name <- nchar(object@name) == 0
    if (no_name) invalids <- "no <name> provided."
    if (length(invalids)) invalids else TRUE
  }
)
# prey
setClass(
  Class = "prey",
  slots = list(
    hide = "numeric"
  ),
  contains = "animal",
  prototype = list(
    hide = 1
  ),
  validity = function(object) {
    validate_animal(object)
  }
)



# mouse-class
setClass(
  Class = "mouse",
  contains = c("animal", "prey"),
  validity = function(object) {
    validate_animal(object)
  }
)
# mouse-constructor
mouse <- function(name = make_name(),
                  weight = runif(n = 1, min = 0.5, max = 1),
                  hide = runif(n = 1, min = 0.6, max = 1),
                  female = FALSE) {
  new("mouse", name = name, weight = weight, hide = hide, female = female)
}

# rabbit-class
setClass(
  Class = "rabbit",
  contains = c("animal", "prey"),
  validity = function(object) {
    validate_animal(object)
  }
)

# rabbit-constructor
rabbit <- function(name = make_name(),
                   weight = runif(n = 1, min = 1, max = 5),
                   hide = runif(n = 1, min = 0.3, max = 0.8),
                   female = FALSE) {
  new("rabbit", name = name, weight = weight, hide = hide, female = female)
}

# deer- class
setClass(
  Class = "deer",
  contains = c("animal", "prey"),
  validity = function(object) {
    validate_animal(object)
  }
)

# deer-constructor
deer <- function(name = make_name(),
                 weight = runif(n = 1, min = 15, max = 30),
                 hide = runif(n = 1, min = 0.2, max = 0.7),
                 female = FALSE) {
  new("deer", name = name, weight = weight, hide = hide, female = female)
}

# predator
setClass(
  Class = "predator",
  slots = list(seek = "numeric"),
  contains = "animal",
  prototype = list(seek = 1),
  validity = function(object) {
    validate_animal(object)
  }
)

# hawk-class

setClass(
  Class = "hawk",
  contains = c("animal", "predator"),
  validity = function(object) {
    validate_animal(object)
  }
)

# hawk-constructor
hawk <- function(name = make_name(),
                 weight = runif(n = 1, min = 3, max = 8),
                 seek = runif(n = 1, min = 0.6, max = 1),
                 female = FALSE) {
  new("hawk", name = name, weight = weight, seek = seek, female = female)
}

# lynx-class
setClass(
  Class = "lynx",
  contains = c("animal", "predator"),
  validity = function(object) {
    validate_animal(object)
  }
)
# lynx-constructor
lynx <- function(name = make_name(),
                 weight = runif(n = 1, min = 20, max = 60),
                 seek = runif(n = 1, min = 0.5, max = 0.9),
                 female = FALSE) {
  new("lynx", name = name, weight = weight, seek = seek, female = female)
}
