# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

submit_log <- function(...){
  si <- Sys.info()
  user_info <- paste(names(si), ": ", si, sep = "", collapse = "; ")
  
  e <- get("e", parent.frame())

  if(e$val == "Yes"){
    # Please edit the link below
  # pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSfjfN0Pb2Gnyj8sksCFJFym90jJBkfgv55lzQFVhoEvXL11hQ/viewform?entry.1752962042"
    pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLScJ2lYafz7lqnhnD9Z7Dw-PZLfhhC3IihZKWkURFGcMseYeGg/viewform?entry.1752962042"
    
    # Do not edit the code below
    if(!grepl("=$", pre_fill_link)){
      pre_fill_link <- paste0(pre_fill_link, "=")
    }
    
    p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}
    
    temp <- tempfile()
    log_ <- getLog()
    nrow_ <- max(unlist(lapply(log_, length)))
    log_tbl <- data.frame(user = rep(log_$user, nrow_),
                          course_name = rep(log_$course_name, nrow_),
                          lesson_name = rep(log_$lesson_name, nrow_),
                          question_number = p(log_$question_number, nrow_, NA),
                          correct = p(log_$correct, nrow_, NA),
                          attempt = p(log_$attempt, nrow_, NA),
                          skipped = p(log_$skipped, nrow_, NA),
                          datetime = p(log_$datetime, nrow_, NA),
                          stringsAsFactors = FALSE)
    write.csv(user_info, file = temp, row.names = FALSE) # drop if not working
    write.csv(log_tbl, file = temp, row.names = FALSE, append = TRUE)
    encoded_log <- base64encode(temp)
    browseURL(paste0(pre_fill_link, encoded_log))
  }
}

test_func <- function() {
  try({
    func <- get('f', globalenv())
    t1 <- identical(func(9), 2*9+3)
    t2 <- identical(func(4), 2*4+3)
    t3 <- identical(func(0), 2*0+3)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_sa <- function() {
  r <- runif(1)
  h <- runif(1)
  try({
    func <- get('sa_cyl', globalenv())
    t1 <- identical(func(r = r, h = h), 2 * pi * r * h + 2 * pi * r ^ 2)
    ok <- all(t1)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_area <- function() {
  r <- runif(1)
  try({
    func <- get('area_circle', globalenv())
    t1 <- identical(func(r = r), pi * r ^ 2)
    ok <- all(t1)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}
