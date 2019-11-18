# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# repeating execution of printing and plotting commands.

# AUTO_DETECT_NEWVAR <- FALSE

match_call <- function(correct_call = NULL) {
  e <- get("e", parent.frame())
  # Trivial case
  if(is.null(correct_call)) return(TRUE)
  # Get full correct call
  full_correct_call <- expand_call(correct_call)  
  # Expand user's expression
  expr <- deparse(e$expr)
  full_user_expr <- expand_call(expr)
  # Compare function calls with full arg names
  identical(full_correct_call, full_user_expr)
}

# Utility function for match_call answer test
# Fills out a function call with full argument names
expand_call <- function(call_string) {
  # Quote expression
  qcall <- parse(text=call_string)[[1]]
  # If expression is not greater than length 1...
  if(length(qcall) <= 1) return(qcall)
  # See if it's an assignment
  is_assign <- is(qcall, "<-")
  # If assignment, process righthandside
  if(is_assign) {
    # Get righthand side
    rhs <- qcall[[3]]
    # Get function from function name
    fun <- match.fun(rhs[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Get expanded call
    full_rhs <- match.call(fun, rhs)
    # Full call
    qcall[[3]] <- full_rhs
  } else { # If not assignment, process whole thing
    # Get function from function name
    fun <- match.fun(qcall[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Full call
    qcall <- match.call(fun, qcall)
  } 
  # Return expanded function call
  qcall
}

notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "No") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    name <- readline_clean("What is your full name? ")
    address <- readline_clean("What is the email address of the person you'd like to notify? ")
    
    # Repeat back to them
    message("\nDoes everything look good?\n")
    message("Your name: ", name, "\n", "Send to: ", address)
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  # Get course and lesson names
  course_name <- attr(e$les, "course_name")
  lesson_name <- attr(e$les, "lesson_name")
  
  subject <- paste(name, "just completed", course_name, "-", lesson_name)
  body = ""
  
  # Send email
  swirl:::email(address, subject, body)
  
  hrule()
  message("I just tried to create a new email with the following info:\n")
  message("To: ", address)
  message("Subject: ", subject)
  message("Body: <empty>")
  
  message("\nIf it didn't work, you can send the same email manually.")
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}
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
  
  e <- get("e", parent.frame())
  # Please edit the link below
  form_link <- "https://docs.google.com/forms/d/e/1FAIpQLSf3zl61CnY1W-BksMj6E1JuSypIC7x0zIAM4hPPxnV-Co-3_w/viewform"
  form_link2 <- "bit.ly/Stats20s19SwirlSubmit"
  # Do not edit the code below
  # if(!grepl("=$", form_link)){
  #   form_link <- paste0(form_link, "=")
  # }
  
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
                        datetime = p(as.POSIXct.numeric(log_$datetime, origin="1970-01-01"), nrow_, NA),
                        stringsAsFactors = FALSE)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  View(encoded_log)
  logname <- paste0("logfile - ", log_$lesson_name, ".txt")
  fileConn<-file(logname)
  writeLines(encoded_log, fileConn)
  close(fileConn)
  # if(clipr_available()){
  #   write_clip(encoded_log)  
  # }
  # write.csv(log_tbl, file = "rawlog")
  if(e$val == "Yes"){
    file.show(logname, title = "Lesson Log")
    browseURL(url = form_link)
    cat(paste0("If there is an error submitting your lesson log, please email the CONTENTS of your encoded log record, the file named:\n\n",
               logname,
               "\n\nlocated in your working directory, along with acompanying screenshots, and system information.\n\n"))
  } else {
    cat(paste0("You have chosen not to submit, unless this is for a good reason (e.g. you are just repeating lessons for practice) you SHOULD submit it yourself.\nYou may do so by copying the encoded log record located in:\n\n",
               logname,
               "\n\nand pasting its contents into the form at:\n\n",
               form_link2, "\n\n"))
  }
}
