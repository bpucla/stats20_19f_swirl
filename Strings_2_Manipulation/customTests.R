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
