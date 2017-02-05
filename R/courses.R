clicker.running.courses.observer = function(app=getApp(), glob=app$glob) {
  restore.point("get.running.clicker.courses")
  running.dir = glob$running.dir
  files = list.files(running.dir, full.names=FALSE)
}


write.clicker.running = function(courseid="", need.login=FALSE, code="", active.min=60*4, clicker.dir=NULL) {
  restore.point("write.clicker.running")

  file = paste0(
    as.integer(Sys.time())+active.min*60,
    "-",first.non.null(courseid,""),
    "-",need.login,
    "-",code,
    "-"
  )
  writeLines("",file.path(clicker.dir,"running",file))
}

get.running.clicker.courses = function(clicker.dir) {
  restore.point("get.running.clicker.courses")
  running.dir = file.path(clicker.dir, "running")

  files = list.files(running.dir, full.names=FALSE)
  if (length(files)==0) return(NULL)

  df = as_data_frame(do.call("rbind",strsplit(files,"-", fixed=TRUE)))
  names(df) = c("stop","courseid","need.login","code")
  df$file = files
  df$stop = as.integer(df$stop)
  df$need.login = as.logical(df$need.login)

  df = df[order(-df$stop),]
  time = as.integer(Sys.time())

  # remove running files for courses that are not active anymore or are duplicated
  del = df$stop < time | duplicated(df$courseid)

  # try to delete old files
  for (file in df$file[del]) {
    try(file.remove(file.path(running.dir,file)))
  }
  df = df[!del,]
  if (NROW(df)==0) return(NULL)
  df
}


login.find.course = function(courseid=NULL, coursecode=NULL, courses=getApp()$glob$running.courses) {
  if (!is.null(courseid)) {
    return(list(courseid=courseid, ok=TRUE))
  }
  return(list(ok=FALSE, msg="Finding courses via code not yet implemented."))
}
