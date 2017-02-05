# clicker app modes
#
# login: show.course.list, show.course.code
# running:


clicker.client.example = function() {
  restore.point.options(display.restore.point = TRUE)

  clicker.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  clicker.dir = "D:/libraries/RTutorTeacher/teacherhub/tgroups/kranz/clicker"
  setwd(clicker.dir)
  port = 4634
  app.url = "127.0.0.1:4634"
  app.url = "http://localhost:4634"
  opts = clicker.client.opts(clicker.dir=clicker.dir, app.url=app.url, show.course.list = TRUE, show.course.code=FALSE, use.token=FALSE, use.login.db=FALSE)
  app = clickerClientApp(opts)
  viewApp(app, port=port)
}



clickerClientApp = function(opts=clicker.client.opts()) {
  restore.point("clickerClientApp")
  app = eventsApp()
  glob = app$glob
  copy.into.env(source=opts, dest=glob)

  glob$clicker.dir = glob$clicker.dir
  glob$cur.task.file = NULL
  glob$mainUI = "mainUI"
  glob$courses = list()
  glob$imported.task.files = NULL

  lop = clicker.client.lop(glob)
  set.lop(lop)

  if (!dir.exists(glob$token.dir)) (
    stop(paste0("token.directory ", glob$token.dir, " does not exist."))
  )

  # list of reactive nounces that monitor updates
  # for given courses
  glob$courses.rv = reactiveValues()

  obs = observe({
    clicker.update.tasks(clicker.dir = clicker.dir,glob = glob)
  })
  set.global.observer("myobs",obs=obs)

  app$ui = bootstrapPage(
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    glob = app$glob
    courses.rv = glob$courses.rv
    # we must observe the query string to perform initialization
    app$init.observer = observe(priority = -100,x = {
      if (isTRUE(app$is.initialized)) {
        app$init.observer$destroy()
        return()
      }
      app$is.initialized = TRUE
      query <- parseQueryString(session$clientData$url_search)
      restore.point("clicker.client.observe.query")
      clicker.client.init.app.with.query(query)
    })

  })
  app
}

clicker.client.init.app.with.query = function(query, app=getApp()) {
  restore.point("clicker.client.init.app.with.query")
  glob = app$glob
  mainUI = glob$mainUI

  # check if we have a token
  key = query$token_key
  if (!is.null(query$token_key)) {
    clicker.client.init.app.with.token(token_key=query$token_key, token_file=query$token_file)
    return()
  }

  # use normal login dispatch
  lop = get.lop(app=app)
  initLoginDispatch(lop)
}

clicker.client.init.app.with.token = function(token_key, token_file, app=getApp()) {
  restore.point("clicker.client.init.app.with.token")

  # check if token file exists
  failed.ui = clicker.client.failed.login.ui()
  file = file.path(glob$token.dir,token_file)
  if (!file.exists(file)) {
    setUI(mainUI, failed.ui)
    return()
  }

  tok = read.login.token(file=file)

  now = as.numeric(Sys.time())
  if (!isTRUE(now <= tok$expire)) {
    html="<h2>Timout. Your login token is not active anymore. Please login again.</h2>"
    setUI(mainUI, HTML(html))
    return()
  }
  clicker.client.start.task.observer(tok=tok)
}

clicker.client.opts = function(
  clicker.dir=getwd(),
  db.dir = file.path(clicker.dir,"db"),
  running.dir = file.path(clicker.dir,"running"),
  token.dir = file.path(clicker.dir, "tokens"),
  need.token = FALSE,
  just.login = FALSE,
  use.token = TRUE,
  use.login.db=TRUE,
  app.url=NULL, clicker.app.url = app.url,
  init.userid="", init.password="", init.code="",
  app.title="RClicker",
  email.domain = NULL, check.email.fun = NULL,
  email.text.fun=default.email.text.fun,
  smtp = NULL,
  token.valid.min = 180,
  allow.guest.login=TRUE, open.app.in.new.tab=TRUE,
  show.course.list=FALSE, show.course.code=TRUE,
  ...)
{
  c(as.list(environment()),list(...))
}


.glob.obs.env = new.env()

clicker.client.start.task.observer = function(tok=app$glob$default.token,app=getApp()) {
  restore.point("clicker.client.start.task.observer")
  app$tok = tok
  app$userid = tok$userid
  app$courseid = tok$courseid

  # destroy old observer
  if (!is.null(app[["task.obs"]]))
    app$task.obs$destroy()

  glob=app$glob
  app$task.obs = observe({
    glob$courses.rv[[app$courseid]]
    restore.point("app.task.observer")
    clicker.update.client.task()
    cat("task.nonce changed...")
  })
  clicker.update.client.task()
}

clicker.update.client.task = function(ct = app$glob$ct.li[[app$courseid]], app=getApp()) {
  restore.point("clicker.update.client.task")

  if (is.null(ct)) {
    if (!isTRUE(app$no.clicker.task)) {
      app$no.clicker.task = TRUE
      ui = tagList(
        h4(app$courseid," - ", app$userid),
        p("Currently no task.")
      )
      setUI(app$glob$mainUI, ui)
    }
    return()
  }
  app$no.clicker.task = FALSE

  if (is.function(ct$init.handler)) {
    ct$init.handler(ct)
  } else if (is.character(ct$init.handler)) {
    do.call(ct$init.handler, list(ct=ct))
  }

  ui = tagList(
    h4(app$courseid," - ", app$userid),
    ct$ui
  )
  setUI(app$glob$mainUI, ui)
}

clicker.update.tasks = function(clicker.dir, glob=app$glob, app=getApp(), millis=1000) {
  restore.point("clicker.update.tasks")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(clicker.dir, "tasks"),full.names = FALSE)

  files = setdiff(files, glob$imported.task.files)
  if (length(files)==0) {
    invalidateLater(millis)
    return()
  }
  # sort filest by date, newest first
  files = files[order(file.mtime(file.path(clicker.dir,"tasks",files)))]
  restore.point("clicker.update.tasks.2")

  for (file in files) {
    ct = read.task.file(file, glob=glob)
    courseid = first.non.null(ct[["courseid"]],"default")
    ct$courseid = courseid
    glob$ct.li[[courseid]] = ct
    # update reactive values
    glob$courses.rv[[courseid]] = runif(1)
  }
  glob$imported.task.files = c(glob$imported.task.files, files)
  invalidateLater(millis)
}


clicker.submit = function(values, app=getApp()) {
  restore.point("clicker.submit")

  cat("\nclicker.submit")
  glob = app$glob
  userid = app$userid
  courseid = app$courseid

  ct = glob$ct.li[[courseid]]
  vals = c(list(submitTime=Sys.time(), userid=app$userid), as.list(values))

  # first submission
  if (ct$num.sub==0) {
    dir.create(ct$sub.dir,showWarnings = TRUE,recursive = TRUE)
    writeLines(paste0(names(vals), collapse=","),file.path(ct$sub.dir,"colnames.csv"))
  }

  sub.file = file.path(ct$sub.dir, paste0(userid,"_",ct$task.id,".sub"))
  write.table(as.data.frame(vals), file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$ct.li[[courseid]]$num.sub = ct$num.sub+1

  setUI(glob$mainUI,p("Your answer is submitted."))
}

read.task.file = function(file, glob=app$glob, app=getApp()) {
  restore.point("read.task.file")
  ct = readRDS(file.path(glob$clicker.dir,"tasks", file))
  ct$courseid = first.non.null(ct[["courseid"]],"default")
  ct$file = file
  ct$num.sub = 0
  ct$sub.dir = file.path(glob$clicker.dir,"sub",ct$courseid, ct$task.id)

  if (isTRUE(ct$type=="quiz")) {
    ct$ui = quiz.ui(ct$qu)
    ct$init.handler="clicker.quiz.handlers"
  }
  ct
}

set.global.observer = function(id, ..., obs=observe(...)) {
  li = getOption("global.shiny.observer", default=list())
  if (!is.null(li[[id]])) {
    try(li[[id]]$destroy())
  }
  li[[id]] = obs
  options(global.shiny.observer=li)
}

destroy.global.observers = function() {
  # destroy.global.observers()
  li = getOption("global.shiny.observer", default=list())
  for (obs in li) {
    try(obs$destroy())
  }
}




clicker.client.failed.login.ui = function(app=getApp()) {
  html="<h2>Login failed</h2>"
  HTML(html)
}
