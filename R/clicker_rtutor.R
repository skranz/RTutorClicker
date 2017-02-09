


# blocks specified in RTutor
RTutorClicker.block.types.df = function(...) {
  restore.point("RTutorClicker.block.types.df")

  types = c("quiz")
  n = length(types)
  bt.df = data_frame(type=types, package="RTutorClicker", is.widget=TRUE, parse.inner.blocks = FALSE, remove.inner.blocks= TRUE, is.parent=FALSE, is.container = TRUE, dot.level=0, arg.li = vector("list",n))

  bt.df
}




rtutor.widget.quiz = function() {
  list(
    is.task = TRUE,
    parse.fun = rt.clicker.quiz.parse,
    init.task.state = rt.clicker.quiz.init.task.state,
    init.handlers = rt.clicker.quiz.init.handlers,
    ui.fun = rt.clicker.quiz.ui,
    rmd.fun = RTutor3::rtutor.quiz.rmd
  )
}

rt.clicker.quiz.init.task.state = function(ts, task.ind=ts$task.ind, ups=NULL,opts=NULL,...) {
  restore.point("rt.clicker.quiz.init.task.state")

  ts$cs = as.environment(list(clicker.dir = opts$clicker.dir, courseid=opts$courseid))
  ts$ct = as.environment(ts$wid$ct)
  ts$ct$wid = ts$wid
  ts$ct$courseid = opts$courseid
  ts$ct$clicker.dir = ts$cs$clicker.dir


  return(ts)
}

rt.clicker.quiz.ui = function(ts, wid=ts$wid, ...) {
  restore.point("rt.clicker.quiz.ui")
  wid$ui
}

update.result.clicker.tags = function(qu,ct, app=getApp(), selected="none") {
  restore.point("update.result.clicker.tags")

  tags = get.clicker.tags(clicker.dir=ct$clicker.dir, ct=ct)
  tags.li = as.list(c("none", "latest", "all",tags))
  names(tags.li) = unlist(tags.li)

  updateSelectizeInput(app$session,inputId = qu$ns("resultsRunSelect"), choices=tags.li,selected = selected)

}

rt.clicker.quiz.init.handlers = function(wid=ts$wid,ps=get.ps(), app=getApp(),ts=NULL,opts=ps$opts,...) {
  restore.point("rt.clicker.quiz.init.handlers")

  if (!isTRUE(opts$use.clicker)) {
    cat("\nDon't use clicker.")
    return()
  }
  if (nchar(opts$clicker.dir)==0) {
    cat("\nNo clicker.dir specified")
    return()
  }


  qu = wid
  cs = ts$cs
  ct = ts$ct
  buttonHandler(qu$ns("startClickerBtn"),function(...) {
    rt.clicker.send(ts=ts,wid=wid,opts=opts)
  })
  buttonHandler(qu$ns("stopClickerBtn"),function(...) {
    stop.in.sec = as.integer(getInputValue(qu$ns("stopClickerBtn")))
    restore.point("rt.clicker.stopBtnHandler")

    if (is.na(stop.in.sec)) stop.in.sec=3
    cs$stop.time = as.integer(Sys.time()) + stop.in.sec
  })

  update.result.clicker.tags(qu=qu,ct=ct)

  selectChangeHandler(id = qu$ns("resultsRunSelect"),ct=ct,fun=function(id,value,ct,qu,...) {
    args = list(...)
    #value = getInputValue(id)
    restore.point("resultsRundSelectChange")
    show.task.results(ct=ct, clicker.tag=value)

    cat("\nresultsSelectClick")
  })

  # set course running for clicker
  if (!isTRUE(ps[["wrote.clicker.running"]])) {
    ps$wrote.clicker.running = TRUE
    write.clicker.running(courseid = opts$courseid, clicker.dir = opts$clicker.dir)
  }


  #add.quiz.handlers(qu=wid, quiz.handler=rtutor.quiz.handler)
}



rt.clicker.send = function(ts=NULL, wid=ts$wid, ct=ts$ct, cs=ts$cs, opts=rt.opts(), app=getApp()) {
  restore.point("rt.clicker.quiz.send")

  write.clicker.task(ct, clicker.dir=opts$clicker.dir)
  rt.clicker.start.task.observer(ts=ts,cs=cs, wid=wid, ct=ct, opts=opts)
}

rt.clicker.start.task.observer = function(ts=NULL, cs=ts$cs, wid=ts$wid,ct=ts$ct,opts=NULL, app=getApp()) {
  restore.point("rt.clicker.start.task.observer")

  cs$clicker.dir = opts$clicker.dir
  cs$start.time = as.integer(Sys.time())
  cs$stop.time = NULL
  cs$stopped = FALSE

  if (!is.null(cs[["task.obs"]])) {
    try(cs$task.obs$destroy())
  }

  cs$task.obs = observe({
    app=getApp()
    restore.point("task.observer")

    dir = file.path(cs$clicker.dir, "sub",ct$courseid, ct$task.id, ct$clicker.tag)
    files = list.files(dir)
    cs$num.sub = max(0,length(files))
    if (!is.null(cs$stop.time)) {
      cs$stop.in.sec = round(cs$stop.time - as.integer(Sys.time()))
      cs$stopped = cs$stop.in.sec < 0
      if (!cs$stopped) {
        stop.tag = p(style="color: #d00",paste0("Stop in ", cs$stop.in.sec, " sec."))
      } else {
        stop.tag = p("Submission has stopped.")
      }
    } else {
      stop.tag = NULL
    }
    setUI(wid$ns("numSubUI"),tagList(
      stop.tag,
      p(paste0("Running: ", round(as.integer(Sys.time())-cs$start.time))," sec."),
      p(paste0("Replies: ", cs$num.sub))
    ))
    if (!cs$stopped) {
      invalidateLater(1000)
    } else {
      setUI(wid$ns("numSubUI"),"")
      update.result.clicker.tags(qu=wid,ct=ct, selected = "latest")
      rt.show.task.results(ts=ts, wid=wid, cs=cs)
    }
  })
}

rt.show.task.results = function(ts=NULL,ct = ts$ct, app=getApp(),...) {
  restore.point("rt.show.task.results")
  show.task.results(ct=ct)
}

rt.clicker.quiz.parse = function(inner.txt,type="quiz",name="",id=paste0("quiz_",bi),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),opts = ps$opts,...) {
  id = paste0(ps$name,"__",id)
  restore.point("rt.clicker.quiz.parse")
  whiskers =NULL
  if (isTRUE(ps$opts$use.whiskers)) {
    whiskers = ps$pre.env$.whiskers
  }

  qu = shinyQuiz(id = id,yaml = merge.lines(inner.txt), bdf = NULL,add.handler = FALSE, whiskers=whiskers, add.check.btn=FALSE)
  qu$ns = NS(id)

  stop.in = first.non.null(opts$clicker.stop.in, 5)
  rt.ui = qu$ui
  qu$ui = tagList(
    rt.ui,
    HTML("<table><tr><td>"),
    smallButton(qu$ns("startClickerBtn"),label="Start", extra.style="margin-bottom: 2px;"),
    HTML("</td><td>"),
    smallButton(qu$ns("stopClickerBtn"),label="Stop in ",extra.style="margin-bottom: 2px;"),
    HTML("</td><td>"),
    tags$input(id = qu$ns("stopInInput"),type = "text", class = "form-control", value = stop.in,style="width: 4em; padding-left: 10px; padding-right: 5px; padding-top: 0; padding-bottom: 0; margin-left: 5px; margin-top:0; margin-bottom: 0; height: 100%;"),
    HTML("</td></tr></table>"),
    uiOutput(qu$ns("numSubUI")),
    #uiOutput(qu$resultsUIId)
    bsCollapse(open="Results",
      slimCollapsePanel("Results",
        uiOutput(qu$ns("resultsUI")),
        tagList(div(class="StopClickPropagation",
          selectInput(qu$ns("resultsRunSelect"), label="Results of run", choices=list("latest"="latest", "all"="all"),multiple=FALSE)
        ))
      )
    )
  )
  qu$ct = clickerQuiz(id=id,yaml = inner.txt, whiskers=whiskers)


  qu
}
