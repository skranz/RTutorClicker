

show.task.results = function(cs=app$cs, ct=cs$ct, app=getApp(),outputId = paste0("resultsUI",postfix), postfix="",...) {
  dat = load.sub.data(cs=cs,ct=ct)
  restore.point("show.task.results")

  if (is.null(dat)) {
    ui = p("No answers submitted.")
    setUI(outputId,ui)
    return()
  }

  qu = ct$qu
  part =qu$parts[[1]]
  if (isTRUE(part$type=="sc")) {
    show.clicker.quiz.sc.results(dat=dat,qu=qu,outputId = outputId)
  } else if (isTRUE(part$type=="mc")) {
    show.clicker.quiz.mc.results(dat=dat,qu=qu,outputId = outputId)
  } else if (isTRUE(part$type=="numeric")) {
    show.clicker.quiz.numeric.results(dat=dat,qu=qu,outputId = outputId)
  }


  return()
}

show.clicker.quiz.sc.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.sc.results.ui")

  choices = unlist(part$choices)
  var = names(dat)[[3]]
  counts = count.choices(dat[[var]], choices)
  shares = round(100*counts / max(1,sum(counts)))

  answer = if (show.sol) part$answer else NULL

  nans = NROW(dat)

  if (do.plot) {


    choice.labels = choices
    if (show.sol) {
      rows = choices == part$answer
      choice.labels[rows] = paste0("*", choice.labels[rows])
    }

    plot = choices.barplot(values=dat[[var]], choices, answer=answer, choice.labels=choice.labels)
    plotId = paste0(outputId,"_Plot")
    ui = tagList(
      div(style="height=14em",
        highchartOutput(plotId, height="14em")
      ),
      p(nans," replies.")
    )
    setUI(outputId,ui)
    dsetUI(outputId,ui)
    app$session$output[[plotId]] = renderHighchart(plot)
  } else {
    n = length(choices)
    bg.color = rep("#fff",n)
    if (show.sol) {
      rows = choices == part$answer
      bg.color[rows] = "#aaf"

    }
    df = data_frame(counts, paste0("(",shares,"%)"), choices)

    html = html.result.table(df,colnames=c("","","",""), font.size="120%", align=c("center","center","left"),bg.color = bg.color)

    ui = tagList(HTML(html))
    setUI(outputId, ui)
  }
  invisible(ui)
}


show.clicker.quiz.mc.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.mc.results.ui")

  choices = unlist(part$choices)

  sum = group_by(dat,answer) %>% summarize(yes=sum(checked),no=sum(!checked))


  ind = match(choices,sum$answer)
  sum = sum[ind,]
  yes = sum$yes
  no = sum$no

  nans = length(unique(dat$userid))
  #yes = lapply(yes, function(y) list(y=y, color="#777"))
  #no = lapply(yes, function(y) list(y=y, color="#d35400"))

  if (show.sol) {
    correct = choices %in% part$answer
    pre = post = ""
    #pre = ifelse(correct,"* ","")
    #post = ifelse(correct," (Answer: Yes)", " (Answer: No)")
    pre = ifelse(correct,"(A: Yes) ","(A: No) ")
    choices = paste0(pre, choices,post)
  }

  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(
      dataLabels=list(enabled=TRUE)
    )
  ) %>%
  hc_xAxis(categories = choices) %>%
  hc_add_series(data = yes,name = "Yes", color="#2980b9") %>%
  hc_add_series(data = no,name = "No",color="#d35400")

  plotId = paste0(outputId,"_Plot")
  ui = tagList(
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    ),
    p("Total: ",nans," replies."),
    if (show.sol & !is.null(qu$explain.html))
      HTML(qu$explain.html)
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)

  invisible(ui)
}



show.clicker.quiz.numeric.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.numeric.results.ui")
  answer = as.numeric(part$answer)
  var = names(dat)[[3]]
  val = dat[[var]]

  pos.dev = (val/answer-1)
  neg.dev = (answer/val-1)
  neg = neg.dev > pos.dev

  dev = pmax(pos.dev,neg.dev) * (-1)^neg


  res = relative.deviation.breaks(size="5")
  br = res$br
  lab = res$lab


  int = findInterval(dev, br)

  choices = lab
  values = lab[int]
  counts = rep(0, length(choices))
  names(counts) = choices
  cc = table(values)
  counts[names(cc)] = cc
  names(counts) = NULL


  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(dataLabels=list(enabled=TRUE),colorByPoint = TRUE,colors=res$colors),
    colors=res$colors,
    series=list(
        pointPadding= 0,
        groupPadding= 0,
        borderWidth= 0.5,
        borderColor= 'rgba(255,255,255,0.5)'
    )
  ) %>%
  hc_xAxis(categories = lab) %>%
  hc_add_series(data = counts,name="Counts",showInLegend=FALSE)

  plotId = paste0(outputId,"_Plot")
  ui = tagList(
    p("Results from ",NROW(dat)," replies:"),
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    ),
    if (show.sol) p("Correct answer: ", answer)
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)


  invisible(ui)
}


quiz.results.plot = function(dat, qu,part = qu$parts[[1]]) {
  restore.point("quiz.results.plot")

  part = qu$parts[[1]]
  if (part$type=="sc") {
    return(quiz.sc.plot(dat=dat,qu=qu,part=part))
  } else if (part$type=="numeric") {
    return(quiz.numeric.plot(dat=dat,qu=qu,part=part))
  }
}


load.sub.data = function(cs=app$cs, ct=cs$ct, app=getApp(),...) {
  restore.point("load.sub.data")
  dir = file.path(cs$clicker.dir, "sub",ct$courseid, ct$task.id)
  files = list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)
  if (length(files)==0) return(NULL)


  header.file = file.path(dir,"colnames.csv")
  txt = readLines(header.file,warn = FALSE)
  li = unlist(lapply(files, readLines,warn=FALSE))
  txt = c(txt, li)

  dat = readr::read_csv(merge.lines(txt))
  dat = transform.sub.data(dat,ct)
  dat
}

# transform submission data into simpler format
transform.sub.data = function(dat, ct) {
  restore.point("transform.sub.data")

  if (isTRUE(ct$type=="quiz")) {
    part = ct$qu$parts[[1]]
    if (isTRUE(part$type=="mc")) {
      dat = transform.to.mc.data(dat, part$choices)
    }
  }
  dat
}

transform.to.mc.data = function(dat, choices) {
  restore.point("transform.to.mc.data")
  choices = unlist(choices)

  library(tidyr)
  mc = expand.grid(answer=choices,userid=unique(dat$userid))
  jd = suppressWarnings(left_join(mc, dat,by=c("userid","answer")))
  jd$checked = !is.na(jd$submitTime)

  jd = select(jd,submitTime,userid,answer,checked)
  jd
}

count.choices = function(values, choices) {
  counts = rep(0, length(choices))
  names(counts) = choices
  cc = table(values)
  counts[names(cc)] = cc

  counts
}

html.result.table = function(df,colnames=colnames(df), bg.color="#fff", font.size=14, align=NULL) {
  restore.point("html.table")
  n = NROW(df)
  row.bgcolor = rep(bg.color,length=n)

  if (is.null(align)) align="left"
  align=rep(align, length=NCOL(df))

  head = paste0('<th class="result-table-th">',colnames,'</th>', collapse="")
  head = paste0('<tr>', head, '</tr>')

  td.class = rep("result-table-td", NROW(df))

  cols = 1:NCOL(df)
  code = paste0('"<td style=\\"text-align: ",align[[',cols,']] ,"\\" class=\\"",td.class,"\\" nowrap bgcolor=\\"",row.bgcolor,"\\">", df[[',cols,']],"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table>\n', head, main, "\n</table>")

  th.style='font-weight: bold; margin: 3px; padding: 3px; text-align: center;'
  td.style='font-family: Verdana,Geneva,sans-serif; margin: 0px 3px 1px 3px; padding: 1px 3px 1px 3px; text-align: center;'

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")
  }

  tab = paste0("<style>",
    " table.result-table-table {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " td.result-table-td {", td.style,"}\n",
    " th.result-table-th {", th.style,"}\n",
     "</style>",tab
  )
  return(tab)
}
