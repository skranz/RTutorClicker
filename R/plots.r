
quiz.results.plot = function(dat, qu,part = qu$parts[[1]]) {
  restore.point("quiz.results.plot")

  part = qu$parts[[1]]
  if (part$type=="sc") {
    return(quiz.sc.plot(dat=dat,qu=qu,part=part))
  } else if (part$type=="numeric") {
    return(quiz.numeric.plot(dat=dat,qu=qu,part=part))
  }
}

quiz.sc.plot = function(dat, qu, part = qu$parts[[1]]) {
  restore.point("quiz.sc.plot")
  choices = unlist(part$choices)

  var = names(dat)[[3]]
  choices.barplot(values=dat[[var]], choices)
}

choices.barplot = function(values, choices=names(counts), counts=NULL,col="#ff8888", axes=FALSE, answer=NULL, colors=clicker.bar.color(choices=choices,answer=answer),choice.labels = choices, ....) {
  restore.point("choices.barplot")

  if (is.null(counts)) {
    counts = rep(0, length(choices))
    names(counts) = choices
    cc = table(values)
    counts[names(cc)] = cc
  }
  nn_counts = counts
  names(nn_counts) = NULL
  shares = nn_counts / max(sum(nn_counts),1)

  highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(
      dataLabels=list(enabled=TRUE),colorByPoint = TRUE,colors=colors
    ),
    colors=colors
  ) %>%
  hc_xAxis(categories = choice.labels) %>%
  hc_add_series(data = nn_counts,name = "Number of answers",showInLegend=FALSE)

}



clicker.bar.color = function(right=NULL, choices=NULL, answer=NULL) {
  if (is.null(right)) {
    right = rep(2, length(choices))
    if (!is.null(answer)) {
      right = choices==answer
    }
  }

  cols = c("#ec8888", "#7cb5ec")
  cols = c("#d35400","#2980b9","#aa22cc")
  cols = c("#d35400","#2960d9","#aa42cc")

  cols = c("#d35400","#2980b9","#2980b9")
  cols[right+1]

}

relative.deviation.breaks = function(size=c("5","10","fine")[1]) {

  if (size == "5") {
    br = c(0.01,0.05, 0.25, 1, 5, 25, 100,Inf)
    br = c(-rev(br),br)
    lab = c(
      "s >100","s 100-25","s 25-5","s 5-1","s 100%-25%","s 25%-5%","s 5%-1%",
     "roughly right",
      "1%-5%","5%-25%", "l 25%-100%","1-5","5-25","25-100",">100")
    colors= clicker.bar.color(c(rep(FALSE,6),2,TRUE,2, rep(FALSE,6)))

  } else if (size=="10") {
    br = c(0.01,0.1,1,10,100,Inf)
    br = c(-rev(br),br)
    lab = c(
      "<100","100-10","10-1","100%-10%","10%-1%",
     "-1% +1%",
      "1%-10%","10%-100%","1-10","10-100",">100"
      )
    colors=clicker.bar.color(c(rep(FALSE,4),rep(TRUE,3), rep(FALSE,4)))
  } else {
    br = c(0.01,0.05,0.1,0.25,0.5,1,3,10,100,Inf)
    br = c(-rev(br),br)
    lab = c(
      "<100","100-10","10-3","3-1","100%-50%","50%-25%","25%-10%","10%-5%","5%-1%",
      "-1% +1%",
      "1%-5%","5%-10%","10%-25%","25%-50%","50%-100%","1-3","3-10","10-100",">100"
      )
    colors=clicker.bar.color(c(rep(FALSE,9),TRUE, rep(FALSE,9)))


  }
  return(list(br=br, lab=lab, colors=colors))
}
