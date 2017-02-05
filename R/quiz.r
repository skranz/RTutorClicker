
quizDefaults = function(lang="en") {
  if (lang=="de") {
    list(
      success = "Richtig!",
      failure= "Leider noch nicht richtig.",
      success_color = "black",
      failure_color = "red",
      points_txt = "Punkte",
      point_txt = "Punkt"
    )
  } else {
    list(
      success = "Great, you answered correctly!",
      failure= "Sorry, not yet correct.",
      success_color = "black",
      failure_color = "red",
      points_txt = "points",
      point_txt = "point"
    )
  }
}

#' Create a shiny quiz widget
#'
#' @param id the id of the quiz
#' @param qu a list that contains the quiz fields as would have
#'        been parsed by read.yaml from package YamlObjects
#' @param yaml alternatively to qu, is yaml a string that specifies the quiz
#' @param quiz.handler a function that will be called if the quiz is checked.
#'        The boolean argument solved is TRUE if the quiz was solved
#'        and otherwise FALSE
clickerQuiz = function(id=paste0("quiz_",sample.int(10e10,1)),qu=NULL, yaml, quiz.handler=NULL, add.handler=TRUE, defaults=quizDefaults(lang=lang), lang="en", whiskers=NULL, task.id=id) {
  restore.point("clickerQuiz")

  if (is.null(qu)) {
    yaml = enc2utf8(yaml)
    qu = try(mark_utf8(parse.hashdot.yaml(txt=yaml)), silent=TRUE)

    if (is(qu,"try-error")) {
      err = paste0("When importing quiz:\n",paste0(yaml, collapse="\n"),"\n\n",as.character(qu))
      stop(err,call. = FALSE)
    }
  }

  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }

  if (!is.null(qu$explain))
    qu$explain.html = md2html(qu$explain)

  qu$checkBtnId = paste0(qu$id,"__checkBtn")
  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu,whiskers=whiskers))
  np = length(qu$parts)

  qu$max.points = sum(sapply(qu$parts, function(part) part[["points"]]))

  qu$ui = quiz.ui(qu)

  #if (add.handler)
  #  add.quiz.handlers(qu, quiz.handler)


  qu

  ct = list(
    type = "quiz",
    task.id = task.id,
    qu = qu
  )
  ct
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, defaults=quizDefaults(), whiskers=list()) {
  restore.point("init.quiz.part")

  part = copy.into.missing.fields(dest=part, source=defaults)

  if (!is.null(part[["sc"]])) {
    part$choices = part$sc
    part$multiple = FALSE
    #part$type = "sc"
  } else if (!is.null(part[["mc"]])) {
    part$choices = part$mc
    part$multiple = TRUE
    #part$type = "mc"
  }


  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)

    part$choices = lapply(part$choices, replace.whiskers,values=whiskers)

    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=1e-7
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  }

  if (is.null(part[["points"]])) {
    part$points = 1
  }
  part$question = md2html(replace.whiskers(part$question,values=whiskers))


  txt = replace.whiskers(part$success,whiskers)

  if (part$points==1) {
    txt = paste0(txt," (", part$points, " ", defaults$point_txt,")")
  } else if (part$points > 0 ) {
    txt = paste0(txt," (", part$points, " ", defaults$points_txt,")")
  }
  txt = colored.html(txt, part$success_color)
  part$success =  md2html(text=txt, fragment.only=TRUE)

  txt = replace.whiskers(part$failure, whiskers)
  txt = colored.html(txt, part$failure_color)
  part$failure =  md2html(text=txt, fragment.only=TRUE)

  part$id = paste0(qu$id,"__part", part.ind)
  part$answerId = paste0(part$id,"__answer")
  part$resultId = paste0(part$id,"__resultUI")
  part$ui = quiz.part.ui(part)
  part$solved = FALSE

  if (is.null(part$points)) {
    part$points = 1
  }

  part
}

quiz.ui = function(qu, solution=FALSE) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    restore.point("quiz.ui.inner")

    part = qu$parts[[i]]
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }

    if (solution) {
      if (is.null(part$sol.ui)) {
        part$sol.ui = quiz.part.ui(part, solution=TRUE)
      }
      setUI(part$resultId,HTML(part$success))

      return(list(part$sol.ui,hr))
    } else {
      return(list(part$ui,hr))
    }
  })
  if (!is.null(qu$checkBtnId)) {
    ids = sapply(qu$parts, function(part) part$answerId)
    pli = c(pli, list(submitButton(qu$checkBtnId,label = "Send",form.ids = ids),br()))
  }

  withMathJax(pli)
}

quiz.part.ui = function(part, solution=FALSE, add.button=!is.null(part$checkBtnId)) {
  head = list(
    HTML(part$question)
  )
  if (solution) {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = NULL,value = part$answer)
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = NULL,value = part$answer)
    } else if (part$type=="mc") {
      answer = wellCheckboxGroupInput(part$answerId, label=NULL,part$choices,selected = part$answer,width = "100%")
    } else if (part$type=="sc") {
      answer = wellRadioButtons(part$answerId, label=NULL,part$choices, selected=part$answer, width="100%")
    }
    #setUI(part$resultId,HTML(part$success))

  } else {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = NULL,value = "")
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = NULL,value = "")
    } else if (part$type=="mc") {
      answer = wellCheckboxGroupInput(part$answerId, label=NULL,part$choices)
    } else if (part$type=="sc") {
      answer = wellRadioButtons(part$answerId, label=NULL,part$choices, selected=NA)
    }
  }

  if (add.button) {
    button = submitButton(part$checkBtnId,label = "Send", form.ids = part$answerId)
  } else {
    button = NULL
  }
  list(head,answer,uiOutput(part$resultId),button)
}

quiz.md = function(qu, solution=FALSE) {
  restore.point("quiz.md")
  li = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    quiz.part.md(part, solution=solution)
  })
  paste0(li, collapse="\n")
}


quiz.part.md = function(part, solution=FALSE) {
  restore.point("quiz.part.md")

  head = paste0("\nQuiz: ",part$question,"\n")
  if (solution) {
    if (part$type=="numeric" | part$type == "text") {
      answer = paste0("Answer: ", part$answer)
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      mark = rep("[ ]", length(ans))
      mark[ans %in% part$answer] =  "[x]"
      answer = paste0("- ", ans, " ", mark,"\n", collapse="\n")
    }
  } else {
    if (part$type=="numeric" | part$type == "text") {
      answer = "Answer: "
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      answer = paste0("- ", ans, "[   ]\n", collapse="\n")
    }
  }
  paste0(head,"\n", answer)
}

submitButton = function (inputId, label, icon = NULL, width = NULL, form.ids = NULL, form.sel = ids2sel(form.ids), ...) {
  restore.point("submitButton")

  actionButton(inputId,label,icon, width, "data-form-selector"=form.sel)
}

clicker.quiz.handlers = function(ct=NULL,qu=ct$qu){
  #restore.point("clicker.quiz.handlers")
  buttonHandler(qu$checkBtnId, function(...) {
    part = qu$parts[[1]]
    answer = getInputValue(part$answerId)

    clicker.submit(values=list(answer=answer))
  })
}

check.part.answer = function(part.ind,qu, answer) {
  part = qu$parts[[part.ind]]
  restore.point("check.clicker.quiz.part")

  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else {
    correct = setequal(answer,part$answer)
  }
  return(correct)
}

check.clicker.quiz = function(qu, formValues, ...) {
  restore.point("click.check.quiz")
  part.solved = sapply(seq_along(qu$parts), check.clicker.quiz.part, qu=qu,app=app, values=formValues)
  solved = all(part.solved)
  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.solved=part.solved, solved=solved)
  }
  solved
}
