
examples.teacherhub = function() {
  restore.point.options(display.restore.point = TRUE)

  tgroup.dir = "D:/libraries/shinyEventsClicker/teacherhub/tgroups/kranz"
  app = TeacherHubApp(tgroup.dir=tgroup.dir,init.userid="kranz", need.password=FALSE, need.user=FALSE)
  viewApp(app)

}

# RTutor Teacher-Hub

# A teaching group shares common docker containers and directories
#
# a container for teachers
# a quiz container
# a problem set container

# directory structure:
#
# tgroup
#   running
#   teacher
#     courses
#       slides
#       problem_sets
#       quiz server

# tgroups must be added by an admin
# teachers can be added by an admin or teacher in the tgroup
#
# login database can be shared among different tgroups


TeacherHubApp = function(tgroup.dir, login.db.dir=NULL, app.title="RTutor TeacherHub", ...) {
  restore.point("TeacherHubApp")
  app = eventsApp()

  app$ui = teacher.hub.main.ui()

  glob = app$glob

  glob$tgroup.dir = tgroup.dir
  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginModule(db.arg = db.arg, login.fun=teacher.hub.login, app.title=app.title,container.id = "centerUI",...)

  restore.point("TeacherHubApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    restore.point("TeachHubApp.initHandler")
    hide.jquery.pane("mainPanes","west")
    initLoginDispatch(lop)
  })
  app
}

teacher.hub.login = function(userid,app=getApp(),...) {
  restore.point("teacher.hub.login")

  tgroup.dir = app$glob$tgroup.dir
  user.dir = file.path(tgroup.dir,"teachers",userid)
  courses.dir = file.path(user.dir, "courses")
  courseids = list.dirs(courses.dir, full.names=FALSE, recursive=FALSE)

  courses = vector("list", length(courseids))
  names(courses) = courseids

  th = as.environment(nlist(
    userid,
    tgroup.dir,
    user.dir,
    courses.dir,
    courseids,
    courses
  ))


  app$th = th

  show.teacher.hub.ui(th, app)
}

teacher.hub.main.ui = function(app=getApp()) {
  restore.point("show.teacher.hub.ui")

  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  },
  west: {
    resizable: true,
    size: 0.5
  },

  "

  panes = jqueryLayoutPanes(id="mainPanes",json.opts=json.opts,
    north = div(p("TeacherHub"),thinHR()),
    west = div(uiOutput("westUI")),
    center = div(uiOutput("centerUI"))
  )

  ui = bootstrapPage(
    contextMenuHeader(),
    fancytreeHeader(extensions=c("table","gridnav","dnd")),
    aceEditorHeader(),
    jqueryLayoutHeader(),
    panes
  )

}

show.teacher.hub.ui = function(th=app$th,app=getApp()) {
  restore.point("show.teacher.hub.ui")

  treeId = "thTree"
  ns = NS(treeId)
  tree = fancy.file.tree(treeId,root.dir = th$courses.dir,modify.nodes.fun = teacher.hub.modify.file.tree.nodes)

  setUI("westUI",tagList(
    filetreeButtons(treeId,c("Rename", "Duplicate","MakeDir","Delete")),
    tree
    ,filetreeButtons(treeId,c("Upload"))
  ))

  setUI("centerUI",HTML(""))
  show.jquery.pane("mainPanes",c("west","north"))
  #setUI("westUI",tree)
  #setUI("centerUI",HTML(""))
}

# change filetree nodes
teacher.hub.modify.file.tree.nodes = function(cur.dir, label.nodes, head.nodes, file.nodes,..., app=getApp(), th=app$th) {
  restore.point("teacher.hub.modify.file.tree.nodes")


  below = file.path.length(cur.dir) - file.path.length(th$courses.dir)

  folders = rev(file.path.split(cur.dir))


  if (NROW(head.nodes)>0) {
    head.nodes$title = ".."
  }

  # coursesdir
  if (below==0) {
    label.nodes$title = "Course"
    label.nodes$col2 = label.nodes$col3 = ""
    file.nodes$col2 = file.nodes$col3 = ""
  # main folder of a course
  } else if (below==1) {
    label.nodes$title = folders[1]
    label.nodes$col2 = label.nodes$col3 = ""

    file.nodes = file.nodes %>% arrange(desc(itemId))
    file.nodes$col2 = file.nodes$col3 = ""

    try({
      num.ps = list.dirs(file.path(cur.dir,"ps"))
      num.slides = list.dirs(file.path(cur.dir,"slides"))
      file.nodes$col2 <- c(num.slides, num.ps)
    })

  # slides
  } else if (below==2 & folders[1]=="slides") {
    label.nodes$title = paste0("Slides ", folders[2])
    label.nodes$col2 = label.nodes$col3 = ""


    file.nodes = file.nodes %>% filter(itemType=="folder")
    file.nodes$col2 = file.nodes$col3 = ""
    file.nodes$col2 <- sapply(file.nodes$itemId, function(slide) {
      as.character(smallButton(id=paste0("thShowSlide_",slide),extra.class = "thShowSlideBtn",label = "Show"))
    })
  }

  nlist(label.nodes, head.nodes, file.nodes)
}

old.show.teacher.hub.ui = function(th=app$th,app=getApp()) {
  restore.point("show.teacher.hub.ui")

  id = "thTree"
  n = length(th$courses)
  game.nodes = NULL
  if (length(n)>0) {
    courses.nodes = data_frame(key = paste0("courseNode_",th$courseids), title=th$courseids, expanded=TRUE, nodeType = "course", courseid=th$courseids)
  }
  tree.nodes = list(
    list(key = "thTreeCourses", title = "Courses", folder=TRUE, expanded=TRUE, children = courses.nodes)
  )
  tree = fancytree(id="thTree", source=tree.nodes)

  clickHandler("thTree", function(...) {
    args = list(...)
    restore.point("thTreeClick")
    nodeType = args$data$nodeType
    if (is.null(nodeType)) return(NULL)
    if (nodeType == "course") {
      th.show.course(args$data$courseid)
    }

  })
  setUI("westUI",tree)
  setUI("centerUI",HTML(""))
  show.jquery.pane("mainPanes",c("west","north"))
  #setUI("westUI",tree)
  #setUI("centerUI",HTML(""))
}

th.show.course = function(courseid, app=getApp(), th=app$th) {
  restore.point("th.show.course")

  th$courseid = courseid
  th$course.dir = file.path(th$courses.dir,courseid)

  types = c("slides","ps")
  #types = "slides"

  tree.nodes = vector("list",length(types))
  for (i in seq_along(types)) {
    type = types[i]
    els = list.dirs(file.path(th$course.dir,type),full.names = FALSE,recursive = FALSE)
    if (length(els)==0) {
      child.nodes = NULL
    } else {

      # get files of slides or ps
      gchildren = lapply(els, function(dir) {
        restore.point("course.files")
        files = list.files(file.path(th$course.dir,type,dir),full.names = FALSE,recursive = FALSE)

        # upload node
        head = data_frame(key = paste0(type,"_Upload_Node_",dir), title="", icon=FALSE, expanded=FALSE, nodeType = "upload",itemId=dir, itemType="upload")

        if (length(files)==0) return(head)

        body = data_frame(key = paste0(type,"Node_",dir,"_",files), title=files, icon=TRUE, expanded=FALSE, nodeType = "file",itemId=files, itemType="file")
        rbind(head, body)

      })

      child.nodes = data_frame(key = paste0(type,"Node_",els), title=els,folder=TRUE, expanded=FALSE, nodeType = type, courseid=th$courseid, itemId=els, itemType=type, children=gchildren)

      # upload handler
      for (el in els) {
        upload.id = paste0("upload_",el)
        changeHandler(upload.id, upload.id=upload.id, function(value,...) {
          args = list(...)
          restore.point("file.Input.Upload")
          th.upload.files(upload.id=upload.id, courseid=courseid, file.df=args$value)
        })
      }


    }

    tree.nodes[[i]] = list(key = paste0("ctree",type), title = type, folder=TRUE, expanded=TRUE, children = child.nodes)
  }

  # javascript code that specifies how tree
  # table columns are rendered
  js.render = paste0('
    // a presentation
    if (node.data.itemType === "slides") {
      ',fancytree.table.button(2,"showSlidesBtn","node.data.itemId","Show"),'
    //}

    // upload node inside presentation or ps
    //if (node.data.itemType === "upload") {
      ',fancytree.table.fileInput(3,"fileInput","node.data.itemId", progress.bar=FALSE),'
    }
  ')
  cat(js.render)

  tree = fancytree.table(id="courseTree",col.width=c("*"), num.cols=3, js.render=js.render,keyboard=FALSE,tabable=FALSE,source=tree.nodes)

  classEventHandler("showSlidesBtn",stop.propagation = TRUE, event = "click",
    function(...){
      args = list(...)
      restore.point("showSlidesButton_click")
      th.show.slides(slidesId=args$data$rowid)
    }
  )

  clickHandler("courseTree", function(...) {
    args = list(...)
    restore.point("courseTree.click")
    data = args$data
    if (isTRUE(data$itemType=="slides")) {
      upload.id = paste0("fileInput_",data$itemId)
      setHtmlShow(class = "form-group shiny-input-container")
      setHtmlShow(class = "input-group")
      setHtmlShow(id = upload.id)
    }
  })


  ui = div(
    h4(courseid),
    tree,
    HTML('<div id=\"upload_progress\" class=\"progress progress-striped active shiny-file-input-progress\">    <div class=\"progress-bar\"></div>  </div>')

  )

  setUI("centerUI",ui)
}

th.upload.files = function(update.id, courseid,...) {
  restore.point("th.upload.files")

  source.dir = value$data.path
  dest.dir = file.path(th$course.dir,type,el)
  # copy uploaded files
  file.copy(from=source.dir, to=dest.dir, recursive=TRUE)
  # update tree
  th.show.course(courseid)

}

th.show.slides = function(slidesId, app=getApp(), th=app$th) {
  restore.point("th.show.slides")
  cat("\nshow slides ", slidesId)
}

shiny.to.js.html = function(txt,quotes='"') {
  txt = paste0(as.character(txt),collapse="")
  txt = gsub("\n","",txt, fixed=TRUE)
  if (isTRUE(quotes=="'")){
    txt = gsub('"',quotes,txt, fixed=TRUE)
  } else if (isTRUE(quotes=='"')) {
    txt = gsub("'",quotes,txt, fixed=TRUE)
  }
  txt
}
