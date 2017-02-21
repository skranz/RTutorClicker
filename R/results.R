


normalize.clicker.tag = function(ct, clicker.tag) {
  restore.point("normalize.clicker.tag")

  if (length(clicker.tag)==0) return(NULL)

  if ("none" %in% clicker.tag) return(NULL)

  if ("all" %in% clicker.tag | "latest" %in% clicker.tag) {
    dirs = get.clicker.tags(ct=ct)
    if ("all" %in% clicker.tag) return(dirs)
    if ("latest" %in% clicker.tag) {
      nums = na.omit(as.numeric(dirs))
      if (length(nums)>0) {
        clicker.tag = union(clicker.tag, as.character(max(nums)))
      } else {
        return(NULL)
      }
    }
  }
  clicker.tag

}

load.sub.data = function(ct, clicker.tag = ct$clicker.tag, app=getApp(), Wid = get.Widget(ct$type),...) {
  restore.point("load.sub.data")
  clicker.tag = normalize.clicker.tag(ct=ct, clicker.tag=clicker.tag)

  if (length(clicker.tag)==0) return(NULL)

  dirs = file.path(ct$clicker.dir, "sub",ct$courseid, ct$task.id, clicker.tag)
  files = unlist(lapply(dirs, function(dir) list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)))
  if (length(files)==0) return(NULL)

  header.file = file.path(ct$clicker.dir, "sub",ct$courseid, ct$task.id,"colnames.csv")
  txt = readLines(header.file,warn = FALSE)
  li = unlist(lapply(files, readLines,warn=FALSE))
  txt = c(txt, li)

  dat = readr::read_csv(merge.lines(txt))

  if (!is.null(Wid$clicker$server$transform.sub.data)) {       dat = call.fun(Wid$clicker$server$transform.sub.data,dat,ct)
  }
  dat
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
