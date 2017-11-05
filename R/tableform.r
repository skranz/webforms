examples.tableform.ui = function() {
  setwd("D:/libraries/webforms/examples")
  li = read.yaml("coursestaff.yaml",keep.quotes = FALSE)
  fields = li$fields

  sets = read.yaml("sets.yaml",keep.quotes = FALSE)$lang_de

  selectizeInput("test", "test", 1:2, width="5em")
  data = data_frame("Position"=1:2,staffid=c("skranz","NN"), teaching_role=c("LE","TA"), responsible=FALSE, commission="")
  sets = c(sets, list(staff=list("Sebastian Kranz"="skranz","Max Mustermann"="mm","NN"="NN")))

  form = list(fields=fields,lang="de", sets = sets)
  ui = tableform.ui(form=form, data=data)

  app = eventsApp()
  app$ui = fluidPage(
    selectizeHeaders(),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}")),
    div(style="margin-bottom: 5px; margin-top:5px;",ui),
    simpleButton("addCourseStaffBtn","Mitarbeiterzeile hinzufügen"),
    simpleButton("createStaffBtn","Neue Person in Datenbank erstellen."),
    actionButton("removeBtn","", icon("trash-o"))
    #,p(style="height=300px;","hello")
  )
  viewApp(app)

}

selectizeHeaders = function() {
  htmlDependency("selectize", "0.11.2", c(href = "shared/selectize"),
        stylesheet = "css/selectize.bootstrap3.css", head = format(tagList(HTML("<!--[if lt IE 9]>"),
            tags$script(src = "shared/selectize/js/es5-shim.min.js"),
            HTML("<![endif]-->"), tags$script(src = "shared/selectize/js/selectize.min.js"))))

}


tableform.ui = function(fields=form$fields,data=NULL, colnames=tableform.colnames(fields,data, lang), sets=form$sets,lang=first.non.null(form$lang, "en"), buttons.col=Inf, add.btn.label = labels$add.btn, delete.btn.label = labels$delete.btn, use.add.btn=TRUE, use.delete.btn=TRUE, labels=tableform.default.labels(lang), form=NULL, ...) {
  restore.point("tableform.ui")

  field.names = names(fields)
  df = data
  col = field.names[1]
  for (col in field.names) {
    df[[col]] = fieldInputVector(name=col,fields = fields, value=data[[col]], sets=sets, label=NULL, lang=lang)
  }

  if (use.delete.btn) {

  }

  colnames(df) = colnames

  HTML(html.table(df,bg.color = "#ffffff"))
}

tableform.colnames = function(fields, data, lang="en") {
  restore.point("tableform.colnames")
  names = colnames(data)
  field.names = names(fields)
  labels = sapply(fields, function(field) {
    field = get.lang.field(field, lang)
    field$label
  })
  ma = match(field.names, names)
  rows = !is.na(ma)
  names[ma[rows]] = labels[rows]
  names
}

tableform.default.labels = function(lang="en") {
  if (lang=="de") {
    list(
      add.btn = "Hinzufügen",
      delete.btn = "Löschen"
    )
  } else {
    list(
      add.btn = "Add",
      delete.btn = "Delete"
    )

  }
}