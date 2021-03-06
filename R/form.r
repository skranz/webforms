.APP.FORMS.GLOB = new.env()

examples.appForm = function() {
  setwd("D:/libraries/SeminarMatching/")
  library(yaml)
  library(YamlObjects)


  restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/webforms/examples/investgame1")
  file = "game1_input.yaml"

  setwd()
  form = yaml.form(file,utf8 = TRUE, prefix = "myform_", lang="de")
  form$submit.handler = function(ok,values,...) {
    if (!ok) return()
    restore.point("snhdhhfrbf")

    cat("\nGreat you inserted valid numbers!")
    store.form.values(values, file="formvalues.json")
  }
  #form$lang = "de"
  #form$widget.as.character=FALSE
  ui = form.ui.simple(form)


  rmd = readLines("game1_input.rmd")
  set.form(form)
  comp = compile.rmd(text=rmd, whiskers="render",use.commonmark = FALSE,out.type = "shiny")
  eval.placeholders(comp)
  ph = comp$ph
  ui = render.compiled.rmd(comp)

  app = eventsApp()
  add.form.handlers(form)
  app$ui = fluidPage(ui)
  add.form.handlers(form)
  runEventsApp(app, launch.browser = rstudio::viewer)

  df = read.ndjson("formvalues.json")
  df
  df$alpha_1
}


view.form = function(form, params, launch.browser = rstudioapi::viewer, ...) {
  app = eventsApp()
  if (is.null(ui)) {
    ui = form.ui(form=form, params=params,...)
    form = get.form()
  }
  if (!is.null(form)) {
    add.form.handlers(form,function(...) cat("\nGreat, all values are ok!"))
  }
  app$ui = fluidPage(with_mathjax(ui))
  runEventsApp(app, launch.browser=launch.browser)
}


load.and.init.form = function(file=NULL, text=NULL, utf8 = TRUE, lang="en", prefix=NULL, postfix=NULL, warn.no.prefix = TRUE,keep.quotes=FALSE,...) {
  form = read.yaml(file=file, text=text,utf8 = utf8,keep.quotes = keep.quotes)
  init.form(form, lang=lang, prefix=prefix, postfix=postfix, warn.no.prefix = warn.no.prefix, ...)
}

init.form = function(form, compile.markdown.help=TRUE, lang="en", prefix=form$prefix, postfix=form$postfix, show.alerts = TRUE,   widget.as.character=first.none.null(form$widget.as.character,FALSE), warn.no.prefix=TRUE ) {
  restore.point("init.form")

  if (is.null(prefix) & is.null(postfix) & isTRUE(warn.no.prefix)) {
    warning("It is strongly recommended to init forms with a prefix or postfix that guarantees that field names are unique in your application. If field inputs have the same name, errors can arise that are not easy to detect. So please provide a prefix or postfix to init.form.")
  }


  form$submit.on.failure = first.none.null(form$submit.on.failure, FALSE)
  form$widget.as.character=widget.as.character
  form$show.alerts = show.alerts
  form$prefix = prefix
  form$postfix = postfix
  form$lang = lang
  form$texts = get.form.texts(form, lang=lang)

  if (is.null(form$type))
    form$type = infer.form.type(form)

  if (compile.markdown.help) {
    form = compile.form.markdown.tags(form=form, tags=c("help","note"))
  }

  if (!identical(form$type,"simple")) {
    init.form.fun = paste0("init.form.",form$type)
    if (exists(init.form.fun,mode = "function"))
      form = do.call(init.form.fun, list(form=form))
  }


  form
}

compile.form.markdown.tags = function(form, values=NULL,  tags = c("help","note")
) {
  restore.point("compile.form.markdown.tags")

  inner.compile = function(obj, tags, values, use.lang = TRUE) {
    restore.point("inner.compile")
    if (!is.list(obj)) return(obj)

    for (tag in tags) {
      if (!is.null(obj[[tag]])) {
        obj[[paste0(tag,"_html")]] = replace.whiskers(md2html(obj[[tag]]), values=values)
      }
    }
    if (use.lang) {
      lang.tags = names(obj)[str.starts.with(names(obj),"lang_")]
      for (lang.tag in lang.tags) {
        obj[[lang.tag]] = inner.compile(obj[[lang.tag]], tags=tags, use.lang=FALSE,values=values)
      }
    }
    obj
  }

  form = inner.compile(form,tags=tags, values=values)
  form$fields = lapply(form$fields, inner.compile, tags=tags, values=values)
  form

}

infer.form.type = function(form) {
  restore.point("infer.form.type")

  if (!is.null(form[["type"]])) form$type

  if (!is.null(form[["forms"]])) "side_by_side"

  if (!is.null(form[["file"]])) {
    ext = tolower(tools::file_ext(form$file))
    if (ext == "md" | ext == "rmd") return("markdown")
  }

  type = "simple"
  return(type)

}

set.form = function(form, app=getApp(), params) {
  if (!missing(params))
    form$params = params
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]] = form
  } else {
    app$.ACTIVE.FORM = form
  }
}

get.form = function(app=getApp()) {
  if (is.null(app)) {
    .APP.FORMS.GLOB[[".ACTIVE.FORM"]]
  } else {
    app$.ACTIVE.FORM
  }
}

formSubmitButton = function(label=form$texts$submitBtnLabel, form=get.form(), add.form.alert=TRUE) {
  restore.point("formSubmitButton")

  id = paste0(form$prefix,"submitBtn",form$postfix)
  btn = HTML(as.character(actionButton(id, label)))
  if (!add.form.alert) return(btn)

  tagList(formAlertUI(form), btn)


}

formAlertUI = function(form=get.form()) {
  alertId = paste0(form$prefix,"form___Alert",form$postfix)
  formAlert = uiOutput(alertId)
  try(clear.form.alert(form=form), silent = TRUE)
  formAlert
}

add.form.handlers = function(form, submit.handler=form$submit.handler, btn.id = paste0(form$prefix,"submitBtn",form$postfix),...) {
  restore.point("add.form.handlers")


  buttonHandler(btn.id,formSubmitClick, form=form, submit.handler=submit.handler,...)
}

form.ui = function(form, params=form$params, add_handlers=FALSE,  success_fun=form$success_fun,...) {
  restore.point("form.ui")

  ui = NULL

  form.fun = paste0("form.ui.",form$type)
  ui = do.call(form.fun, list(form=form, params=params,...))
  if (add_handlers) {
    add.form.handlers(form=form, success_fun=success_fun,...)
  }
  ui
}


formSubmitClick = function(form, submit.handler = NULL,app=getApp(),id=NULL,session=NULL,...) {
  restore.point("formSubmitClick")

  res = get.form.values(form=form)
  restore.point("formSubmitClick_2")
  if (!is.null(submit.handler)) {
    if (isTRUE(form$submit.on.failure) | res$ok)
      submit.handler(ok=res$ok,values=res$values, form=form,...)
  }
}

set.form.params = function(form, params) {
  form$params = params
  form
}

set.form.widgets.values = function(form=get.form(),values,prefix=form$prefix, postfix=form$postfix) {
  names(values) = paste0(prefix, names(values),postfix)
  setWidgetValues(values)
}

get.form.values = function(form=get.form(),fields=form$fields,field.names=names(fields), prefix=form$prefix, postfix=form$postfix, check.values = TRUE, show.alerts=isTRUE(form$show.alerts)) {

  values = lapply(field.names, function(name) {
    id = paste0(prefix,name,postfix)
    getInputValue(id)
  })
  #cat("get.form.values:\n ", paste0(names(values),": ",values,collapse="\n"))
  restore.point("get.form.values")


  names(values) = field.names
  if (!check.values)
    return(values)

  check = check.form.values(values, form=form, show.alerts=show.alerts)

  if (show.alerts) {
    if (!check$ok) {
      show.form.alert(form=form,msg=form$texts$submitFailure)
    } else {
      show.form.alert(form=form,msg=form$texts$submitSuccess, color=NULL)
    }
  }
  return(check)
}

check.form.values = function(values, form, fields=form$fields[field.names], field.names=names(values), show.alerts = TRUE, get.failure.msg = FALSE) {
  restore.point("check.form.values")

  li = lapply(field.names, function(name) {
    ret = check.field.value(values[[name]], fields[[name]], lang=form$lang)
    if (!ret$ok & show.alerts) {
      show.field.alert(name=name, msg=ret$msg, form=form)
    } else {
      clear.field.alert(name=name, form=form)
    }
    ret
  })
  names(li)= field.names
  values = lapply(li, function(el) el$value)
  failed.fields = field.names[sapply(li, function(el) !el$ok)]
  ok = length(failed.fields) == 0
  if (get.failure.msg) {
    failure.msg = sapply(li[failed.fields], function(el) el$msg)
    return(list(ok=ok,values=values,failed.fields=failed.fields, failure.msg=failure.msg))
  }
  return(list(ok=ok,values=values,failed.fields=failed.fields))
}


clear.form.alert = function(form=NULL,msg="", prefix=form$prefix, postfix=form$postfix, color=NULL,id = paste0(prefix,"form___Alert",postfix)) {
  show.form.alert(msg=msg, form=form, color=color, prefix=prefix,postfix=postfix, id=id)
}

show.form.alert = function(form=NULL,msg="", prefix=form$prefix, postfix=form$postfix, color="red",id = paste0(prefix,"form___Alert",postfix)) {
  restore.point("show.form.alert")

  #cat("\n*********************************")
  #cat("\nshow.form.alert:\n", msg)
  #cat("\n*********************************")

  if (is.null(msg)) msg = ""
  if (!is.null(color))
    msg = colored.html(msg, color)

  setUI(id,HTML(msg))
}


get.lang.form = function(form, lang=NULL) {
  restore.point("get.lang.form")

  if (is.null(lang)) return(form)
  name = paste0("lang_",lang)
  if (!is.null(form[[name]])) return(form[[name]])
  form
}

form.default.values = function(form, values = NULL, sets=form[["sets"]], boolean.correction=TRUE) {
  vals = lapply(form$fields, function(field) field.default.values(form=form, field=field,sets=sets))

  replace = intersect(names(vals), names(values))

  if (length(replace)>0) {
    if (boolean.correction)
      boolean = sapply(vals[replace], function(val) is.logical(val) & !is.na(val))
    vals[replace] = values[replace]
    if (boolean.correction)
      vals[replace][boolean] =  lapply(vals[replace][boolean], as.logical)
  }
  vals
}

form.random.values = function(form, values = NULL, sets=form[["sets"]]) {
  restore.point("form.random.values")

  vals = lapply(form$fields, function(field) field.random.value(form=form, field=field,sets=sets))

  replace = intersect(names(vals), names(values))

  if (length(replace)>0) {
    vals[replace] = values[replace]
  }
  vals
}


form.schema.template = function(form) {
  li = form.default.values(form)
  schema.template(li)
}

get.form.texts = function(form, lang="en") {
  restore.point("get.form.texts")

  if (isTRUE(lang=="de")) {
    texts = list(
      submitBtnLabel = "Speichern",
      submitFailure = "Einige notwendige Angaben fehlen oder sind inkorrekt.",
      submitSuccess = ""
    )
  } else {
    texts = list(
      submitBtnLabel = "Submit",
      submitFailure = "Some required entries are missing or wrong.",
      submitSuccess = ""
    )
  }
  if (!is.null(form$texts)) {
    te = get.lang.object(form$texts,lang=lang)
    texts[names(te)] = te
  }

  texts
}

get.lang.object = function(obj, lang="en") {
  restore.point("get.lang.object")

  if (!is.null(obj[[paste0("lang_",lang)]])) {
    return(obj[[paste0("lang_",lang)]])
  } else if (!is.null(obj[["lang_en"]])) {
    return(obj[["lang_en"]])
  }
  return(obj)
}
