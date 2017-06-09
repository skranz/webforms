yaml.form = function(file=NULL, text=NULL, utf8 = TRUE, lang="en", prefix=NULL, postfix=NULL, warn.no.prefix = TRUE,...) {
  form = read.yaml(file=file, text=text,utf8 = utf8)
  init.form(form, lang=lang, prefix=prefix, postfix=postfix, warn.no.prefix = warn.no.prefix, ...)
}

#' Create a form ui simply based on the yaml form definition
form.ui.simple = function(form, fields=form$fields, values=NULL, submitBtn=NULL, submitLabel=texts$submitBtnLabel,add.submit=TRUE,lang=form[["lang"]], postfix = form$postfix, prefix = form$prefix, add.form.alert=TRUE, texts = form$texts, submit.handler = NULL, ...) {
  restore.point("form.ui.simple")

  li = lapply(names(fields), function(name) {
    list(
      fieldInput(name=name,form=form, value = values[[name]], lang=lang, widget.as.character=FALSE, postfix=postfix, prefix=prefix),
      hr()
    )
  })
  if (!add.submit) return(li)


  if (add.form.alert) {
    submitAlertId = paste0(form$prefix,"form___Alert",form$postfix)
    submitAlert = uiOutput(submitAlertId)
    try(clear.form.alert(form=form), silent = TRUE)
  }

  if (is.null(submitBtn)) {
    id = paste0(form$prefix,"submitBtn",form$postfix)
    submitBtn = actionButton(id,submitLabel)
  }

  if (!is.null(submit.handler)) {
    btn.id = submitBtn$attribs$id
    add.form.handlers(form=form,btn.id=btn.id, submit.handler)
  }

  tagList(li, submitAlert,submitBtn)
}
