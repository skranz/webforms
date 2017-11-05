label.and.input = function(label, input, inputId, label.left=FALSE) {
  if (is.null(label)) return(input)
  lab = paste0('<label for="',inputId,'">',label,'</label>')
  if (label.left) {
    paste0("<table><tr><td style='padding-right: 1em'>",lab,"</td><td>",input,"</td></tr></table>")
  } else {
    paste0('<div class="form-group shiny-input-container">',lab,input,'</div>')
  }
}

textInputVector = function(inputId, label=NULL, value="", readonly = rep(FALSE, length(inputId)), style="", autocomplete = "off",class=extra.class, size=NULL, label.left=TRUE, form.control.class=TRUE, extra.class=""){
  restore.point("textInputVector")

  if (!is.null(size)) {
    style = paste0(style, ifelse(nchar(style)>0," ",""), "width: ",size,"em;")
  }
  if (form.control.class) {
    class = paste0("form-control ",class)
  }

  input = paste0('
    <input id="',inputId,'" type="text" style="',style,'" class="',class,'" value="',value,'"', ifelse(readonly,' readonly',''),' autocomplete="',autocomplete,'"/>')
  label.and.input(label = label, input=input, inputId = inputId, label.left=label.left)
}

numericInputVector = function (inputId, label=NULL, value=0, extra.class="")  {
  code = paste0('
<div class="form-group shiny-input-container">
  <label for="',inputId,'">',label,'</label>
  <input id="',inputId,'" type="number" class="form-control ', extra.class,'" value="',value,'"/>
</div>
  ')
  code = paste0('
  <input id="',inputId,'" type="number" class="form-control" value="',value,'"/>
  ')

  code
}

extraSmallButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button btn-xs">',icon,label,'</button>')
}


smallButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button btn-sm">',icon,label,'</button>')
}

simpleButtonVector = actionButtonVector = function(id, label="",icon=NULL, extra.class = "", extra.head="") {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button ',extra.class,'" ',extra.head,'>',icon,label,'</button>')
}

checkBoxInputVector = function (inputId, label, value = FALSE, extra.class="",...)  {
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))

  code = paste0('
<div class="form-group shiny-input-container">
  <div class="checkbox">
    <label>
      <input id="',inputId,'" type="checkbox" ', checked.str,' class="', extra.class,'"/>
      <span>',label,'</span>
    </label>
  </div>
</div>
  ')
  code
}

uiOutputVector = function(id) {
  paste0('<div id="',id,'" class="shiny-html-output"></div>')
}

selectizeInputVector = function (inputId, label=NULL, choices, selected=1, multiple=FALSE, value=NULL, options=list(dropdownParent="body"),width=NULL, extra.class = "", ...)  {
  restore.point("selectizeInputVector")

  if (multiple)
    stop("multiple = TRUE not yet implemented for selectizeInputVector")

  code = rep("", length(inputId))

  choices.lab = names(choices)
  if (is.null(choices.lab)) choices.lab = choices


  if (is.null(width)) {
    chars = max(nchar(choices.lab))
    chars = min(20, chars)
    chars = max(4, round(0.8)*chars)
    width=paste0(chars,"em")
  }

  style = paste0("width: ", width,";")


  if (length(value)<=1 & !is.list(selected)) {
    if (!is.null(value)) {
      selected = match(value[[1]], choices)
    }
    selected.str = rep("", length(choices))
    selected.str[selected] = "selected"

    options.str = paste0(collapse="\n",
      '<option value="',choices,'" ',  selected.str,'>',
      choices.lab,'</option>'
    )
  } else if (!is.null(value)) {
    options.str = unlist(lapply(value, function(val) {
      selected.str = rep("", length(choices))
      selected = match(val, choices)
      selected.str[selected] = "selected"

      paste0(collapse="\n",
        '<option value="',choices,'" ',  selected.str,'>',
        choices.lab,'</option>'
      )
    }))

  } else {
    stop("selected as list not yet implemented")
  }

  label.code = if(!is.null(label))
    paste0('<label class="control-label" for="',inputId,'">"',inputId,'</label>')

  options.code = paste0('<script type=\"application/json\" data-for=\"', inputId,'">',toJSON(options,auto_unbox = TRUE),'</script>')


  code = paste0('
<div class="form-group shiny-input-container vector-input-container" style="', style,'">
  ',label.code,'
  <div>
    <select id="',inputId,'" class="form-control ', extra.class,'">
    ',options.str,'
    ',options.code,'
  </div>
</div>
  ')
  code
}

uiOutputVector = function(id, tag="div", ...) {
  paste0('<',tag,' id="', id,'" class="shiny-html-output"></', tag,'>')
}
