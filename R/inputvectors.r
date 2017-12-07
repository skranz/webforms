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


textAreaInputVector = function(inputId, label=NULL, value="", readonly = rep(FALSE, length(inputId)), style="overflow:hidden", autocomplete = "off",class=extra.class, size=NULL, label.left=TRUE, extra.class="",...){
  restore.point("textAreaInputVector")

  if (!is.null(size)) {
    style = paste0(style, ifelse(nchar(style)>0," ",""), "width: ",size,"em;")
  }

  input = paste0('
    <textarea id="',inputId,'" style="',style,'" class="',class,'"', ifelse(readonly,' readonly',''),' autocomplete="',autocomplete,'"/>', value, '</textarea>')
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


smallButtonVector = function(id, label="",icon=NULL,...) {
  simpleButtonVector(id, label, icon,size="sm",...)
}



simpleButtonVector = actionButtonVector = function(id, label="",icon=NULL, size=c("default","sm","xs")[1], class=paste0("btn btn-default action-button", if (size != "default") paste0(" btn-",size)), extra.class = "", extra.head="") {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="',class, ' ',extra.class,'" ',extra.head,'>',icon,label,'</button>')
}

checkBoxInputVector = function (inputId, label=NULL, value = FALSE, extra.class="",extra.head = "", wrap.shiny=FALSE,...)  {
  restore.point("checkBoxInputVector")
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))
  input.code = paste0('<input id="',inputId,'" type="checkbox" ', checked.str,' class="', extra.class,'" ', extra.head,'/>')

  if (!wrap.shiny) {
    return(input.code)
  }

  code = paste0('
<div class="form-group shiny-input-container">
  <div class="checkbox">
    ',
    #if(is.null(label)) {
    #  input.code
    #} else {
      paste0('<label>', input.code,'
      <span>',label,'</span>
      </label>')
    #},'
    ,'
  </div>
</div>
  ')
  code
}

uiOutputVector = function(id) {
  paste0('<div id="',id,'" class="shiny-html-output"></div>')
}

selectizeInputVector = function (inputId, label=NULL, choices, selected=1, multiple=FALSE, value=NULL, options=list(dropdownParent="body"), width=NULL, extra.class = "", size=NULL, ...)  {
  restore.point("selectizeInputVector")

  if (multiple)
    stop("multiple = TRUE not yet implemented for selectizeInputVector")

  code = rep("", length(inputId))

  choices.lab = names(choices)
  if (is.null(choices.lab)) choices.lab = choices


  if (is.null(width)) {
    if (!is.null(size)) {
      width = paste0(size,"em")
    } else {
      chars = max(nchar(choices.lab))
      chars = min(20, chars)
      chars = max(4, round(0.8)*chars)
      width=paste0(chars,"em")
    }
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


multiSelectizeInputVector = function (inputId, label=NULL, value=NULL, choices, selected=value, multiple=FALSE, options=list(dropdownParent="body"), width=NULL, extra.class = "", extra.head="", size=NULL, choices.lab=first.none.null(names(choices),choices), sep=",", ...)  {
  restore.point("multiSelectizeInputVector")

  value = first.non.null(value, selected)

  if (length(inputId)==1 & length(value)>1) {
    inputId = paste0(inputId,"_",seq_along(value))
  }

  if (is.null(width)) {
    if (!is.null(size)) {
      width = paste0(size,"em")
    } else {
      width = "15em"
    }
  }

  style = paste0("width: ", width,";")

  if (!is.list(value)) {
    value = strsplit(value,sep, fixed=TRUE)
  }

  choices.code = lapply(value, function(val) {
    #restore.point("isjidjf")
    selected.str = ifelse(choices %in% val, "selected","")
    paste0(collapse="\n",
      '<option value="',choices,'" ',  selected.str,'>',
      choices.lab,'</option>'
    )
  })

  options.code = paste0('<script type=\"application/json\" data-for=\"', inputId,'">',toJSON(options,auto_unbox = TRUE),'</script>')


  code = paste0('
<div class="form-group shiny-input-container vector-input-container" style="', style,'">
  <div>
    <select id="',inputId,'" multiple="multiple" class="form-control ', extra.class,'" ', extra.head,'>
    ',choices.code,'
    ',options.code,'
  </div>
</div>
  ')
  code
}
