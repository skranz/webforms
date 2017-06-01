label.and.input = function(label, input, inputId, label.left=FALSE) {
  if (is.null(label)) return(input)
  lab = paste0('<label for="',inputId,'">',label,'</label>')
  if (label.left) {
    paste0("<table><tr><td style='padding-right: 1em'>",lab,"</td><td>",input,"</td></tr></table>")
  } else {
    paste0('<div class="form-group shiny-input-container">',lab,input,'</div>')
  }
}

textInputVector = function(inputId, label=NULL, value="", readonly = rep(FALSE, length(inputId)), style="", autocomplete = "off",class="", size=NULL, label.left=TRUE, form.control.class=TRUE){

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

numericInputVector = function (inputId, label=NULL, value=0)  {
  code = paste0('
<div class="form-group shiny-input-container">
  <label for="',inputId,'">',label,'</label>
  <input id="',inputId,'" type="number" class="form-control" value="',value,'"/>
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

actionButtonVector = function(id, label="",icon=NULL) {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="btn btn-default action-button">',icon,label,'</button>')
}

checkBoxInputVector = function (inputId, label, value = FALSE,...)  {
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))

  code = paste0('
<div class="form-group shiny-input-container">
  <div class="checkbox">
    <label>
      <input id="',inputId,'" type="checkbox" ', checked.str,'/>
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

selectizeInputVector = function (inputId, label, choices, selected=1)  {
  code = rep("", length(inputId))
  checked.str = ifelse(value,' checked="checked"',"")
  value = rep(value, length.out=length(code))

  choices.lab = names(choices)
  if (is.null(choices.lab)) choices.lab = choices

  if (!is.list(selected)) {
    selected.str = rep("", length(inputId))
    selected.str[selected] = "selected"

    options.str = paste0(collapse="\n",
      '<option value="',choices,'" ',  selected.str,'>',
      choices.lab,'</option>'
    )
  } else {
    stop()
  }

  code = paste0('
<div class="form-group shiny-input-container">
  <label class="control-label" for="',inputId,'">"',inputId,'</label>
  <div>
    <select id="',inputId,'" class="form-control">
    ',options.str,'
    <script type="application/json" data-for="', inputId,'">{}</script>
  </div>
</div>
  ')
  code
}
