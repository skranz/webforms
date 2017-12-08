form.table.default.css = function(class="r-webform-table") {
  css = paste0('
table.',class,' {
  border-collapse: collapse;
  display: block;
  overflow-x: auto;
}

.',class,' th {
  font-weight: bold;
  margin: 5px;
  padding: 5px;
  //border: solid 1px black;
  text-align: center;
  font-size: 100%;
}
.',class,' td {
  font-family: Verdana,Geneva,sans-serif;
  margin: 0px 3px 1px 3px;
  padding: 1px 3px 1px 3px;
  //border-left: solid 1px black;
  //border-right: solid 1px black;
  text-align: left;
  white-space: nowrap;
  font-size: 100%;
}

')
}


form.html.table = function(df, class="r-webform-table", id = random.string(), col.names=colnames(df), col.tooltips=NULL, round.digits=8, signif.digits=8, just.tr = FALSE, rowids = seq_len(NROW(df)),nowrap=TRUE, has.filter=FALSE, ...) {
  restore.point("form.html.table")
  n = NROW(df)

  if (!is.null(col.names) & !just.tr) {
    colnames=col.names
    if (is.null(col.tooltips)) {
      inner = colnames
    } else {
      inner = paste0('<span title="', col.tooltips,'">', colnames, '<span>')
      #inner[nchar(col.tooltips)==0] = colnames
    }

    head = paste0('<th class="data-frame-th">',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }

  td.class = rep(c("odd-row","even-row"), length.out=NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="td-bottom"
  }
  tr.class = paste0("data-row tr-row-", rowids)

  my.format.vals = function(vals) {
    rmdtools::format.vals(vals, signif.digits=signif.digits, round.digits=round.digits)
  }

  cols = 1:NCOL(df)
  rows = 1:NROW(df)
  code = paste0('"<td data-row = \\"",rows,"\\" data-col = \\"',cols,'\\" class=\\"",td.class,"\\" ', if (nowrap) 'nowrap','>", my.format.vals(df[[',cols,']]),"</td>"', collapse=",")

  # filter row will be part of thead

  #code = paste0('paste0("<tr class=\\"",tr.class,"\\">",',code,',"</tr>", collapse="\\n")')
  code = paste0('paste0("<tr class=\\"",tr.class,"\\">",',code,',"</tr>")')
  call = parse(text=code)
  body = eval(parse(text=code))

  if (has.filter) {
    head = paste0(head,"\n", body[1])
    body = body[-1]
  }
  body = paste0(body, collapse="\n")

  if (just.tr) return(body)


  tab = paste0('<table class="',class,'" id="',id,'">\n <thead>\n', head,'\n</thead><tbody>\n', body, "\n</tbody>\n</table>")
  return(tab)
}



html.table = function(df, id = random.string(), sel.row=NULL, show.header=TRUE, header=colnames(df), row.names=FALSE, border=TRUE, bg.color =c("#dddddd","#ffffff"), sel.color='#ffdc98', font.size="100%", round.digits=8, signif.digits=8,col.tooltips=NULL, td.padding = "3px 5px 3px 5px", td.margin = "2px 4px 2px 4px", nowrap=TRUE, ...) {
  restore.point("html.table")
  n = NROW(df)

  #bg.color =c("#ededfe","#fcfcff")
  #F7F7F7
  row.bgcolor = rep(bg.color,length=n)

  if (!is.null(sel.row)) {
    #row.bgcolor[sel.row]='#ffdc98'
    #row.bgcolor[sel.row]='#00ff00'
    #row.bgcolor[sel.row]=sel.color
  }

  if (show.header) {
    colnames=header
    if (is.null(col.tooltips)) {
      inner = colnames
    } else {
      inner = paste0('<span title="', col.tooltips,'">', colnames, '<span>')
      #inner[nchar(col.tooltips)==0] = colnames
    }

    head = paste0('<th class="data-frame-th">',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }


  td.class = rep("data-frame-td td-not-bottom", NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="data-frame-td td-bottom"
  }
  #td.class = paste0(td.class," td-row-",1:NROW(df))
  tr.class = paste0("tr-row-", 1:NROW(df))
  if (length(sel.row)>0)
    tr.class[sel.row] = paste0("tr-sel-row")

  cols = 1:NCOL(df)
  rows = 1:NROW(df)
  code = paste0('"<td data-row = \\"",rows,"\\" data-col = \\"',cols,'\\" class=\\"",td.class,"\\" ', if(nowrap) "nowrap", '>", (df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr class=\\"",tr.class,"\\" style =\\"background-color:",row.bgcolor,";\\">",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table class="data-frame-table" id="',id,'">\n', head, main, "\n</table>")

  th.style='font-weight: bold; margin: 5px; padding: 5px; border: solid 1px black; text-align: center;'
  td.style= paste0('font-family: Verdana,Geneva,sans-serif;  margin: ', td.margin, '; padding: ', td.padding,'; border: solid 1px black; text-align: left;')

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")

  }

  table.sel = paste0("#",id)
  tab = paste0("<style>",
    " ", table.sel," {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " ", table.sel, " td.data-frame-td {", td.style,"}\n",
    " ", table.sel, " tr.tr-sel-row { background-color:", sel.color," !important;}\n",
    " ", table.sel, " td-bottom {border-bottom: solid 1px black;}\n",
    " ", table.sel, " th.data-frame-th {", th.style,"}\n",
    " ", table.sel, " tbody>tr:last-child>td {
      border-bottom: solid 1px black;
    }\n",
    "</style>",
    tab
  )

  #writeLines(tab, "test.html")
  tab

  return(tab)
}


#' Handler for a click on a table row
#' @param id id of the HTML img object
#' @param fun the handler fun that will be called when the image is clicked
#' @param ... additional arguments passed to the handler fun
tdClickHandler = function(id=NULL, fun, ..., eventId=if(stop.propagation) "tdClickEvent" else "tdClickEventWithPropagation", css.selector=paste0("#",id, " td.data-frame-td"), app=getApp(),no.authentication.required=FALSE, stop.propagation=TRUE, auto.select = FALSE, remove.sel.row.selector=NULL) {
  restore.point("tdClickHandler")

  sp = if (stop.propagation) "e.stopPropagation();" else ""
  inner.js = paste0('
    var table = $(this).closest("table");
    var td = $(this);
  ',sp)
  if (auto.select) {
    if (is.null(remove.sel.row.selector)) {
      remove.sel = 'table.find("tr").removeClass("tr-sel-row");'
    } else {
      remove.sel = paste0('$("',remove.sel.row.selector,'").removeClass("tr-sel-row");')
    }
    inner.js = paste0(inner.js,'
      var tr = $(this).closest("tr");
      ',remove.sel,'
      tr.addClass("tr-sel-row");
    ')
  }

  shiny.value.code = paste0('{eventId: "',eventId,'", id: table.attr("id"), tdClass: td.attr("class"), data: td.data(), tdId: td.attr("id"),  tableId: table.attr("id"), tableClass: table.attr("class")}')

  customEventHandler(eventId = eventId,css.locator = css.selector,event = "click",id = id,inner.js.code = inner.js, shiny.value.code = shiny.value.code, fun=fun,...)
}

