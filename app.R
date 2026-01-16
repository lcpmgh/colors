##### packages ##########################################################################
library(shiny)
library(ggplot2)
library(stringr)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(colourpicker)
library(DT)

##### setting ##########################################################################
# 数据
colors_init  <- readLines("@colors.txt") %>% str_split(",") %>% lapply(., sort) %>% .[!duplicated(.)]  #读取、排序、去重
colors_nasc  <- sapply(colors_init, length) %>% order() %>% colors_init[.]                             #按子颜色数量排序
colors_table <- data.table(col_id=1:length(colors_nasc), col_num=sapply(colors_nasc, length))          #颜色序号数量表
colors_sect  <- colors_table[, c(min(.SD),max(.SD)), .SDcols=1, by="col_num"]  %>% 
  .[, type:=rep(c("min", "max"), nrow(.)/2)] %>% 
  dcast(., col_num~type, value.var = "V1") %>% 
  rbind(data.table(col_num="all", max=max(colors_table$col_id),  min=min(colors_table$col_id)), .)     #提取每个数量区间的id范围

# 函数
is_colors         <- function(str){
  # 函数，使用str_detect检查str是否是正确的颜色HEX码
  if(is.na(str))  return(F)                     #如果str是NA，直接返回F              
  if(str_length(str_trim(str))<1) return(F)     #如果str是空，直接返回F
  colo <- str_split(str, "[,，;、 ]") %>% unlist() %>% str_trim() %>% .[nchar(.) > 0] %>% .[!duplicated(.)]
  sig  <- str_detect(colo, "^#[A-Fa-f0-9]{6}$")
  if(any(!sig) | length(colo)>16){
    # 如不是颜色，返回F
    return(F)
  } else{
    # 如是颜色，返回颜色HEX
    return(colo)
  }
}
show_color        <- function(id, customcolor=NA, alpha, type){
  # 函数，根据给定颜色id/自定义颜色+透明度，绘制特定类型的图bar/box/point/line/pattern
  # id==0时，自定义颜色，否则按colors_nasc中的颜色
  # id==0时，如输入颜色有误，则绘制错误提示图像
  if(id == 0){
    sigcolor <- is_colors(customcolor)
    if(isFALSE(sigcolor)){
      plot <- ggplot()+
        annotate(geom="text", x=1,y=1,label="颜色有误！",size=10)+
        theme_void()+
        theme(panel.border = element_rect(color="black", linewidth = 1))
      return(plot)
    } else{
      tcolor <- sigcolor
      ncolor <- length(tcolor)
    }
  } else{
    tcolor <- colors_nasc[[id]]
    ncolor <- length(tcolor)
  }
  if(type == "bar"){
    dat_bar <- data.frame(x=letters[1:ncolor], y=runif(ncolor, 7, 10))
    plot <- ggplot(dat_bar, aes(x, y, fill=x))+
      geom_bar(color="black", stat = "identity", alpha=alpha)+
      scale_y_continuous(expand = c(0,0,0.05,0))+
      scale_fill_manual(values = tcolor)+
      labs(fill="fill", title = "Bar Chart with outlines")
  } else if(type == "box"){
    dat_box <- data.frame(x=letters[1:ncolor], y=runif(ncolor*10, 7, 10))
    plot <- ggplot(dat_box, aes(x, y, fill=x))+
      stat_boxplot(geom = "errorbar", linewidth=0.8, width = 0.3)+
      geom_boxplot(alpha=alpha)+
      scale_fill_manual(values = tcolor)+
      labs(fill="fill", title = "Boxplot with outlines")
  } else if(type == "point"){
    dat_point <- data.frame(x=rep(runif(30), ncolor),
                            y=rep(runif(30), ncolor),
                            c=rep(sample(letters, ncolor, replace = F), 30))
    plot <- ggplot(dat_point, aes(x, y, color=c))+
      geom_point(shape=16, size=5, alpha=alpha)+
      scale_color_manual(values = tcolor)+
      labs(color="color", title = "scatterplot without outlines")
  } else if(type == "line"){
    dat_line <- data.frame(x=rep(1:20, ncolor),
                           y=rep(1:ncolor, each=20)+rnorm(20*ncolor, 0, 0.3),
                           c=rep(sample(letters, ncolor), each=20))
    plot <- ggplot(dat_line, aes(x, y, color=c, group=c))+
      geom_line(linewidth=1, alpha=alpha)+
      scale_color_manual(values = tcolor)+
      labs(color="color", title = "Line chart without outlines")
  } else if(type == "pattern"){
    plot <- scales::show_col(tcolor)
    return(invisible()) 
  }
  # 公共主题
  plot <- plot+
    theme_bw()+
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    )
  return(plot)
}
make_color_blocks <- function(value) {
  # 一个函数，转换color_show为html色块
  colors <- strsplit(value, ",\\s*")[[1]]
  paste0(
    sprintf(
      '<span style="
        display:inline-block;
        width:20px;
        height:20px;
        background:%s;
        border:1px solid #000;
        margin:0 1px;
        vertical-align: middle; 
      "></span>',
      colors
    ),
    collapse = ""
  )
}
make_color_dt     <- function(id, customcolor=NA, colors=colors_nasc){
  # 利用颜色id或自定义颜色，生成展示表格所需的数据
  # 注意，当id=="all"时，代表所有颜色，当id==0时，代表自定颜色，此时必须要有customcolor
  if(id==0){
    sigcolor <- is_colors(customcolor)
    if(isFALSE(sigcolor)){
      color_dt <- data.table(id=0, colors_n=0, colors_hex="ERROR", colors_show="ERROR")
      return(color_dt)
    } else{
      tcolor_dt <- data.table(
        id=0,
        colors_n=length(sigcolor),
        colors_hex=paste0(sigcolor, collapse = ", "),
        colors_show=paste0(sigcolor, collapse = ", ")
      )
    }
  } else if(id=="all"){
    tcolor_dt <- data.table(
      id=1:length(colors_nasc),
      colors_n=sapply(colors_nasc, length),
      colors_hex=sapply(colors_nasc, function(x) paste0(x, collapse = ", ")),
      colors_show=sapply(colors_nasc, function(x) paste0(x, collapse = ", "))
    )
  } else{
    tcolor_dt <- data.table(
      id=id,
      colors_n=length(colors_nasc[[id]]),
      colors_hex=paste0(colors_nasc[[id]], collapse = ", "),
      colors_show=paste0(colors_nasc[[id]], collapse = ", ")
    )
  }
  # 将colors_show转换为可现实色块的html语句
  color_dt <- copy(tcolor_dt) %>% .[, colors_show:=vapply(colors_show, make_color_blocks, character(1))]
  return(color_dt)
}

##### ui ################################################################################
ui <- fluidPage(
  tags$title("科研绘图配色推荐器"),
  tags$head(tags$link(rel = "shortcut icon", href = "pmgh.ico")),
  tags$head(
    tags$style(HTML("
      /* DT表格样式*/
      table.dataTable {
        border: 0px solid #525252 !important;
        border-collapse: collapse !important;       /*相邻单元格边框合并为一条线*/
        border-spacing: 0 !important;
      }        /* 全表，左上右框线，不包括下框线 */
      table.dataTable th, table.dataTable td {
        border: 1px solid #525252 !important;
      }        /* 全表，内框+外框所有线 */
      div.dataTables_scrollBody table.dataTable {
        border-top: 0px solid #525252 !important;
        border-bottom: 0px solid #525252 !important;
      }        /* 全表，上下框线 */
      div.dataTables_scrollHead table.dataTable {
        border-bottom: 0px solid #525252 !important;
      }        /* title，下框线 */
      div.dataTables_scrollBody, div.dataTables_scrollHead {
        border-bottom: 0px solid #525252 !important;
      }        /* 全表，下框线 */
      table.dataTable thead th, table.dataTable thead td {
        border-bottom: 0px solid #525252 !important;
      }        /* title，下框线 */
      table.dataTable tbody tr:first-child td {
        border-top: 0px solid #525252 !important;
      }        /* title，下框线 */
      table.dataTable thead th:nth-child(3), table.dataTable thead th:nth-child(4) {
        text-align: center !important;
      }        /* 第 3、4 列：表头居中 */
      table.dataTable tbody td:nth-child(3), table.dataTable tbody td:nth-child(4) {
        text-align: left !important;
      }        /* 第 3、4 列：表体左对齐（保险） */
      table.dataTable tbody tr.selected td,
      table.dataTable td.selected {
        box-shadow: inset 0 0 0 9999px #e3f0ff !important;
        color: #1a1a1a !important;
        text-shadow: 0 0 1px rgba(0,0,0,0.08) !important;
        background-color: transparent !important;     /* 避免 background 干扰 */
      }        /* 选中状态：柔和淡蓝 + 深色文字 */
      table.dataTable tbody tr.selected:hover td,
      table.dataTable tbody tr:hover td.selected,
      table.dataTable.hover tbody tr.selected:hover td,
      table.dataTable.display tbody tr.selected:hover td {
        box-shadow: inset 0 0 0 9999px #e3f0ff !important;
        color: #1a1a1a !important;
        background-color: transparent !important;
      }        /* 当选中且 hover 时，强制使用相同颜色，覆盖内置 hover 效果 */
    "))
  ),
  # body部分
  div(
    style = "margin:10px 0; display: flex; align-items: flex-end; gap: 2px;",
    h3("方案选择"),
    h5(sprintf("(数据库内现有%d个配色方案)",length(colors_nasc)))
  ),
  # 选择按数据库查找，还是自定义颜色
  radioGroupButtons(
    inputId = "showtype",
    label = NULL,
    choices = c("配色数据库方案id" = "id", "自定义配色方案" = "custom"),
    selected = "id",
    checkIcon = list(
      yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
      no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
  ),
  # 如果按配色数据库方案
  conditionalPanel(
    condition = "input.showtype == 'id'",
    sliderTextInput(
      inputId = "num_select", 
      label = "选择颜色数量", 
      choices = colors_sect$col_num,
      selected = "all",
      width = "700px", 
      grid  = T
    ),
    div(
      style = "display: flex; align-items: center; gap:30px;",
      sliderTextInput(
        inputId = "id_select", 
        label = "选择方案id", 
        choices = seq(colors_sect[[1, "min"]], colors_sect[[1, "max"]]),
        selected = colors_sect[[1, "min"]],
        width = "700px", 
        grid  = T
      ),
      div(
        style = "display: flex; align-items: center; gap:10px;",
        shiny::actionButton(
          inputId = "pre",
          label = "上一个",
          icon  = icon("angle-left")
        ),
        shiny::actionButton(
          inputId = "nex",
          label = "下一个",
          icon  = icon("angle-right")
        )
      ),
    )
  ),
  # 如果自定义颜色方案
  conditionalPanel(
    condition = "input.showtype == 'custom'",
    div(
      style = "display: flex; align-items: center; width:700px",
      colourInput(
        inputId = "sele_col", 
        label = "追加颜色", 
        allowTransparent = F,
        width = "174px",
        value = "skyblue"),
      div(
        style = "display: flex; text-align: center; gap: 20px; margin-top: 10px; margin-left: 20px",
        shiny::actionButton(
          inputId = "add_color", 
          label = "加入",
          icon  = icon("plus")
        ),
        shiny::actionButton(
          inputId = "reset_color", 
          label = "重置",
          icon  = icon("redo")
        )
      ),
    ),
    textInput(
      inputId = "color_custom",
      label = "自定义颜色（HEX码，多个颜色以逗号、顿号、空格、分号间隔，颜色数量不可超过16，结果将去重）：",
      width = "1000px",
      value = "#4DBBD5, #00A087, #E64B35"
    )
  ),
  # 选择结果
  h3("所选配色方案"),
  div(
    style = "width: 1080px; margin: 0; overflow-x: left;",
    DTOutput(outputId = "colors_info")
  ),
  # 更改透明度
  div(
    style = "margin-top: 20px;",   
    sliderTextInput(
      inputId = "color_alpha",
      label = "颜色透明度（颜色图层的alpha值）",
      choices = seq(0, 1, by = 0.05),
      selected = 1,
      grid = TRUE,
      width = "500px"
    )
  ),
  # 绘图效果展示
  div(
    style = "display: flex; align-items: top; width:1080px",
    div(
      style = "width: 50%; text-align: center;", 
      h3("绘图效果"),
      div(
        style = "display: flex; gap:1px; margin:1px 0;",
        plotOutput(outputId = "plot_bar", width = "268px", height = "220px"),
        plotOutput(outputId = "plot_box", width = "268px", height = "220px")
      ),
      div(
        style = "display: flex; gap:1px; margin:1px 0;",
        plotOutput(outputId = "plot_point", width = "268px", height = "220px"),
        plotOutput(outputId = "plot_line",  width = "268px", height = "220px")
      ),
    ),
    div(
      style = "width: 50%; text-align: center;", 
      h3("方案样式"),
      plotOutput(outputId = "plot_pattern", width = "536px", height = "440px")
    )
  ),
  
  # 配色数据库
  conditionalPanel(condition = "input.showtype == 'id'",h3("配色数据库（点击表格显示绘图效果）")),
  conditionalPanel(condition = "input.showtype == 'custom'",h3("配色数据库")),
  div(
    style = "width: 1080px; margin: 0; overflow-x: left;",
    DTOutput(outputId = "colors_dataset")
  ),
  
  # 网页页脚 ...................................................................
  hr(),
  div(
    tags$style(HTML("
        #footer-container { text-align:center; font-size:13px; color:#666666; line-height:0.8; margin:12px 0px; } 
        #footer-container a { color:#0066cc; text-decoration:none; margin: 0 2px; } 
        #footer-container a:hover { text-decoration:none; } 
        .footer-links, .footer-records { display:inline-flex; flex-wrap:wrap; justify-content:center; margin: 0 2px; gap: 10px; }
      ")),
    div(
      id = "footer-container",
      tags$p(sprintf("© 2021–%s, Lcpmgh. All rights reserved.", format(Sys.Date(), "%Y"))),
      div(
        class = "footer-links",
        tags$a(icon("github"), " lcpmgh", href = "https://github.com/lcpmgh", target="_blank"),
        tags$a(icon("envelope"), " lcpmgh@gmail.com", href = "mailto:lcpmgh@gmail.com"),
        tags$a(icon("home"), " lcpmgh.com", href = "http://lcpmgh.com/", target="_blank")
      ),
      div(
        class = "footer-records",
        tags$a("冀ICP备2022003075号", href = "https://beian.miit.gov.cn", target="_blank"),
        tags$a("川公网安备51010702002736", href = "http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", target="_blank")
      )
    )
  )
  # ............................................................................
)

##### server ############################################################################
server <- function(input, output, session){
  # 更改颜色数量，更新下面sliderTextInput中备选id的区间
  observe({
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    updateSliderTextInput(session, "id_select", choices = selected_seq, selected = selected_seq[1])
  }) %>% bindEvent(input$num_select)
  
  # 点击上一个或下一个后，更新slider对应的值
  observe({
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    newValue <- max(selected_seq[1], as.numeric(input$id_select) - 1)
    updateSliderTextInput(session, "id_select", selected = newValue)
  }) %>% bindEvent(input$pre)
  observe({
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    newValue <- min(tail(selected_seq,1), as.numeric(input$id_select) + 1)
    updateSliderTextInput(session, "id_select", selected = newValue)
  }) %>% bindEvent(input$nex)
  
  # 点击加入或重置颜色，更新textinput
  observe({
    newValue <- str_split(input$color_custom, "[,，;、 ]") %>%
      unlist() %>%
      str_trim() %>%
      .[nchar(.) > 0] %>%
      c(., input$sele_col) %>%
      .[!duplicated(.)] %>%
      paste0(collapse = ",")
    updateTextInput(session, "color_custom", value = newValue)
  }) %>% bindEvent(input$add_color)
  observe({
    newValue <- "#4DBBD5, #00A087, #E64B35"
    updateTextInput(session, "color_custom", value = newValue)
  }) %>% bindEvent(input$reset_color)
  
  # 动态数据
  color_id_rv <- reactiveVal(1)          #存储颜色id，注意不用reactiveValues
  color_custom_rv <- reactiveVal(NULL)   #储存自定义颜色。
  observe({
    # 获取实时id数据
    color_id_rv(as.numeric(input$id_select))
    # 获取实时color_custom
    color_custom_rv(input$color_custom)
  })
  
  # 配色方案信息
  output$colors_info  <- renderDT({
    showtype <- input$showtype
    color_id <- color_id_rv()
    color_custom <- color_custom_rv()
    if(showtype=="id"){
      color_dt <- make_color_dt(color_id, NA, colors_nasc)
    } else{
      color_dt <- make_color_dt(0, color_custom, colors_nasc)
    }
    # color_dt <- make_color_dt("all", color_custom, colors_nasc)
    DT::datatable(
      color_dt,
      escape = FALSE,
      colnames = c("id", "数量", "HEX码", "预览"),
      rownames = FALSE,
      width = 1080,
      selection = "none",
      options = list(
        columnDefs = list(
          list(width = '30px',  targets = 0, className = 'dt-center'), 
          list(width = '30px',  targets = 1, className = 'dt-center'), 
          list(width = '700px', targets = 2),
          list(width = '320px', targets = 3)
        ),
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        scrollX = FALSE,
        scrollY = FALSE,
        info = FALSE
      ),
      class = "compact stripe hover"
    )
  })
  
  # 绘图效果 
  output$plot_bar     <- renderPlot({
    showtype      <- input$showtype
    color_alpha   <- input$color_alpha
    color_id      <- color_id_rv()
    color_custom  <- color_custom_rv()
    if(showtype=="id"){
      show_color(color_id, NA, color_alpha, "bar")
    } else{
      show_color(0, color_custom, color_alpha, "bar")
    }
  })
  output$plot_box     <- renderPlot({
    showtype      <- input$showtype
    color_alpha   <- input$color_alpha
    color_id      <- color_id_rv()
    color_custom  <- color_custom_rv()
    if(showtype=="id"){
      show_color(color_id, NA, color_alpha, "box")
    } else{
      show_color(0, color_custom, color_alpha, "box")
    }
  })
  output$plot_point   <- renderPlot({
    showtype      <- input$showtype
    color_alpha   <- input$color_alpha
    color_id      <- color_id_rv()
    color_custom  <- color_custom_rv()
    if(showtype=="id"){
      show_color(color_id, NA, color_alpha, "point")
    } else{
      show_color(0, color_custom, color_alpha, "point")
    }
  })
  output$plot_line    <- renderPlot({
    showtype      <- input$showtype
    color_alpha   <- input$color_alpha
    color_id      <- color_id_rv()
    color_custom  <- color_custom_rv()
    if(showtype=="id"){
      show_color(color_id, NA, color_alpha, "line")
    } else{
      show_color(0, color_custom, color_alpha, "line")
    }
  })
  output$plot_pattern <- renderPlot({
    showtype      <- input$showtype
    # color_alpha   <- input$color_alpha
    color_id      <- color_id_rv()
    color_custom  <- color_custom_rv()
    if(showtype=="id"){
      show_color(color_id, NA, NA, "pattern")
    } else{
      show_color(0, color_custom, NA, "pattern")
    }
  })
  
  # 颜色数据库
  output$colors_dataset  <- renderDT({
    current_showtype   <- input$showtype
    current_color_numb <- input$num_select
    tcolor_dt <- make_color_dt("all", NA, colors_nasc) 
    # 当showtype=="id"时，显示选中的颜色数量的数据
    # 当showtype=="custom"时，显示全部颜色数据
    if(current_showtype=="id"){
      if(current_color_numb == "all"){
        color_dt <- copy(tcolor_dt)
      } else{
        color_dt <- copy(tcolor_dt) %>%.[colors_n==as.integer(current_color_numb), ]
      }
    } else{
      color_dt <- copy(tcolor_dt)
    }
    DT::datatable(
      color_dt,
      escape = FALSE,
      colnames = c("id", "数量", "HEX码", "预览"),
      rownames = FALSE,
      width = 1080,
      selection = "single",
      options = list(
        columnDefs = list(
          list(width = '30px',  targets = 0, className = 'dt-center'), 
          list(width = '30px',  targets = 1, className = 'dt-center'), 
          list(width = '700px', targets = 2),
          list(width = '320px', targets = 3)
        ),
        pageLength = 25,
        searching = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        scrollX = FALSE,
        scrollY = FALSE,
        info = FALSE,
        rowCallback = JS("
        function(row, data, index) {
          $(row).off('click.dt').on('click.dt', function() {
            // 只在 showtype == 'id' 时生效
            if (Shiny && Shiny.shinyapp &&
                Shiny.shinyapp.$inputValues['showtype'] === 'id') {
              window.scrollTo({
                top: 330,
                left: 0,
                behavior: 'smooth'
              });
            }
          });
        }
      ")
      ),
      class = "compact stripe hover"
    )
  })
  
  # 当showtype == "id"时，点击配色数据库表格，更新id_select的值
  observe({
    req(input$showtype == "id")    
    selected_row <- input$colors_dataset_rows_selected
    colnum <- colors_sect[col_num == input$num_select,]
    selected_id <- seq(colnum$min, colnum$max)[selected_row]
    updateSliderTextInput(session, "id_select", selected = selected_id)
  }) %>% bindEvent(input$colors_dataset_rows_selected)
}

##### app #################################################################################
shinyApp(ui, server)
