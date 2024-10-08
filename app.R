library(shiny)
library(purrr)
library(scales)
library(ggplot2)
library(stringr)
library(magrittr)
library(reactable)
library(gridExtra)
library(htmltools)
library(data.table)
library(shinyWidgets)
library(colourpicker)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "科研绘图配色推荐器",titleWidth = "100%"),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    uiOutput("uipanel"),
    tags$hr(),
    tags$div(align = "center", 
             tags$p("\ua9 2021-2024, LIANG Chen, Institute of Mountain Hazards and Environment, CAS. All rights reserved.", style="height:8px"),
             tags$div(align = "center",
                      actionLink(inputId = "", label = "lcpmgh ", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh')"),
                      tags$p("  ", style = "display:inline;white-space:pre"),
                      actionLink(inputId = "", label = "lcpmgh@gmail.com", icon = icon("envelope"), onclick ="window.location.href='mailto:lcpmgh@gmail.com'"),
                      tags$p("  ", style = "display:inline;white-space:pre"),
                      actionLink(inputId = "", label = "lcpmgh.com", icon = icon("home"), onclick ="window.location.href='http://lcpmgh.com/'")
             ),
             tags$div(align = "center",
                      tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                      tags$p("  ", style = "display:inline;white-space:pre"),
                      # tags$img(src="gaba.png"),
                      tags$a("川公网安备51010702002736", target="_blank", href="http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", style="color:#06c; display:inline;")
             )
    )
  )
)

server <- function(input, output, session){
  # 数据
  colors <- readLines("@colors.txt") %>% str_split(",") %>% lapply(., sort) %>% .[!duplicated(.)]  #读取、排序、去重
  colors_nasc  <- map_int(colors, length) %>% order() %>% colors[.]                                #按子颜色数量排序
  colors_table <- data.frame(col_id=1:length(colors_nasc), col_num=map_int(colors_nasc, length))   #颜色序号数量表
  setDT(colors_table)
  colors_sect  <- colors_table[, c(min(.SD),max(.SD)), .SDcols=1,by="col_num"]  %>% 
    .[, type:=rep(c("min", "max"), nrow(.)/2)] %>% 
    dcast(., col_num~type, value.var = "V1") %>% 
    rbind(data.table(col_num="all", 
                     max=max(colors_table$col_id), 
                     min=min(colors_table$col_id)), .)                                              #提取每个数量区间的id范围          
  # 函数
  examp_plot   <- function(id, colors_nasc=NULL, custom=c("#F5A889", "#ACD6EC")){
    # 函数，根据给定颜色id或自定义的颜色，画4个案例图
    # id=0时自定义颜色，否则按colors_nasc中的颜色
    if(id == 0){
      tcolor <- custom
      ncolor <- length(tcolor)
    } else{
      tcolor <- colors_nasc[[id]]
      ncolor <- length(tcolor)
    }
    # 图1.bar
    dat_bar <- data.frame(a=sample(letters, ncolor, replace = F), 
                          b=runif(ncolor, 7, 10))
    p_bar <- ggplot(dat_bar, aes(a, b, fill=a))+
      geom_bar(color="black", stat = "identity")+
      scale_y_continuous(expand = c(0,0,0,0.1))+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis", title = "Bar Chart with outlines")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    # 图2.box
    dat_box <- data.frame(a=sample(letters, ncolor, replace = F),
                          b=runif(ncolor*20, 7, 10))
    p_box <- ggplot(dat_box, aes(a, b, fill=a))+
      stat_boxplot(geom = "errorbar", linewidth=0.8, width = 0.3)+
      geom_boxplot()+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis", title = "Boxplot with outlines")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    # 图3.point
    dat_point <- data.frame(a=rep(runif(30), ncolor),
                            b=rep(runif(30), ncolor),
                            t=rep(sample(letters, ncolor, replace = F), 30))
    p_point <- ggplot(dat_point, aes(a, b, color=t, fill=t))+
      geom_point(shape=21, size=5)+
      scale_color_manual(values = tcolor)+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis", title = "scatterplot without outlines")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    # 图4.line
    dat_line <- data.frame(a=rep(1:20, ncolor),
                           b=rep(1:ncolor, each=20)+rnorm(20*ncolor, 0, 0.3), 
                           t=rep(sample(letters, ncolor), each=20))
    p_line <- ggplot(dat_line, aes(a, b, color=t, group=t))+
      geom_line(linewidth=1)+
      scale_color_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis", title = "Line chart without outlines")+
      theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    # 合并
    p <- grid.arrange(p_bar, p_box, p_point, p_line, padding=0, nrow = 2, ncol = 2)
    return(p)
  }            
  iscolors     <- function(str){
    # 函数，使用str_detect检查str是否是正确的颜色HEX码
    if(nchar(str_trim(str))<1) return(F)     #如果str是空，直接返回F
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
  
  ##### ui #####
  output$uipanel <- renderUI({
    tagList(
      tags$head(tags$link(rel = "shortcut icon", href = "pmgh.ico")),
      tags$style(HTML(".custom-margin {margin-bottom: 20px;}")),
      tags$div(class = "custom-margin",
               HTML("<h3 style='display: inline;'>方案选择</h3>"),
               HTML(paste("<h5 style='display: inline;'>(数据库内现有", length(colors_nasc), "种配色方案)</h5>"))),
      radioButtons(inputId  = "showtype",
                   label    = NULL,
                   choices  = list("配色数据库方案id" = "id", "自定义配色方案" = "custom"),
                   inline   = T,
                   selected = c("按数量" = "id")),
      # 按id
      conditionalPanel(condition = "input.showtype == 'id'",
                       selectInput(inputId = "num_select", 
                                   label = "选色颜色数量", 
                                   choices = colors_sect$col_num,
                                   selected = "all",
                                   multiple = FALSE),
                       div(style = "display: flex; align-items: center; width:700px",
                           sliderTextInput(inputId = "id_select", 
                                           label = "选择方案id", 
                                           choices = seq(colors_sect[[1, "min"]], colors_sect[[1, "max"]]),
                                           selected = colors_sect[[1, "min"]],
                                           width = "500px", 
                                           grid  = T),
                           div(style = "width: 15%; text-align: center; padding-left: 20px;", 
                               shiny::actionButton(inputId = "pre", 
                                                   label = "上一个",
                                                   icon  = icon("angle-left"))),
                           div(style = "width: 15%; text-align: center;", 
                               shiny::actionButton(inputId = "nex", 
                                                   label = "下一个",
                                                   icon  = icon("angle-right"))))),
      # 自定义
      conditionalPanel(condition = "input.showtype == 'custom'",
                       div(style = "display: flex; align-items: center; width:700px",
                           colourInput(inputId = "sele_col", 
                                       label = "追加颜色", 
                                       width = "174px",
                                       value = "skyblue"),
                           div(style = "width: 15%; text-align: center; padding-left: 20px;  padding-top: 10px;", 
                               shiny::actionButton(inputId = "add_col", 
                                                   label = "加入",
                                                   icon  = icon("plus"))),
                           div(style = "width: 15%; text-align: center; padding-left: 10px; padding-top: 10px;", 
                               shiny::actionButton(inputId = "reset_col", 
                                                   label = "重置",
                                                   icon  = icon("redo")))),
                       textInput(inputId = "col_custom",
                                 label = "自定义颜色（HEX码，多个颜色以逗号、顿号、空格、分号间隔，颜色数量不可超过16，结果将去重）：",
                                 width = "1000px",
                                 value = "#4DBBD5, #00A087, #E64B35")),
      # 选择结果
      h3("所选配色方案"),
      reactableOutput(outputId = "colors_info"),
      div(style = "display: flex; align-items: center; width:1001px",
          div(style = "width: 50%; text-align: center;", 
              h3("绘图效果"),
              plotOutput(outputId = "plot_example", width = "500px", height = "400px")),
          div(style = "width: 50%; text-align: center;", 
              h3("方案样式"),
              plotOutput(outputId = "plot_color", width = "500px", height = "400px"))),
      h3("配色数据库（点击表格显示绘图效果）"),
      reactableOutput(outputId = "colors_db")
    )
  })
  
  ##### server #####
  # 点击颜色数量，更新下面sliderTextInput中备选id的区间
  observeEvent(input$num_select, {
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    updateSliderTextInput(session, "id_select",
                          choices = selected_seq,
                          selected = selected_seq[1])
  })
  # 点击上一个或下一个后，更新slider对应的值
  observeEvent(input$pre, {
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    newValue <- max(selected_seq[1], as.numeric(input$id_select) - 1)
    updateSliderTextInput(session, "id_select", selected = newValue)
  })
  observeEvent(input$nex, {
    selected_row <- colors_sect[col_num == input$num_select,]
    selected_seq <- seq(selected_row$min, selected_row$max)
    newValue <- min(tail(selected_seq,1), as.numeric(input$id_select) + 1)
    updateSliderTextInput(session, "id_select", selected = newValue)
  })
  # 点击加入或重置颜色，更新textinput
  observeEvent(input$add_col, {
    newValue <- str_split(input$col_custom, "[,，;、 ]") %>%
      unlist() %>%
      str_trim() %>%
      .[nchar(.) > 0] %>%
      c(., input$sele_col) %>%
      .[!duplicated(.)] %>%
      paste0(collapse = ",")
    updateTextInput(session, "col_custom", value = newValue)
  })
  observeEvent(input$reset_col, {
    newValue <- "#4DBBD5, #00A087, #E64B35"
    updateTextInput(session, "col_custom", value = newValue)
  })
  
  # 动态数据
  rv <- reactiveValues()  #创建一个反应值对象rv，用于存储反应性的数据
  rv$value <- 1           #初始值为1
  observeEvent(c(input$id_select, input$showtype), {
    # 监听多个位置，如果有变动，则运行下面的内容，生成显示颜色方案的id
    if(input$showtype == "id"){
      # 如果此时页面为按id，则按选择的id更新id
      id <- input$id_select %>% as.numeric()
      rv$value <- id
    }
  })
  
  # table.1：所选配色方案
  output$colors_info  <- renderReactable({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype == "id"){
      # 如果非自定义，则按id显示方案信息
      id <- rv$value
      colo_inf <- data.frame(id=id, 
                             colors_n=length(colors_nasc[[id]]), 
                             colors_hex=paste0(colors_nasc[[id]], collapse = ", "),
                             colors_show=paste0(colors_nasc[[id]], collapse = ", "))
    } else{
      # 如果自定义
      colsig <- iscolors(custtext)
      if(isFALSE(colsig)){
        # 如果输入的字符不是颜色，则显示ERROR
        colo_inf <- data.frame(id=0, 
                               colors_n="ERROR", 
                               colors_hex="ERROR",
                               colors_show="FFFFFF")
      } else{
        # 如果未输入或输入的字符是颜色，则显示自定义的颜色信息
        colo_inf <- data.frame(id=0, 
                               colors_n=length(colsig), 
                               colors_hex=paste0(colsig, collapse = ", "),
                               colors_show=paste0(colsig, collapse = ", "))
      }
    }
    # 显示表格
    reactable(colo_inf,
              columns = list(id=colDef(name="方案id", width = 60, align = "center"),
                             colors_n=colDef(name="所含颜色数", width = 90, align = "center"),
                             colors_hex=colDef(name = "颜色HEX码", width = 650, align = "center"),
                             colors_show=colDef(name = "颜色预览", align = "center", cell = function(value) {
                               color_list <- strsplit(value, ", ")[[1]]
                               color_divs <- lapply(color_list, function(color) {
                                 tags$div(style = paste("width: 20px; height: 20px; background-color:", color, "; display: inline-block; margin-right: 5px; border: 1px solid black;"))
                               })
                               do.call(tagList, color_divs)
                             })
              ),
              sortable = F, 
              resizable = F,
              showPageSizeOptions = F,
              highlight = F,
              striped = T,
              bordered = T,
              compact = F,
              width = "1001px",
              fullWidth = F)
  })
  
  # fig.1：绘图效果
  output$plot_example <- renderPlot({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype == "id"){
      id <- rv$value
      pic <- examp_plot(id, colors_nasc)
    } else{
      colsig <- iscolors(custtext)
      if(isFALSE(colsig)){
        pic <- ggplot(data=NULL, aes(1,1))+
          geom_text(label="ERROR", hjust=0.5, vjust=0.5, size=8)+
          theme_void()
      } else{
        pic <- examp_plot(0, NULL, colsig)
      }
    }
    return(pic)
  }) 
  
  # fig.2：方案样式
  output$plot_color   <- renderPlot({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype == "id"){
      id <- rv$value
      pic <- show_col(colors_nasc[[id]])
    } else{
      colsig <- iscolors(custtext)
      if(isFALSE(colsig)){
        pic <- ggplot(data=NULL, aes(1,1))+
          geom_text(label="ERROR", hjust=0.5, vjust=0.5, size=8)+
          theme_void()
      } else{
        pic <- show_col(colsig)
      }
    }
    return(pic)
  })  
  
  # table.2：配色数据库详情
  output$colors_db    <- renderReactable({
    showtype <- input$showtype
    colnum <- colors_sect[col_num == input$num_select,] #根据所选颜色数量更新数据库表格
    id_min <- colnum$min
    id_max <- colnum$max
    colo_db <- data.table(id=colors_table$col_id,
                          colors_n=colors_table$col_num,
                          colors_hex=map_chr(colors_nasc, ~paste0(.x, collapse = ", ")),
                          colors_show=map_chr(colors_nasc, ~paste0(.x, collapse = ", "))) %>% 
      .[id>=id_min&id<=id_max,]
    reactable(colo_db,
              columns = list(id=colDef(name="方案id", width = 60, align = "center"),
                             colors_n=colDef(name="所含颜色数", width = 90, align = "center"),
                             colors_hex=colDef(name = "颜色HEX码", width = 650, align = "center"),
                             colors_show=colDef(name = "颜色预览", align = "center", cell = function(value) {
                               color_list <- strsplit(value, ", ")[[1]]
                               color_divs <- lapply(color_list, function(color) {
                                 tags$div(style = paste("width: 20px; height: 20px; background-color:", color, "; display: inline-block; margin-right: 5px; border: 1px solid black;"))
                               })
                               do.call(tagList, color_divs)
                             })
              ),
              language = reactableLang(
                searchPlaceholder = "查找..."),
              sortable = F,
              resizable = F,
              showPageSizeOptions = T,
              searchable = T,
              highlight = T,
              striped = T,
              bordered = T,
              compact = F,
              width = "1001px",
              fullWidth = F,
              theme = reactableTheme(searchInputStyle = list("margin-top" = "7px", "margin-right" = "7px")),
              onClick = if(showtype != "custom"){
                JS("function(rowInfo, colInfo) {
                if (window.Shiny) {
                  Shiny.setInputValue('selected_row', rowInfo.index + 1);
                  window.scrollTo({
                  top: 0, 
                  left: 0, 
                  behavior:'smooth'});  // 平滑滚动到页面顶部
                }
                }")}
    )
  })
  
  # 点击表格更新绘图效果
  observeEvent(input$selected_row, {
    selected_row <- input$selected_row
    if (!is.null(selected_row)) {
      colnum <- colors_sect[col_num == input$num_select,]      #所选颜色数量对应的id区间
      selected_id <- seq(colnum$min, colnum$max)[selected_row] #提取id区间，点击的行号，对应的准确id
      updateSliderTextInput(session, "id_select", selected = selected_id) #更新selected
    }
  })
  
}

# 输出app
shinyApp(ui, server)