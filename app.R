library(shiny)
library(reactable)
library(shinydashboard)
library(scales)
library(gridExtra)
library(ggplot2)
library(magrittr)
library(stringr)
library(purrr)
ui <- dashboardPage(
  dashboardHeader(title = "科研绘图配色推荐器",titleWidth = "100%"),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    uiOutput("uipanel"),
    tags$hr(),
    tags$div(align = "center", 
             tags$p("\ua9 2021-2024, LIANG Chen, Institute of Mountain Hazards and Environment, CAS. All rights reserved.", style="height:8px"),
             actionLink(inputId = "", label = "Github", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh')"),
             tags$p("  ", style = "display:inline;white-space:pre"),
             tags$p("Email: lcpmgh@gmail.com", style="display:inline;white-space:pre"),  
             tags$div(align = "center",
                      tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                      tags$p("  ", style = "display:inline;white-space:pre"),
                      tags$img(src="gaba.png"),
                      tags$a("川公网安备51010702002736", target="_blank", href="http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", style="color:#06c; display:inline;")
             )
    )
  )
)

server <- function(input, output, session){
  # 数据和函数
  colors <- readLines("@colors.txt") %>% str_split(",")             #读取并识别颜色
  colors_nasc  <- map_int(colors, length) %>% order() %>% colors[.] #按子颜色数量排序
  colors_table <- data.frame(col_id=1:length(colors_nasc), col_num=map_int(colors_nasc, length)) #颜色序号数量表
  examp_plot   <- function(id, colors_nasc=NULL, custom=c("#F5A889", "#ACD6EC")){
    # id=0时自定义颜色，否则按colors_nasc中的颜色
    if(id == 0){
      tcolor <- custom
      ncolor <- length(tcolor)
    } else{
      tcolor <- colors_nasc[[id]]
      ncolor <- length(tcolor)
    }
    # 1.bar
    dat_bar <- data.frame(a=sample(letters, ncolor, replace = F), 
                          b=runif(ncolor, 7, 10))
    p_bar <- ggplot(dat_bar, aes(a, b, fill=a))+
      geom_bar(color="black", stat = "identity")+
      scale_y_continuous(expand = c(0,0,0,0.1))+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis")+
      theme_bw()
    # 2.point
    dat_point <- data.frame(a=rep(runif(30), ncolor),
                            b=rep(runif(30), ncolor),
                            t=rep(sample(letters, ncolor, replace = F), 30))
    p_point <- ggplot(dat_point, aes(a, b, color=t, fill=t))+
      geom_point(shape=21, size=5)+
      scale_color_manual(values = tcolor)+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis")+
      theme_bw()
    # 3.box
    dat_box <- data.frame(a=sample(letters, ncolor, replace = F),
                          b=runif(ncolor*20, 7, 10))
    p_box <- ggplot(dat_box, aes(a, b, fill=a))+
      stat_boxplot(geom = "errorbar", linewidth=0.8, width = 0.3)+
      geom_boxplot()+
      scale_fill_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis")+
      theme_bw()
    # 5.line
    dat_line <- data.frame(a=rep(1:20, ncolor),
                           b=rep(1:ncolor, each=20)+rnorm(20*ncolor, 0, 0.3), 
                           t=rep(sample(letters, ncolor), each=20))
    p_line <- ggplot(dat_line, aes(a, b, color=t, group=t))+
      geom_line(linewidth=1)+
      scale_color_manual(values = tcolor)+
      labs(x="x-axis", y="y-axis")+
      theme_bw()
    # 合并
    p <- grid.arrange(p_bar, p_line, p_point, p_box, padding=0, nrow = 2, ncol = 2)
    return(p)
  }            #函数，画4个案例图
  iscolors     <- function(str){
    # 使用 str_detect 函数检查 str 是否是正确的颜色HEX码
    colo <- str_split(str, "[,，;、 ]") %>% unlist() %>% str_trim() %>% .[nchar(.) > 0] %>% .[!duplicated(.)]
    sig  <- str_detect(colo, "^#[A-Fa-f0-9]{6}$")
    if(any(!sig) | length(colo)>16){
      return(F)
    } else{
      return(colo)
    }
  }                                                             #函数，判断输入是否为HEX颜色
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
                   choices  = list("按方案颜色数量（随机显示配色方案）" = "num", "按配色方案id" = "id", "自定义配色方案" = "custom"),
                   inline   = T,
                   selected = c("按数量" = "num")),
      # 按数量
      conditionalPanel(condition = "input.showtype == 'num'",
                       div(style = "display: flex; align-items: center; width:800px",
                           div(style = "padding-top: 0px;", 
                               selectInput(inputId = "num_select", 
                                           label = "颜色数量：", 
                                           choices = unique(colors_table$col_num),
                                           selected = 2,
                                           multiple = FALSE)), 
                           div(style = "width: 15%; text-align: center; padding-top: 5px;",
                               shiny::actionButton(inputId = "plot",
                                                   label   = "换一个",
                                                   icon    = icon("play"))))),
      # 按id
      conditionalPanel(condition = "input.showtype == 'id'",
                       div(style = "display: flex; align-items: center; width:700px",
                           sliderInput(inputId = "id_select", 
                                       label = "配色方案id：", 
                                       min = 1, 
                                       max = length(colors_nasc), 
                                       width = "500px",
                                       step = 1, 
                                       value = 1),
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
                       textInput(inputId = "col_custom",
                                 label = "自定义颜色（HEX码，多个颜色以逗号、顿号、空格、分号间隔，颜色数量不可超过16，结果将去重）：",
                                 width = "1000px",
                                 value = "#4DBBD5, #00A087, #E64B35")),
      # 选择结果
      h3("所选配色方案："),
      reactableOutput(outputId = "colors_info"),
      div(style = "display: flex; align-items: center; width:1001px",
          div(style = "width: 50%; text-align: center;", 
              h3("绘图效果"),
              plotOutput(outputId = "plot_example", width = "500px", height = "400px")),
          div(style = "width: 50%; text-align: center;", 
              h3("方案样式"),
              plotOutput(outputId = "plot_color", width = "500px", height = "400px"))),
      h3("配色数据库详情："),
      reactableOutput(outputId = "colors_db")
    )
  })
  ##### server #####
  # 更新slider
  observeEvent(input$pre, {
    newValue <- max(1, input$id_select - 1)
    updateSliderInput(session, "id_select", value = newValue)
  })
  observeEvent(input$nex, {
    newValue <- min(length(colors_nasc), input$id_select + 1)
    updateSliderInput(session, "id_select", value = newValue)
  })
  # 动态数据
  rv <- reactiveValues()
  rv$value <- 1
  observeEvent(c(input$id_select, input$plot, input$num_select, input$showtype), {
    # reactive({
    if(input$showtype == "id"){
      id <- input$id_select %>% as.numeric()
    } else{
      num <- input$num_select %>% as.numeric()
      id  <- sample(colors_table[colors_table$col_num==num,]$col_id, 1)
    }
    rv$value <- id
  })  
  
  # table.1
  output$colors_info  <- renderReactable({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype != "custom"){
      id <- rv$value
      colo_inf <- data.frame(id=id, colors_n=length(colors_nasc[[id]]), colors=paste0(colors_nasc[[id]], collapse = ", "))
    } else{
      colsig <- iscolors(custtext)
      if(isFALSE(colsig)){
        colo_inf <- data.frame(id=0, colors_n="ERROR", colors="ERROR")
      } else{
        colo_inf <- data.frame(id=0, colors_n=length(colsig), colors=paste0(colsig, collapse = ", "))
      }
    }
    reactable(colo_inf,
              columns = list(id=colDef(name="方案id", width = 60, align = "center"),
                             colors_n=colDef(name="所含颜色数", width = 90, align = "center"),
                             colors=colDef(name = "颜色HEX码", width = 650, align = "center")),
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
  # fig.1
  output$plot_example <- renderPlot({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype != "custom"){
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
  # fig.2
  output$plot_color <- renderPlot({
    showtype <- input$showtype
    custtext <- input$col_custom
    if(showtype != "custom"){
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
  # table.2
  output$colors_db <- renderReactable({
    colo_db <- data.frame(id=colors_table$col_id,
                          colors_n=colors_table$col_num,
                          colors_nex=map_chr(colors_nasc, ~paste0(.x, collapse = ", ")))
    reactable(colo_db,
              columns = list(id=colDef(name="方案id", width = 60, align = "center"),
                             colors_n=colDef(name="所含颜色数", width = 90, align = "center"),
                             colors_nex=colDef(name = "颜色HEX码", width = 650, align = "center")),
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
              theme = reactableTheme(searchInputStyle = list("margin-top" = "7px", "margin-right" = "7px")))
  })
}

# 输出app
shinyApp(ui, server)