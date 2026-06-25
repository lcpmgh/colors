# 学术期刊配色推荐器 v4.0 2026.06.25

##### packages ##########################################################################
library(shiny)
library(ggplot2)
library(stringr)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(colourpicker)
library(DT)

##### settings ##########################################################################
# 数据和函数
pal.dat   <- fread("@palettes.csv")
# 函数
treat_colors <- function(str, direct="split"){
  # 处理颜色字符串，合并"merge"，或者分离"split"
  # 原子函数，只支持最小单位的一个字符串（不支持向量和list）(一个字符串中有多个颜色用分隔符区分)
  # 变量规则：文件里保存了多个palette，一个palette是很多颜色，它们用字符串或向量保存时叫colors，单个颜色叫color，分hex和rgb
  if(direct=="split"){
    res <- str_remove_all(str, "[,，;；、 ]") %>% str_trim() %>% str_split_1("(?=#)") %>% .[str_length(.)>1]
  } else{
    res <- paste0(str, collapse=", ")
  }
  return(res)
}
is_colors    <- function(str){
  # 函数，使用str_detect检查str是否是正确的颜色HEX码，如果是颜色HEX则返回颜色向量
  # 原子函数，只支持最小单位的一个字符串（不支持向量和list）(一个字符串中有多个颜色用分隔符区分)
  if(is.na(str))  return(F)                         #如果str是NA，直接返回F              
  if(str_length(str_trim(str))<1) return(F)         #如果str是空，直接返回F
  colors <- treat_colors(str) %>% .[!duplicated(.)] #颜色去重复
  sig    <- str_detect(colors, "^#[A-Fa-f0-9]{6}([A-Fa-f0-9]{2})?$")
  if(any(!sig)){
    # 如不是颜色，返回F
    return(F)
  } else{
    # 如是颜色，返回颜色HEX
    return(colors)
  }
}
hex2rgb      <- function(c_hex){
  # 将颜色的HEX转为RGB，如果透明度为0，则不显示
  # 向量函数，要求每个元素是HEX格式的单个颜色
  is_alpha <- str_length(c_hex) %>% is_greater_than(7) %>% any()
  c_rgb <- col2rgb(c_hex, alpha = is_alpha) %>% apply(2, function(x) paste0(x, collapse = "/"))
  return(c_rgb)
}
add_color_tp <- function(c_hex, tpp){
  # 为颜色追加透明度，tpp是透明度百分比(0-100)，透明度为0时不改变颜色值，对已有透明度的不改变颜色值
  # 向量函数，要求每个元素是HEX格式的单个颜色
  if(tpp == 0){
    colors <- c_hex
  } else{
    ctp <- ((100-tpp)*255/100) %>% round() %>% sprintf("%02X", .)
    no_tp_id <- str_length(c_hex)==7
    c_hex[no_tp_id] <- paste0(c_hex[no_tp_id], ctp)
    colors <- c_hex
  }
  return(colors)
}
make_color_blocks <- function(colors_inte) {
  # 一个函数，转换color_show为html色块
  # 原子函数，只支持最小单位的一个字符串（不支持向量和list）(一个字符串中有多个颜色用分隔符区分)
  colors <- treat_colors(colors_inte)
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
make_color_dt     <- function(pid, custom_color=NA, tpp=0){
  # 利用颜色id或自定义颜色，生成展示表格所需的数据
  # 注意，当id=="all"时，代表所有颜色，当id==0时，代表自定颜色，此时必须要有customcolor
  if(pid==0){
    colors <- is_colors(custom_color)
    if(isFALSE(colors)){
      color_dt <- data.table(
        item = c("Id", "信息", "HEX", "RGB", "预览"),
        valu = c(0, "错误", "错误", "错误", "错误")
      )
      return(color_dt)
    } else{
      tinfo <- sprintf("来源: 自定义, 名称: 无, 数量: %s", length(colors))
      colors_inte_hex <- add_color_tp(colors, tpp) %>% treat_colors(direct="merge")
      colors_inte_rgb <- hex2rgb(colors) %>% paste0(collapse=", ")
      color_dt <- data.table(
        item = c("Id", "信息", "HEX", "RGB", "预览"),
        valu = c(0, tinfo, colors_inte_hex, colors_inte_rgb, make_color_blocks(colors_inte_hex))
      )
    }
  } else if(pid=="all"){
    color_dt <- copy(pal.dat) %>% .[,preview:=lapply(colors, make_color_blocks)]
  } else{
    td <- pal.dat[id==pid,] 
    tinfo <- sprintf("来源: %s, 名称: %s, 数量: %s", td$p_sour, td$p_name, td$c_numb)
    colors_inte_hex <- pal.dat[id==pid, colors] %>% treat_colors() %>% add_color_tp(tpp) %>% treat_colors("merge")
    colors_inte_rgb <- treat_colors(colors_inte_hex) %>% hex2rgb() %>% treat_colors("merge")
    color_dt <- data.table(
      item = c("Id", "信息", "HEX", "RGB", "预览"),
      valu = c(td$id, tinfo, colors_inte_hex, colors_inte_rgb, make_color_blocks(colors_inte_hex))
    )
  }
  return(color_dt)
}
show_pal          <- function(cdt, numb_t, numb, type){
  # 函数，根据选定颜色结果，绘制特定类型的图bar/box/point/line/pattern
  # 其中numb是input$color_numb选定的颜色数量，numb_t是input$show_type选择的展示方式
  c_hex <- cdt[item=="HEX", valu] %>% is_colors()
  if(isFALSE(c_hex)){
    plot <- ggplot()+
      annotate(geom="text",x=1,y=1,label="ERROR！",size=10)+
      theme_void()+
      theme(panel.border = element_rect(color="black", linewidth = 1))
    return(plot)
  }
  tcolor <- c_hex
  ncolor <- length(tcolor)
  # 只有当numb_t是selected并且numb不是all时，按选定数量画前numb个，否则全画，最多前16个
  if(numb_t=="selected"){
    if(numb!="all"){
      ncolor <- as.numeric(numb)
      tcolor <- tcolor[1:ncolor]
    }
  }
  # 无论如何，画图颜色不超过16
  if(ncolor>16){
    # 只画前16个颜色
    tcolor <- tcolor[1:16]
    ncolor <- 16
  }
  # 绘图部分
  if(type == "bar"){
    dat_bar <- data.frame(x=letters[1:ncolor], y=runif(ncolor, 7, 10))
    plot <- ggplot(dat_bar, aes(x, y, fill=x))+
      geom_bar(color="black", stat = "identity")+
      scale_y_continuous(expand = c(0,0,0.05,0))+
      scale_fill_manual(values = tcolor)+
      labs(fill="fill", title = "Bar Chart with outlines")
  } else if(type == "box"){
    dat_box <- data.frame(x=letters[1:ncolor], y=runif(ncolor*10, 7, 10))
    plot <- ggplot(dat_box, aes(x, y, fill=x))+
      stat_boxplot(geom = "errorbar", linewidth=0.8, width = 0.3)+
      geom_boxplot()+
      scale_fill_manual(values = tcolor)+
      labs(fill="fill", title = "Boxplot with outlines")
  } else if(type == "point"){
    dat_point <- data.frame(
      x=runif(30*ncolor, 0, 1),
      y=unlist(lapply(seq(0,1,length.out=ncolor), function(i) rnorm(30, mean=i, sd=1/ncolor*0.5))),
      c=rep(letters[1:ncolor], each=30)
    )
    plot <- ggplot(dat_point, aes(x, y, color=c))+
      geom_point(shape=16, size=4)+
      scale_color_manual(values = tcolor)+
      labs(color="color", title = "scatterplot without outlines")
  } else if(type == "line"){
    dat_line <- data.frame(
      x=rep(1:20, ncolor),
      y=rep(1:ncolor, each=20)+rnorm(20*ncolor, 0, 0.3),
      c=rep(sample(letters, ncolor), each=20)
    )
    plot <- ggplot(dat_line, aes(x, y, color=c, group=c))+
      geom_line(linewidth=1)+
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
  # browser()  
  return(plot)
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
      table.dataTable tbody tr.selected td,
      table.dataTable td.selected {
        box-shadow: inset 0 0 0 9999px #e3f0ff !important;
        color: #1a1a1a !important;
        text-shadow: 0 0 1px rgba(0,0,0,0.08) !important;
        background-color: transparent !important;     /* 避免 background 干扰 */
      }        /* 选中状态：柔和淡蓝 + 深色文字 */
      table.dataTable tbody tr.selected:hover td, table.dataTable tbody tr:hover td.selected,
      table.dataTable.hover tbody tr.selected:hover td, table.dataTable.display tbody tr.selected:hover td {
        box-shadow: inset 0 0 0 9999px #e3f0ff !important;
        color: #1a1a1a !important;
        background-color: transparent !important;
      }        /* 当选中且 hover 时，强制使用相同颜色，覆盖内置 hover 效果 */
    "))
  ),
  # 网页主体 ...................................................................
  div(
    style = "max-width: 1080px; margin: 0 auto; padding: 0 10px;",
    h3("科研绘图配色推荐器"),
    # 调色板参数设置部分~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 参数栏，左右两栏布局
    div(
      style = "display: flex; align-items:  stretch; gap: 20px;",
      # 左栏参数~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      div(
        style = "flex: 1; background: #FFFFFF; border-radius: 6px; padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08)", 
        # 左-配色方式
        h4(style="text-align:center;", "调色板-L"),
        radioGroupButtons(
          inputId = "pal_type_L",
          label = NULL,
          choices = c("数据库方案" = "dataset", "自定义方案" = "custom"),
          selected = "dataset",
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
        ),
        # 左-如果按数据库方案
        conditionalPanel(
          condition = "input.pal_type_L == 'dataset'",
          div(
            style = "display: flex; align-items: center; gap:10px;",
            sliderTextInput(
              inputId = "color_numb_L",
              label = "选择颜色数量",
              choices = c(2:16, "all"),
              selected = "all",
              width = "100%",
              grid  = F
            ),
            div(
              style = "display: flex; align-items: center; gap:10px;",
              shiny::actionButton(
                inputId = "color_numb_pre_L",
                label = NULL,
                icon  = icon("angle-left")
              ),
              shiny::actionButton(
                inputId = "color_numb_nex_L",
                label = NULL,
                icon  = icon("angle-right")
              )
            )
          ), 
          div(
            style = "display: flex; align-items: center; gap:10px;",
            sliderTextInput(
              inputId = "palette_id_L",
              label = "选择方案id",
              choices = sort(pal.dat$id),
              selected = pal.dat$id[1],
              width = "100%",
              grid  = F
            ),
            div(
              style = "display: flex; align-items: center; gap:10px;",
              shiny::actionButton(
                inputId = "id_pre_L",
                label = NULL,
                icon  = icon("angle-left")
              ),
              shiny::actionButton(
                inputId = "id_nex_L",
                label = NULL,
                icon  = icon("angle-right")
              )
            )
          )
        ),
        # 左-如果按自定义方案
        conditionalPanel(
          condition = "input.pal_type_L == 'custom'",
          div(
            style = "display: flex; align-items: center;",
            colourInput(
              inputId = "choose_color_L",
              label = "追加颜色",
              allowTransparent = T,
              value = "skyblue",
              width = "110px"
            ),
            div(
              style = "display: flex; text-align: center; gap: 10px; margin-top: 10px; margin-left: 20px",
              shiny::actionButton(
                inputId = "add_color_L",
                label = NULL,
                icon  = icon("plus")
              )
            )
          ),
          div(
            style = "display: flex; align-items: center;",
            textInput(
              inputId = "color_custom_L",
              label = "自定义颜色（HEX码，多个颜色以逗号、顿号、空格、或分号间隔，颜色数量不可超过16，结果将去重）：",
              value = "#4DBBD5, #00A087, #E64B35",
              width = "100%"
            ),
            div(
              style = "display: flex; margin-top: 30px; margin-left: 20px",
              shiny::actionButton(
                inputId = "reset_color_L",
                label = NULL,
                icon  = icon("redo")
              )
            )
          )
        ),
        # 左-透明度选项
        div(
          style = "display: flex; align-items: center; gap:10px;",
          sliderTextInput(
            inputId = "color_tpp_L",
            label = "追加透明度",
            choices = seq(0, 100, by = 1),
            selected = 0,
            grid = F,
            post = "%",
            width = "100%"
          ),
          div(
            style = "display: flex; align-items: center; gap:10px;",
            shiny::actionButton(
              inputId = "color_tpp_pre_L",
              label = NULL,
              icon  = icon("angle-left")
            ),
            shiny::actionButton(
              inputId = "color_tpp_nex_L",
              label = NULL,
              icon  = icon("angle-right")
            )
          )
        ),
        # 左-选择结果
        h4("指定的配色方案"),
        div(
          style = "width: 500px; margin: 0; overflow-x: left;",
          DTOutput(outputId = "colors_info_L")
        )
      ),  # 左栏div结束
      # 右栏参数~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      div(
        style = "flex: 1; background: #FFFFFF; border-radius: 6px; padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08)",
        h4(style="text-align:center;", "调色板-R"),
        radioGroupButtons(
          inputId = "pal_type_R",
          label = NULL,
          choices = c("数据库方案" = "dataset", "自定义方案" = "custom"),
          selected = "dataset",
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
        ),
        # 右-如果按数据库方案
        conditionalPanel(
          condition = "input.pal_type_R == 'dataset'",
          div(
            style = "display: flex; align-items: center; gap:10px;",
            sliderTextInput(
              inputId = "color_numb_R",
              label = "选择颜色数量",
              choices =  c(2:16, "all"),
              selected = "all",
              width = "100%",
              grid  = F
            ),
            div(
              style = "display: flex; align-items: center; gap:10px;",
              shiny::actionButton(
                inputId = "color_numb_pre_R",
                label = NULL,
                icon  = icon("angle-left")
              ),
              shiny::actionButton(
                inputId = "color_numb_nex_R",
                label = NULL,
                icon  = icon("angle-right")
              )
            )
          ), 
          div(
            style = "display: flex; align-items: center; gap:10px;",
            sliderTextInput(
              inputId = "palette_id_R",
              label = "选择方案id",
              choices = sort(pal.dat$id),
              selected = pal.dat$id[1],
              width = "100%",
              grid  = F
            ),
            div(
              style = "display: flex; align-items: center; gap:10px;",
              shiny::actionButton(
                inputId = "id_pre_R",
                label = NULL,
                icon  = icon("angle-left")
              ),
              shiny::actionButton(
                inputId = "id_nex_R",
                label = NULL,
                icon  = icon("angle-right")
              )
            )
          )
        ),
        # 右-如果按自定义方案
        conditionalPanel(
          condition = "input.pal_type_R == 'custom'",
          div(
            style = "display: flex; align-items: center;",
            colourInput(
              inputId = "choose_color_R",
              label = "追加颜色",
              allowTransparent = T,
              value = "skyblue",
              width = "110px"
            ),
            div(
              style = "display: flex; text-align: center; gap: 20px; margin-top: 10px; margin-left: 20px",
              shiny::actionButton(
                inputId = "add_color_R",
                label = NULL,
                icon  = icon("plus")
              )
            )
          ),
          div(
            style = "display: flex; align-items: center;",
            textInput(
              inputId = "color_custom_R",
              label = "自定义颜色（HEX码，多个颜色以逗号、顿号、空格、或分号间隔，颜色数量不可超过16，结果将去重）：",
              value = "#4DBBD5, #00A087, #E64B35",
              width = "100%"
            ),
            div(
              style = "display: flex; margin-top: 30px; margin-left: 20px",
              shiny::actionButton(
                inputId = "reset_color_R",
                label = NULL,
                icon  = icon("redo")
              )
            )
          )
        ),
        # 右-透明度选项
        div(
          style = "display: flex; align-items: center; gap:10px;",
          sliderTextInput(
            inputId = "color_tpp_R",
            label = "追加透明度",
            choices = seq(0, 100, by = 1),
            selected = 0,
            grid = F,
            post = "%",
            width = "100%"
          ),
          div(
            style = "display: flex; align-items: center; gap:10px;",
            shiny::actionButton(
              inputId = "color_tpp_pre_R",
              label = NULL,
              icon  = icon("angle-left")
            ),
            shiny::actionButton(
              inputId = "color_tpp_nex_R",
              label = NULL,
              icon  = icon("angle-right")
            )
          )
        ),
        # 右-选择结果
        h4("指定的配色方案"),
        div(
          style = "width: 500px; margin: 0; overflow-x: left;",
          DTOutput(outputId = "colors_info_R")
        )
      ) # 右栏div结束
    ),  # 分栏父辈div结束
    hr(),
    
    # 结果展示部分~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    h4("绘图效果展示"),
    conditionalPanel(
      condition = "input.pal_type_L == 'dataset' | input.pal_type_R == 'dataset'",
      div(
        style = "display: flex; align-items: flex-start; gap: 10px;",
        h5("对于数据库方案："),
        radioGroupButtons(
          inputId = "show_type",
          label = NULL,
          choices = c("按已选颜色数量（调色板的前n个颜色）" = "selected", "按调色板颜色总数（最多展示前16个颜色）" = "palette"),
          selected = "selected",
          direction = "horizontal", 
          checkIcon = list(
            yes = icon("circle-dot", style = "color: steelblue"),   
            no  = icon("circle", style = "color: steelblue")        
          )
        )
      )
    ),
    # 结果展示栏，左右两栏布局
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      # 左栏结果~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      div(
        style = "flex: 1; text-align: center; background: #FFFFFF; border-radius: 6px; padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);", 
        # 左-绘图结果
        h4(style="text-align:center;", "绘图效果-L"),
        div(
          style = "display: flex; gap: 1px; margin:1px 0;",
          plotOutput(outputId = "plot_bar_L", width = "250px", height = "200px"),
          plotOutput(outputId = "plot_box_L", width = "250px", height = "200px")
        ),
        div(
          style = "display: flex; gap: 1px; margin:1px 0;",
          plotOutput(outputId = "plot_point_L", width = "250px", height = "200px"),
          plotOutput(outputId = "plot_line_L",  width = "250px", height = "200px")
        ),
        plotOutput(outputId = "plot_pattern_L", width = "500px", height = "460px")
        
      ),
      # 右栏结果~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      div(
        style = "flex: 1; text-align: center; background: #FFFFFF; border-radius: 6px; padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
        # 右-绘图结果
        h4(style="text-align:center;", "绘图效果-R"),
        div(
          style = "display: flex; gap: 1px; margin:1px 0;",
          plotOutput(outputId = "plot_bar_R", width = "250px", height = "200px"),
          plotOutput(outputId = "plot_box_R", width = "250px", height = "200px")
        ),
        div(
          style = "display: flex; gap: 1px; margin:1px 0;",
          plotOutput(outputId = "plot_point_R", width = "250px", height = "200px"),
          plotOutput(outputId = "plot_line_R",  width = "250px", height = "200px")
        ),
        plotOutput(outputId = "plot_pattern_R", width = "500px", height = "460px")
      ),
    ), # 分栏父辈div结束
    hr(),
    # 配色数据库
    conditionalPanel(condition = "input.pal_type_L == 'dataset'",h3("配色数据库（点击表格更新调色板-L）")),
    conditionalPanel(condition = "input.pal_type_L == 'custom'",h3("配色数据库")),
    div(
      style = "width: 1080px; margin: 0; overflow-x: left;",
      DTOutput(outputId = "pals_dataset")
    )
  ),  # 网页主体div结束
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
  ) # 页脚div结束
) # UI函数结束  

##### server ############################################################################
server <- function(input, output, session){
  # 点击上一个或下一个后，更新slider对应的值~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 左-参数设置 
  # 左-颜色数量
  observe({
    val_range <- c(2:16, "all")
    old_val   <- input$color_numb_L
    old_rank  <- match(old_val, val_range)
    new_rank  <- max(1, old_rank-1)
    new_val   <- val_range[new_rank]
    updateSliderTextInput(session, inputId="color_numb_L", choices=val_range, selected=new_val) #必须要有choices，否则失效
  }) %>% bindEvent(input$color_numb_pre_L)
  observe({
    val_range <- c(2:16, "all")
    old_val   <- input$color_numb_L
    old_rank  <- match(old_val, val_range)
    new_rank  <- min(16, old_rank+1)
    new_val   <- val_range[new_rank]
    updateSliderTextInput(session, inputId="color_numb_L", choices=val_range, selected=new_val)
  }) %>% bindEvent(input$color_numb_nex_L)
  # 左-方案id
  observe({
    color_numb <- input$color_numb_L
    if(color_numb=="all"){
      idset <- sort(pal.dat$id)
    } else{
      idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    id_numb     <- length(idset)
    old_id      <- input$palette_id_L
    old_id_rank <- match(old_id, idset)
    new_id_rank <- max(1, old_id_rank-1)
    new_id      <- idset[new_id_rank]
    updateSliderTextInput(session, inputId="palette_id_L", choices=idset, selected = new_id)
  }) %>% bindEvent(input$id_pre_L)
  observe({
    color_numb <- input$color_numb_L
    if(color_numb=="all"){
      idset <- sort(pal.dat$id)
    } else{
      idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    id_numb     <- length(idset)
    old_id      <- input$palette_id_L
    old_id_rank <- match(old_id, idset)
    new_id_rank <- min(id_numb, old_id_rank+1)
    new_id      <- idset[new_id_rank]
    updateSliderTextInput(session, inputId="palette_id_L", choices=idset, selected = new_id)
  }) %>% bindEvent(input$id_nex_L)
  # 左-透明度
  observe({
    old_ttp <- input$color_tpp_L %>% as.numeric()
    new_ttp <- max(0, old_ttp-1)
    updateSliderTextInput(session, inputId="color_tpp_L", choices=seq(0, 100, by=1), selected=new_ttp)
  }) %>% bindEvent(input$color_tpp_pre_L)
  observe({
    old_ttp <- input$color_tpp_L %>% as.numeric()
    new_ttp <- min(100, old_ttp+1)
    updateSliderTextInput(session, inputId="color_tpp_L", choices=seq(0,100,by=1), selected=new_ttp)
  }) %>% bindEvent(input$color_tpp_nex_L)
  # 左-根据颜色数量更新id可选范围
  observe({
    color_numb <- input$color_numb_L
    if(color_numb=="all"){
      new_idset <- sort(pal.dat$id)
    } else{
      new_idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    updateSliderTextInput(session, inputId="palette_id_L", choices=new_idset, selected=new_idset[1])
  }) %>% bindEvent(input$color_numb_L)
  # 右-参数设置
  # 右-颜色数量
  observe({
    val_range <- c(2:16, "all")
    old_val   <- input$color_numb_R
    old_rank  <- match(old_val, val_range)
    new_rank  <- max(1, old_rank-1)
    new_val   <- val_range[new_rank]
    updateSliderTextInput(session, inputId="color_numb_R", choices=val_range, selected=new_val) #必须要有choices，否则失效
  }) %>% bindEvent(input$color_numb_pre_R)
  observe({
    val_range <- c(2:16, "all")
    old_val   <- input$color_numb_R
    old_rank  <- match(old_val, val_range)
    new_rank  <- min(16, old_rank+1)
    new_val   <- val_range[new_rank]
    updateSliderTextInput(session, inputId="color_numb_R", choices=val_range, selected=new_val)
  }) %>% bindEvent(input$color_numb_nex_R)
  # 右-方案id
  observe({
    color_numb <- input$color_numb_R
    if(color_numb=="all"){
      idset <- sort(pal.dat$id)
    } else{
      idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    id_numb     <- length(idset)
    old_id      <- input$palette_id_R
    old_id_rank <- match(old_id, idset)
    new_id_rank <- max(1, old_id_rank-1)
    new_id      <- idset[new_id_rank]
    updateSliderTextInput(session, inputId="palette_id_R", choices=idset, selected = new_id)
  }) %>% bindEvent(input$id_pre_R)
  observe({
    color_numb <- input$color_numb_R
    if(color_numb=="all"){
      idset <- sort(pal.dat$id)
    } else{
      idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    id_numb     <- length(idset)
    old_id      <- input$palette_id_R
    old_id_rank <- match(old_id, idset)
    new_id_rank <- min(id_numb, old_id_rank+1)
    new_id      <- idset[new_id_rank]
    updateSliderTextInput(session, inputId="palette_id_R", choices=idset, selected = new_id)
  }) %>% bindEvent(input$id_nex_R)
  # 右-透明度
  observe({
    old_ttp <- input$color_tpp_R %>% as.numeric()
    new_ttp <- max(0, old_ttp-1)
    updateSliderTextInput(session, inputId="color_tpp_R", choices=seq(0, 100, by=1), selected=new_ttp)
  }) %>% bindEvent(input$color_tpp_pre_R)
  observe({
    old_ttp <- input$color_tpp_R %>% as.numeric()
    new_ttp <- min(100, old_ttp+1)
    updateSliderTextInput(session, inputId="color_tpp_R", choices=seq(0,100,by=1), selected=new_ttp)
  }) %>% bindEvent(input$color_tpp_nex_R)
  # 右-根据颜色数量更新id可选范围
  observe({
    color_numb <- input$color_numb_R
    if(color_numb=="all"){
      new_idset <- sort(pal.dat$id)
    } else{
      new_idset <- pal.dat[c_numb>=as.numeric(color_numb), id] %>% sort()
    }
    updateSliderTextInput(session, inputId="palette_id_R", choices=new_idset, selected=new_idset[1])
  }) %>% bindEvent(input$color_numb_R)
  
  # 点击添加或重置颜色后，更新textinput、透明度对应的值~~~~~~~~~~~~~~~~~~~~~~~~~
  # 左-点击添加或重置颜色，更新textinput、SliderTextInput
  observe({
    old_colors <- input$color_custom_L %>% treat_colors()
    add_colors <- input$choose_color_L
    new_colors <- c(old_colors, add_colors) %>% .[!duplicated(.)] %>% treat_colors(direct="merge")
    updateTextInput(session, inputId="color_custom_L", value=new_colors)
  }) %>% bindEvent(input$add_color_L)
  observe({
    new_colors <- "#4DBBD5, #00A087, #E64B35"
    updateTextInput(session, inputId="color_custom_L", value = new_colors)
    updateSliderTextInput(session, inputId="color_tpp_L", choices=seq(0,100,by=1), selected=0)
  }) %>% bindEvent(input$reset_color_L)
  # 右-点击添加或重置颜色，更新textinput、SliderTextInput
  observe({
    old_colors <- input$color_custom_R %>% treat_colors()
    add_colors <- input$choose_color_R
    new_colors <- c(old_colors, add_colors) %>% .[!duplicated(.)] %>% treat_colors(direct="merge")
    updateTextInput(session, inputId="color_custom_R", value=new_colors)
  }) %>% bindEvent(input$add_color_R)
  observe({
    new_colors <- "#4DBBD5, #00A087, #E64B35"
    updateTextInput(session, inputId="color_custom_R", value = new_colors)
    updateSliderTextInput(session, inputId="color_tpp_R", choices=seq(0,100,by=1), selected=0)
  }) %>% bindEvent(input$reset_color_R)
  
  # 动态数据~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  palette_id_rv <- reactiveValues(L=1, R=1)
  color_cu_rv   <- reactiveValues(L=NULL, R=NULL)
  color_dt_rv   <- reactiveValues(L=NULL, R=NULL)
  observe({
    # 左栏动态数据赋值
    # 获取实时id数据
    palette_id_rv$L <- input$palette_id_L
    # 获取实时color_custom
    color_cu_rv$L <- input$color_custom_L
    # 获取实时col_dat
    pid_L   <- palette_id_rv$L
    ccu_L   <- color_cu_rv$L
    ctp_L   <- input$color_tpp_L
    if(input$pal_type_L=="dataset"){
      color_dt_rv$L <- make_color_dt(pid_L, "NA", ctp_L)
    } else{
      color_dt_rv$L <- make_color_dt(0, ccu_L, ctp_L)
    }
  })
  observe({
    # 右栏动态数据赋值
    # 获取实时id数据
    palette_id_rv$R <- input$palette_id_R
    # 获取实时color_custom
    color_cu_rv$R <- input$color_custom_R
    # 获取实时col_dat
    pid_R   <- palette_id_rv$R
    ccu_R   <- color_cu_rv$R
    ctp_R   <- input$color_tpp_R
    if(input$pal_type_R=="dataset"){
      color_dt_rv$R <- make_color_dt(pid_R, "NA", ctp_R)
    } else{
      color_dt_rv$R <- make_color_dt(0, ccu_R, ctp_R)
    }
  })
  
  # 配色方案信息~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$colors_info_L  <- renderDT({
    color_dt <- color_dt_rv$L
    DT::datatable(
      color_dt,
      escape = FALSE,
      colnames = c("项目", "内容"),
      rownames = FALSE,
      selection = "none",
      options = list(
        columnDefs = list(
          list(width = '40px',  targets = 0, className = 'dt-left'),
          list(targets = 1, className = 'dt-left')
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
  output$colors_info_R  <- renderDT({
    color_dt <- color_dt_rv$R
    DT::datatable(
      color_dt,
      escape = FALSE,
      colnames = c("项目", "内容"),
      rownames = FALSE,
      selection = "none",
      options = list(
        columnDefs = list(
          list(width = '40px',  targets = 0, className = 'dt-left'),
          list(targets = 1, className = 'dt-left')
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
  
  # 画图加了bindEvent，原因有二：
  # 1.链式响应顺序是color_numb_L传到palette_id_L，然而这两个又都得传到renderPlot，导致color_numb_L改变、传到palette_id_L之前画图就刷新了，
  #   传过去后画图又刷新了，一瞬间两次，而且由于数量更新快，导致第一次画图会有warning，现在只依赖最后的配色方案数据，它变了才画图
  # 2.指定L或R后，左右两栏不会同步刷新，注意reactiveValue的赋值部分，也要左右分开，否则会影响另一边
  # 左-绘图效果~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot_bar_L     <- renderPlot({
    color_dt <- color_dt_rv$L
    numb_t   <- ifelse(input$pal_type_L=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_L
    show_pal(color_dt, numb_t, numb, "bar")
  }) %>% bindEvent(color_dt_rv$L, input$show_type)
  output$plot_box_L     <- renderPlot({
    color_dt <- color_dt_rv$L
    numb_t   <- ifelse(input$pal_type_L=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_L
    show_pal(color_dt, numb_t, numb, "box")
  }) %>% bindEvent(color_dt_rv$L, input$show_type)
  output$plot_point_L   <- renderPlot({
    color_dt <- color_dt_rv$L
    numb_t   <- ifelse(input$pal_type_L=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_L
    show_pal(color_dt, numb_t, numb, "point")
  }) %>% bindEvent(color_dt_rv$L, input$show_type)
  output$plot_line_L    <- renderPlot({
    color_dt <- color_dt_rv$L
    numb_t   <- ifelse(input$pal_type_L=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_L
    show_pal(color_dt, numb_t, numb, "line")
  }) %>% bindEvent(color_dt_rv$L, input$show_type)
  output$plot_pattern_L <- renderPlot({
    color_dt <- color_dt_rv$L
    numb_t   <- ifelse(input$pal_type_L=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_L
    show_pal(color_dt, numb_t, numb, "pattern")
  }) %>% bindEvent(color_dt_rv$L, input$show_type)
  
  # 右-绘图效果~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot_bar_R     <- renderPlot({
    color_dt <- color_dt_rv$R
    numb_t   <- ifelse(input$pal_type_R=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_R
    show_pal(color_dt, numb_t, numb, "bar")
  }) %>% bindEvent(color_dt_rv$R, input$show_type)
  output$plot_box_R     <- renderPlot({
    color_dt <- color_dt_rv$R
    numb_t   <- ifelse(input$pal_type_R=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_R
    show_pal(color_dt, numb_t, numb, "box")    
  }) %>% bindEvent(color_dt_rv$R, input$show_type)
  output$plot_point_R   <- renderPlot({
    color_dt <- color_dt_rv$R
    numb_t   <- ifelse(input$pal_type_R=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_R
    show_pal(color_dt, numb_t, numb, "point")
  }) %>% bindEvent(color_dt_rv$R, input$show_type)
  output$plot_line_R    <- renderPlot({
    color_dt <- color_dt_rv$R
    numb_t   <- ifelse(input$pal_type_R=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_R
    show_pal(color_dt, numb_t, numb, "line")
  }) %>% bindEvent(color_dt_rv$R, input$show_type)
  output$plot_pattern_R <- renderPlot({
    color_dt <- color_dt_rv$R
    numb_t   <- ifelse(input$pal_type_R=="dataset", input$show_type, "palette") #调色板为自定义时，没有numb_t的选选项，都用所有颜色画图
    numb     <- input$color_numb_R
    show_pal(color_dt, numb_t, numb, "pattern")
  }) %>% bindEvent(color_dt_rv$R, input$show_type)
  
  # 颜色数据库~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$pals_dataset   <- renderDT({
    color_numb <- input$color_numb_L
    if(color_numb=="all"){
      color_dt <-  make_color_dt("all", NA, 0) %>% .[,p_sour:=as.factor(p_sour)] 
    } else{
      color_dt <-  make_color_dt("all", NA, 0) %>% .[,p_sour:=as.factor(p_sour)] %>% .[c_numb>=as.numeric(color_numb),]
    }
    DT::datatable(
      color_dt,
      escape = FALSE,
      colnames = c("id", "来源", "名称", "数量", "HEX", "预览"),
      rownames = FALSE,
      # width = 1080,
      selection = "single",
      filter = "top",
      options = list(
        columnDefs = list(
          list(width = '30px',  targets = 0, className = 'dt-center', orderable = T, searchable = F),
          list(width = '80px',  targets = 1, className = 'dt-center', orderable = T, searchable = T),
          list(width = '40px',  targets = 2, className = 'dt-center', orderable = T, searchable = F),
          list(width = '35px',  targets = 3, className = 'dt-center', orderable = T, searchable = T),
          list(width = '320px', targets = 4, className = 'dt-left',   orderable = F, searchable = F),
          list(                 targets = 5, className = 'dt-left',   orderable = F, searchable = F)
          
        ),
        pageLength = 25,
        autoWidth = FALSE,
        scrollX = FALSE,
        scrollY = FALSE,
        info = FALSE, 
        rowCallback = JS("
        function(row, data, index) {
          $(row).off('click.dt').on('click.dt', function() {
            // 只在 pal_type_L == 'dataset' 时生效
            if (Shiny && Shiny.shinyapp &&
                Shiny.shinyapp.$inputValues['pal_type_L'] === 'dataset') {
              window.scrollTo({
                top: 390,
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
  
  # 表格点击行为~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 当pal_type_L == "dataset"时，点击配色数据库表格，更新palette_id_L的值
  observe({
    # 先根据数量获取dt和id可选范围
    req(input$pal_type_L == "dataset")  #添加执行限制，如果不满足则后面代码不会运行
    color_numb <- input$color_numb_L
    if(color_numb=="all"){
      color_dt <-  make_color_dt("all", NA, 0)  
    } else{
      color_dt <-  make_color_dt("all", NA, 0) %>% .[c_numb>=as.numeric(color_numb),]
    }
    idset <- sort(color_dt$id)
    selected_row <- input$pals_dataset_rows_selected
    new_id <- color_dt[selected_row, id]
    updateSliderTextInput(session, inputId="palette_id_L", choices=idset, selected = new_id)
  }) %>% bindEvent(input$pals_dataset_rows_selected)
}

##### app #################################################################################
shinyApp(ui, server)
