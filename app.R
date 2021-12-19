



############################PACKAGES############################################
library(shinydashboard)
library(shiny)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggthemes)
library(dplyr)
# library(qrcode)
library(shinyWidgets)
library(shinyBS)
library(openxlsx)
library(shinycssloaders)
library(googlesheets4)
library(googledrive)
# library(viridis)
# library(DBI)
# library(RMySQL)

###########################INPUTS ##############################################
# Sys.setlocale(locale = "vietnamese")

dataLogin <-
  data.frame(
    usernames = c("quan", "thu", "doan", "toan"),
    passwords = c("123", "123", "123", "123"),
    name = c("ĐẬU VĂN QUÂN", "NGUYỄN THỊ THU", "ĐẬU VĂN ĐOÀN", "ĐẬU XUÂN TOÀN"),
    roles = c("quanAdmin", "thuEmploy", "doanAdmin", "toanAdmin")
  )

jsCode <- 'shinyjs.winprint = function(){window.print();}'


UnknownCustomer <- "Khách Lạ"
unitID <- c("Chai", "Hộp", "Gói", "Thùng", "Bao", "Kg", "Lít")
catag <-
  c("Rượu", "Đồ Ngâm Rượu", "Chè/Trà", "Thảo Dược", "Nước", "Khác")
passData <- "123"

ExpDate <- 30
OutSms <- 80 # 1 thang  = 2592000 giay

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'Đã có lỗi xảy ra ở chức năng này.'; }
}
"


ReportError <- function(header, detail) {
  showModal(modalDialog(
    title = header,
    detail,
    easyClose = TRUE,
    footer = NULL,
    fade = TRUE
  ))
}

#+++++++++++++++++++++++++++++SERVER CONNECTION AERA +++++++++++++++++++++++++++

# options(mysql = list(
#   "host" = "103.153.215.162",
#   "port" = 3306,
#   "user" = "quan",
#   "password" = "hongquan9x"
# ))




# loadData <- function(databaseName, table) {
#   db <-
#     dbConnect(
#       MySQL(),
#       dbname = databaseName,
#       host = options()$mysql$host,
#       port = options()$mysql$port,
#       user = options()$mysql$user,
#       password = options()$mysql$password
#     )
#   query <- paste("SELECT * FROM ", table, " ORDER BY id DESC")
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }

# loadOrder <- function(databaseName, table) {
#   db <-
#     dbConnect(
#       MySQL(),
#       dbname = databaseName,
#       host = options()$mysql$host,
#       port = options()$mysql$port,
#       user = options()$mysql$user,
#       password = options()$mysql$password
#     )
#   query <- paste("SELECT * FROM ", table)
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#  data
#
# }

# loadAddress <- function(databaseName, table) {
#   db <-
#     dbConnect(
#       MySQL(),
#       dbname = databaseName,
#       host = options()$mysql$host,
#       port = options()$mysql$port,
#       user = options()$mysql$user,
#       password = options()$mysql$password
#     )
#   query <- paste("SELECT * FROM ", table)
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
#
# }






# saveData <- function(data) {
#   db <-
#     dbConnect(
#       MySQL(),
#       dbname = databaseName,
#       host = options()$mysql$host,
#       port = options()$mysql$port,
#       user = options()$mysql$user,
#       password = options()$mysql$password
#     )
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     "sanpham",
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   dbGetQuery(db, query)
#
#   dbDisconnect(db)
#   data
# }


# killDbConnections <- function () {
#   all_cons <- dbListConnections(MySQL())
#   print(all_cons)
#   for (con in all_cons)
#     +  dbDisconnect(con)
#   print(paste(length(all_cons), " connections killed."))
#
# }


################################################################################
##################################LAYOUTS#######################################

ui <- dashboardPage(
  dashboardHeader(title = "QUÀ QUÊ ĐẤT VIỆT", dropdownMenuOutput("messageMenu")),
  
  ###############################SIDEBAR #######################################
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Thành viên",
        tabName = "member" ,
        icon = icon("user-alt"),
        uiOutput("welcome"),
        tags$style("#welcome{color: white; font-size: 16px;font-style: bold}"),
        textInput("user", "", placeholder = "Tên đăng nhập ", width = "100%"),
        textInput("pass", "", placeholder = "Mật khẩu", width = "100%"),
        actionBttn(
          "login",
          "Đăng nhập",
          icon = icon("lock"),
          style = "simple",
          color = "success",
          size = "xs"
        ),
        actionButton(
          "logout",
          "Thoát",
          icon = icon("unlock"),
          width = '80%'
        ),
        tags$style("#logout{background: transparent; color:white ; border:0px}")
      ),
      menuItem("Thống Kê",
               tabName = "home" ,
               icon = icon("chart-pie")),
      menuItem(
        "Nhập dữ liệu",
        tabName = "importData",
        icon = icon("database")
      ),
      menuItem(
        "Bán hàng",
        tabName = "sellData",
        icon = icon("shopping-cart")
      ),
      menuItem(
        "Hóa đơn",
        tabName = "invoice",
        icon = icon("receipt"),
        badgeColor = "red",
        menuSubItem(
          "Hiển thị hóa đơn",
          tabName = "invoice",
          icon = icon("file-invoice")
        ),
        selectizeInput("city", "Chọn thành phố", choices = ""),
        selectizeInput("district", "Chọn huyện", choices = ""),
        selectizeInput("Commune", "Chọn xã", choices = ""),
        textInput("phone", "Số điện thoại", placeholder = "số điện thoại ..."),
        sliderInput(
          "promote",
          "Giảm giá (%)",
          min = 0,
          max = 100,
          step = 1,
          value = 0
        )
        
      ),
      menuItem(
        "Quản lý nợ",
        tabName = "debt",
        icon = icon("money-bill-wave")
      ),
      menuItem("Phòng chát",
               tabName = "payroll",
               icon = icon("envelope")),
      menuItem(
        "Phục hồi dữ liệu",
        tabName = "backup",
        icon = icon("power-off")
      )
    )
  ),
  
  ##############################################################################
  #################################BODYWEB######################################
  dashboardBody(
    tags$style('.content-wrapper, .right-side {background-color: white}'),
    shinyjs::useShinyjs(),
    tabItems(tabItem(tabName  = "member")),
    #Thanh Vien
    
    #++++++++++++++++++++++++++++TRANG THONG KE ++++++++++++++++++++++++++++++
    tabItems(
      tabItem(
        tabName  = "home",
        # Thong Ke
        fluidRow(column(
          12,
          plotlyOutput("timeLine") %>% withSpinner(type = 8)
          
        )),
        # Danh sach san pham da ban
        fluidRow(
          column(4,
                 plotlyOutput("expired")),
          
          # San pham sap het han
          column(4,
                 plotOutput("remainProduct")),
          # So nguoi dang no
          column(4,
                 plotlyOutput("listDebt"))
        ),
        
        fluidRow(
          column(4,
                 plotlyOutput("employ")),
          column(4,
                 plotOutput("soldAll")),
          column(
            4,
            selectInput("listSold", "Chọn tên sản phẩm", choices = ""),
            plotlyOutput("soldGraph")
            
          )
        )
      ),
      
      #+++++++++++++++++++++++++++TRANG NHAP DU LIEU +++++++++++++++++++++++++
      tabItem(tabName =  "importData",
              fluidRow(
                column(
                  2,
                  textInput("ID", "Mã sản phẩm"),
                  textInput("name", "Tên sản phẩm"),
                  numericInput("quantity", "Số lượng", value = "", min = 0),
                  selectInput("unit", "Đơn vị", choices = unitID),
                  numericInput(
                    "importPrice",
                    "Giá nhập",
                    value = "",
                    min = 1000,
                    step = 1000
                  ),
                  numericInput(
                    "soldPrice",
                    "Giá bán",
                    value = "",
                    min = 1000,
                    step = 1000
                  ),
                  selectInput("catalogue", "Loại", choices = catag),
                  textInput("webLink", "Link Website"),
                  dateInput("expireDate", label = "Ngày hết hạn"),
                  fileInput(
                    "image",
                    "Hình đại diện jpg/png",
                    accept = c('image/png', 'image/jpeg')
                  ),
                  actionBttn(
                    "save",
                    "Lưu dữ liệu",
                    icon = icon("save"),
                    style = "unite",
                    size = "md",
                    color = "success",
                    block = T
                  )
                ),
                column(
                  7,
                  shinydashboard::box(DT::DTOutput("table"),
                                      width = "100%",
                                      height = "100%"),
                  box(
                    selectInput("removeRow", "Chọn tên sản phầm cần xóa", choices = " "),
                    actionBttn(
                      "xoaDulieu",
                      "Xóa dữ liệu",
                      icon = icon("trash-alt"),
                      size = "md",
                      style = "jelly",
                      color = "danger"
                    ),
                    
                    fileInput(
                      "loadImportData",
                      "Chọn dữ liệu file Excel (Tên cột phải giống như trên)",
                      accept = ".xlsx",
                      width  = "100%",
                      buttonLabel = "Tải lên dữ liệu bằng file Excel",
                      placeholder = "Định dạng: [Code] [Product] [Quantity] [Unit] [Import] [Sell] [Catalogue] [Date] [Link] [Expired] "
                    ),
                    
                    tableOutput("contents"),
                    width = "100%",
                    height = "100%"
                  ),
                  
                  bsModal(
                    "deleteProduct",
                    paste0("Bạn có chắc xóa dữ liệu này?"),
                    "xoaDulieu",
                    size = "small",
                    actionBttn(
                      "xoa",
                      "Xác nhận để xóa",
                      icon = icon("trash-alt"),
                      size = "md",
                      style = "unite",
                      color = "danger"
                    )
                  )
                )
              )),
      
      
      #+++++++++++++++++++++++++++++BAN SAN PHAM +++++++++++++++++++++++++++++
      
      tabItem(tabName =  "sellData",
              fluidRow(
                column(
                  3,
                  sliderInput(
                    "vatValue",
                    "Giá trị VAT (%)",
                    min = 1,
                    max = 15,
                    post = "% VAT",
                    value = 10,
                    step = 1
                  ),
                  selectInput("soldProduct", "Chọn tên sản phẩm", ""),
                  selectInput("CodeID", "Hoặc Code sản phẩm", ""),
                  # selectInput("soldProduct", "Chọn tên sản phẩm", c("", unique(Data_Table$Product))),
                  # selectInput("CodeID", "Hoặc Code sản phẩm", c("", unique(Data_Table$Code))),
                  uiOutput("stock"),
                  numericInput("soldToday", "Số lượng bán", value = "" , min = 0),
                  textInput("cusName", "Tên khách hàng ", placeholder = "Khách vãng lai thì để trống"),
                  radioButtons(
                    "price",
                    choices = c("Tự động tính", "Khác (số lượng bán = 1)"),
                    "Giá bán"
                  ),
                  numericInput(
                    "otherPrice",
                    "Giá bán",
                    value = "" ,
                    min = 1000,
                    step = 1000
                  ),
                  actionBttn(
                    "sold",
                    "Thêm vào giỏ",
                    icon = icon("shopping-bag"),
                    size = "md",
                    style = "pill",
                    
                    color = "success"
                  )
                ),
                column(
                  6,
                  box(
                    title = "Giỏ Hàng",
                    width = "100%",
                    background = "light-blue",
                    uiOutput("warning"),
                    tags$style("#warning{color: yellow; font-style: italic}"),
                    DTOutput('liveOrder'),
                    selectInput("deleteOrder", "", choices = ""),
                    actionBttn(
                      "deleteOrderName",
                      "Xóa sản phẩm ra khỏi danh sách",
                      icon = icon("minus-circle"),
                      size = "sm",
                      style = "stretch",
                      block = T,
                      color = "default"
                    )
                  ),
                  shinydashboard::box(
                    fluidRow(
                      column(
                        3,
                        box(
                          background = "maroon",
                          width = "100%",
                          textInput(
                            "cusPay",
                            "Khách trả",
                            placeholder = "Giá bán ..",
                            width = "100%"
                          )
                        ),
                        tags$style("#cusPay{color: red; font-size: 20px;font-style: bold}")
                      ),
                      column(
                        3,
                        box(
                          title = "Tổng",
                          width = "100%",
                          background = "aqua",
                          uiOutput("tong"),
                          tags$style("#tong{font-size: 25px;font-style: bold}"),
                        )
                      ),
                      column(
                        3,
                        box(
                          title = "Thối",
                          background = "green",
                          width = "100%",
                          uiOutput("thoi"),
                          tags$style("#thoi{font-size: 25px;font-style: bold}"),
                        )
                      ),
                      column(
                        3,
                        box(
                          title = "Nợ",
                          background = "olive",
                          width = "100%",
                          uiOutput("no"),
                          tags$style("#no{font-size: 25px;font-style: bold}"),
                        )
                      )
                      
                    ),
                    
                    fluidRow(column(
                      6,
                      awesomeCheckbox(
                        inputId = "vat",
                        label = "Chọn vào đây nếu tính VAT",
                        value = F,
                        status = "danger"
                      )
                    )),
                    
                    width = "100%",
                    height = "100%",
                    
                    
                    actionBttn(
                      "ask2pay",
                      "Thanh Toán",
                      icon = icon("shopping-cart"),
                      style = "bordered",
                      size = "md",
                      color = "danger",
                      block = T
                    ),
                    
                    actionBttn(
                      "pay",
                      "Xác nhận để tiếp tục",
                      icon = icon("arrow-circle-right"),
                      style = "material-flat",
                      size = "md",
                      color = "warning",
                      block = T
                      
                    ),
                    column(
                      4,
                      textInput("debtNameID", label = "Tên khách hàng ", placeholder = "Vui lòng tên"),
                      
                    ),
                    column(
                      4,
                      textInput("phoneNo", label = "Số điện thoại ", placeholder = "Vui lòng nhập SDT")
                    ),
                    column(
                      4,
                      textInput("CMND", label = "Số thẻ CCCD/CMND ", placeholder = "Vui lòng nhập CCCD/CMND")
                    ),
                    
                    actionBttn(
                      "saveInfor",
                      "TIẾP TỤC",
                      icon = icon("angle-double-right"),
                      style = "jelly",
                      size = "md",
                      color = "danger"
                    )
                  )
                ),
                
                column(
                  3,
                  
                  imageOutput("hinh", width = "200px", height = "300px"),
                  
                  shinydashboard::box(
                    title = "Tổng số bán ",
                    uiOutput("totalSoldTitle"),
                    width = "100%",
                    height = "100%"
                  ),
                  actionBttn (
                    "allPayment",
                    "Lịch sử các lần thanh toán",
                    icon = icon("dollar-sign"),
                    style  = 'stretch',
                    color = "primary",
                    size = "xs",
                    block = T
                  ),
                  bsModal(
                    "allRecord",
                    "Lịch sử các lần giao dịch",
                    "allPayment",
                    size = "large",
                    
                    DTOutput('TableSold')
                  )
                  
                )
              )),
      
      
      #++++++++++++++++++++++++++++TAO HOA DON ++++++++++++++++++++++++++++++
      
      tabItem(tabName =  "invoice",
              fluidRow(column(
                10,
                box(
                  width = "100%",
                  fluidRow(
                    column(
                      2,
                      img(
                        src = 'logo.jpg',
                        align = "center",
                        height = "20%",
                        width = "20%"
                      ),
                      align = "center"
                    ),
                    column(
                      10,
                      uiOutput("company"),
                      align = "left",
                      tags$style("#company{color: green; font-size: 20px;font-style: bold}"),
                      uiOutput("companyAddress1"),
                      tags$style("#companyAddress1{font-size: 12px;}"),
                      uiOutput("companyAddress2"),
                      tags$style("#companyAddress2{font-size: 12px;}"),
                      uiOutput("companyAddress3"),
                      tags$style("#companyAddress3{font-size: 12px;}"),
                      uiOutput("companyAddress4"),
                      tags$style("#companyAddress4{font-size: 12px;}"),
                      align = "left"
                    )
                  ),
                  fluidRow(column(
                    12,
                    uiOutput("reciept"),
                    tags$style("#reciept{color: black; font-size: 30px;font-style: bold}")
                  ), align = "center"),
                  fluidRow(column(
                    12,
                    uiOutput("recieptNo"),
                    tags$style("#recieptNo{font-size: 15px;font-style: bold}")
                  ), align = "center"),
                  fluidRow(column(
                    12,
                    uiOutput("cusName"),
                    tags$style("#cusName{ font-size: 17px;font-style: bold}"),
                    uiOutput("cusInfor")
                  )),
                  fluidRow(
                    column(
                      12,
                      DTOutput("customerOrder", width = "60%"),
                      uiOutput("sumPrice"),
                      uiOutput("vat"),
                      uiOutput("promotion"),
                      tags$style("#promotion{color: red; font-size: 15px;}"),
                      uiOutput("lines"),
                      uiOutput("PriceVAT"),
                      tags$style("#PriceVAT{color: black; font-size: 18px;font-style: bold}"),
                      uiOutput("repayment"),
                      tags$style("#repayment{black; font-size: 15px;font-style: bold}"),
                      uiOutput("remainPay"),
                      tags$style("#remainPay{font-size: 13px;font-style: bold}"),
                    ),
                    fluidRow(style = "height:20px;"),
                    fluidRow(
                      column(
                        6,
                        uiOutput("buyerSign"),
                        uiOutput("buyer"),
                        tags$style("#buyer{font-size: 18px;font-style: bold}"),
                        fluidRow(style = "height:40px;"),
                        uiOutput("date1")
                      ),
                      column(
                        6,
                        uiOutput("sellerSign"),
                        uiOutput("seller"),
                        tags$style("#seller{font-size: 18px;font-style: bold}"),
                        fluidRow(style = "height:40px;"),
                        uiOutput("date2")
                      )
                    ),
                    fluidRow(
                      column(
                        4,
                        actionBttn(
                          "updateCustom",
                          "Lưu hóa đơn",
                          icon = icon("save"),
                          style = "minimal",
                          size = "xs",
                          color = "primary"
                        ),
                        bsModal(
                          "uploadGoogle",
                          paste0("Bạn có muốn cập nhật tất cả dữ liệu lên Google Drive?"),
                          "updateCustom",
                          size = "small",
                          actionBttn(
                            "upload",
                            "Tải lên Google",
                            icon = icon("upload"),
                            size = "md",
                            style = "unite",
                            color = "danger"
                          )
                        )
                      ),
                      column(
                        4,
                        useShinyjs(),
                        extendShinyjs(text = jsCode, functions = c("winprint")),
                        
                        actionBttn(
                          "revokeReceipt",
                          "Tiếp tục mua hàng",
                          icon = icon("shopping-bag"),
                          style = "minimal",
                          size = "xs",
                          color = "danger"
                        )
                        
                      ),
                      column(
                        4,
                        actionBttn(
                          "print",
                          "In hóa đơn",
                          icon = icon("print"),
                          style = "minimal",
                          size = "xs",
                          color = "royal"
                        )
                        
                        
                      ),
                      align = "center"
                    ),
                    align = "center"
                  )
                )
              ))),
      
      #++++++++++++++++++++++++++++QUAN LY NO ++++++++++++++++++++++++++++++++
      
      tabItem(tabName = "debt",
              fluidRow(
                column(
                  2,
                  selectInput("debtList", "Chọn tên khách hàng", choices = ""),
                  selectInput("phoneID", "Số điện thoại", choices = ""),
                  selectInput("CMNDID", "CMND/CCCD", choices = ""),
                  uiOutput("amountDebt"),
                  tags$style("#amountDebt{font-size: 30px;color:red; font-style: bold}"),
                  uiOutput("dateDebt"),
                  actionBttn(
                    "returnDebt",
                    "Trả nợ",
                    icon = icon("fas fa-money-bill-alt"),
                    size = "lg",
                    style = "pill"
                  )
                ),
                column(
                  9,
                  DTOutput("individualDebt"),
                  uiOutput("lines2"),
                  uiOutput("totalAmount"),
                  uiOutput("paid"),
                  uiOutput("VATDebt"),
                )
              )),
      
      #+++++++++++++++++++++++++GUI TIN NHAN ++++++++++++++++++++++++++++++++
      tabItem(tabName =  "payroll",
              fluidRow(
                column(
                  3,
                  selectInput("receiver", "Người gửi", choices = dataLogin[, 3]),
                  pickerInput(
                    inputId = "theme",
                    label = "Chọn một chủ đề email",
                    choices = c("Bình thường", "Sản phẩm", "Tiền bạc", "Thông báo",
                                "Kỹ thuật"),
                    selected = "Badge danger",
                    choicesOpt = list(content = sprintf(
                      "<span class='label label-%s'>%s</span>",
                      c("info", "success", "danger", "primary", "warning"),
                      c("Bình thường", "Hàng hóa", "Tiền bạc", "Cá nhân",
                        "Kỹ thuật")
                    ))
                  ),
                  textAreaInput("writeSms", "", placeholder = "Nhắn tin ở đây ", height = "300px"),
                  actionBttn(
                    "sendSms",
                    "Gửi",
                    icon = icon("paper-plane"),
                    size = "sm",
                    style = "pill"
                  )
                ),
                column(
                  9,
                  column(
                    9,
                    selectInput(
                      "removeSms",
                      "Chọn tin nhắn cần xóa",
                      choices = " ",
                      width = "100%"
                    ),
                  ),
                  column(
                    1,
                    actionBttn(
                      "smsDel",
                      "Xóa",
                      icon = icon("trash-alt"),
                      size = "sm",
                      style = "jelly",
                      color = "danger"
                    )
                  ),
                  DTOutput("messageList")
                ),
                width = "100%"
                
              )),
      tabItem(tabName =  "backup",
              fluidRow(box(
                textInput(
                  "askPass",
                  "Nhập mật khẩu",
                  value = "" ,
                  placeholder = "Nhập mật khẩu để phục hồi dữ liệu.."
                ),
                actionBttn(
                  "loginData",
                  "Đăng nhập",
                  icon = icon("key"),
                  size = "md",
                  style = "bordered",
                  color = "royal"
                )
              )),
              fluidRow(
                box(
                  title = "CẬP NHẬT DỮ LIỆU",
                  background = "teal",
                  actionBttn(
                    "backupData",
                    "Backup",
                    icon = icon("hdd"),
                    size = "md",
                    style = "jelly",
                    color = "warning"
                  )
                ),
                box(
                  title = "KHÔI PHỤC DỮ LIỆU",
                  background = "teal",
                  selectInput("dateData", "Chọn ngày dữ liệu được backup", choices = ""),
                  actionBttn(
                    "restoreData",
                    "Phục Hồi",
                    icon = icon("sync-alt"),
                    size = "md",
                    style = "jelly",
                    color = "success"
                  )
                )
              ))
    ),
    tags$style('.col-sm-8 {width:100%}'),
  )
  
  
)



################################################################################
###############################SERVER ##########################################




server <- function(input, output, session) {




  # options(mysql = list(
  #   "host" = "103.153.215.162",
  #   "port" = 3306,
  #   "user" = "quan",
  #   "password" = "hongquan9x"
  # ))
  
  # options(mysql = list(
  #   "host" = "localhost",
  #   "port" = 3306,
  #   "user" = "root",
  #   "password" = "1989"
  # ))
  #
  #
  # db <-
  #   dbConnect(
  #     MySQL(),
  #     dbname = "quan",
  #     host = options()$mysql$host,
  #     port = options()$mysql$port,
  #     user = options()$mysql$user,
  #     password = options()$mysql$password,
  #     encoding = "UTF-8"
  #   )
  
  
  Data_Table  <-  read.xlsx("data/database.xlsx")
  # Data_Table  <- Data_Table[order(Data_Table$id, decreasing = TRUE), ]
  
  
  
  #++++++++++++++++++++++++++++DISABLE VARIABLES +++++++++++++++++++++++++++++++
  shinyjs::hide("debtNameID")
  shinyjs::hide("phoneNo")
  shinyjs::hide("CMND")
  shinyjs::hide("saveInfor")
  shinyjs::hide("pay")
  shinyjs::hide("backupData")
  shinyjs::hide("restoreData")
  shinyjs::hide("dateData")
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++++++++++++++++LOGIN DETAILS+++++++++++++++++++++++++++++++++++
  hideAll <- function(x) {
    shinyjs::hide(selector = '[data-value="importData"]')
    shinyjs::hide(selector = '[data-value="sellData"]')
    shinyjs::hide(selector = '[data-value="invoice"]')
    shinyjs::hide(selector = '[data-value="debt"]')
    shinyjs::hide(selector = '[data-value="payroll"]')
    shinyjs::hide(selector = '[data-value="home"]')
    shinyjs::hide(selector = '[data-value="member"]')
    shinyjs::hide("loadImportData")
    shinyjs::hide("xoaDulieu")
    shinyjs::hide("removeRow")
  }
  showAll <- function(x) {
    shinyjs::show(selector = '[data-value="debt"]')
    shinyjs::show(selector = '[data-value="payroll"]')
    shinyjs::show(selector = '[data-value="invoice"]')
    shinyjs::show(selector = '[data-value="home"]')
    shinyjs::show(selector = '[data-value="importData"]')
    shinyjs::show(selector = '[data-value="sellData"]')
    shinyjs::show("loadImportData")
    shinyjs::show("xoaDulieu")
    shinyjs::show("removeRow")
  }
  hideAll()
  #++++++++++++++++++++++++IDENTIFY MEMBER+++++++++++++++++++++++++++++++++++
  observe({
    load("data/adminID1.Rda")
    load("data/adminID2.Rda")
    load("data/adminID3.Rda")
    load("data/nhanvienID1.Rda")
    
    activeAdmin <- function(adminUser) {
      shinyjs::hide("user")
      shinyjs::hide("pass")
      shinyjs::hide("login")
      shinyjs::show("logout")
      shinyjs::show("welcome")
      showAll()
      output$welcome  <-
        renderUI({
          paste0("Chào ", adminUser)
        })
    }
    
    if (role1 == "quanAdmin") {
      activeAdmin(dataLogin[1, 3])
    }
    if (role3 == "doanAdmin") {
      activeAdmin(dataLogin[3, 3])
    }
    if (role4 == "toanAdmin") {
      activeAdmin(dataLogin[4, 3])
    }
    
    if (role2 == "thuEmploy") {
      activeAdmin(dataLogin[2, 3])
      hideAll()
      shinyjs::show(selector = '[data-value="importData"]')
      shinyjs::show(selector = '[data-value="sellData"]')
      
    }
    
    
    loginID <- function(name) {
      shinyjs::hide("user")
      shinyjs::hide("pass")
      shinyjs::hide("login")
      shinyjs::show("welcome")
      shinyjs::show("logout")
      output$welcome  <- renderUI({
        paste0("Chào: ", name)
      })
      
    }
    
    
    #+++++++++++++++++++++++DANG NHAP +++++++++++++++++++++++++++++++++++++++
    observeEvent(input$login, {
      if (isTRUE(input$user == dataLogin[1, 1]) &
          isTRUE(input$pass == dataLogin[1, 2])) {
        loginID(dataLogin[1, 3])
        role1 <- dataLogin[1, 4]
        showAll()
        save(role1, file = "data/adminID1.Rda")
        name <- dataLogin[1, 3]
        save(name, file = "data/activeMember.Rda")
        
      } else if (isTRUE(input$user == dataLogin[2, 1]) &
                 isTRUE(input$pass == dataLogin[2, 2])) {
        loginID(dataLogin[2, 3])
        shinyjs::show(selector = '[data-value="importData"]')
        shinyjs::show(selector = '[data-value="sellData"]')
        role2 <- dataLogin[2, 4]
        save(role2, file = "data/nhanvienID1.Rda")
        name <- dataLogin[2, 3]
        save(name, file = "data/activeMember.Rda")
        
      } else if (isTRUE(input$user == dataLogin[3, 1]) &
                 isTRUE(input$pass == dataLogin[3, 2])) {
        loginID(dataLogin[3, 3])
        role3 <- dataLogin[3, 4]
        showAll()
        save(role3, file = "data/adminID2.Rda")
        name <- dataLogin[3, 3]
        save(name, file = "data/activeMember.Rda")
        
      } else if (isTRUE(input$user == dataLogin[4, 1]) &
                 isTRUE(input$pass == dataLogin[4, 2])) {
        loginID(dataLogin[4, 3])
        role4 <- dataLogin[4, 4]
        showAll()
        save(role4, file = "data/adminID3.Rda")
        name <- dataLogin[4, 3]
        save(name, file = "data/activeMember.Rda")
      } else
        sendSweetAlert(session = session,
                       title = "Đăng nhập không đúng",
                       type = "error")
    })
    
    #++++++++++++++++++++++++THOAT+++++++++++++++++++++++++++++++++++++++++++
    observeEvent(input$logout, {
      shinyjs::show("user")
      shinyjs::show("pass")
      shinyjs::show("login")
      shinyjs::hide("logout")
      shinyjs::hide("welcome")
      shinyjs::hide("inTabset")
      updateTabItems(session, "tabs", selected = "member")
      hideAll()
      role1 <- ""
      role2 <- ""
      role3 <- ""
      role4 <- ""
      save(role1, file = "data/adminID1.Rda")
      save(role2, file = "data/nhanvienID1.Rda")
      save(role3, file = "data/adminID2.Rda")
      save(role4, file = "data/adminID3.Rda")
    })
  })
  
  
  
  ############################################################################
  ############################HEADERS#########################################
  
  
  ############################THONG KE ######################################
  
  # timeline <-
  #   select(dbReadTable(db,"sold"), Date, Item, Quantity)
  sold <- read.xlsx("data/sold.xlsx")
  timeline <-
    select(sold, Date, Item, Quantity)
  
  # So luong da ban
  output$timeLine <- renderPlotly({
    p <-
      ggplot(timeline, aes(Date, Quantity, color = Item, group = 1)) +
      geom_line(aes(y = Quantity)) +
      geom_point(size = 1.8) +
      theme_few(base_size = 13) +
      ggtitle("Số lượng đã bán") +
      xlab("Thời gian") +
      ylab("Số lượng") +
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly(p) %>%
      config(displaylogo = F) %>%
      animation_opts(frame = 500,
                     transition = 500,
                     redraw = FALSE)
  })
  
  # Danh sach nguoi no
  output$listDebt <- renderPlotly({
    debt <- read.xlsx("data/debtlist.xlsx")
    debtValue <- select(debt, NoVAT:Date)
    # debtValue <- select(dbReadTable(db,"listdebt"), NoVAT:Date)
    dataDept <-
      data.frame(select(debtValue, Customer:Date), Price = debtValue[1, 1] + debtValue[1, 2])
    p <-
      ggplot(dataDept, aes(Customer, Price, fill = Customer)) +
      geom_bar(stat = "identity") +
      theme_few(base_size = 10) +
      ggtitle("Khách hàng đang nợ") +
      xlab("Tên khách hàng") +
      ylab("Số tiền đang nợ (VND") +
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly(p) %>%
      config(displaylogo = F)
    
  })
  
  # San pham sap het han su dung
  output$expired <- renderPlotly({
    # dataDate <- dbReadTable(db, "sanpham")
    dataDate <- read.xlsx("data/database.xlsx")
    dataDate$ExpiredDate <-
      round(as.POSIXlt.character(dataDate$Expired) - as.POSIXct(format(Sys.time(), "%d-%m-%Y %H:%M"))) /
      365
    Days <- filter(dataDate, (ExpiredDate) < ExpDate)
    remainDate <-
      select(Days, Product, ExpiredDate)
    p <-
      ggplot(remainDate, aes(Product, ExpiredDate, fill = Product)) +
      geom_bar(stat = "identity") +
      theme_few(base_size = 10) +
      ggtitle("Sản phẩm sắp hết hạn (ngày)") +
      xlab("Tên sản phẩm") +
      ylab("- Hết hạn  | Sắp hết hạn + ") +
      geom_hline(aes(yintercept = 0), size = 1.2,
                 color = "firebrick") +
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly(p) %>%
      config(displaylogo = F)
  })
  
  # NHAN VIEN BAN HANG
  output$employ <- renderPlotly({
    # dataDate <- dbReadTable(db, "sanpham")
    sold <- read.xlsx("data/Invoice.xlsx")
    sold <- select(sold, Quantity, TobePaid, Seller)
    p <-
      ggplot(sold, aes(Seller, Quantity, fill = TobePaid  , group = Seller)) +
      geom_violin(alpha = 0.7) +
      theme_few(base_size = 10) +
      ggtitle("Nhân viên bán hàng") +
      xlab("Tên nhân viên") +
      ylab("Số lượng sản phẩm") +
      theme(legend.position = "none") +
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly(p) %>%
      config(displaylogo = F)
  })
  # So luong da ban /san pham
  observe({
    # Data_Table <- dbReadTable(db,"sanpham")
    Data_Table <- read.xlsx("data/database.xlsx")
    updateSelectInput(session, "listSold",  choices = Data_Table[2])
  })
  
  
  output$soldAll <- renderPlot({
    data <- read.xlsx("data/sold.xlsx")
    data <- select(data, Item,  Quantity)
    ggplot(data, aes(x = Item, y = Quantity)) +
      theme_pander(base_size = 15) +
      geom_col(aes(fill = factor(Item))) +
      labs(x = "", y = "Số sản phẩm đã bán") +
      coord_polar() +
      theme(axis.text.x = element_text(angle = 0, size = 12)) +
      guides(fill = "none")
  })
  
  
  output$soldGraph <- renderPlotly({
    # dataDate <- dbReadTable(db, "sold")
    dataDate <- read.xlsx("data/sold.xlsx")
    ratio <-
      c(sum(filter(dataDate, Item == input$listSold)[2]),
        as.numeric(filter(Data_Table , Product == input$listSold)[3]))
    labels <- c("Đã bán", "Đang có")
    compare <- data.frame(Title = labels, Quantity = ratio)
    plot_ly(
      compare,
      labels = ~ Title,
      values = ~ Quantity,
      type = 'pie'
    ) %>%
      config(displaylogo = F)
    
  })
  
  
  # San pham dang con
  output$remainProduct <- renderPlot({
    # dataDate <- dbReadTable(db, "sold")
    remain <- read.xlsx("data/database.xlsx")
    remain <- select(remain, Product, Quantity)
    
    remain$fraction <- remain$Quantity / sum(remain$Quantity)
    
    # Compute the cumulative percentages (top of each rectangle)
    remain$ymax <- cumsum(remain$fraction)
    
    # Compute the bottom of each rectangle
    remain$ymin <- c(0, head(remain$ymax, n = -1))
    
    # Compute label position
    remain$labelPosition <- (remain$ymax + remain$ymin) / 2
    
    # Compute a good label
    remain$label <-
      paste0(remain$Product, "\n còn lại: ", remain$Quantity)
    
    # Make the plot
    ggplot(remain,
           aes(
             ymax = ymax,
             ymin = ymin,
             xmax = 4,
             xmin = 3,
             fill = Product
           )) +
      geom_rect() +
      ggtitle("Sản phẩm trong kho") +
      geom_text(
        x = 2,
        aes(
          y = labelPosition,
          label = label,
          color = Product
        ),
        size = 3
      ) + # x here controls label position (inner / outer)
      coord_polar(theta = "y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
    
  })
  
  
  
  ############################NHAP SAN PHAM ##################################
  observe({
    # Data_Table<- dbReadTable(db, "sanpham")
    # Data_Table<- Data_Table[order(Data_Table$Date, decreasing = TRUE), ]
    Data_Table <- read.xlsx("data/database.xlsx")
    
    
    # DataDatViet <- Data_Table
    
    # Tao FUnction table
    
    DTtable <- function(tableName) {
      DT::renderDT(server = FALSE, {
        DT::datatable(
          tableName,
          selection = "none",
          extensions = 'Buttons',
          option = list(
            searching = TRUE,
		scrollX = TRUE,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            dom = 'Bfrtip',
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json')
            
          )
          
        ) %>%
          formatCurrency(
            5:6,
            currency = "VND ",
            interval = 3,
            mark = ",",
            digits = 0
          )
      })
    }
    
    
    observe({
      # output$table <- DTtable(Data_Table[, 1:10])
      output$table <- DTtable(Data_Table[, 1:8])
      
      
      
      # Them san pham moi vao danh sach
      observeEvent(input$save, {
        tryCatch({
          Data_Table <- read.xlsx("data/database.xlsx")
          
          newLine <- data.frame(
            Code = input$ID,
            Product = input$name,
            Quantity = as.numeric(input$quantity),
            Unit =  input$unit,
            Import  = as.numeric(input$importPrice),
            Sell = as.numeric(input$soldPrice),
            Catalogue = input$catalogue,
            Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
            Link = input$webLink ,
            Expired = as.character(format(
              input$expireDate, "%d-%m-%Y %H:%M"
            ))
          )
          
          addProduct <- rbind(newLine, Data_Table)
          
          # Write locally
          write.xlsx(addProduct, file = "data/database.xlsx", overwrite = T)
          
          #Write to Google
          tryCatch({
            gs4_auth(cache = ".secrets",
                     email = TRUE,
                     use_oob = TRUE)
            database_sheet <-
              "https://docs.google.com/spreadsheets/d/1uw-BOnnicx7hcOPND492G62t9Yh4tMfcV5yFMVMt_Jk/edit?usp=sharing"
            write_sheet(addProduct, database_sheet, sheet = "database")
          },
          error = function(e) {
            showModal(modalDialog(
              div(
                paste0(
                  "Máy tính bạn đang trong trạng thái OFFLINE, dữ liệu đã lưu trong máy tính và sẽ được đăng lên Google sau"
                )
              ),
              title = "CHÚ Ý" ,
              footer = modalButton("ĐÓNG"),
              easyClose = T
            ))
            return()
          })
          # dbWriteTable(db,
          #              "sanpham",
          #              newLine,
          #              append  = TRUE,
          #              row.names = F)
          
          sendSweetAlert(
            session = session,
            title = paste("Sản phẩm " , input$name, " đã lưu"),
            type = "success"
          )
          
          Data_Table <- read.xlsx("data/database.xlsx")
          
          # Data_Table  <- dbReadTable(db, "sanpham")
          # Data_Table <-Data_Table[order(Data_Table$Date, decreasing = TRUE), ]
          
          output$table <- DTtable(Data_Table[, 1:8])
          
          
          # Reset lai cac lua chon sau khi da luu du lieu
          updateTextInput(session, "ID", value = " ")
          updateTextInput(session, "name", value = " ")
          updateTextInput(session, "quantity", value = " ")
          updateTextInput(session, "importPrice", value = " ")
          updateTextInput(session, "sellPrice", value = " ")
          
          #Xoa du lieu cua san pham nhap
          updateSelectInput(session,
                            "removeRow",
                            "Chọn tên sản phẩm cần xóa",
                            choices = Data_Table$Product)
          
          
          observeEvent(input$image, {
            inFile <- input$image
            if (is.null(inFile))
              return()
            # file.copy(inFile$datapath, file.path("data/image/", inFile$name) )
            file.copy(inFile$datapath, file.path("www/", paste0(input$ID, ".jpg")))
          })
          
          
          
          
        },
        error = function(e) {
          showModal(
            modalDialog(
              div(
                paste0("VUI LÒNG NHẬP [ SỐ LƯỢNG | GIÁ NHẬP | GIÁ BÁN ] CỦA SẢN PHẨM")
              )
              ,
              title = "Lỗi chưa điền đầy đủ thông tin hoặc Lỗi khác" ,
              footer = modalButton("ĐÓNG"),
              easyClose = T
            )
          )
          return()
        })
        
      })
      
      
      
      #Danh sach san pham theo ten
      observeEvent(input$soldProduct, {
        # database <-  dbReadTable(db, "sanpham")[, 2:11]
        database <- read.xlsx("data/database.xlsx")
        if (input$soldProduct == "") {
          updateSelectInput(session, "CodeID", choices = c("", unique(database$Code)))
        } else{
          updateSelectInput(
            session,
            "CodeID",
            choices = unique(database$Code[database$Product == input$soldProduct]),
            selected = isolate(input$CodeID)
          )
        }
      })
      
      # Danh sach san pham theo CODE
      observeEvent(input$CodeID, {
        # database <- dbReadTable(db, "sanpham")[, 2:11]
        database <- read.xlsx("data/database.xlsx")
        if (input$CodeID == "") {
          updateSelectInput(session, "soldProduct", choices = c("", unique(database$Product)))
        } else{
          updateSelectInput(
            session,
            "soldProduct",
            choices = unique(database$Product[database$Code == input$CodeID]) ,
            selected = isolate(input$soldProduct)
          )
        }
      })
      
    })
    
    
    
    updateSelectInput(session,
                      "removeRow",
                      "Chọn tên sản phẩm cần xóa",
                      choices = Data_Table$Product)
    
    observeEvent(input$xoa, {
      tryCatch({
        database <- read.xlsx("data/database.xlsx")
        
        numName <- which(database$Product == input$removeRow)
        reMove <- database[-c(numName), ]
        tryCatch({
          write.xlsx(reMove, file = "data/database.xlsx", overwrite = T)
          
          gs4_auth(cache = ".secrets",
                   email = TRUE,
                   use_oob = TRUE)
          database_sheet <-
            "https://docs.google.com/spreadsheets/d/1uw-BOnnicx7hcOPND492G62t9Yh4tMfcV5yFMVMt_Jk/edit?usp=sharing"
          write_sheet(reMove, database_sheet, sheet = "database")
          
        },
        error = function(e) {
          showModal(modalDialog(
            div(
              paste0(
                "Máy tính bạn đang trong trạng thái OFFLINE, dữ liệu đã lưu trong máy tính và sẽ được đăng lên Google sau"
              )
            ),
            title = "CHÚ Ý" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          ))
          return()
        })
        
        output$table <-
          DTtable(read.xlsx("data/database.xlsx")[, 1:8])
        sendSweetAlert(
          session = session,
          title = paste("Sản phẩm", as.character(input$removeRow) , " Đã Xóa"),
          type = "success"
        )
        updateSelectInput(
          session,
          "removeRow",
          "Chọn tên sản phẩm cần xóa",
          choices = read.xlsx("data/database.xlsx")[, 2]
        )
        
        
        # Xpa hinh anh cua san pham theo ten
        link <- filter(database, Product == input$removeRow)[, 1]
        file.remove(file.path(src =  paste0("www/", link, ".jpg")))
        
        
        
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(paste0("OPPS! ĐÃ CÓ LỖI XẢY RA"))
            ,
            title = "Lỗi chưa chọn tên sản phẩm Hoặc lỗi khác" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          )
        )
        return()
      })
      
    })
    
    
    
    
    # THem san pham bang cach cho phep load du lieu bang EXCEL
    output$contents <- renderTable({
      file <- input$loadImportData
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "xlsx", "Load dữ liệu nhập lên "))
      a <- read.xlsx(file$datapath)
      a$Date <- format(Sys.time(), "%d-%m-%Y %H:%M")
      a$Expired <- format(Sys.time(), "%d-%m-%Y %H:%M")
      # saveData(as.data.frame(a))
      write.xlsx(a, file = "data/database.xlsx", overwrite = T)
      tryCatch({
        gs4_auth(cache = ".secrets",
                 email = TRUE,
                 use_oob = TRUE)
        database_sheet <-
          "https://docs.google.com/spreadsheets/d/1uw-BOnnicx7hcOPND492G62t9Yh4tMfcV5yFMVMt_Jk/edit?usp=sharing"
        write_sheet(a, database_sheet, sheet = "database")
        
      },
      error = function(e) {
        showModal(modalDialog(
          div(
            paste0(
              "Máy tính bạn đang trong trạng thái OFFLINE, dữ liệu đã lưu trong máy tính và sẽ được đăng lên Google sau"
            )
          ),
          title = "CHÚ Ý" ,
          footer = modalButton("ĐÓNG"),
          easyClose = T
        ))
        return()
      })
      # Data_Table  <- dbReadTable(db, "sanpham")[, 2:11]
      # Data_Table  <- Data_Table[order(Data_Table$id, decreasing = TRUE), ]
      output$table <- DTtable(Data_Table[, 1:8])
      a
    })
    
    
    
    
    
    
    #############################BAN SAN PHAM ################################
    # Dinh nghia VAT
    observe({
      VAT <- as.numeric(input$vatValue) / 100
    })
    
    check <- read.xlsx("data/liveOrder.xlsx")
    if (nrow(check) > 0) {
      output$warning <- renderUI({
        paste(
          "Chú ý ! Vui lòng xóa ",
          nrow(check),
          " sản phẩm đang hiển thị trong giỏ hàng trước khi mua"
        )
      })
    }
    
    # Hien thi san pham dang co san
    SPdangco <- function(x) {
      observe({
        # database <- dbReadTable(db, "sanpham")[, 2:11]
        database <- read.xlsx("data/database.xlsx")
        if (input$soldProduct == "") {
          CodeProduct <-
            dplyr::filter(database , Code == input$CodeID)
        } else
          CodeProduct <-
            dplyr::filter(database , Product == input$soldProduct)
        output$stock <-
          renderUI({
            paste0("Đang có: ", CodeProduct[, 3], " " , CodeProduct[, 4])
          })
      })
    }
    
    SPdangco()
    # Hien thi so tien can tra lai
    output$thoi <- renderUI({
      # base <- dbReadTable(db, "liveorder")[, 2:6]
      base <- read.xlsx("data/liveOrder.xlsx")
      if (isTRUE(input$cusPay == "")) {
        0
      } else if (as.numeric(input$cusPay) <= as.numeric(sum(base[4]))) {
        0
      } else {
        (as.numeric(input$cusPay) > as.numeric(sum(base[4])))
        if (input$vat == TRUE) {
          format(
            as.numeric(input$cusPay) - as.numeric(sum(base[4]) + (sum(base[4]) * 0.1)),
            big.mark = ",",
            scientific = F
          )
        } else
          format(
            as.numeric(input$cusPay) - as.numeric(sum(base[4])),
            big.mark = ",",
            scientific = F
          )
      }
    })
    
    
    #Hien thi so tien dang no
    output$no <- renderUI({
      # base <-  dbReadTable(db, "liveorder")[, 2:6]
      base <- read.xlsx("data/liveOrder.xlsx")
      if (isTRUE(input$cusPay == "")) {
        0
      } else if (as.numeric(input$cusPay) >= as.numeric(sum(base[4]))) {
        0
      } else {
        (as.numeric(input$cusPay) < as.numeric(sum(base[4])))
        if (input$vat == TRUE) {
          format(
            as.numeric(sum(base[4]) + (sum(base[4]) * 0.1)) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          )
        } else
          format(
            as.numeric(sum(base[4])) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          )
      }
    })
    
    
    
    
    
    
    # Hien thi hinh anh san pham
    
    output$hinh <- renderImage({
      Data_Table <- read.xlsx("data/database.xlsx")
      link <- filter(Data_Table, Product == input$soldProduct)[, 1]
      
      list(src = paste0("www/", link, ".jpg"), width = "280px")
      
    }, deleteFile = FALSE)
    
    
    
    
    #+++++++++++++++++++++++++++THEM SAN PHAM VAO GIO HANG +++++++++++++++++
    
    # hien thi danh sach san pham neu co
    sellproductTable <- function(x) {
      renderDT({
        datatable(
          read.xlsx("data/liveOrder.xlsx")[, 1:6],
          width = "auto",
          selection = "none",
          options = list(
          scrollX = TRUE,
            dom = 't',
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json')
          )
        ) %>%
          formatCurrency(
            3:4,
            currency = "VND ",
            interval = 3,
            mark = ",",
            digits = 0
          )
      })
    }
    
    
    output$liveOrder <- sellproductTable ()
    
    
    observeEvent(input$sold, {
      SPdangco()
      
      
      #Hien thi hinh anh cua tung san pham
      
      
      
      
      
      
      
      
      
      
      if (input$price == "Tự động tính") {
        # Data_Table <- dbReadTable(db, "sanpham")[, 2:11]
        Data_Table <- read.xlsx("data/database.xlsx")
        
        if (input$soldProduct == "") {
          autoPrice <-
            as.numeric(filter(Data_Table, Code == input$CodeID)[6])
        } else
          autoPrice <-
            as.numeric(filter(Data_Table, Product == input$soldProduct)[6])
        
      } else
        autoPrice <- as.numeric(input$otherPrice)
      
      
      
      # Data_Table <- dbReadTable(db, "sanpham")[, 2:11]
      Data_Table <- read.xlsx("data/database.xlsx")
      orderList <- read.xlsx("data/liveOrder.xlsx")
      
      if (input$soldProduct == "") {
        liveOrder <-
          rbind(
            data.frame(
              Item = as.character(filter(Data_Table, Code == input$CodeID)[2]),
              Quantity = as.numeric(input$soldToday),
              Price = as.numeric(autoPrice),
              Total = as.numeric(autoPrice) * as.numeric(input$soldToday),
              Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
              Customer = input$cusName,
              Prepayment = 0,
              VATCost = 0
              
            ),
            orderList
          )
        
        
      } else
        liveOrder <-
        rbind(
          data.frame(
            Item = as.character(filter(
              Data_Table, Product == input$soldProduct
            )[2]),
            Quantity = as.numeric(input$soldToday),
            Price = as.numeric(autoPrice),
            Total = as.numeric(autoPrice) * as.numeric(input$soldToday),
            Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
            Customer = input$cusName,
            Prepayment = 0,
            VATCost = 0
            
          ),
          orderList
        )
      # Neu ten khach hang de trong thi se tu dong la Khach La
      if (input$cusName == "") {
        liveOrder$Customer <- UnknownCustomer
      }
      
      
      write.xlsx(liveOrder, file = "data/liveOrder.xlsx", overwrite = T)
      
      # dbWriteTable(db,
      #              "liveorder",
      #              liveOrder,
      #              append  = TRUE,
      #              row.names = F)
      
      
      ### Hien thi du lieu
      
      # output$liveOrder <-    renderTable(dbReadTable(db, "liveorder")[, 1:7], width = "auto")
      
      
      output$liveOrder <- sellproductTable ()
      #Update ten san pham vao danh sach xoa
      updateSelectInput(
        session,
        "deleteOrder",
        "Chọn tên sản phẩm cần xóa",
        choices = read.xlsx("data/liveOrder.xlsx")[, 1]
      )
      
      # Begin Table appear on Invoice Tab
      output$customerOrder <- renderDT({
        datatable(
          select(
            read.xlsx("data/liveOrder.xlsx"),
            # dbReadTable(db, "liveorder"),
            Item,
            Quantity,
            Price,
            Total
          ),
          selection = "none",
          options = list(dom = 't')
        ) %>%
          formatCurrency(
            3:4,
            currency = "VND ",
            interval = 3,
            mark = ",",
            digits = 0
          )
      })
      
      # Hien thi tong cua so tien
      
      
      displaySUM <- function(x) {
        # list_Live_Order <- dbReadTable(db,"liveorder")[, 2:6]
        list_Live_Order <- read.xlsx("data/liveOrder.xlsx")
        if (input$vat == TRUE) {
          format(sum(
            list_Live_Order[4] + (list_Live_Order[4] * as.numeric(input$vatValue) / 100)
          ),
          big.mark = ",",
          scientific = F)
        } else
          format(sum(list_Live_Order[4]),
                 big.mark = ",",
                 scientific = F)
      }
      
      
      output$tong <- renderUI({
        displaySUM()
      })
      
      
      updateTextInput(session, "soldToday", value = " ")
      
      
      output$totalSoldTitle <- renderUI({
        # saveSold <- dbReadTable(db, "sold")[, 2:10]
        saveSold <- read.xlsx("data/sold.xlsx")
        totSold <-
          sum(filter(saveSold, Item == input$soldProduct)[2])
        
        paste0(input$soldProduct,
               " đã bán: ",
               as.numeric(totSold),
               " sản phẩm")
      })
    })
    
    
    # Hien thi TOng
    tong <- function(x) {
      renderUI({
        list_Live_Order <- read.xlsx("data/liveOrder.xlsx")
        if (input$vat == TRUE) {
          format(sum(
            list_Live_Order[4] + (list_Live_Order[4] * as.numeric(input$vatValue) / 100)
          ),
          big.mark = ",",
          scientific = F)
        } else
          format(sum(list_Live_Order[4]),
                 big.mark = ",",
                 scientific = F)
      })
    }
    
    output$tong <- tong()
    #Xoa du lieu tung san pham
    observeEvent(input$deleteOrderName, {
      order <- read.xlsx("data/liveOrder.xlsx")
      numName <- which(order$Item == input$deleteOrder)
      reMove <- order[-c(numName), ]
      
      write.xlsx(reMove, file = "data/liveOrder.xlsx", overwrite = T)
      updateSelectInput(
        session,
        "deleteOrder",
        "Chọn tên sản phẩm cần xóa",
        choices = read.xlsx("data/liveOrder.xlsx")[, 1]
      )
      
      shinyjs::hide("warning")
      
      sendSweetAlert(
        session = session,
        title = paste0("Sản phẩm ", input$deleteOrder, " đã xóa"),
        btn_labels = "Xong"
      )
      
      output$liveOrder <- sellproductTable ()
      
      output$tong <- tong()
      
    })
    
    
    
    # table <-select(dbReadTable(db, "sold"), id, Item:Date, Employee)
    # table <- table[order(table$id, decreasing = TRUE), ]
    table <- read.xlsx("data/sold.xlsx")
    output$TableSold <-   renderDT(server = FALSE, {
      datatable(
        table[, 1:6],
        extensions = 'Buttons',
        option = list(
          searching = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          dom = 'Bfrtip'
        )
      )
      
    })
    
    
    
    
    
    
    #++++++++++++++++++++++++++THANH TOAN+++++++++++++++++++++++++++++++++++++
    
    observeEvent(input$ask2pay, {
      shinyjs::show("pay")
    })
    
    
    observeEvent(input$pay, {
      tryCatch({
        # base <- dbReadTable(db, "liveorder")[2:6]
        base <- read.xlsx("data/liveOrder.xlsx")
        
        # Cap nhat hoa don theo VAT
        
        if (input$vat == TRUE) {
          # base$Customer = as.character(input$cusName)
          # base$Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          base$Prepayment <- as.numeric(input$cusPay)
          base$VATCost <-
            (sum(base[4]) * as.numeric(input$vatValue) / 100)
          
        } else
          # base$Customer = as.character(input$cusName)
          # base$Date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          base$Prepayment <- as.numeric(input$cusPay)
        base$VATCost <- 0
        
        # Neu ten khach hang de trong thi se tu dong la Khach La
        if (input$cusName == "") {
          base$Customer <- UnknownCustomer
        }
        
        
        # Luu lai sach sach cac san pham da thanh toan trong SoLD
        listsold <- read.xlsx("data/sold.xlsx")
        addSold <- rbind(base, listsold)
        write.xlsx(addSold, file = "data/sold.xlsx", overwrite = T)
        # dbWriteTable(db,
        #              "sold",
        #              base,
        #              append  = TRUE,
        #              row.names = F)
        
        
        # Neu khach hang tra tien dang thieu thi thong tin ca nhan la bat buoc
        if (as.numeric(input$cusPay) < as.numeric(sum(base[4]))) {
          shinyjs::show("debtNameID")
          shinyjs::show("phoneNo")
          shinyjs::show("CMND")
          shinyjs::show("saveInfor")
          
          
          # Luu thong tin khach hang dang no tien vao danh sach
          
          observeEvent(input$saveInfor, {
            load("data/activeMember.Rda")
            
            if (input$vat == TRUE) {
              debt <-
                data.frame(
                  base[, 1:4],
                  Paid = input$cusPay,
                  NoVAT = 0,
                  VATPrice = as.numeric(sum(base[4]) + (
                    sum(base[4]) * as.numeric(input$vatValue) / 100
                  )) - as.numeric(input$cusPay),
                  Customer = input$debtNameID,
                  Phone = input$phoneNo,
                  CMND = input$CMND,
                  Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
                  Employee = name
                )
              
            } else
              
              debt <-
                data.frame(
                  base[, 1:4],
                  Paid = input$cusPay,
                  NoVAT = as.numeric(sum(base[4])) - as.numeric(input$cusPay),
                  VATPrice = 0,
                  Customer = input$debtNameID,
                  Phone = input$phoneNo,
                  CMND = input$CMND,
                  Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
                  Employee = name
                )
            oldSold <- read.xlsx("data/debtlist.xlsx")
            newSold <- rbind(debt, oldSold)
            write.xlsx(newSold, file = "data/debtlist.xlsx", overwrite = T)
            # dbWriteTable(db,
            #              "listdebt",
            #              debt,
            #              append  = TRUE,
            #              row.names = F)
            
            sendSweetAlert(
              session = session,
              title = paste0("Đơn hàng ", input$debtNameID, " đã đặt"),
              text = paste0("Khoản nợ ", money, " VND đã đươc lưu trên hệ thống"),
              type = "warning ",
              btn_labels = "Xong"
            )
            
            # Sau khi cap nhat thong tin khach hang dang no, he thong chuyen sang trang hoa don
            updateTabItems(session, "tabs", selected = "invoice")
            
          })
          
          
          # Hien thi so tien no khach hang trong thong bao
          if (input$vat == TRUE) {
            money <- format(
              as.numeric(sum(base[4]) + (
                sum(base[4]) * as.numeric(input$vatValue) / 100
              )) - as.numeric(input$cusPay),
              big.mark = ",",
              scientific = F
            )
          } else
            money <-  format(
              as.numeric(sum(base[4])) - as.numeric(input$cusPay),
              big.mark = ",",
              scientific = F
            )
          
          sendSweetAlert(
            session = session,
            title = paste0("Đơn hàng đang nợ ", money, " VND"),
            text = "Thông tin khách hàng là bắt buộc" ,
            type = "warning ",
            btn_labels = "Vui lòng thêm thông tin khách hàng"
          )
          
          
        } else {
          # Neu khach hang tra tien nhieu hon so luong thi chuyen sang trang hoa don
          updateTabItems(session, "tabs", selected = "invoice")
          
          sendSweetAlert(
            session = session,
            title = paste0("Đơn hàng ", input$cusName, " đã đặt"),
            type = "success",
            btn_labels = "Tạo hóa đơn",
          )
        }
        
        
        # Tru so luong trong kho
        
        # Data_Table<- dbReadTable(db, "sanpham")
        # Data_Table<-Data_Table[order(Data_Table$id, decreasing = TRUE), ]
        Data_Table <- read.xlsx("data/database.xlsx")
        
        Ban <-
          dplyr::filter(Data_Table, Product == input$soldProduct)
        Ban[3]  <-
          as.numeric(Ban[3]) - sum(filter(base, Item == input$soldProduct)[2])
        
        Data_Table <-
          Data_Table[-c(which(Data_Table$Product == input$soldProduct)), ]
        
        Update_Data_Table <- rbind(Ban, Data_Table)
        
        write.xlsx(Update_Data_Table,
                   file = "data/database.xlsx",
                   overwrite = T)
        # dbWriteTable(db,
        #              "sanpham",
        #              Data_Table,
        #              overwrite  = TRUE,
        #              row.names = F)
        
        
        #++++++++++++++++++Thong bao Loi khi thanh toan ++++++++++++++++++++++++
        
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(
              paste0(
                "VUI LÒNG NHẬP [ SỐ TIỀN THANH TOÁN TRẢ TRƯỚC ] CỦA KHÁCH HÀNG VÀO"
              )
            )
            ,
            title = "Lỗi chưa thêm đầy đủ thông tin" ,
            footer = modalButton("Bỏ qua"),
            easyClose = T
          )
        )
        return()
      })
      
    })
    
    
    
    # Hien thi so luong san pham da ban
    output$totalSoldTitle <- renderUI({
      order <- read.xlsx("data/sold.xlsx")
      # order <- dbReadTable(db, "sold")
      
      totSold <-
        sum(filter(order, Item == input$soldProduct)[2])
      paste0(input$soldProduct, " đã bán: ", totSold, " sản phẩm")
    })
    
    
    # HIen thi danh sach khach hang dang no tien
    # debtPeople <- dbReadTable(db, "sold")
    # debtPeople <- read.xlsx("data/sold.xlsx")
    # debtPeople <-
    #   filter(debtPeople, Customer == debtPeople[6])
    #
    # output$debtList <- renderDT(
    #   format(
    #     data.frame(
    #       'Tên' = debtPeople[, 6],
    #       'Còn' = debtPeople[, 7] + debtPeople[, 8],
    #       "VND" = "VND",
    #       SDT = debtPeople[, 9]
    #     ),
    #     big.mark = ",",
    #     scientific = F
    #   ),
    #
    #   options = list(
    #     dom = 'ft',
    #     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json')
    #   )
    # )
    
    
    
    
    #########################TRANG  HOA DON #################################
    # Ten Thanh Pho
    address <- read.xlsx("data/address.xlsx")
    observe({
      updateSelectizeInput(session,
                           "city" ,
                           # choices =  dbReadTable(db, "address")[1],
                           choices =  address[1],
                           selected = "")
    })
    # Danh sach quan/huyen
    observe({
      updateSelectizeInput(
        session,
        "district" ,
        choices =  filter(address, City == input$city)[2],
        selected = ""
      )
    })
    # Danh sach xom
    observe({
      updateSelectizeInput(
        session,
        "Commune" ,
        choices =  filter(address,
                          City == input$city &
                            District ==  input$district)[3],
        selected = ""
      )
    })
    
    # Hien thi Box de nhap so tien khac
    observe(if (input$price == "Tự động tính") {
      shinyjs::hide("otherPrice")
    } else
      shinyjs::show("otherPrice"))
    
    
    
    
    #+++++++++++++++++++++Bat dau thong tin o hoa don +++++++++++++++++++++++
    
    output$sumPrice <- renderUI({
      # base <- dbReadTable(db, "liveorder")[2:6]
      base <- read.xlsx("data/liveOrder.xlsx")
      
      paste0("(1) Tổng trước thuế = ",
             format(sum((base)[4]), big.mark = ",", scientific = F),
             " VND")
    })
    
    # Tao phuong trinh khi dong thue VAT va khong thue
    vatCalculate <- function(withVAT, withOut) {
      if (input$vat == TRUE) {
        withVAT
      } else
        withOut
    }
    
    # So tien truoc va sau thue
    output$vat <-  renderUI({
      # base <- dbReadTable(db, "liveorder")[2:6]
      base <- read.xlsx("data/liveOrder.xlsx")
      # order <- loadOrder("quan","tempreceipt")
      vatCalculate(
        paste0(
          "(2) Thuế ",
          as.numeric(input$vatValue),
          "% VAT = ",
          format(
            sum(base[4]) * as.numeric(input$vatValue) / 100,
            big.mark = ",",
            scientific = F
          ),
          " VND"
        ),
        paste0("(2) Thuế ", as.numeric(input$vatValue), "% VAT = ",
               "0 VND")
      )
    })
    
    # Hien thi so tien khuyen mai
    output$promotion <- renderUI({
      # base <- dbReadTable(db, "liveorder")[2:6]
      base <- read.xlsx("data/liveOrder.xlsx")
      # order <-loadOrder("quan","tempreceipt")
      vatCalculate(
        paste0(
          "(3) Khuyến mãi (",
          input$promote,
          "%)",
          " = ",
          format((sum((base)[4]) + (
            sum(base[4]) * as.numeric(input$vatValue) / 100
          )) * (input$promote / 100),
          big.mark = ",",
          scientific = F
          ),
          " VND"
          
        ),
        paste0(
          "(3) Khuyến mãi (",
          input$promote,
          "%)",
          " = " ,
          format(
            sum((base)[4]) * (input$promote / 100),
            big.mark = ",",
            scientific = F
          ),
          "VND"
          
        )
      )
    })
    
    # Tao duong line
    output$lines <- renderUI({
      "______________________________________________"
    })
    
    
    # Tong thanh toan sau thue va khuyen mai
    output$PriceVAT <-
      renderUI({
        # base <- dbReadTable(db,"liveorder")[2:6]
        base <- read.xlsx("data/liveOrder.xlsx")
        # order <- loadOrder("quan","tempreceipt")
        vatCalculate(
          paste0(
            "Tổng thanh toán (1) + (2) - (3) = ",
            format((sum((base)[4]) + (
              sum(base[4]) * as.numeric(input$vatValue) / 100
            )) - ((sum((
              base
            )[4]) + (
              sum(base[4]) * as.numeric(input$vatValue) / 100
            )) * (input$promote / 100)),
            big.mark = ",",
            scientific = F
            ),
            " VND"
          ),
          paste0(
            "Tổng thanh toán (1) + (2) - (3) = ",
            format((sum((base)[4]) + 0) - ((sum((
              base
            )[4]) + 0) * (input$promote / 100)),
            big.mark = ",",
            scientific = F
            ),
            " VND"
          )
        )
      })
    
    
    # Hien thi so tien khach hang da tra
    output$repayment <- renderUI({
      # order <- loadOrder("quan","tempreceipt")
      paste0(
        "Khách hàng đã trả trước ",
        format(
          as.numeric(input$cusPay),
          big.mark = ",",
          scientific = F
        ),
        " VND"
      )
    })
    
    # Hien thi so tien can tra lai khach hang
    output$remainPay <- renderUI({
      base <- read.xlsx("data/liveOrder.xlsx")
      # base <- dbReadTable(db, "liveorder")[2:6]
      
      
      
      # order <- loadOrder("quan","tempreceipt")
      if (isTRUE(as.numeric(input$cusPay) < as.numeric(sum(base[4])))) {
        hoadonNo <- "Khách hàng đang nợ chúng tôi số tiền là: "
      } else
        hoadonNo <- "Trả lại khách hàng: "
      
      vatCalculate(paste0(
        hoadonNo,
        format(
          as.numeric(input$cusPay) - as.numeric((sum((
            base
          )[4]) + (
            sum(base[4]) * as.numeric(input$vatValue) / 100
          )) - ((
            sum((base)[4]) +  (sum(base[4]) * as.numeric(input$vatValue) / 100)
          ) * (
            input$promote / 100
          ))) ,
          big.mark = ",",
          scientific = F
        ),
        " VND"
      ),
      
      paste0(
        hoadonNo,
        format(
          as.numeric(input$cusPay) - as.numeric((sum((
            base
          )[4]) + 0) - ((sum((base)[4]
          ) +  0) * (
            input$promote / 100
          ))) ,
          big.mark = ",",
          scientific = F
        ),
        " VND"
      ))
      
    })
    
    
    # Mot so thong tin tren hoa don
    output$company <- renderUI({
      "Quà Quê Đất Việt"
    })
    output$companyAddress1 <- renderUI({
      " Đ/c 1: QL1A - T.T.Nghèn, Can Lộc, Hà Tĩnh"
    })
    output$companyAddress2 <- renderUI({
      "  Đ/c 2: 123 Đường Phố Hiến, Tp. Hưng Yên"
    })
    output$companyAddress3 <- renderUI({
      " Đ/c 3: 80 đường số 7, P. Bình Hưng Hòa, Q. Bình Tân, TPCM"
    })
    output$companyAddress4 <- renderUI({
      " ĐT: (028)-6272 2381 - Mobile: 097.555.6469 - www.quaquedatviet.com"
    })
    output$reciept <- renderUI({
      "HÓA ĐƠN"
    })
    
    # Ten khach hang in tren hoa don
    output$cusName <- renderUI({
      base <- read.xlsx("data/liveOrder.xlsx")
      # base <- dbReadTable(db, "liveorder")
      if (input$debtNameID == "") {
        paste0("Tên khách hàng: ",  base[1, 6])
      } else
        paste0("Tên khách hàng: ",  input$debtNameID)
    })
    #Thong tin dia chi SDT cua khach hang tren hoa don
    output$cusInfor <- renderUI({
      paste0(
        "Đia chỉ: ",
        input$Commune,
        " - ",
        input$district ,
        " - ",
        input$city,
        " ||  Số điện thoại: ",
        input$phone
      )
    })
    
    # Chu ky nguoi mua va nguoi ban
    output$buyerSign <- renderUI({
      "Người mua (Đã Ký)"
    })
    output$sellerSign <- renderUI({
      "Người bán (Đã Ký)"
    })
    
    # Ten nguoi mua hang, neu de trong thi tu dong dat ten la Khach La hoac ten nguoi no
    output$buyer <- renderUI({
      # base <- dbReadTable(db, "liveorder")
      base <- read.xlsx("data/liveOrder.xlsx")
      if (input$debtNameID == "") {
        base[1, 6]
      } else
        input$debtNameID
    })
    
    # Ten nguoi ban
    output$seller <- renderUI({
      load("data/activeMember.Rda")
      name
      
    })
    
    # Ngay ban
    output$date1 <- renderUI({
      read.xlsx("data/liveOrder.xlsx")[1, 5]
      
      
    })
    output$date2 <- renderUI({
      read.xlsx("data/liveOrder.xlsx")[1, 5]
      
    })
    
    
    # Tao ma so hoa don tu dong
    output$recieptNo <- renderUI({
      no <-
        paste0("Số: ", round(runif(1, min = 00000, max = 10000)), "/", Sys.Date())
      save(no, file = "data/InvoiceNo.Rda")
      no
    })
    
    
    
    # Luu hoa don 1 ban copy vao he thong
    observeEvent(input$updateCustom, {
      tryCatch({
        # TAO 1 ban copy INVOICE va luu vao he thong
        load("data/InvoiceNo.Rda")
        load("data/activeMember.Rda")
        base <- read.xlsx("data/liveOrder.xlsx")
        invoice <- read.xlsx("data/Invoice.xlsx")
        dataCustomer <-
          data.frame(
            NO = no,
            Name =  base[1, 6],
            Phone = input$phone,
            select(base,
                   Item:Total),
            BeforeTax =  format(sum((base)[4]), big.mark = ",", scientific = F),
            VAT = format(base[1, 8], big.mark = ",", scientific = F),
            #CHECK
            Promote =  format((sum((base)[4]) + base[1, 8]) * (input$promote / 100),
                              big.mark = ",",
                              scientific = F
            ),
            TobePaid = format((sum((base)[4]) + base[1, 8]) - ((sum((
              base
            )[4]) + base[1, 8]) * (input$promote / 100)),
            big.mark = ",",
            scientific = F
            ),
            
            Seller =  name,
            # Can sua lai
            Address = paste0(input$Commune, "-", input$district, "-", input$city)
          )
        
        addCus <- rbind(dataCustomer, invoice)
        
        write.xlsx(addCus, file = "data/Invoice.xlsx", overwrite = T)
        
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(paste0("ĐÃ CÓ LỖI XẢY RA"))
            ,
            title = "Hóa đơn không tồn tại" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          )
        )
        return()
      })
      
    })
    
    # Tải lên google drive
    observeEvent(input$upload, {
      withProgress(
        message = 'Vui lòng chờ ! đang tải dữ liệu lên...',
        value = 1,
        {
          tryCatch({
            gs4_auth(cache = ".secrets",
                     email = TRUE,
                     use_oob = TRUE)
            
            database_sheet <-
              "https://docs.google.com/spreadsheets/d/1uw-BOnnicx7hcOPND492G62t9Yh4tMfcV5yFMVMt_Jk/edit?usp=sharing"
            sold_sheet <-
              "https://docs.google.com/spreadsheets/d/1tZKLu_GGrqCmdfBAFs5RJsB16U4jST0TGu-_E1sX3hc/edit?usp=sharing"
            invoice_sheet <-
              "https://docs.google.com/spreadsheets/d/1lxua6hwnHwyzev7XNqKnYeHhyET18wGbodLGpRy7k1g/edit?usp=sharing"
            listdebt_sheet <-
              "https://docs.google.com/spreadsheets/d/1mq_DDGTg_kHnVhaFceMmncgWDMePyQWBNqLrxvlPppk/edit?usp=sharing"
            sms_sheet <-
              "https://docs.google.com/spreadsheets/d/1XOy3S-cdZGGESxW0GU5Vx_BxSKV1hUH7x77O4bjdcH8/edit?usp=sharing"
            
            database <- read.xlsx("data/database.xlsx")
            sold <- read.xlsx("data/sold.xlsx")
            invoice <- read.xlsx("data/Invoice.xlsx")
            listdebt <- read.xlsx("data/debtlist.xlsx")
            sms <- read.xlsx("data/sms.xlsx")
            
            write_sheet(database, database_sheet, sheet = "database")
            write_sheet(sold, sold_sheet, sheet = "sold")
            write_sheet(invoice, invoice_sheet, sheet = "invoice")
            write_sheet(listdebt, listdebt_sheet, sheet = "listdebt")
            write_sheet(sms, sms_sheet, sheet = "sms")
            
            sendSweetAlert(
              session = session,
              title =
                "Dữ liệu đã lưu trên Google Drive "  ,
              type = "success ",
              btn_labels = "Đóng"
            )
         
        },
        error = function(e) {
          showModal(
            modalDialog(
              div(paste0("Vui lòng kết nối Internet")),
              title = "NHẬN DẠNG MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
              footer = modalButton("ĐÓNG"),
              easyClose = T
            )
          )
          return()
        }
      )
      
    })
      
    })
    
    # Huy Hoa don va quay lai mua hang
    observeEvent(input$revokeReceipt, {
      base <- read.xlsx("data/liveOrder.xlsx")
      
      base <- base[0,]
      write.xlsx(base, file = "data/liveOrder.xlsx", overwrite = T)
      
      refresh()
      updateTabItems(session, "tabs", selected = "sellData")
      
      
    })
    
    
    # In hoa don
    observeEvent(input$print, {
      js$winprint()
      base <- read.xlsx("data/liveOrder.xlsx")
      
      base <- base[0,]
      write.xlsx(base, file = "data/liveOrder.xlsx", overwrite = T)
      
      
    })
    
    
    
    
    
  })
  
  
  ##############################QUAN LY NO #####################################
  # debtName <- dbReadTable(db, "listdebt")
  debtName <- read.xlsx("data/debtlist.xlsx")
  
  observe({
    updateSelectInput(session, "debtList", choices = debtName[, 8])
  })
  observe({
    updateSelectInput(session,
                      "phoneID",
                      choices = filter(debtName, Customer == input$debtList)[, 9])
  })
  observe({
    updateSelectInput(
      session,
      "CMNDID",
      choices = filter(debtName, Customer == input$debtList &
                         Phone == input$phoneID)[, 10]
    )
  })
  
  observe({
    Amount <-
      filter(debtName,
             Customer == input$debtList &
               Phone == input$phoneID &
               CMND == input$CMNDID)
    output$amountDebt <- renderUI({
      paste0(format(
        Amount[1, 6] + Amount[1, 7],
        big.mark = ",",
        scientific = F
      ),
      " VND")
    })
    
    output$dateDebt <- renderUI({
      Amount[1, 11]
    })
    
    
    
    output$individualDebt <- renderDT({
      datatable(
        select(
          filter(
            debtName,
            Customer == input$debtList &
              Phone == input$phoneID & CMND == input$CMNDID
          )
          ,
          Item:Total,
          Date
        ),
        extensions = 'Buttons',
        option = list(
          searching = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          dom = 'Bfrtip',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json')
        )
      ) %>%
        formatCurrency(
          3:4,
          currency = "VND ",
          interval = 3,
          mark = ",",
          digits = 0
        )
    })
    
    
    # output$lines2 <- renderUI({
    #   "________________________________________________________________________"
    # })
    output$totalAmount <- renderUI({
      paste0("Tổng số tiền sản phầm là: ", sum(
        filter(
          debtName,
          Customer == input$debtList &
            Phone == input$phoneID &
            CMND == input$CMNDID
        )[4]
      ), " VND")
      
    })
    output$paid <- renderUI({
      paste0(
        "Đã trả trước: ",
        filter(
          debtName,
          Customer == input$debtList &
            Phone == input$phoneID &
            CMND == input$CMNDID
        )[1, 5],
        " VND"
      )
    })
    output$VATDebt <- renderUI({
      VAT <- as.numeric(input$vatValue)
      Amount <-
        filter(
          debtName,
          Customer == input$debtList &
            Phone == input$phoneID & CMND == input$CMNDID
        )
      paste0(
        "Còn lại: ",
        format(
          Amount[1, 6] + Amount[1, 7],
          big.mark = ",",
          scientific = F
        ),
        " VND (có thể đã bao gồm ",
        as.numeric(input$vatValue) / 100,
        "% thuế nếu có)"
      )
    })
    
  }) # End of observe
  
  
  #XOa no dua vao ten khach hang
  observeEvent(input$returnDebt, {
    # debtName <- dbReadTable(db, "listdebt")
    debtName <- read.xlsx("data/debtlist.xlsx")
    numName <- which(
      debtName$Customer == input$debtList &
        debtName$Phone == input$phoneID &
        debtName$CMND == input$CMNDID
    )
    reMove <- debtName[-c(numName), ]
    
    write.xlsx(reMove, file = "data/debtlist.xlsx", overwrite = T)
    # dbWriteTable(db,
    #              "listdebt",
    #              reMove,
    #              overwrite  = T,
    #              row.names = F)
    updateSelectInput(session, "debtList", choices = reMove[, 8])
    updateSelectInput(session, "phoneID", choices = reMove[, 9])
    updateSelectInput(session, "CMNDID", choices = reMove[, 10])
    sendSweetAlert(
      session = session,
      title = paste0("Số nợ của khách hàng ", input$debtList, " đã thanh toán"),
      text = "Thông tin khách hàng đã được xóa trên hệ thống" ,
      type = "warning ",
      btn_labels = "Đóng"
    )
    
  })
  
  
  
  
  #####################QUAN LY TIN NHAN#############################
  
  plotSms <- function(x) {
    renderDT(read.xlsx("data/sms.xlsx"),
             width = "100%" ,
             options = list(dom = 't'))
  }
  
  observeEvent(input$sendSms, {
    oldsms <-  read.xlsx("data/sms.xlsx")
    smsTable <- data.frame(
      Name = as.character(input$receiver),
      Subject = input$theme,
      Details = as.character(input$writeSms),
      Date = format(Sys.time(), "%d-%m-%Y %H:%M")
    )
    
    newsms <- rbind(smsTable, oldsms)
    write.xlsx(newsms, file = "data/sms.xlsx", overwrite = T)
    # dbWriteTable(db,
    #              "sms",
    #              smsTable,
    #              append  = T,
    #              row.names = F)
    
    output$messageList <- plotSms()
    
    output$messageMenu <- renderMenu({
      messageData <-
        data.frame(from = smsTable[, 1],
                   message = smsTable[, 3])
      
      msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]])
      })
      
      # This is equivalent to calling:
      dropdownMenu(
        type = "messages",
        .list = msgs,
        badgeStatus = "primary",
        icon = icon("envelope-open-text"),
        headerText = "Tin nhắn"
      )
      # dropdownMenu(type = "messages", .list = msgs)
    })
  })
  
  
  # Lay thoi gian tin nhan lau hon 30 ngay
  #
  # oldSms$timeOut <-
  #   as.POSIXlt(Sys.time(), tryFormats = "%Y-%m-%d %H:%M") -
  #   as.POSIXlt.character(select(oldSms, Date), tryFormats = "%d-%m-%Y %H:%M")
  
  # He thong se xoa tin nhan sau 30 ngay
  # 	if(isTRUE(oldSms$timeOut > OutSms)) {
  #     removeSms <- which(oldSms$timeOut > OutSms)
  #     remaindDay <- oldSms[-c(removeSms),]
  #     write.xlsx(remaindDay, file = "data/sms.xlsx", overwrite = T)
  # 	}
  # Hien thi Table khi chua nhan nut GUi
  # smsTable <- dbReadTable(db,"sms"
  
  output$messageList <- plotSms()
  
  
  updateSelectInput(session, "removeSms", choices = read.xlsx("data/sms.xlsx")[3])
  
  
  # Xoas Tin nhan
  observeEvent(input$smsDel, {
    sms <- read.xlsx("data/sms.xlsx")
    numName <- which(sms$Details == input$removeSms)
    reMove <- sms[-c(numName), ]
    
    write.xlsx(reMove, file = "data/sms.xlsx", overwrite = T)
    
    updateSelectInput(session, "removeSms", choices = read.xlsx("data/sms.xlsx")[3])
    
    sendSweetAlert(
      session = session,
      title = paste0(
        "Tin nhắn của ",
        filter(sms, Details == input$removeSms)[1],
        " vào ngày ",
        filter(sms, Details == input$removeSms)[4],
        " đã bị xóa"
      ),
      type = "warning ",
      btn_labels = "Đóng"
    )
    output$messageList <- plotSms()
    
  })
  
  
  ###############################PHUC HOI DU LIEU #############################
  
  observeEvent(input$loginData, {
    if (input$askPass == "1989") {
      shinyjs::show("backupData")
      shinyjs::show("restoreData")
      shinyjs::show("dateData")
      shinyjs::hide("loginData")
      shinyjs::hide("askPass")
      
    } else
      sendSweetAlert(session = session,
                     title = "Sai mật khẩu !",
                     type = "error")
    
  })
  
  observeEvent(input$backupData, {
    withProgress(message = 'Vui lòng chờ ! đang tải dữ liệu lên...', value = 1, {
      
      database <- read.xlsx("data/database.xlsx")
      debt <- read.xlsx("data/debtlist.xlsx")
      invoice <- read.xlsx("data/Invoice.xlsx")
      sold <- read.xlsx("data/sold.xlsx")
      sms <- read.xlsx("data/sms.xlsx")
      
      old <- read.xlsx("data/backupdate.xlsx")
      date <- format(Sys.time(), "%d-%m-%Y")
      new <- rbind(date, old)
      write.xlsx(new, file = "data/backupdate.xlsx", overwrite = T)
      
      tryCatch({
        gs4_auth(cache = ".secrets",
                 email = TRUE,
                 use_oob = TRUE)
        database_backup_sheet <-"https://docs.google.com/spreadsheets/d/1tprxcXn-cebnP9DNZpplUp2PDADFcMtsjJeopZqLDuY/edit?usp=sharing"
        sold_backup_sheet <-"https://docs.google.com/spreadsheets/d/1gJfBNDsZCXm9iF3UhMEX_iLwk7pGFMmx5kpMI--xv-Q/edit?usp=sharing"
        invoice_backup_sheet <-"https://docs.google.com/spreadsheets/d/1GyPA1uEqG7gKxzZf0NGdcvQpGuU29JYFlRrCuJ2wCZ8/edit?usp=sharing"
        listdebt_backup_sheet <-"https://docs.google.com/spreadsheets/d/1GYaWI3O6mHnvO3iF2PzOLpF8PhAJ-aJ-iG_wizKh9wU/edit?usp=sharing"
        sms_backup_sheet <-"https://docs.google.com/spreadsheets/d/1Ewa3fevhy1c3PTC8ac-gHpf3YCnOCncl4pM7RMiRR_A/edit?usp=sharing"
        
        write_sheet(database, database_backup_sheet, sheet = date)
        write_sheet(debt, listdebt_backup_sheet, sheet = date)
        write_sheet(invoice, invoice_backup_sheet, sheet = date)
        write_sheet(sold, sold_backup_sheet, sheet = date)
        write_sheet(sms, sms_backup_sheet, sheet = date)
        
        sendSweetAlert(
          session = session,
          title = paste("Dữ liệu được cập nhật vào ngày ", date),
          type = "success"
          
        )
      
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(paste0("Vui lòng kết nối Internet và thử lại sau")),
            title = "NHẬN DẠNG MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          )
        )
        return()
      })
    })
  
   
  })
  
  listday <- read.xlsx("data/backupdate.xlsx")
  updateSelectInput(session,
                    "dateData",
                    "Chọn ngày mà dữ liệu được phục hồi",
                    choices = listday[, 1])
  
  
  observeEvent(input$restoreData, {
    withProgress(message = 'Vui lòng chờ ! đang tải dữ liệu lên...', value = 1, {
      tryCatch({
        gs4_auth(cache = ".secrets",
                 email = TRUE,
                 use_oob = TRUE)
        
        database <-
          read_sheet(database_sheet, sheet = input$dateData)
        debt <- read_sheet(listdebt_sheet, sheet = input$dateData)
        invoice <- read_sheet(invoice_sheet, sheet = input$dateData)
        sold <- read_sheet(sold_sheet, sheet = input$dateData)
        sms <- read_sheet(sms_sheet, sheet = input$dateData)
        
        write_sheet(database, database_backup_sheet)
        write_sheet(debt, listdebt_backup_sheet)
        write_sheet(invoice, invoice_backup_sheet)
        write_sheet(sold, sold_backup_sheet)
        write_sheet(sms, sms_backup_sheet)
        
        write.xlsx(database, file = "data/database.xlsx", overwrite = T)
        write.xlsx(debt, file = "data/debtlist.xlsx", overwrite = T)
        write.xlsx(invoice, file = "data/Invoice.xlsx", overwrite = T)
        write.xlsx(sold, file = "data/sold.xlsx", overwrite = T)
        write.xlsx(sms, file = "data/sms.xlsx", overwrite = T)
        sendSweetAlert(
          session = session,
          title = paste("Dữ liệu ngày ", input$dateData, " đã được phục hồi"),
          type = "success"
          
        )
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(paste0("Dữ liệu được phục hồi trên máy tính cá nhân, tuy nhiên chưa được phục hồi trên GOOGLE DRIVE")),
            title = "MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          )
        )
        return()
      })
    })
 
  })
  
  
}




# Run the application
shinyApp(ui = ui, server = server)
