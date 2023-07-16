

list.of.packages <-
  c(
    "shinydashboard",
    "ggplot2",
    "shiny",
    "DT",
    "shinyjs",
    "plotly",
    "tidyr",
    "ggthemes",
    "dplyr",
    "shinyWidgets",
    "shinyBS",
    "openxlsx",
    "shinycssloaders",
    "googledrive"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)


library(shinydashboard)
library(shiny)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)
library(tidyr)
library(ggthemes)
library(dplyr)
library(shinyWidgets)
library(shinyBS)
# library(openxlsx)
library(shinycssloaders)
library(DBI)
library(RMySQL)
# library(dbx)
# library(googlesheets4)
library(googledrive)
#library(dashboardthemes)


###########################INPUTS ##############################################

version <- "V1.1.1"
updateDate <- "11/12/2021"
change <-
  '
- Thêm chức năng quản lý kho hóa đơn, xuất kho
'

don_vi <- c("Chai",
            "Thùng",
            "Gói",
            "Hộp",
            "Can",
            "Bao",
            "Bì",
            "Gam",
            "Kg",
            "Lít",
            "Lọ",
            "Mg",
            "Cái")

nhom <- c(
  "Rượu",
  "Đồ Ngâm Rượu",
  "Chè/Trà",
  "Thảo Dược",
  "Bia",
  "Thuốc" ,
  "Chai/Hũ",
  "Mật ong",
  "Đồ Nhắm",
  "Bánh Kẹo",
  "Hoa Quả",
  "Quà tặng chung",
  "Nước các loại",
  "Loại khác"
)

jsCode <- 'shinyjs.winprint = function(){window.print();}'

UnknownCustomer <- "Khách Lạ"

ExpDate <- 30

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'Đã có lỗi xảy ra ở chức năng này.'; }
}
"


myFun <- function(n = 5000) {
  do.call(paste0, replicate(10, sample(LETTERS, n, TRUE), FALSE))
}


if (!file.exists("data/settime.rda")) {
  timeStop <-
    "17:00:00"
  save(timeStop, file = "data/settime.rda")
}



#+++++++++++++++++++++++++++++SERVER CONNECTION AERA +++++++++++++++++++++++++++
con <- dbConnect(
  MySQL(),
  host = "127.0.0.1",
  port = 3306,
  username = "root",
  password = "1989",
  dbname = "quan"
)

dataLogin <- dbReadTable(con, "login_user")

listSkin<- as.character(dbReadTable(conn = con, "skin"))

################################################################################
##################################LAYOUTS#######################################

ui <- dashboardPage(
  skin = listSkin,

  dashboardHeader(
    title = "QUÀ QUÊ ĐẤT VIỆT",
    tags$li(
      textInput("user", "", placeholder = "Tên đăng nhập ", width = "120px"),
      class = "dropdown"
    ),
    tags$li(tags$head(tags$style(
      HTML("#user{height: 25px;font-size: 10px;}")
    )), class = "dropdown"),

    tags$li(
      passwordInput("pass", "", placeholder = "Mật khẩu ", width = "120px"),
      class = "dropdown"
    ),
    tags$li(tags$head(tags$style(
      HTML("#pass{height: 25px;font-size: 10px;}")
    )), class = "dropdown"),

    tags$li(uiOutput("welcome"), class = "dropdown"),
    tags$li(
      tags$style("#welcome{color: white; font-size: 16px;font-style: bold}"),
      class = "dropdown"
    ),
    tags$li(actionButton("login",
                         "Đăng nhập",
                         icon = icon("key")), class = "dropdown"),
    tags$li(
      tags$style("#login{background: transparent; color:white ; border:0px}"),
      class = "dropdown"
    ),
    tags$li(actionButton("logout",
                         "Thoát",
                         icon = icon("unlock")), class = "dropdown"),
    tags$li(
      tags$style("#logout{background: transparent; color:white ; border:0px}"),
      class = "dropdown"
    )
  ),




  ###############################SIDEBAR #######################################
  dashboardSidebar(
    tags$style('.skin-blue .main-header .logo {height:60px}'),
    width = 130,
      sidebarMenu(
        id = "tabs",
        menuItem("",
                 tabName = "welcome"),
        menuItem("Thống Kê",
                 tabName = "home" ,
                 icon = icon("chart-pie")),
        menuItem(
          "Kho Hàng",
          tabName = "stockroom" ,
          icon = icon("person-booth")
        ),
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
        menuItem("Hóa đơn",
                 tabName = "invoice",
                 icon = icon("receipt")),
        menuItem(
          "Quản lý nợ",
          tabName = "debt",
          icon = icon("money-bill-wave")
        ),
        menuItem("Quản trị",
                 tabName = "backup",
                 icon = icon("power-off"))
      )

  ),

  ##############################################################################
  #################################BODYWEB######################################
  dashboardBody(
    tags$style('.content-wrapper, .right-side {background-color: white}'),
    tags$style(
      'table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd {background-color: #fff}'
    ),
     shinyjs::useShinyjs(),

    tabItems(
      #Thanh Vien
      tabItem(
        tabName  = "welcome",
        img(
          src = 'logo.jpg',
          align = "center",
          height = "20%",
          width = "20%"
        ),
        textOutput("copyright"),
        align = "center"
      ),
      #++++++++++++++++++++++++++++TRANG THONG KE ++++++++++++++++++++++++++++++

      tabItem(
        tabName  = "home",

        fluidRow(
          column(
            3,
            uiOutput("thongke_nhapvao_text"),
            uiOutput("thongke_nhapvao_no"),
            tags$style('#thongke_nhapvao_no {color: red; font-size: 70px}'),
            align = "center"
          ),
          column(
            3,
            uiOutput("thongke_tong_tien_text"),
            uiOutput("thongke_tong_tien_no"),
            tags$style('#thongke_tong_tien_no {color: red; font-size: 70px}'),
            align = "center"
          ),
          column(
            3,
            uiOutput("thongke_tien_nhan_text"),
            uiOutput("thongke_tien_nhan_no"),
            tags$style('#thongke_tien_nhan_no {color: red; font-size: 70px}'),
            align = "center"
          ),

          column(3)
        ),
        # Thong Ke
        fluidRow(column(
          12,
          plotlyOutput("timeLine") %>% withSpinner(type = 6)

        )),
        fluidRow(column(6,
                        plotOutput("soldAll")),
                 column(6,
                        plotlyOutput("soldGraph"))),

      ),


      #KHOHANG
      #+++++++++++++++++++++++++++TRANG STOCKROOM  +++++++++++++++++++++++++
      tabItem(tabName  = "stockroom",
              tabsetPanel(
                tabPanel(
                  "NHẬP KHO",
                  icon = icon("user-edit"),
                  fluidRow(
                    valueBoxOutput("supplierInfo", width = 3),
                    valueBoxOutput("invoiceInfo", width = 3),
                    valueBoxOutput("totalPrice", width = 3),
                    valueBoxOutput("remainMoney", width = 3)
                  ),

                  fluidRow(
                    column(
                      3,
                      selectInput("supplierList",
                                  " Chọn tên nhà cung cấp",
                                  choices = ""),
                      uiOutput("sdt_supplier_text"),
                      tags$style('#sdt_supplier_text {color: green; font-size: 14px}'),
                      uiOutput("address_supplier_text"),
                      tags$style('#address_supplier_text {color: green; font-size: 14px}'),

                      prettySwitch(
                        inputId = "addsupplier",
                        label = "Hoặc thêm mới nhà cung cấp",
                        status = "success",
                        fill = TRUE
                      ),
                      textInput(
                        "supplier",
                        "Thêm tên mới",
                        value = "",
                        placeholder = "Tên nhà cung cấp"
                      ),
                      tags$style('#supplier {background: yellow}'),
                      textInput(
                        "sdt_supplier",
                        "Số điện thoại",
                        value = "",
                        placeholder = "SDT"
                      ),
                      tags$style('#sdt_supplier {background: yellow}'),
                      textInput(
                        "address_supplier",
                        "Địa chỉ",
                        value = "",
                        placeholder = "Địa chỉ"
                      ),
                      tags$style('#address_supplier {background: yellow}'),
                      textInput("invNo", "Số hóa đơn", value = 0, placeholder = "số bắt buộc..."),
                      dateInput("dateImport", "Hóa đơn ngày mua", format = "dd-mm-yyyy"),
                      selectInput(
                        "listProduct",
                        "Chọn tên sản phẩm",
                        choices = dbReadTable(con, "product")[, 2]
                      ),
                      prettySwitch(
                        inputId = "addSP",
                        label = "Hoặc thêm sản phẩm mới",
                        status = "success",
                        fill = TRUE
                      ),
                      textInput(
                        "addnewSP",
                        "Thêm sản phẩm mới",
                        value = "",
                        placeholder = "Thêm SP nếu danh sách không có..."
                      ),
                      tags$style('#addnewSP {background: aquamarine}'),
                      numericInput(
                        "moneyPerItem",
                        "Giá nhập (bắt buộc)",
                        value = 0,
                        min = 1000,
                        step = 1000
                      ),
                      numericInput(
                        "quantity4",
                        "Số lượng (tự động = 1)",
                        value = 1,
                        min = 0
                      ),
                      selectInput("unit4",
                                  "Đơn vị",
                                  choices = don_vi),
                      actionBttn(
                        "submit",
                        "Thêm vào danh sách",
                        icon = icon("save"),
                        style = "pill",
                        size = "lg",
                        color = "primary",
                        block = F
                      )
                    ),
                    column(
                      9,

                      column(
                        6,

                        numericInput(
                          "paystock",
                          "1 - Nhập số tiền trả trước",
                          value = 0,
                          min = 1000,
                          step = 1000
                        ),
                        tags$head(tags$style(
                          HTML("#paystock{height: 60px;font-size: 50px; font-style: bold}")
                        ))
                      ),
                      column(
                        6,
                        fileInput(
                          "attachInvoice",
                          "2 - Lưu hình hóa đơn (Đặt theo tên khách hàng và số hóa đơn)",
                          accept = c('image/png', 'image/jpeg'),
                          width = "100%"
                        ),
                        actionBttn(
                          "saveData2",
                          "Lưu thông tin",
                          icon = icon("save"),
                          style = "jelly",
                          size = "md",
                          color = "success",
                          block = T
                        )
                      ),

                      DTOutput("khohang"),
           
                      column(2,
                             selectInput("rowDelete", "Invoice Number", choice = "")
                             ),
                      column(4,
                             fluidRow(style = "height:30px"),
                             actionBttn(
                               "rowDelete_button",
                               "Nhập số Invoice muốn xóa vào đây",
                               icon = icon("trash-alt"),
                               style = "jelly",
                               size = "xs",
                               color = "danger",
                               block = F
                             )
                             )
                    )
                  )
                ),
                tabPanel(
                  "XUẤT KHO",
                  icon = icon("dolly"),
                  fluidRow(
                    column(2,
                           selectInput("supplierTable", "Nhà cung cấp", choices = "")),
                    column(2,
                           selectInput(
                             "invoiceTable", "Ngày nhập sản phẩm", choices = ""
                           )),
                    column(2,
                           selectInput("itemTable", "Tên sản phảm", choices = "")),

                    column(
                      2,
                      uiOutput("undone_text"),
                      uiOutput("undone_no"),
                      tags$style('#undone_no {color: red; font-size: 75px}'),
                      align = "center"

                    ),
                    column(
                      2,
                      uiOutput("done_text"),
                      uiOutput("done_no"),
                      tags$style('#done_no {color: green; font-size: 75px}'),
                      align = "center"

                    ),
                    column(2,
                           uiOutput("same_item")),
                  ),
                  fluidRow(dataTableOutput("xuatkho")),

                  fluidRow(
                    column(
                      2,
                      numericInput(
                        "sellquantity",
                        "Số lượng",
                        value = 0,
                        min = 0,
                        step = 1,
                        width = "100%"
                      )
                    ),
                    column(
                      2,
                      numericInput(
                        "sellcost",
                        "Giá bán",
                        value = 0,
                        min = 1000,
                        step = 1000,
                        width = "100%"
                      )
                    ),

                    column(
                      2,
                      selectInput("unitTable",
                                  "Đơn vị",
                                  choices = don_vi,
                                  width = "100%")
                    ),

                    column(
                      2,
                      selectInput(
                        "catalogueStock",
                        "Nhóm",
                        choices = nhom,
                        width = "100%"
                      )
                    ),

                    column(
                      2,
                      dateInput("dateTable", "Hết hạn", format =
                                  "dd-mm-yyyy", width = "100%")
                    ),
                    column(2,
                           fileInput(
                             "imageTable",
                             "Lưu hình",
                             accept = c('image/png', 'image/jpeg'),
                             width = "100%"
                           ))
                  ),
                  fluidRow(column(
                    12,
                    fluidRow(style = "height:5px;"),
                    actionBttn(
                      "save123",
                      "Lưu dữ liệu và tự động thêm sản phẩm này vào gian hàng",
                      icon = icon("save"),
                      style = "fill",
                      size = "md",
                      color = "primary",
                      block = T
                    )
                  )),
                  fluidRow(icon("chevron-down"),
                           align = "center"),

                  dataTableOutput("xuatkho_xong")

                ),
                tabPanel(
                  "QUẢN LÝ ĐƠN",
                  icon = icon("chalkboard-teacher"),
                  tabsetPanel(
                    tabPanel(
                      div(HTML("<em>Chi tiết cho mỗi hóa đơn</em>")),
                      icon = icon("dolly-flatbed"),
                      fluidRow(column(
                        9,
                        box(
                          width = "100%",
                          solidHeader  = F,
                          valueBoxOutput("total2", width = 4),
                          valueBoxOutput("prePaid2", width = 4),
                          valueBoxOutput("remain2", width = 4)
                        ),
                        box(
                          width = "100%",
                          status  = "primary",
                          collapsible = T,
                          collapsed = F,
                          title = span(icon("scroll"), "Tóm tắt thông tin về hóa đơn đã chọn"),
                          DTOutput("summaryInvoice")
                        ),
                        box(
                          width = "100%",
                          status  = "primary",
                          collapsible = T,
                          collapsed = T,
                          title = span(icon("list-ul"), "Hiển thị chi tiết hóa đơn đã chọn"),
                          DTOutput("detailInvoice")
                        ),
                        box(
                          width = "100%",
                          status  = "warning",
                          collapsible = T,
                          collapsed = T,
                          title = span(
                            icon("list-ul"),
                            "Hiển thị tất cả các thông tin hóa đơn liên quan về nhà cung cấp này"
                          ),
                          DTOutput("allInvoice")
                        )
                      ),
                      column(
                        3,
                        box(
                          background =  listSkin,
                          width = "100%",
                          selectInput("supplierID", "Tên nhà cung cấp", choices = ""),
                          selectInput("invoiceID", "Số hóa đơn", choices = "")
                        ),
                        box(
                          background =  listSkin,
                          width = "100%",
                          title = span(icon("file-invoice-dollar"), "Quản lý Hóa Đơn"),
                          numericInput(
                            "return",
                            "Nhập số tiền để trả (VND)",
                            min = 1000,
                            value = "",
                            step = 1000
                          ),
                          tags$head(tags$style(
                            HTML(
                              "#return{height: 70px;font-size: 45px; color: red; font-style: bold}"
                            )
                          )),
                          actionBttn(
                            "returnMoney",
                            "Thanh toán",
                            icon = icon("credit-card"),
                            style = "jelly",
                            size = "md",
                            color = "warning",
                            block = T
                          )
                        ),
                        box(
                          width = "100%",
                          status = "danger",
                          imageOutput("hinhHoadon", width = "140px", height = "220px")
                        )
                      ))
                    ),

                    tabPanel(div(HTML(
                      "<em>Hiện thị chung các hóa đơn</em>"
                    )),
                    icon = icon("tasks"),
                    column(
                      6,
                      box(
                        title = span(icon("hourglass-end"), "Danh sách Hóa Đơn đang nợ nhà cung cấp"),
                        width = "100%",
                        status = "danger",
                        DTOutput("owningSupplierTable")
                      )
                    ),
                    column(
                      6,
                      box(
                        title = span(icon("handshake"), "Danh sách Hóa Đơn đã thanh toán hết"),
                        width = "100%",
                        status = "success",
                        DTOutput("paidSupplierTable")
                      )
                    ))
                  )
                )
              )),


      #+++++++++++++++++++++++++++TRANG NHAP DU LIEU +++++++++++++++++++++++++
      tabItem(tabName =  "importData",

              fluidRow(
                column(
                  3,
                  textInput("ID", "Mã sản phẩm (tự động = 10 ký tự)", value = myFun(1)),
                  textInput("name", "Tên sản phẩm", placeholder = "Bắt buộc.."),
                  numericInput(
                    "quantity",
                    "Số lượng (Tự động = 1)",
                    value = 1,
                    min = 0
                  ),
                  selectInput("unit",
                              "Đơn vị",
                              choices = don_vi),
                  numericInput(
                    "importPrice",
                    "Giá nhập (Không cần)",
                    value = 0,
                    min = 1000,
                    step = 1000
                  ),
                  numericInput(
                    "soldPrice",
                    "Giá bán/SP (Bắt buộc)",
                    value = 0,
                    min = 1000,
                    step = 1000
                  ),
                  selectInput("catalogue",
                              "Loại",
                              choices = nhom),
                  dateInput("expireDate", label = "Ngày hết hạn (Không cần)"),
                  fileInput("image",
                            "Hình .jpg",
                            accept = c('image/png', 'image/jpeg')),
                  actionBttn(
                    "save",
                    "Lưu dữ liệu",
                    icon = icon("save"),
                    style = "bordered",
                    size = "lg",
                    color = "success",
                    block = F
                  )
                ),
                column(9,
                       tabsetPanel(
                         tabPanel(
                           "Nhập dữ liệu",
                           icon = icon("database"),
                           shinydashboard::box(DT::DTOutput("table"),
                                               width = "100%",
                                               height = "100%"),
                           box(
                             fileInput(
                               "loadImportData",
                               "Chọn dữ liệu file Excel (Tên cột phải giống như trên)",
                               accept = ".xlsx",
                               width  = "100%",
                               buttonLabel = "File Excel",
                               placeholder = "Định dạng: [Code] [Product] [Quantity] [Unit] [Import] [Sell] [Catalogue] [Date] [Link] [Expired] "
                             ),

                             tableOutput("contents"),
                             width = "100%",
                             height = "100%"
                           )

                         ),
                         tabPanel(
                           "Sửa dữ liệu",
                           icon = icon("edit"),
                           passwordInput(
                             "passEditData",
                             "Mật khẩu đăng nhập",
                             value = "",
                             placeholder = "Nhập mật khẩu ..."
                           ),
                           actionBttn(
                             "loginImportdata",
                             "Đăng nhập",
                             icon = icon("key"),
                             size = "sm",
                             style = "simple",
                             color = "primary"
                           ),

                           fluidRow(DT::DTOutput("table2")),
                           column(
                             6,
                             selectInput("removeRow", "Chọn tên sản phầm cần xóa", choices = " ")
                           ),
                           column(
                             6,
                             selectInput("sp2", "Chọn tên sản phầm cần thay hình", choices = " "),
                             fileInput(
                               "image2",
                               "Cập nhật hình (.jpg) cho sản phẩm",
                               accept = c('image/png', 'image/jpeg')
                             )

                           ),

                           fluidRow(
                             actionBttn(
                               "xoaDulieu",
                               "Xóa dữ liệu",
                               icon = icon("trash-alt"),
                               size = "xs",
                               style = "simple",
                               color = "danger"
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
                             ),
                             align = "center",
                             tags$head(tags$style(
                               "#deleteProduct .modal-footer{ display:none}"
                             )),
                             actionBttn(
                               "save2",
                               "Lưu dữ liệu",
                               icon = icon("save"),
                               size = "xs",
                               style = "simple",
                               color = "success"
                             )
                           )
                         )
                       ))

              )),


      #+++++++++++++++++++++++++++++BAN SAN PHAM +++++++++++++++++++++++++++++

      tabItem(tabName =  "sellData",
              fluidRow(
                column(
                  3,
                  box(
                    background =  listSkin,
                    width = "100%",
                    selectInput("soldProduct", "Chọn tên sản phẩm", ""),
                    materialSwitch(
                      inputId = "openEdit",
                      label = "Thêm sản phẩm mới",
                      value = FALSE,
                      right = TRUE,
                      status = "info"
                    ),
                    actionBttn (
                      "addProduct",
                      "Thêm sản phẩm",
                      icon = icon("edit"),
                      style  = 'material-flat',
                      color = "default",
                      size = "xs",
                      block = T
                    ),

                    bsModal(
                      "addPro",
                      "Thêm sản phẩm mới",
                      "addProduct",
                      size = "small",

                      textInput("ID2", "Mã sản phẩm (tự động = 10 ký tự)", value = myFun(1)),
                      textInput("name2", "Tên sản phẩm", placeholder = " bắt buộc"),
                      numericInput(
                        "quantity2",
                        "Số lượng (tự động = 1)",
                        value = 1,
                        min = 0
                      ),
                      selectInput("unit2",
                                  "Đơn vị",
                                  choices = don_vi),
                      numericInput(
                        "importPrice2",
                        "Giá nhập (không cần)",
                        value = "",
                        min = 1000,
                        step = 1000
                      ),

                      numericInput(
                        "soldPrice2",
                        "Giá bán (bắt buộc)",
                        value = "",
                        min = 1000,
                        step = 1000
                      ),
                      selectInput("catalogue2",
                                  "Loại",
                                  choices = nhom),
                      dateInput("expireDate2", label = "Ngày hết hạn (không cần)"),
                      fileInput(
                        "image2",
                        "Hình jpg (không cần)",
                        accept = c('image/png', 'image/jpeg')
                      ),
                      actionBttn(
                        "save_newdata",
                        "Lưu dữ liệu",
                        icon = icon("save"),
                        style = "unite",
                        size = "md",
                        color = "success",
                        block = T
                      )
                    ),
                    tags$head(tags$style("#addPro .modal-footer{ display:none}")),
                    tags$style('.modal-body {background: cadetblue}'),
                    selectInput("CodeID", "Hoặc Code sản phẩm", ""),
                    uiOutput("stock"),
                    uiOutput("totalSoldTitle")
                  ),

                  numericInput(
                    "soldToday",
                    "Số lượng bán (tự động = 1)",
                    value = 1 ,
                    min = 0
                  ),
                  tags$head(tags$style(
                    HTML("#soldToday{height: 40px;font-size: 25px; font-style: bold}")
                  )),

                  awesomeRadio(
                    inputId = "price",
                    label = "Hình thức",
                    choices = c("Auto", "Khuyến mãi", "Tặng/Cho"),
                    selected = "Auto",
                    inline = TRUE,
                    status = "success"
                  ),
                  sliderInput(
                    "discount",
                    "",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 0,
                    post = " %",
                    pre = "Giảm ",
                    width = "100%"
                  ),

                  actionBttn(
                    "sold",
                    "Thêm vào giỏ",
                    icon = icon("shopping-bag"),
                    size = "md",
                    style = "pill",
                    block = T,
                    color = "success"
                  )
                ),
                column(
                  7,
                  box(
                    title = span(icon("cart-plus"), "Giỏ Hàng"),
                    width = "100%",

                    uiOutput("warning"),
                    tags$style("#warning{color: red; font-style: regular}"),
                    DTOutput('liveOrder'),
                    column(6,
                           selectInput("deleteOrder", "", choices = "")),
                    column(
                      6,
                      fluidRow(style = "height:24px;"),
                      actionBttn(
                        "deleteOrderName",
                        "Xóa sản phẩm ra khỏi danh sách",
                        icon = icon("minus-circle"),
                        size = "sm",
                        style = "simple",
                        block = T,
                        color = "danger"
                      )
                    )
                  ),

                  box(
                    fluidRow(column(
                      4,
                      box(
                        title = span(icon("cash-register"), "Tổng"),
                        width = "100%",
                        background = listSkin,
                        uiOutput("tong"),
                        tags$style("#tong{font-size: 35px;font-style: bold}")
                      )
                    ),
                    column(
                      4,
                      box(
                        title = span(icon("dollar-sign"), "Trả"),
                        background = listSkin,
                        width = "100%",
                        uiOutput("thoi"),
                        tags$style("#thoi{font-size: 35px;font-style: bold}")
                      )
                    ),
                    column(
                      4,
                      box(
                        title = span(icon("dollar-sign"), "Nợ"),
                        background = listSkin,
                        width = "100%",
                        uiOutput("no"),
                        tags$style("#no{font-size: 35px;font-style: bold}")
                      )
                    )),

                    fluidRow(
                      column(
                        6,
                        fluidRow(column(
                          7,
                          awesomeRadio(
                            inputId = "ship",
                            label = "Phí vận chuyển",
                            choices = c("Miễn phí", "Mất phí"),
                            selected = "Miễn phí",
                            inline = TRUE,
                            status = "success"
                          )
                        ),
                        column(
                          5,
                          numericInput(
                            "discount_amount",
                            "Phí (VND)",
                            value = 0,
                            min = 0,
                            step = 1000
                          )
                        )),
                        fluidRow(
                          numericInput(
                            "cusPay",
                            "Khách trả trước",
                            value = 0,
                            min = 0,
                            step = 1000,
                            width = "100%"
                            # )
                          ),
                          tags$head(tags$style(
                            HTML("#cusPay{height: 70px;font-size: 45px; font-style: bold}")
                          ))

                        )
                      ),
                      column(
                        6,
                        sliderInput(
                          "vatValue",
                          "",
                          min = 1,
                          max = 15,
                          pre = "Áp dụng thuế ",
                          post = "% VAT",
                          ticks = F,
                          value = 10,
                          step = 1
                        ),
                        awesomeCheckbox(
                          inputId = "vat",
                          label = strong("Chọn vào đây nếu tính VAT"),
                          value = F,
                          status = "danger"
                        ),
                        awesomeCheckbox(
                          inputId = "postorder",
                          label = strong("Chọn vào đây nếu khách hàng trả sau"),
                          value = F,
                          status = "danger"
                        ),
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
                          color = "danger",
                          block = T

                        )
                      )
                    ),

                    width = "100%",
                    height = "100%",

                    column(
                      4,
                      textInput("debtNameID", label = "Tên khách hàng ", placeholder = "Tên khách hàng ...")

                    ),
                    column(
                      4,
                      textInput(
                        "phoneNo",
                        label = "Số điện thoại ",
                        value = 0,
                        placeholder = "SDT khách hàng ..."
                      )
                    ),
                    column(
                      4,
                      textInput(
                        "CMND",
                        label = "Số thẻ CCCD/CMND ",
                        value = 0,
                        placeholder = "CCCD/CMND khách hàng ..."
                      )
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

                column(2,

                       imageOutput(
                         "hinh", width = "120px", height = "200px"
                       ))
              )),


      #++++++++++++++++++++++++++++TAO HOA DON ++++++++++++++++++++++++++++++

      tabItem(tabName =  "invoice",
              fluidRow(
                column(
                  12,

                  fluidRow(
                    column(
                      2,
                      img(
                        src = 'logo.jpg',
                        align = "center",
                        height = "40%",
                        width = "40%"
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
                    tags$style("#recieptNo{font-size: 15px;font-style: bold}"),
                    uiOutput("date1")
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
                      uiOutput("shipfee"),
                      tags$style("#promotion{color: red; font-size: 15px;}"),
                      uiOutput("lines"),
                      tags$style("#lines{color: #EEEBDD }"),
                      uiOutput("PriceVAT"),
                      tags$style("#PriceVAT{color: black; font-size: 18px;font-style: bold}"),
                      uiOutput("lines2"),
                      tags$style("#lines2{color:  #EEEBDD }"),
                      uiOutput("repayment"),
                      tags$style("#repayment{black; font-size: 13px;font-style: bold}"),
                      uiOutput("remainPay"),
                      tags$style("#remainPay{font-size: 13px;font-style: bold}")
                    ),
                    fluidRow(style = "height:20px;"),
                    fluidRow(
                      column(
                        6,
                        uiOutput("buyerSign"),
                        uiOutput("buyer"),
                        tags$style("#buyer{font-size: 18px;font-style: bold}"),
                        fluidRow(style = "height:40px;")

                      ),
                      column(
                        6,
                        uiOutput("sellerSign"),
                        uiOutput("seller"),
                        tags$style("#seller{font-size: 18px;font-style: bold}"),
                        fluidRow(style = "height:40px;")

                      )
                    ),
                    fluidRow(uiOutput("thanks"), align = "center"),
                    fluidRow(style = "height:60px;"),
                    fluidRow(
                      column(
                        1,
                        actionBttn(
                          inputId = "modifyInvoice",
                          style = "material-flat",
                          label = "",
                          size = "xs",
                          icon = icon("edit"),
                           color = "success"
                        )
                      ),
                      bsModal(
                        "invoiceInfor",
                        "Thay đổi thông tin khách hàng",
                        "modifyInvoice",
                        size = "small",
                        textInput(
                          "cusNames",
                          "Tên khách hàng",
                          placeholder = "Tên khách hàng ..." ,
                          width = "140px"
                        ),
                        textInput(
                          "house",
                          "Số nhà và đường đi",
                          placeholder = "Số nhà, đường ...",
                          width = "140px"
                        ),
                        textInput("city", "Chọn thành phố", placeholder = "City ..." , width = "140px"),
                        textInput("district", "Chọn huyện", placeholder = "District ...", width = "140px"),
                        textInput("commune", "Chọn xã", placeholder = "Ward ...", width = "140px"),
                        textInput(
                          "phone",
                          "Số điện thoại",
                          placeholder = "số điện thoại ...",
                          width = "140px"
                        ),
                        prettyRadioButtons(
                          inputId = "online_offline",
                          label = "Đơn hàng Online hay ở Cửa hàng",
                          choices = c("Online", "Cửa hàng"),
                          inline = TRUE,
                          status = "danger",
                          fill = TRUE
                        )
                      ),
                      tags$head(tags$style(
                        "#invoiceInfor .modal-footer{ display:none}"
                      )),
                      column(
                        1,
                        actionBttn(
                          "updateCustom",
                          "",
                          icon = icon("save"),
                          style = "material-flat",
                          size = "xs",
                          color = "primary"
                        )
                      ),
                      column(
                        1,
                        useShinyjs(),
                        extendShinyjs(text = jsCode, functions = c("winprint")),
                        actionBttn(
                          "print",
                          "",
                          icon = icon("print"),
                          style = "material-flat",
                          size = "xs",
                          color = "royal"
                        )

                      ),
                      column(
                       1,
                        actionBttn(
                          "revokeReceipt",
                          "",
                          icon = icon("shopping-bag"),
                          style = "material-flat",
                          size = "xs",
                          color = "danger"
                        )

                      ),
                      column(8),
                      align = "center"
                    ),
                    align = "center"
                  )

                )
              )),

      #++++++++++++++++++++++++++++QUAN LY NO ++++++++++++++++++++++++++++++++

      tabItem(tabName = "debt",
              tabsetPanel(
                tabPanel(
                  "NỢ DÀI HẠN",
                  icon = icon("frown"),
                  fluidRow(column(
                    2,
                    box(
                      width = "100%",
                      background = listSkin,
                      selectInput("debtList", "Chọn tên khách hàng", choices = ""),
                      selectInput("phoneID", "Số điện thoại", choices = ""),
                      selectInput("CMNDID", "CMND/CCCD", choices = "")
                    ),
                    box(
                      width = "100%",
                      background = listSkin,
                      numericInput(
                        "own_return_amount",
                        "Nhập số tiền phải trả",
                        value = 0,
                        min = 1000,
                        step = 1000,
                        width = "100%"
                      ),
                      tags$style("#own_return_amount{font-size: 13px;font-style: bold}"),
                      actionBttn(
                        "returnDebt",
                        "Trả nợ",
                        icon = icon("fas fa-money-bill-alt"),
                        size = "md",
                        style = "pill",
                        block = T
                      )
                    )
                  ),

                  column(
                    10,
                    fluidRow(
                      valueBoxOutput("own_name", width = 3),
                      valueBoxOutput("own_amount", width = 3),
                      valueBoxOutput("onw_prepaid", width = 3),
                      valueBoxOutput("onw_remain", width = 3)
                    ),
                    fluidRow(
                      DTOutput("own_summaryTable"),
                      uiOutput("own_detail_title"),
                      align = "center",
                      tags$style('#own_detail_title { font-size: 25px; font-style: bold }'),
                      DTOutput("individualDebt")
                    )
                  )),
                  fluidRow(column(
                    12,
                    box(
                      title = "Danh sách khách hàng đã thanh toán hết",
                      width = "100%",
                      status = "success",
                      DTOutput("done_debt")
                    )
                  ))
                )
              )),




      #+++++++++++++++++++++++++BACK UP DU LIEU ++++++++++++++++++++++++++++++++

      tabItem(tabName =  "backup",

              fluidRow(
                column(
                  4,
                  fluidRow(
                    img(
                      src = 'logo.jpg',
                      align = "center",
                      height = "40%",
                      width = "40%"
                    ),
                    column(
                      12,
                      actionBttn(
                        inputId = "you_tube",
                        style = "material-flat",
                        label = "Nghe nhạc",
                        size = "xs",
                        icon = icon("music"),
                        color = "primary"
                      ),
                      bsModal("nhac",
                              "Nghe Nhạc",
                              "you_tube",
                              size = "large",
                              uiOutput("video"))
                    ),
                    align = "center"
                  ),
                  fluidRow(style = "height: 40px"),
                  box(
                    title = span(icon("palette"), "THAY ĐỔI MÀU SẮC"),
                    width = "100%",
                    background = listSkin,
                    column(6,
                           selectInput(
                             "theme",
                             "Chọn màu sắc cần đổi",
                             choices = c("blue", "black", "purple", "green", "red", "yellow")
                           )),
                    column(
                      6,
                      fluidRow(style ="height:10px"),
                      actionBttn(
                        inputId = "theme_confirm",
                        style = "material-flat",
                        label = "Thay đổi màu sắc",
                        size = "xs",
                        icon = icon("paint-roller"),
                        color = "default"
                      ),
                      uiOutput("note_theme")

                    )
                  )
                ),
                column(
                  4,
                  box(
                    title = span(icon("hdd"), "CẬP NHẬT DỮ LIỆU"),
                    width = "100%",
                    background = listSkin,
                    fluidRow(
                      actionBttn(
                        "backupData",
                        "Backup dữ liệu",
                        icon = icon("hdd"),
                        size = "xs",
                        style = "material-flat",
                        color = "default"
                      ),
                      align = "center"
                    ),
                    fluidRow(uiOutput("title_auto_update"), align = "center"),
                    fluidRow(
                      column(
                        4,
                        numericInput(
                          "hours",
                          "Giờ",
                          value = 00,
                          min = 00,
                          max = 24,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        numericInput(
                          "minutes",
                          "Phút",
                          value = 00,
                          min = 00,
                          max = 60,
                          width = "100%"
                        )
                      ),
                      column(
                        4,
                        numericInput(
                          "seconds",
                          "Giây",
                          value = 00,
                          min = 00,
                          max = 60,
                          width = "100%"
                        )
                      )
                    ),
                    fluidRow(column(
                      12,

                      actionBttn(
                        "setup_time",
                        "Kích hoạt thời gian",
                        icon = icon("clock"),
                        size = "xs",
                        style = "material-flat",
                        color = "default",
                        block = T
                      )
                    )),
                    uiOutput("auto_update_time")
                  ),

                  box(
                    title = span(icon("redo-alt"), "KHÔI PHỤC DỮ LIỆU"),
                    width = "100%",
                    background = listSkin,
                    selectInput("dateData", "Chọn ngày dữ liệu được backup", choices = ""),
                    actionBttn(
                      "restoreData",
                      "Restore dữ liệu",
                      icon = icon("sync-alt"),
                      size = "xs",
                      style = "material-flat",
                      color = "default"
                    ),
                    uiOutput("settime"),
                    align = "center"
                  )
                ),
                column(
                  4,
                  box(
                    width = "100%",
                    title = span(icon("sync"), "NÂNG CẤP ỨNG DỤNG"),
                    background = listSkin,

                    actionBttn(
                      "updateCode",
                      "Nâng cấp",
                      icon = icon("history"),
                      size = "md",
                      style = "unite",
                      color = "default"
                    ),
                    align = "center",
                    uiOutput("updateNote"),
                    htmlOutput("debug")
                  ),
                  box(
                    width = "100%",
                    title = span(icon("file-excel"), "XUẤT DỮ LIỆU RA EXCEL"),
                    background = listSkin,
                    fluidRow(
                      column(
                        4,
                        downloadButton(
                          "excel_database",
                          "Gian hàng",
                          icon = icon("database")
                        )
                      ),
                      column(
                        4,
                        downloadButton(
                          "excel_khohang",
                          "Kho hàng",
                          icon = icon("person-booth")
                        )
                      ),
                      column(
                        4,
                        downloadButton(
                          "excel_donhang",
                          "Đơn hàng",
                          icon = icon("file-invoice-dollar")
                        )
                      )
                    ),
                    fluidRow(style = "height:20px;"),
                    fluidRow(
                      column(
                        4,
                        downloadButton(
                          "excel_debt",
                          "Nợ tóm tắt",
                          icon = icon("money-check-alt")
                        )
                      ),
                      column(
                        4,
                        downloadButton(
                          "excel_debtlist",
                          "Nợ chi tiết",
                          icon = icon("money-bill-alt")
                        )
                      ),
                      column(
                        4,
                        downloadButton(
                          "excel_invoice",
                          "Hóa đơn",
                          icon = icon("receipt")
                        )
                      )

                    ),
                    align = "center"
                  ),

                  box(
                    title = span(icon("trash-alt"), "XÓA DỮ LIỆU | DELETE EVERYTHING ON DATA"),
                    background = listSkin,
                    width = "100%",
                    column(
                      6,
                      selectInput("reset_metadata",
                                  "Chọn tên dữ liệu cần xóa",
                                  choices = "")
                    ),
                    column(
                      6,
                      fluidRow(style = "height = 20px"),
                      actionBttn(
                        "reset_metadata_button",
                        "Xóa dữ liệu",
                        icon = icon("trash-alt"),
                        size = "md",
                        style = "pill",
                        color = "danger",
                        block = T
                      )
                    ),
                    align = "center"
                  )

                )
              ),

              fluidRow(
                box(width = "100%",
                    uiOutput("remindBackup"),
                    align = "center")
              ))
    )
  )


)



################################################################################
###############################SERVER ##########################################




server <- function(input, output, session) {
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$copyright <- renderText({"Developed by QUAN DAU"})
  #+++++++++++++++++++++++++++++LOGIN DETAILS+++++++++++++++++++++++++++++++++++

  hideAll <- function(x) {
    shinyjs::hide(selector = '[data-value="importData"]')
    shinyjs::hide(selector = '[data-value="sellData"]')
    shinyjs::hide(selector = '[data-value="invoice"]')
    shinyjs::hide(selector = '[data-value="debt"]')
    shinyjs::hide(selector = '[data-value="payroll"]')
    shinyjs::hide(selector = '[data-value="home"]')
    shinyjs::hide(selector = '[data-value="backup"]')
    shinyjs::hide(selector = '[data-value="stockroom"]')
    shinyjs::hide("loadImportData")
    shinyjs::hide("xoaDulieu")
    shinyjs::hide("removeRow")
    shinyjs::hide("save2")
    shinyjs::hide("table2")
    shinyjs::hide("removeRow")
    shinyjs::hide("xoaDulieu")
    shinyjs::hide("sp2")
    shinyjs::hide("image2")

  }
  showAll <- function(x) {
    shinyjs::show(selector = '[data-value="debt"]')
    shinyjs::show(selector = '[data-value="payroll"]')
    shinyjs::show(selector = '[data-value="invoice"]')
    shinyjs::show(selector = '[data-value="home"]')
    shinyjs::show(selector = '[data-value="importData"]')
    shinyjs::show(selector = '[data-value="sellData"]')
    shinyjs::show(selector = '[data-value="backup"]')
    shinyjs::show(selector = '[data-value="stockroom"]')
    shinyjs::show("loadImportData")
    shinyjs::show("backupData")
    shinyjs::show("restoreData")
    shinyjs::show("dateData")
    shinyjs::show("setup_time")
    shinyjs::show("updateCode")
    shinyjs::show("excel_database")
    shinyjs::show("excel_khohang")
    shinyjs::show("excel_donhang")
    shinyjs::show("excel_debt")
    shinyjs::show("excel_debtlist")
    shinyjs::show("excel_invoice")
  }
  hideAll()


  #++++++++++++++++++++++++IDENTIFY MEMBER+++++++++++++++++++++++++++++++++++

  observe({
    role <- dbReadTable(con, "user_member")

    activeAdmin <- function(x) {
      shinyjs::hide("user")
      shinyjs::hide("pass")
      shinyjs::hide("login")
      shinyjs::show("logout")
      shinyjs::show("welcome")
      showAll()
      output$welcome  <-
        renderUI({
          paste0("Xin Chào!")
        })
    }

    # Hien thi cho Nhan vien
    show_employ <- function() {
      shinyjs::show(selector = '[data-value="importData"]')
      shinyjs::show(selector = '[data-value="sellData"]')
      shinyjs::show(selector = '[data-value="backup"]')
      shinyjs::hide("reset_metadata")
      shinyjs::hide("reset_metadata_button")
      shinyjs::hide("excel_database")
      shinyjs::hide("excel_khohang")
      shinyjs::hide("excel_donhang")
      shinyjs::hide("excel_debt")
      shinyjs::hide("excel_debtlist")
      shinyjs::hide("excel_invoice")
    }

   

    if (isTRUE(role$roles == "admin")) {
      activeAdmin()
    } else if (isTRUE(role$roles == "member") ) {
      activeAdmin()
      hideAll()
      show_employ()
    } else  (isTRUE(role$roles == "")) 
      
    hideAll()
  

    loginID <- function(name) {
      shinyjs::hide("user")
      shinyjs::hide("pass")
      shinyjs::hide("login")
      shinyjs::show("welcome")
      shinyjs::show("logout")
      output$welcome  <- renderUI({
        paste0("Xin Chào: ", name)
      })

    }


    #+++++++++++++++++++++++DANG NHAP +++++++++++++++++++++++++++++++++++++++
    observeEvent(input$login, {
 
      if (isTRUE(input$user == filter(dataLogin, usernames == input$user)[1]) &
          isTRUE(input$pass == filter(dataLogin, usernames == input$user & passwords == input$pass)[2])  
          &  isTRUE(filter(dataLogin, usernames == input$user & passwords == input$pass)[4] == "admin")) {
        
        dbWriteTable(con, "user_member", value = 
                       filter(dataLogin, usernames == input$user,
                                         passwords == input$pass), row.names = F, overwrite = T) 
        
        loginID(filter(dataLogin, usernames == input$user, passwords == input$pass)[3])
        
        showAll()
        dbWriteTable(con, "active_user", value = filter(dataLogin, usernames == input$user,
                                                        passwords == input$pass)[, 1:3], row.names = F, overwrite =T) 
        
      } else if (isTRUE(input$user == filter(dataLogin, usernames == input$user)[1]) &
                 isTRUE(input$pass == filter(dataLogin, usernames == input$user & passwords == input$pass)[2]) 
                 &  isTRUE(filter(dataLogin, usernames == input$user & passwords == input$pass)[4] == "member")) {
      
      
      dbWriteTable(con, "user_member", value = 
                     filter(dataLogin, usernames == input$user,
                            passwords == input$pass), row.names = F, overwrite = T) 
      
       loginID(filter(dataLogin, usernames == input$user, passwords == input$pass)[3])
      
       show_employ()
       
       dbWriteTable(con, "active_user", value = filter(dataLogin, usernames == input$user,
                                                      passwords == input$pass)[, 1:3], row.names = F, overwrite =T) 
      } else if (isTRUE(input$user == "") &
                 isTRUE(input$pass == "")) {
        
        
        dbWriteTable(con, "user_member", value =
                       filter(dataLogin, usernames == input$user,
                              passwords == input$pass), row.names = F, overwrite = T)
        
        hideAll()
  
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
      hideAll()
      updateTabItems(session, "tabs", selected = "welcome")
      dbExecute(con, paste0("DELETE FROM user_member;"))

    })
  })



  #++++++++++++++++++++++++++++DISABLE VARIABLES +++++++++++++++++++++++++++++++
  shinyjs::hide("debtNameID")
  shinyjs::hide("phoneNo")
  shinyjs::hide("CMND")
  shinyjs::hide("saveInfor")
  shinyjs::hide("pay")
  shinyjs::hide("backupData")
  shinyjs::hide("restoreData")
  shinyjs::hide("dateData")
  shinyjs::hide("updateCode")
  shinyjs::hide("revokeReceipt")
  shinyjs::hide("print")
  shinyjs::hide("excel_database")
  shinyjs::hide("excel_khohang")
  shinyjs::hide("excel_donhang")
  shinyjs::hide("excel_debt")
  shinyjs::hide("excel_debtlist")
  shinyjs::hide("excel_invoice")

  ############################################################################
  ############################HEADERS#########################################



  ############################THONG KE ######################################

  output$thongke_nhapvao_text <-
    renderUI(strong("Nhập vào (VND)"))
  output$thongke_nhapvao_no <- renderUI({
    a <- dbReadTable(con, "supplier_receipt")
    a <- format(sum(a$Total),
                big.mark = ",",
                scientific = F)
  })
  output$thongke_tong_tien_text <-
    renderUI(strong("Bán ra (VND)"))
  output$thongke_tong_tien_no <- renderUI({
    a <- dbReadTable(con, "invoice")
    a <- format(sum(a$Total),
                big.mark = ",",
                scientific = F)
  })

  output$thongke_tien_nhan_text <-
    renderUI(strong("Số người đang nợ"))
  output$thongke_tien_nhan_no <- renderUI({
    nrow(filter(dbReadTable(con, "supplier_debt"), Status == "Nợ"))

  })

  output$timeLine <- renderPlotly({
    sold <- dbReadTable(con, "invoice")
    timeline <-
      select(sold, Date, Item, Quantity)
    p <-
      ggplot(timeline, aes(Date, Quantity, color = Item, group = Item)) +
      geom_line(aes(y = Quantity)) +
      geom_point(size = 2) +
      theme_few(base_size = 10) +
      ggtitle("Số lượng đã bán") +
      xlab("Thời gian") +
      ylab("Số lượng") +
      # scale_x_datetime(date_breaks = "1 week") +
      theme(plot.title = element_text(hjust = 0.5))
    plotly::ggplotly(p) %>%
      config(displaylogo = F) %>%
      animation_opts(frame = 500,
                     transition = 500,
                     redraw = FALSE)
  })

  # Danh sach nguoi no
  output$listDebt <- renderPlotly({
    debt <- dbReadTable(con, "debt")
    debtValue <- select(debt, NoVAT:Date)

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
    dataDate <- dbReadTable(con, "product")
    dataDate$ExpiredDate <-
      round(as.POSIXlt.character(dataDate$Expired) - as.POSIXct(format(Sys.time(), "%d-%m-%Y"))) /
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
    sold <- dbReadTable(con, "invoice")
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
    Data_Table <-dbReadTable(con, "product")
    updateSelectInput(session, "listSold",  choices = Data_Table[2])
  })


  output$soldAll <- renderPlot({
    data <- dbReadTable(con, "invoice")
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
    dataDate <- dbReadTable(con, "invoice")
    online <- nrow(filter(dataDate, Option == "Online"))
    offline  <- nrow(filter(dataDate, Option == "Cửa hàng"))
    labels <- c("Qua mạng", "Cửa hàng")
    compare <-
      data.frame(Title = labels, Quantity = c(online, offline))
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
    remain <- dbReadTable(con, "product")
    remain <- select(remain, Product, Quantity)

    remain$fraction <- remain$Quantity / sum(remain$Quantity)

    remain$ymax <- cumsum(remain$fraction)

    remain$ymin <- c(0, head(remain$ymax, n = -1))

    remain$labelPosition <- (remain$ymax + remain$ymin) / 2

    remain$label <-
      paste0(remain$Product, "\n còn lại: ", remain$Quantity)

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

  #######################FUNCTION ############################################

  # Tao FUnction table
  DTtable <- function(tableName) {
    DT::renderDT(server = FALSE, {
      DT::datatable(
        tableName,
        selection = "none",
        extensions = 'Buttons',
        escape = FALSE,
        option = list(
          searching = TRUE,
          scrollX = TRUE,
          buttons = c('excel', 'pdf', 'print'),
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




  ############################ KHO HANG#######################################


  observe({
    if (input$addSP == TRUE) {
      shinyjs::show("addnewSP")
      shinyjs::hide("listProduct")
    } else{
      shinyjs::hide("addnewSP")
      shinyjs::show("listProduct")
    }
  })


  observe({
    if (input$addsupplier == TRUE) {
      shinyjs::show("supplier")
      shinyjs::show("sdt_supplier")
      shinyjs::show("address_supplier")
      shinyjs::hide("supplierList")
      shinyjs::hide("sdt_supplier_text")
      shinyjs::hide("address_supplier_text")
    } else{
      shinyjs::hide("supplier")
      shinyjs::hide("sdt_supplier")
      shinyjs::hide("address_supplier")
      shinyjs::show("supplierList")
      shinyjs::show("sdt_supplier_text")
      shinyjs::show("address_supplier_text")
    }
  })

  observe(
    updateSelectInput(
      session,
      "supplierList",
      "Chọn tên nhà cung cấp",
      choices = dbReadTable(con, "stock")[1]
    ),
  )


  output$supplierInfo <- renderValueBox({
    if (input$supplier == "") {
      name <- input$supplierList
    } else
      name <- input$supplier
    valueBox(name,
             "Nhà cung cấp",
             icon = icon("user"),
             color = listSkin)
  })
  output$invoiceInfo <- renderValueBox({
    valueBox(
      paste0("Số ", input$invNo),
      "Hóa đơn",
      icon = icon("file-invoice-dollar"),
      color = listSkin
    )
  })

  remainCost <- function() {
    output$totalPrice <- renderValueBox({
      cost <-
        sum(filter(
          dbReadTable(con, "stock"),
          (Supplier == input$supplier |
              Supplier == input$supplierList
          ) & Invoice == input$invNo
        )[6])
      valueBox(
        paste0(cost, " VND"),
        "Tổng số tiền",
        icon = icon("money-check-alt"),
        color = listSkin
      )
    })
  }

  remainCost()

  output$remainMoney <- renderValueBox({
    valueBox(
      paste0((input$paystock -

                sum(filter(
                  dbReadTable(con, "stock"),
                  (Supplier == input$supplier |
                     Supplier == input$supplierList) &
                    Invoice == input$invNo
                )[6])


              ), " VND"),
      "Số tiền còn lại",
      icon = icon("dollar-sign"),
      color = listSkin
    )
  })

  output$sdt_supplier_text <- renderUI({
    a <-
      filter(dbReadTable(con, "stock"),
             Supplier == input$supplierList)
    paste("Số điện thoại: ", a$Phone[1])
  })
  output$address_supplier_text <- renderUI({
    a <-
      filter(dbReadTable(con, "stock"),
             Supplier == input$supplierList)
    paste("Địa chỉ: ", a$Address[1])
  })


  observeEvent(input$submit, {
    if (input$addsupplier == TRUE) {
      SDTvalue <- input$sdt_supplier
      Addressvalue <- input$address_supplier
    } else{
      SDTvalue <- filter(dbReadTable(con, "stock"), Supplier == input$supplierList)[1, 10]
      Addressvalue <-filter(dbReadTable(con, "stock"), Supplier == input$supplierList)[1, 11]
    }

    Newinvoice <- data.frame(
      Supplier = input$supplierList,
      Invoice = input$invNo,
      Items  = input$listProduct ,
      Quantity = input$quantity4,
      Price  = input$moneyPerItem,
      Total = input$quantity4 * input$moneyPerItem,
      Unit = input$unit4,
      Date =  format(input$dateImport, "%Y-%m-%d"),
      Status = "CHƯA XỬ LÝ",
      Phone = SDTvalue,
      Address = Addressvalue
    )

    if (input$addnewSP == "") {

    } else
      Newinvoice$Items <-  input$addnewSP

    if (input$supplier == "") {
      Newinvoice$Supplier <-  input$supplierList
    } else
      Newinvoice$Supplier <-  input$supplier


    dbWriteTable(
      conn = con,
      name = "stock",
      value = Newinvoice,
      row.names = FALSE,
      append = TRUE
    )

    query <- paste("SELECT * FROM ", "stock", " ORDER BY Items DESC")
    dbGetQuery(con, query)


    output$khohang =  renderDT(dbReadTable(con, "stock"))


    sendSweetAlert(
      session = session,
      title = paste("Sản phẩm " , input$name, " đã lưu"),
      type = "success"
    )

    remainCost()
    chua_xong()
    khach_hang_menu()
    hoa_don_menu()
    san_pham_menu()

  })


  output$khohang <- renderDT(dbReadTable(con, "stock"))


  # xoa du lieu
  updateSelectInput(session, "rowDelete", "Số hóa đơn", choices = filter(dbReadTable(con, "stock")[2]))
  observeEvent(input$rowDelete_button, {
    dbExecute(con, paste0("DELETE FROM stock WHERE Invoice= '", input$rowDelete, "';"))
    output$khohang <- DTtable(dbReadTable(con, "stock"))
    remainCost()
    chua_xong()
    khach_hang_menu()
    hoa_don_menu()
    san_pham_menu()
    sendSweetAlert(
      session = session,
      title = "Sản phẩm đã xóa",
      btn_labels = "OK"
    )
  })


  observeEvent(input$saveData2, {
    name <- dbReadTable(con, "active_user")

    if (input$supplier == "") {
      Suppliername <- input$supplierList
    } else
      Suppliername <- input$supplier

    invoice <- data.frame(
      Supplier = Suppliername,
      Invoice = input$invNo,
      Date = format(Sys.time(), "%Y-%m-%d"),
      Total =  sum(
        filter(
          dbReadTable(con, "stock"),
          Supplier == Suppliername &
            Invoice == input$invNo
        )[6]
      ),
      RePaid = input$paystock,
      Remain = input$paystock - sum(
        filter(
          dbReadTable(con, "stock"),
          Supplier == Suppliername &
            Invoice == input$invNo
        )[6]
      ),

      FillBy =  name[1, 3],
      Status = "Nợ",
      Paid = ""
    )

    if (invoice$Remain[1] == 0) {
      invoice$Status[1] <- "Không nợ"
    }

    dbWriteTable(
      conn = con,
      name = "supplier_receipt",
      value = invoice,
      row.names = FALSE,
      append = TRUE
    )

    sendSweetAlert(session = session,
                   title = "Đã lưu dữ liệu",
                   type = "success")

    updateTextInput(session, "paystock", value = " ")
    updateSelectInput(session, "supplierID", "Số hóa đơn", choices = filter(dbReadTable(con, "supplier_receipt")[1]))
    updateSelectInput(session, "invoiceID", "Số hóa đơn", choices = filter(dbReadTable(con, "supplier_receipt")[2]))

    ownTable()
    paidTable()
  })



  ###############################################################################



  observe({
    updateSelectInput(session, "supplierID", choices = dbReadTable(con, "supplier_receipt")[1])
  })
  observe({
    updateSelectInput(
      session,
      "invoiceID",
      "Số hóa đơn",
      choices = filter(
        dbReadTable(con, "supplier_receipt"),
        Supplier == input$supplierID
      )[2]
    )
  })



  output$total2 <- renderValueBox({
    tien <- filter(
      dbReadTable(con, "supplier_receipt"),
      Supplier == input$supplierID & Invoice == input$invoiceID
    )
    total <- tien[1, 4]

    if (isTRUE(tien[6] < 0)) {
      iconstuatus = icon("file-invoice-dollar")
      colorStatus = "red"
    } else {
      iconstuatus = icon("thumbs-up")
      colorStatus = "green"
    }

    valueBox(paste0(total, " VND"),
             "Tổng số tiền của hóa đơn",
             icon = iconstuatus,
             color = colorStatus)
  })

  output$prePaid2 <- renderValueBox({
    paid <-
      filter(
        dbReadTable(con, "supplier_receipt"),
        Supplier == input$supplierID & Invoice == input$invoiceID
      )

    if (isTRUE(paid[6] < 0)) {
      iconstuatus = icon("file-invoice-dollar")
      colorStatus = "red"
    } else {
      iconstuatus = icon("thumbs-up")
      colorStatus = "green"
    }

    valueBox(paste0(max(paid[5]), " VND"),
             "Đã thanh toán trước",
             icon = iconstuatus,
             color = colorStatus)
  })


  observe({
    staTus <-
      max(filter(
        dbReadTable(con, "supplier_receipt"),
        Supplier == input$supplierID & Invoice == input$invoiceID
      )[6])

    if (isTRUE(staTus < 0)) {
      shinyjs::show("returnMoney")
      shinyjs::show("return")
    } else {
      shinyjs::hide("returnMoney")
      shinyjs::hide("return")
    }
  })


  Remain <- function() {
    output$remain2 <- renderValueBox({
      remain <-
        max(filter(
          dbReadTable(con, "supplier_receipt"),
          Supplier == input$supplierID &
            Invoice == input$invoiceID
        )[6])

      if (isTRUE(remain < 0)) {
        text <- "Đang nợ nhà cung cấp"
        iconstuatus = icon("exclamation-triangle")
        colorStatus = "red"
      } else if (isTRUE(remain == 0)) {
        text <- "Hóa đơn đã thanh toán hết"
        iconstuatus = icon("thumbs-up")
        colorStatus = "green"
      } else {
        text <- "Nhà cung cấp đang nợ chúng ta"
        iconstuatus = icon("comment-dollar")
        colorStatus = "yellow"
      }

      valueBox(paste0(remain, " VND"),
               text,
               icon = iconstuatus,
               color = colorStatus)
    })
  }

  Remain()

  summaryInvoice <- function() {
    observe({
      datatable <-
        filter(
          dbReadTable(con, "supplier_receipt"),
          Supplier == input$supplierID &
            Invoice == input$invoiceID
        )
      output$summaryInvoice <- DTtable(datatable)
    })
  }
  summaryInvoice()
  detailInvoice <- function() {
    observe({
      allinvoice <-
        filter(dbReadTable(con, "supplier_receipt"),
               Supplier == input$supplierID)
      output$allInvoice <- DTtable(allinvoice)
    })
  }
  detailInvoice()

  observe({
    detail <-
      filter(
        dbReadTable(con, "stock"),
        Supplier == input$supplierID & Invoice == input$invoiceID
      )
    output$detailInvoice <- DTtable(detail)
  })



  # Tra tien
  observeEvent(input$returnMoney, {
    oldInvoice <- dbReadTable(con, "supplier_receipt")

    presentInvoice <-
      filter(dbReadTable(con, "supplier_receipt"),
             Supplier == input$supplierID &
               Invoice == input$invoiceID)[1, ]

    presentInvoice$Paid <-
      paste("Đã trả ",
            input$return,
            " VND vào ngày: ",
            format(Sys.time(), "%Y-%m-%d"))

    presentInvoice$Remain <-
      (presentInvoice$Repaid[1] + input$return) - presentInvoice$Total[1]

    presentInvoice$Repaid[1] <-
      (presentInvoice$Repaid[1] + input$return)

    if (presentInvoice$Remain[1] == 0) {
      presentInvoice$Status[1] <- "Không nợ"
    } else{
      presentInvoice$Status[1] <- "Vẫn còn nợ"
    }


     newData <- rbind(presentInvoice, oldInvoice)

    row <-
      which(
        newData$Supplier == input$supplierID &
          newData$Invoice == input$invoiceID  &
          newData$Status == "Nợ"
      )
    newData[row, 8] <- "Vẫn còn nợ"


    dbWriteTable(
      conn = con,
      name = "supplier_receipt",
      value = newData,
      row.names = FALSE,
      overwrite = TRUE
    )


    Remain()
    summaryInvoice()
    detailInvoice()
    ownTable()
    paidTable()
    sendSweetAlert(
      session = session,
      title = paste0(
        "Đã thanh toán " ,
        input$return,
        " VND, còn lại ",
        max(
          filter(
            dbReadTable(con, "supplier_receipt"),
            Supplier == input$supplierID &
             Invoice == input$invoiceID
          )[6]
        ),
        " VND"
      ),
      type = "success"
    )

    updateNumericInput(session, "return", value = "")

  })

  observeEvent(input$attachInvoice, {
    inFile <- input$attachInvoice
    if (is.null(inFile))
      return()

    if (input$supplier != "") {
      supply <- input$supplier
    } else
      supply <- input$supplierList

    file.copy(inFile$datapath,
              file.path(
                "www/",
                paste0("invoice_", supply, "_", input$invNo, ".jpg")
              ),
              overwrite = TRUE)
  })

  output$hinhHoadon <- renderImage({
    list(
      src = paste0(
        "www/invoice_",
        input$supplierID,
        "_",
        input$invoiceID ,
        ".jpg"
      ),
      width = "285px"
    )
  }, deleteFile = FALSE)




############# XUAT KHO ######################################################
  
  khach_hang_menu <- function() {
    observe({
      updateSelectInput(session,  "supplierTable",  "", choices = filter(dbReadTable(con, "stock"), Status == "CHƯA XỬ LÝ") [1]) })
  }

  hoa_don_menu <- function() {
    observe({
      updateSelectInput(
        session,
        "invoiceTable",
        "",
        choices = filter(
          dbReadTable(con, "stock"),
          Status == "CHƯA XỬ LÝ" & Supplier == input$supplierTable
        )[, 2]
      )
    })
  }

  san_pham_menu <- function() {
    observe({
      updateSelectInput(
        session,
        "itemTable",
        "",
        choices = filter(
          dbReadTable(con, "stock"),
          Status == "CHƯA XỬ LÝ" &
            Supplier == input$supplierTable &
            Invoice == input$invoiceTable
        )[, 3]
      )
    })
  }


  ownTable <- function() {
    output$owningSupplierTable <-
      DTtable(filter(dbReadTable(con, "supplier_receipt"), Status == "Nợ")[, 1:6])

  }

  paidTable <- function() {
    output$paidSupplierTable <-
      DTtable(filter(dbReadTable(con, "supplier_receipt"), Status == "Không nợ")[, 1:6])
  }

  ownTable()
  paidTable()

  khach_hang_menu()
  hoa_don_menu()
  san_pham_menu()



  output$undone_text <- renderUI({
    strong("ĐANG CHỜ XỬ LÝ")
  })

  chua_xong <- function() {
    output$undone_no <- renderUI({
      strong(nrow(filter(
        dbReadTable(con, "stock"), Status == "CHƯA XỬ LÝ"
      )))
    })
  }

  chua_xong()

  output$done_text <- renderUI({
    strong("HOÀN TẤT")
  })

  hoantat <- function() {
    output$done_no <- renderUI({
      strong(nrow(filter(
        dbReadTable(con, "stock"), Status == "XONG"
      )))
    })
  }

  hoantat()



  observe(if (input$itemTable %in% filter(dbReadTable(con, "product"), product == input$itemTable)[2]) {
    output$same_item <- renderUI(em(
      paste0(
        "Lưu ý: sản phẩm ",
        input$itemTable,
        " cùng tên với một sản phẩm đã có trên gian hàng. Số lượng mới sẽ được cộng dồn với số lượng đang có trên gian hàng"
      )
    ))
  })




  data_all <-  select(dbReadTable(con, "stock"),
                      Supplier:Total,
                      Unit, Status)
  randomCode <- myFun(1)

  printImage <- function(x) {
    observeEvent(input$imageTable, {
      inFile <- input$imageTable
      if (is.null(inFile))
        return()

      file.copy(inFile$datapath,
                file.path("www/",
                          paste0(x, ".jpg")),
                overwrite = TRUE)
    })
  }

  printImage(randomCode)


  # LUU DU LIEU VAO GIAN HANG

  observeEvent(input$save123, {
    add_new_data <- data.frame(
      code_product = randomCode,
      product = as.character(input$itemTable),
      quantity = input$sellquantity,
      unit = input$unitTable,
      importprod = 0,
      sell = input$sellcost,
      catalogue = input$catalogueStock,
      dateno = format(Sys.time(), "%Y-%m-%d"),
      link = paste0("<img src=", randomCode, ".jpg height=52></img>"),
      expired = format(input$dateTable, "%Y-%m-%d")
    )

    #
    kho_Hang <- dbReadTable(con, "stock")
    row <-
      which(
        kho_Hang$Supplier == input$supplierTable &
          kho_Hang$Invoice == input$invoiceTable  &
          kho_Hang$Items == input$itemTable
      )

    kho_Hang[row, 9] <- "XONG"
    
    dbWriteTable(
      conn = con,
      name = "stock",
      value = kho_Hang,
      row.names = FALSE,
      overwrite = TRUE
    )
    
 


     database <- dbReadTable(con, "product")

    data_bane <-  filter(dbReadTable(con, "product"), product == input$itemTable)


    if (data_bane[2] %in% add_new_data[2]) {
      sendSweetAlert(
        session = session,
        title = "NHẬN DẠNG SP CÙNG TÊN",
        text = paste0(
          "Một sản phẩm cùng tên với ",
          input$itemTable,
          " đã có sẵn trên gian hàng. Số lượng này sẽ được tự động cộng dồn vào số lượng đang có ở trên gian hàng. Ngoài ra, giá tiền, đơn vị, ... sẽ được cập nhật mới theo lần thay đổi này"
        ),
        type = "warning ",
        btn_colors = "red"
      )

      row <- which(database$product ==  input$itemTable)

      database[row, 3] <- data_bane[3] + add_new_data[3]
      database[row, 4] <- add_new_data$unit
      database[row, 6] <- add_new_data$sell
      database[row, 7] <- add_new_data$catalogue
      database[row, 9] <- paste0("<img src=", data_bane[1], ".jpg height=52></img>")
      database[row, 10] <- add_new_data$expired

      dbWriteTable(
        conn = con,
        name = "product",
        value = database,
        row.names = FALSE,
        overwrite = TRUE
      )

      printImage(data_bane[1])
      
    } else {

      dbWriteTable(
        conn = con,
        name = "product",
        value = add_new_data,
        row.names = FALSE ,
        append = TRUE
      )

      sendSweetAlert(
        session = session,
        title = paste(
          input$itemTable,
          " đã được chuyển từ kho vào hệ thống gian hàng"
        ),
        type = "success"
      )
    }


    editTable()
    yetdoneTable()


    output$table <- DTtable(dbReadTable(con, "product"))
    output$table2 <- DTtable(dbReadTable(con, "product"))
    chua_xong()
    hoantat()

    khach_hang_menu()
    hoa_don_menu()
    san_pham_menu()

    updateSelectInput(session,
                      "soldProduct",
                      choices = dbReadTable(con, "product")[2])

  })



  yetdoneTable <- function() {
    output$xuatkho <-  DT::renderDataTable(
      DT::datatable(
        filterTable <- filter(
          dbReadTable(con, "stock"),
          Status == "CHƯA XỬ LÝ" &
            Supplier == input$supplierTable &
            Invoice == input$invoiceTable &
            Items == input$itemTable
        ),
        escape = FALSE,
        selection = 'none',
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        )
      ) %>%
        formatCurrency(
          5:6,
          currency = "VND ",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>% formatStyle(
          9,
          target = "cell",
          color = "white",
          backgroundColor = "red",
          fontWeight = 'bold'
        )
    )

  }



  yetdoneTable()


  editTable <- function() {
    output$xuatkho_xong <-
      DT::renderDataTable({
        DT::datatable(
          filter( dbReadTable(con, "stock"),
                 Status == "XONG"),
          escape = FALSE,
          selection = 'none',
          options = list(
            dom = 't',
            paging = T,
            ordering = FALSE,
            pageLength = 10
          )
        ) %>% formatStyle(
          9,
          target = "cell",
          color = "white",
          backgroundColor = "green",
          fontWeight = 'bold'
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



  editTable()



  ############################NHAP SAN PHAM ##################################


  observe({
    Data_Table<- dbReadTable(con, "product")
    observe({
      output$table <- DTtable(dbReadTable(con, "product"))

      # Them san pham moi vao danh sach
      observeEvent(input$save, {
          Data_Table<- dbReadTable(con, "product")
        if (input$name == "") {
          sendSweetAlert(
            session = session,
            title = "THÊM TÊN SẢN PHẨM",
            text = "Tên sản phẩm là bắt buộc khi đưa lên gian hàng. Vui lòng thêm một tên sản phẩm và lưu lại",
            type = "danger"
          )
        } else if (input$name %in% filter(Data_Table, product == input$name)[2]) {
          sendSweetAlert(
            session = session,
            title = "SẢN PHẨM CÙNG TÊN",
            text = "Tên sản phẩm này trùng tên với một sản phẩm đã có trên gian hàng. Nếu muốn cập nhật số lượng và giá cho sản phẩm này thì có thể vào mục Sửa dữ liệu để cập nhật lại",
            type = "danger"
          )
        } else {

          newLine <- data.frame(
            code_product = input$ID,
            product = input$name,
            quantity = as.numeric(input$quantity),
            unit =  input$unit,
            importprod  = as.numeric(input$importPrice),
            sell = as.numeric(input$soldPrice),
            catalogue = input$catalogue,
            dateno = format(Sys.time(), "%Y-%m-%d"),
            link = paste0("<img src=", input$ID, ".jpg height=52></img>"),
            expired = as.Date(format(input$expireDate, "%Y-%m-%d"))
          )

           dbWriteTable(conn = con,
                       name = "product",
                       value = newLine,
                       row.names = FALSE,
                      append = TRUE)

           query <- paste("SELECT * FROM ", "product", " ORDER BY product DESC")
           dbGetQuery(con, query)

           output$table  <- DTtable(dbReadTable(con, "product"))

          sendSweetAlert(
            session = session,
            title = paste("Sản phẩm " , input$name, " đã lưu"),
            type = "success"
          )


          # Reset lai cac lua chon sau khi da luu du lieu
          updateTextInput(session, "ID", value = " ")
          updateTextInput(session, "name", value = " ")

          updateTextInput(session, "importPrice", value = " ")
          updateTextInput(session, "sellPrice", value = " ")

          #Xoa du lieu cua san pham nhap
          updateSelectInput(session,
                            "removeRow",
                            "Chọn tên sản phẩm cần xóa",
                            choices = Data_Table$code_product)


          observeEvent(input$image, {
            inFile <- input$image
            if (is.null(inFile))
              return()

            file.copy(inFile$datapath,
                      file.path("www/", paste0(input$ID, ".jpg")),
                      overwrite = TRUE)
          })

          updateSelectInput(session,
                            "soldProduct",
                            choices = Data_Table[2])

        }
      })
    })





    updateSelectInput(session,
                      "removeRow",
                      "Chọn tên sản phẩm cần xóa",
                      choices = Data_Table$code_product)



    observeEvent(input$xoa, {

        dbExecute(con, paste0("DELETE FROM product WHERE code_product= '", input$removeRow, "';"))


         output$table <-DTtable(dbReadTable(con, "product"))
         output$table2 <-DTtable(dbReadTable(con, "product"))


        sendSweetAlert(
          session = session,
          title = paste("Sản phẩm", as.character(input$removeRow) , " đã Xóa"),
          type = "success"
        )

        updateSelectInput(
          session,
          "removeRow",
          "Chọn tên sản phẩm cần xóa",
          choices = dbReadTable(con, "product")[, 2]
        )

        # Xpa hinh anh cua san pham theo ten
        link <- filter(dbReadTable(con, "product"), product == input$removeRow)[, 1]
        file.remove(file.path(src =  paste0("www/", link, ".jpg")))

    })




    #Sua san pham

    observeEvent(input$loginImportdata, {
      name <- dbReadTable(con, "active_user")
      if (input$passEditData == name[1, 2]) {
        shinyjs::show("save2")
        shinyjs::show("table2")
        shinyjs::show("removeRow")
        shinyjs::show("sp2")
        shinyjs::show("image2")
        shinyjs::show("xoaDulieu")
        shinyjs::hide("passEditData")
        shinyjs::hide("loginImportdata")


      } else {
        sendSweetAlert(session = session,
                       title = "Mật khẩu không đúng",
                       type = "error")
      }

      output$table2 = renderDT(dbReadTable(con, "product"))

    })

    updateSelectInput(session,
                      "sp2",
                      "Chọn Mã Số sản phẩm cần sủa hình",
                      choices = dbReadTable(con, "product")[1])


    observeEvent(input$image2, {
      inFile <- input$image2
      if (is.null(inFile))
        return()

      Data_Table <- dbReadTable(con, "product")
      # file.remove(inFile$datapath, file.path("www/", paste0(HinhSp[1], ".jpg")))
      file.copy(inFile$datapath, file.path("www/", paste0(input$sp2, ".jpg")), overwrite = TRUE)

      HinhSp <- filter(Data_Table, code_product == input$sp2)

      row <-  which(Data_Table$code_product ==  input$sp2)

      Data_Table[row, 9] <-  paste0("<img src=", input$sp2, ".jpg height=52></img>")

      output$table2 = renderDT(dbReadTable(con, "product"))

      output$table = renderDT(dbReadTable(con, "product"))
    })





    # THem san pham bang cach cho phep load du lieu bang EXCEL
    # output$contents <- renderTable({
    #   old <- read.xlsx("data/database.xlsx")
    #   file <- input$loadImportData
    #   ext <- tools::file_ext(file$datapath)
    #   req(file)
    #   validate(need(ext == "xlsx", "Load dữ liệu nhập lên "))
    #   a <- read.xlsx(file$datapath)
    #
    #   a$Date <- format(Sys.time(), "%d-%m-%Y")
    #   a$Expired <- format(Sys.time(), "%d-%m-%Y")
    #   # saveData(as.data.frame(a))
    #   tryCatch({
    #     b <- rbind(a, old)
    #     write.xlsx(b, file = "data/database.xlsx", overwrite = T)
    #   },
    #   error = function(e) {
    #     showModal(
    #       modalDialog(
    #         title = "LỖI SAI TÊN CỘT DỮ LIỆU" ,
    #         footer = modalButton("ĐÓNG"),
    #         easyClose = T
    #       )
    #     )
    #     return()
    #   })
    #
    #   output$table <- DTtable(Data_Table[, 1:8])
    #   a
    # })






    #############################BAN SAN PHAM ################################


    observe(if (input$vat == TRUE) {
      shinyjs::show("vatValue")
    } else
      shinyjs::hide("vatValue"))

    #Danh sach san pham theo ten
    observeEvent(input$soldProduct, {
      database <- dbReadTable(con, "product")
      if (input$soldProduct == "") {
        updateSelectInput(session, "CodeID", choices = c("", unique(database$code_product)))
      } else{
        updateSelectInput(
          session,
          "CodeID",
          choices = unique(database$code_product[database$product == input$soldProduct]),
          selected = isolate(input$CodeID)
        )
      }
    })

    # Danh sach san pham theo CODE
    observeEvent(input$CodeID, {
      database <- dbReadTable(con, "product")
      if (input$CodeID == "") {
        updateSelectInput(session, "soldProduct", choices = c("", unique(database$product)))
      } else{
        updateSelectInput(
          session,
          "soldProduct",
          choices = unique(database$product[database$code_product == input$CodeID]) ,
          selected = isolate(input$soldProduct)
        )
      }
    })



    # Hien thi san pham dang co san
    SPdangco <- function(x) {
      observe({
        database <- dbReadTable(con, "product")
        if (input$soldProduct == "") {
          CodeProduct <-
            dplyr::filter(database , code_product == input$CodeID)
        } else
          CodeProduct <-
            dplyr::filter(database , product == input$soldProduct)
        output$stock <-
          renderUI({
            strong(paste0("Đang có: ", CodeProduct[, 3], " " , CodeProduct[, 4]))
          })
      })
    }

    SPdangco()
    # Hien thi so tien can tra lai
    output$thoi <- renderUI({
      base <- dbReadTable(con, "live_order")
      if (input$ship == "Miễn phí") {
        shiping <- 0
      } else{
        shiping <- input$discount_amount
      }

      if (isTRUE(input$cusPay == 0)| isTRUE(input$cusPay == "")) {
        paste(0, " VND")
      } else if (isTRUE(as.numeric(input$cusPay) <= as.numeric(sum(base[4]) + shiping))) {
        paste(0, " VND")
      } else {
        (isTRUE(as.numeric(input$cusPay) > as.numeric(sum(base[4]) + shiping)))
        if (input$vat == TRUE) {
          paste0(format(
            as.numeric(input$cusPay) - as.numeric(sum(base[4]) + (sum(base[4]) * 0.1) + shiping),
            big.mark = ",",
            scientific = F
          ),
          " VND")
        } else

          paste0(format(
            as.numeric(input$cusPay) - as.numeric(sum(base[4]) + shiping),
            big.mark = ",",
            scientific = F
          ),
          " VND")
      }
    })


    #Hien thi so tien dang no
    output$no <- renderUI({
      if (input$ship == "Miễn phí") {
        shiping <- 0
      } else{
        shiping <- input$discount_amount
      }
      base <- dbReadTable(con, "live_order")

      if (isTRUE(input$cusPay == 0) | isTRUE(input$cusPay == "")) {
        paste(0, " VND")
      } else if (isTRUE(as.numeric(input$cusPay) >= as.numeric(sum(base[4]) + shiping))) {
        paste(0, " VND")
      } else {
        isTRUE((as.numeric(input$cusPay) < as.numeric(sum(base[4]) + shiping)))
        if (input$vat == TRUE) {
          paste0(format(
            as.numeric(sum(base[4]) + (sum(base[4]) * 0.1) + shiping) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          ),
          " VND")
        } else

          paste0(format(
            as.numeric(sum(base[4]) + shiping) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          ),
          " VND")
      }
    })


    # Hien thi hinh anh san pham

    output$hinh <- renderImage({
      Data_Table <- dbReadTable(con, "product")
      link <- filter(Data_Table, product == input$soldProduct)[, 1]

      list(src = paste0("www/", link, ".jpg"), width = "200px")

    }, deleteFile = FALSE)


    #+++++++++++++++++++++++++++THEM SAN PHAM VAO GIO HANG +++++++++++++++++




    check <- dbReadTable(con, "live_order")
    if (nrow(check) > 0) {
      output$warning <- renderUI({
        paste(
          "Chú ý ! Vui lòng xóa ",
          nrow(check),
          " sản phẩm đang hiển thị trong giỏ hàng trước khi mua"
        )
      })
    }

    # hien thi danh sach san pham neu co
    sellproductTable <- function(x) {
      renderDataTable({
        datatable(
          dbReadTable(con, "live_order")[, c(1:6, 9:10)],
          width = "auto",
          selection = "none",
          editable = T,
          escape = FALSE,
          options = list(scrollX = TRUE,
                         dom = 't')
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


    observe({
      if (input$ship == "Miễn phí") {
        shinyjs::hide("discount_amount")
      } else {
        shinyjs::show("discount_amount")
      }
    })
    
    
    ## Them san pham moi truong hop khi mua ma trong he thong chua co

    observeEvent(input$save_newdata, {
      Data_Table <- dbReadTable(con, "product")
      newSP <- data.frame(
        code_product = input$ID2,
        product = input$name2,
        quantity = as.numeric(input$quantity2),
        unit =  input$unit2,
        importprod  = as.numeric(input$importPrice2),
        sell = as.numeric(input$soldPrice2),
        catalogue = input$catalogue2,
        dateno = format(Sys.time(), "%Y-%m-%d"),
        link = paste0("<img src=", input$ID2, ".jpg height=52></img>"),
        expired = as.character(format(input$expireDate2, "%Y-%m-%d"))
      )


      dbWriteTable(conn = con,
                   name = "product",
                   value = newSP,
                   row.names = FALSE,
                   append = TRUE)


      sendSweetAlert(
        session = session,
        title = paste("Sản phẩm " , input$name2, " đã lưu"),
        type = "success"
      )
      updateTextInput(session, "ID2", value = " ")
      updateTextInput(session, "name2", value = " ")
      updateTextInput(session, "quantity2", value = " ")
      updateTextInput(session, "importPrice2", value = " ")
      updateTextInput(session, "sellPrice2", value = " ")
      updateSelectInput(session,
                        "soldProduct",
                        "",
                        choices = dbReadTable(con, "product")[, 2])

    })



    observe({
      if (input$price == "Auto") {
        shinyjs::hide("discount")
      } else if (input$price == "Khuyến mãi") {
        shinyjs::show("discount")
      } else
        shinyjs::hide("discount")
    })

    observe({
      if (input$openEdit == TRUE) {
        shinyjs::show("addProduct")
        shinyjs::hide("soldProduct")
      } else {
        shinyjs::hide("addProduct")
        shinyjs::show("soldProduct")
      }
    })

    #


    observe({
      database <- dbReadTable(con, "product")
      if (input$soldProduct == "") {
        CodeProduct <-
          dplyr::filter(database , code_product == input$CodeID)
      } else
        CodeProduct <-
          dplyr::filter(database , product == input$soldProduct)



      if (isTRUE(CodeProduct[1, 3] <= 0)) {
        sendSweetAlert(
          session = session,
          title = "SẢN PHẨM HẾT HÀNG",
          text = paste0("Sản phẩm ", CodeProduct[1, 2], " đã hết hàng"),
          btn_labels = "Quay lại",
          btn_colors = "danger"
        )
        shinyjs::hide("soldToday")
        shinyjs::hide("sold")
      } else {
        shinyjs::show("soldToday")
        shinyjs::show("sold")
      }
    })


    observeEvent(input$sold, {
      SPdangco()

      if (input$price == "Auto") {
        StatusProduct <- "Không khuyến mãi"
        Data_Table <- dbReadTable(con, "product")
        if (input$soldProduct == "") {
          autoPrice <-
            as.numeric(filter(Data_Table, code_product == input$CodeID)[6])
        } else
          autoPrice <-
          as.numeric(filter(Data_Table, product == input$soldProduct)[6])

      } else if (input$price == "Khuyến mãi") {
        autoPrice <-
          as.numeric(filter(Data_Table, product == input$soldProduct)[6]) - (as.numeric(input$discount /
                                                                                          100) * as.numeric(filter(
                                                                                            Data_Table, product == input$soldProduct
                                                                                          )[6]))
        StatusProduct <-
          paste0(
            "Giảm ",
            input$discount,
            "% giá gốc (",
            format(
              as.numeric(filter(
                Data_Table, product == input$soldProduct
              )[6]),
              big.mark = ",",
              scientific = F
            ) ,
            " VND)"
          )
      } else {
        autoPrice <- 0
        StatusProduct <- "Biếu tặng"
      }


      Data_Table <- dbReadTable(con, "product")
      orderList <- dbReadTable(con, "live_order")

      if (input$soldProduct == "") {
        liveOrder <-
          rbind(
            data.frame(
              Item = as.character(filter(Data_Table, code_product == input$CodeID)[2]),
              Quantity = as.numeric(input$soldToday),
              Price = as.numeric(autoPrice),
              Total = as.numeric(autoPrice) * as.numeric(input$soldToday),
              Date = format(Sys.time(), "%Y-%m-%d"),
              Customer = UnknownCustomer,
              Prepayment = 0,
              VATCost = 0,
              Images = paste0(
                "<img src=",
                filter(Data_Table, code_product == input$CodeID)[1],
                ".jpg height=52></img>"
              ),
              Status = StatusProduct

            ),
            orderList
          )


      } else
        liveOrder <-
        rbind(
          data.frame(
            Item = as.character(filter(
              Data_Table, product == input$soldProduct
            )[2]),
            Quantity = as.numeric(input$soldToday),
            Price = as.numeric(autoPrice),
            Total = as.numeric(autoPrice) * as.numeric(input$soldToday),
            Date = format(Sys.time(), "%d-%m-%Y"),
            Customer = UnknownCustomer,
            Prepayment = 0,
            VATCost = 0,
            Images = paste0(
              "<img src=",
              filter(Data_Table, product == input$soldProduct)[1],
              ".jpg height=52></img>"
            ),
            Status = StatusProduct
          ),
          orderList
        )


      dbWriteTable(conn = con,
                   name = "live_order",
                   value = liveOrder,
                   row.names = FALSE,
                   overwrite = TRUE)



      output$liveOrder <- sellproductTable ()
      #Update ten san pham vao danh sach xoa
      updateSelectInput(
        session,
        "deleteOrder",
        "Chọn tên sản phẩm cần xóa",
        choices = dbReadTable(con, "live_order")[, 1]
      )

      # Begin Table appear on Invoice Tab
      TableInvoice <-  select(dbReadTable(con, "live_order"),
                              Item,
                              Quantity,
                              Price,
                              Total,
                              Status)
      colnames(TableInvoice) <-
        c("Tên sản phẩm", "Số lượng", "Giá", "Tổng tiền", "Khuyến mãi")
      output$customerOrder <- renderDT({
        datatable(TableInvoice,
                  selection = "none",
                  options = list(dom = 't')) %>%
          formatCurrency(
            3:4,
            currency = "VND ",
            interval = 3,
            mark = ",",
            digits = 0
          )
      })



      # Hien thi tong cua so tien

      output$tong <- renderUI({
        displaySUM()
      })


      output$totalSoldTitle <- renderUI({
        saveSold <- dbReadTable(con, "invoice")
        totSold <-
          sum(filter(saveSold, Item == input$soldProduct)[6])
        strong(paste0("Đã bán: ", as.numeric(totSold), " sp"))
      })

    })


    displaySUM <- function(x) {
      list_Live_Order <- dbReadTable(con, "live_order")
      if (input$ship == "Miễn phí") {
        shiping <- 0
      } else{
        shiping <- input$discount_amount
      }

      if (input$vat == TRUE) {
        paste0(format(
          sum(
            list_Live_Order[4] + (list_Live_Order[4] * as.numeric(input$vatValue) / 100)
          ) + shiping,
          big.mark = ",",
          scientific = F
        ),
        " VND")
      } else

        paste0(format(
          sum(list_Live_Order[4]) + shiping,
          big.mark = ",",
          scientific = F
        ), " VND")
    }


    #Xoa du lieu tung san pham
    
    updateSelectInput(
      session,
      "deleteOrder",
      "Chọn tên sản phẩm cần xóa",
      choices = dbReadTable(con, "live_order")[, 1]
    )
    
    observeEvent(input$deleteOrderName, {
      order <- dbReadTable(con, "live_order")
      
      dbExecute(con, paste0("DELETE FROM live_order WHERE Item= '", input$deleteOrder, "';"))

      updateSelectInput(
        session,
        "deleteOrder",
        "Chọn tên sản phẩm cần xóa",
        choices = dbReadTable(con, "live_order")[, 1]
      )

      shinyjs::hide("warning")

      sendSweetAlert(
        session = session,
        title = paste0("Sản phẩm ", input$deleteOrder, " đã xóa"),
        btn_labels = "Xong"
      )

      output$liveOrder <- sellproductTable ()

      output$tong <- renderUI({
        displaySUM()
      })

    })


    #++++++++++++++++++++++++++THANH TOAN+++++++++++++++++++++++++++++++++++++

    observeEvent(input$ask2pay, {
      shinyjs::show("pay")
    })


    observeEvent(input$pay, {
      base <- dbReadTable(con, "live_order")

      # Cap nhat hoa don theo VAT

      if (input$vat == TRUE) {
        base$Prepayment <- as.numeric(input$cusPay)
        base$VATCost <-
          (sum(base[4]) * as.numeric(input$vatValue) / 100)

      } else {
        base$Prepayment <- as.numeric(input$cusPay)
        base$VATCost <- 0
      }

      if (input$ship == "Miễn phí") {
        shiping <- 0
      } else{
        shiping <- input$discount_amount
      }


      # Neu khach hang tra tien dang thieu thi thong tin ca nhan la bat buoc
      if (as.numeric(input$cusPay) < as.numeric(sum(base[4]))) {
        shinyjs::show("debtNameID")
        shinyjs::show("phoneNo")
        shinyjs::show("CMND")
        shinyjs::show("saveInfor")
        shinyjs::hide("pay")


        # Luu thong tin khach hang dang no tien vao danh sach

        observeEvent(input$saveInfor, {
          name <- dbReadTable(con, "active_user")
          if (input$vat == TRUE) {
            debt <-
              data.frame(
                base[, 1:4],
                Paid = as.numeric(input$cusPay),
                NoVAT = 0,
                VATPrice = as.numeric(sum(base[4]) + (
                  sum(base[4]) * as.numeric(input$vatValue) / 100
                )) - as.numeric(input$cusPay),
                Delivery = shiping,
                Customer = input$debtNameID,
                Phone = input$phoneNo,
                CMND = input$CMND,
                Date = format(Sys.time(), "%Y-%m-%d"),
                Employee = name[1, 3],
                CusReturn	= "",
                Status = "Nợ"

              )

          } else

            debt <-
              data.frame(
                base[, 1:4],
                Paid = as.numeric(input$cusPay),
                NoVAT = as.numeric(sum(base[4])) - as.numeric(input$cusPay),
                VATPrice = 0,
                Delivery = shiping,
                Customer = input$debtNameID,
                Phone = input$phoneNo,
                CMND = input$CMND,
                Date = format(Sys.time(), "%Y-%m-%d"),
                Employee = name[1, 3],
                CusReturn	= "",
                Status = "Nợ"
              )



          dbWriteTable(conn = con,
                       name = "debt",
                       value = debt,
                       row.names = FALSE,
                       append = TRUE)



          # Viet 1 file tom tat no chung
          if (input$vat == TRUE) {
            remainVAT <-
              as.numeric(sum(base[4]) + (sum(base[4]) * as.numeric(input$vatValue) / 100)) - as.numeric(input$cusPay)

          } else
            remainVAT <-
            as.numeric(sum(base[4])) - as.numeric(input$cusPay)


          debtSummary <- data.frame(
            Customer = input$debtNameID,
            Phone = input$phoneNo,
            CMND = input$CMND,
            Date = format(Sys.time(), "%Y-%m-%d"),
            Total =  as.numeric(sum(base[4])),
            Delivery = shiping,
            RePaid = as.numeric(input$cusPay),
            Remain = remainVAT + shiping,
            Employee =  name[1, 3],
            CusReturn = "",
            Status = "Nợ"
          )

          dbWriteTable(conn = con,
                       name = "supplier_debt",
                       value = debtSummary,
                       row.names = FALSE,
                       append = TRUE)

          sendSweetAlert(
            session = session,
            title = paste0("Đơn hàng ", input$debtNameID, " đã đặt"),
            text = paste0("Khoản nợ ", money, " VND đã đươc lưu trên hệ thống"),
            type = "warning ",
            btn_labels = "Xong"
          )


          updateSelectInput(session,
                            "debtList",
                            "Chọn tên khách hàng",
                            choices = dbReadTable(con, "supplier_debt")[1])
          updateSelectInput(session,
                            "phoneID",
                            "Số điện thoại",
                            choices = dbReadTable(con, "supplier_debt")[2])
          updateSelectInput(session,
                            "CMNDID",
                            "CMND/CCCD",
                            choices = dbReadTable(con, "supplier_debt")[3])


          # Sau khi cap nhat thong tin khach hang dang no, he thong chuyen sang trang hoa don
          updateTabItems(session, "tabs", selected = "invoice")

        })


        # Hien thi so tien no khach hang trong thong bao
        if (input$vat == TRUE) {
          money <- format(
            as.numeric(sum(base[4]) + shiping + (
              sum(base[4]) * as.numeric(input$vatValue) / 100
            )) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          )
        } else
          money <-  format(
            as.numeric(sum(base[4]) + shiping) - as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          )

        sendSweetAlert(
          session = session,
          title = paste0("Đơn hàng đang nợ ", money , " VND"),
          text = "Thông tin khách hàng là bắt buộc" ,
          type = "warning ",
          btn_labels = "Vui lòng thêm thông tin khách hàng"
        )


      } else {
        # Neu khach hang tra tien nhieu hon so luong thi chuyen sang trang hoa don
        updateTabItems(session, "tabs", selected = "invoice")

        sendSweetAlert(
          session = session,
          title = "Đơn hàng đã đặt",
          type = "success",
          btn_labels = "OK",
        )
      }


      # Tru so luong trong kho

      Data_Table <- dbReadTable(con, "product")

      Ban <-
        dplyr::filter(Data_Table, product == input$soldProduct)

      row_Data  <-
        which(Data_Table$product ==  input$soldProduct)

      Data_Table[row_Data, 3] <-
        as.numeric(Ban[3]) - sum(filter(base, Item == input$soldProduct)[2])

      dbWriteTable(conn = con,
                   name = "product",
                   value = Data_Table,
                   row.names = FALSE,
                   overwrite = TRUE)

    })



    # Hien thi so luong san pham da ban
    output$totalSoldTitle <- renderUI({
      order <- dbReadTable(con, "invoice")

      totSold <-
        sum(filter(order, Item == input$soldProduct)[6])
      strong(paste0("Đã bán: ", totSold, " sp"))
    })




    #########################TRANG  HOA DON #################################
   


    output$sumPrice <- renderUI({
      base <- dbReadTable(con, "live_order")

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
      base <- dbReadTable(con, "live_order")
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
        paste0("(2) Không áp dụng thuế VAT cho hóa đơn này")
      )
    })


    output$shipfee <- renderUI({
      paste0(
        "(3) Phí vận chuyển = ",
        format(
          input$discount_amount,
          big.mark = ",",
          scientific = F
        ),
        " VND"
      )
    })




    # Tao duong line
    output$lines <- renderUI({
      "____________________________________"
    })
    output$lines2 <- renderUI({
      "____________________________________"
    })


    # Tong thanh toan sau thue
    output$PriceVAT <-
      renderUI({
        base <- dbReadTable(con, "live_order")
        vatCalculate(strong(paste0(
          "Tổng (1) + (2) + (3) = ",
          format((sum((base)[4]) + (
            sum(base[4]) * as.numeric(input$vatValue) / 100
          ) + input$discount_amount),
          big.mark = ",",
          scientific = F
          ),
          " VND"
        )),
        strong(paste0(
          "Tổng (1) + (2) + (3) = ",
          format((sum((base)[4]) + 0  + input$discount_amount),
                 big.mark = ",",
                 scientific = F
          ),
          " VND"
        )))
      })




    # Hien thi so tien khach hang da tra
    output$repayment <- renderUI({
      if (input$postorder == TRUE) {
        em(
          "Khách hàng chọn hình thức thanh toán trả sau. Quý khách có thể chuyển khoản cho chúng tôi."
        )
      } else

        em(paste0(
          "Khách hàng đã trả trước: ",
          format(
            as.numeric(input$cusPay),
            big.mark = ",",
            scientific = F
          ),
          " VND"
        ))

    })


    # Hien thi so tien can tra lai khach hang
    output$remainPay <- renderUI({
      if (input$postorder == TRUE) {
        strong("Dau Van Doan | VIETCOMBANK | 0201-000-668-759")
      } else {
        base <- dbReadTable(con, "live_order")
        if (input$ship == "Miễn phí") {
          shiping <- 0
        } else{
          shiping <- input$discount_amount
        }

        if (isTRUE(as.numeric(input$cusPay) < as.numeric(sum(base[4]) + shiping))) {
          hoadonNo <- "Khách hàng đang nợ chúng tôi: "
        } else
          hoadonNo <- "Trả lại khách hàng: "

        vatCalculate(em(paste0(
          hoadonNo,
          format(
            as.numeric(input$cusPay) - as.numeric((
              sum((base)[4]) + (sum(base[4]) * as.numeric(input$vatValue) / 100) + shiping
            )) ,
            big.mark = ",",
            scientific = F
          ),
          " VND"
        )),

        em(paste0(
          hoadonNo,
          format(
            as.numeric(input$cusPay) - as.numeric((sum((
              base
            )[4]) + 0 + shiping)) ,
            big.mark = ",",
            scientific = F
          ),
          " VND"
        )))
      }
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

    output$companyAddress4 <- renderUI({
      " ĐT: (028)-6272 2381 - Mobile: 097.555.6469 - www.quaquedatviet.com"
    })
    output$reciept <- renderUI({
      strong("HÓA ĐƠN")
    })

    # Ten khach hang in tren hoa don
    output$cusName <- renderUI({
      base <- dbReadTable(con, "live_order")
      if (input$cusNames == "") {
        if (input$debtNameID == "") {
          strong(paste0("Tên khách hàng: ",  base[1, 6]))
        } else
          strong(paste0("Tên khách hàng: ",  input$debtNameID))

      } else
        strong(paste0("Tên khách hàng: ",  input$cusNames))
    })
    #Thong tin dia chi SDT cua khach hang tren hoa don
    output$cusInfor <- renderUI({
      em(
        paste0(
          "Đia chỉ: ",
          input$house,
          " - ",
          input$commune,
          " - ",
          input$district ,
          " - ",
          input$city,
          " -  SDT: ",
          input$phone
        )
      )
    })

    # Chu ky nguoi mua va nguoi ban
    output$buyerSign <- renderUI({
      "Người mua"
    })
    output$sellerSign <- renderUI({
      "Người bán (Đã Ký)"
    })

    # Ten nguoi mua hang, neu de trong thi tu dong dat ten la Khach La hoac ten nguoi no
    output$buyer <- renderUI({
      base <- dbReadTable(con, "live_order")
      if (input$cusNames == "") {
        if (input$debtNameID == "") {
          base[1, 6]
        } else
          input$debtNameID
      } else
        input$cusNames
    })

    # Ten nguoi ban
    output$seller <- renderUI({
      name <- dbReadTable(con, "active_user")
      name[3]

    })

    # Ngay ban
    output$date1 <- renderUI({
      paste0("Ngày ", dbReadTable(con, "live_order")[1, 5])
    })


    output$thanks <- renderUI({
      strong("Cảm ơn Quý khách hàng và hẹn gặp lại !")
    })

    # Tao ma so hoa don tu dong
    letter <- function(n = 5000) {
      do.call(paste0, replicate(2, sample(LETTERS, n, TRUE), FALSE))
    }

      no <- paste0("No: ",
               round(runif(1, min = 000000, max = 100000)),
               letter(1),
               "/", format(Sys.Date(), "%Y%m%d"))
      
      dbWriteTable(conn = con,
                   name = "invoice_no",
                   value = as.data.frame(no),
                   row.names = FALSE,
                   overwrite = TRUE)
      
      output$recieptNo <- renderUI({
        no
    })



    # Luu hoa don 1 ban copy vao he thong
    observeEvent(input$updateCustom, {

      no<- as.character(dbReadTable(con, "invoice_no"))[1]
      name <- as.data.frame(dbReadTable(con, "active_user"))
      base <- dbReadTable(con, "live_order")
      invoice <- dbReadTable(con, "invoice")

      VATValue <- vatCalculate(sum(base[4]) * as.numeric(input$vatValue) / 100,
                     0)
      if (input$ship == "Miễn phí") {
        shiping <- 0
      } else{
        shiping <- input$discount_amount
      }


      dataCustomer <-
        data.frame(
          NO = no,
          Name =  base[1, 6],
          Phone = input$phoneNo,
          CMND = input$CMND,
          select(base, Item:Date, Status),
          Prepaid =  as.numeric(input$cusPay),
          VAT = VATValue,
          Delivery = shiping,
          Remaind = as.numeric(input$cusPay) - (sum(base[4]) + VATValue + shiping),
          Seller =  name[1, 3],
          Address = paste0(input$commune, "-", input$district, "-", input$city),
          Option = input$online_offline
        )



      if (input$cusNames != "") {
        dataCustomer$Name <- input$cusNames
      } else if (input$debtNameID != "") {
        dataCustomer$Name <- input$debtNameID
      } else
        dataCustomer$Name <- base[1, 6]

      if (input$phone != "") {
        dataCustomer$Phone == input$phone
      }


      if (dataCustomer$Remaind >= 0) {
        dataCustomer$Remaind == 0
      }

      dbWriteTable(conn = con,
                   name = "invoice",
                   value = dataCustomer,
                   row.names = FALSE,
                   append = TRUE)


      sendSweetAlert(
        session = session,
        title = paste("Hóa đơn ", no, "đã được lưu"),
        type = "success"
      )

      shinyjs::show("revokeReceipt")
      shinyjs::show("print")
    })



    # Huy Hoa don va quay lai mua hang
    observeEvent(input$revokeReceipt, {
      base <- dbReadTable(con, "live_order")

      dbExecute(con, "DELETE FROM live_order;")

      updateTabItems(session, "tabs", selected = "sellData")
      updateNumericInput(
        session,
        "cusPay",
        value = 0,
        min = 1000,
        step = 1000
      )
      updateTextInput(session, "tong", value = "")
      output$liveOrder <- sellproductTable ()
      shinyjs::hide("pay")
      shinyjs::hide("debtNameID")
      shinyjs::hide("phoneNo")
      shinyjs::hide("CMND")
      shinyjs::hide("saveInfor")
      updateTextInput(session, "debtNameID", value = "")
      updateTextInput(session, "phoneNo", value = 0)
      updateTextInput(session, "CMND", value = 0)
      updateTextInput(session, "cusNames", "Tên khách hàng", value = "")
      updateTextInput(session, "house", "Số nhà", value = "")
      updateTextInput(session, "city", "Thành phố", value = "")
      updateTextInput(session, "district", "Huyện", value = "")
      updateTextInput(session, "commune", "Xã", value = "")
      updateTextInput(session, "phone", "SDT", value = "")

      displaySUM <- function(x) {
        list_Live_Order <- dbReadTable(con, "live_order")
        if (input$vat == TRUE) {
          paste0(format(
            sum(
              list_Live_Order[4] + (list_Live_Order[4] * as.numeric(input$vatValue) / 100)
            ),
            big.mark = ",",
            scientific = F
          ), " VND")
        } else
          paste0(format(
            sum(list_Live_Order[4]),
            big.mark = ",",
            scientific = F
          ), " VND")
      }
      output$tong <- renderUI({
        displaySUM()
      })
    })


    # In hoa don
    observeEvent(input$print, {
      js$winprint()
       dbReadTable(con, "live_order")

    })

  })


  ##############################QUAN LY NO #####################################

  debtName <- dbReadTable(con, "debt")

  observe({
    updateSelectInput(session, "debtList", choices = dbReadTable(con, "supplier_debt")[, 1])
  })
  observe({
    updateSelectInput(session,
                      "phoneID",
                      choices = filter(dbReadTable(con, "supplier_debt"),
                                       Customer == input$debtList)[, 2])
  })
  observe({
    updateSelectInput(
      session,
      "CMNDID",
      choices = filter(
        dbReadTable(con, "supplier_debt"),

        Customer == input$debtList &  Phone == input$phoneID
      )[, 3]
    )
  })


  own_table <- function(X) {
    renderDT({
      datatable(
        select(
          filter(
            dbReadTable(con, "debt"),
            Customer == input$debtList &
              Phone == input$phoneID &
              CMND == input$CMNDID
          ),
          Item:Total,
          Date,
          Status
        ),
        extensions = 'Buttons',
        escape = T,
        selection = "none",
        option = list(
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

  output$own_detail_title <-
    renderUI("CHI    TIẾT    SẢN    PHẨM    ĐÃ    MUA")

  own_table_1 <- function(X) {
    renderDT({
      datatable(
        filter(
          dbReadTable(con, "supplier_debt"),
          Customer == input$debtList &
            Phone == input$phoneID & CMND == input$CMNDID
        ),
        extensions = 'Buttons',
        escape = T,
        selection = "none",
        option = list(
          dom = 't',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json')
        )
      ) %>%
        formatStyle(
          11,
          target = "cell",
          color = "white",
          backgroundColor = styleEqual(c("Hết nợ", "Vẫn còn nợ"), c('green', 'red')),
          fontWeight = 'bold',

        ) %>%
        formatCurrency(
          5:8,
          currency = "VND ",
          interval = 3,
          mark = ",",
          digits = 0
        )
    })
  }


  output$individualDebt <- own_table()

  output$own_summaryTable <- own_table_1()


  done_table <- function() {
    tableList <-
      filter(dbReadTable(con, "supplier_debt"),
             Status == "Hết nợ")
    renderDT({
      datatable(
        tableList,
        escape = T,
        selection = "none",
        option = list(dom = 't')
      )  %>% formatStyle(
        11,
        target = "cell",
        color = "white",
        backgroundColor = "green",
        fontWeight = 'bold'
      ) %>%
        formatCurrency(
          5:8,
          currency = "VND ",
          interval = 3,
          mark = ",",
          digits = 0
        )
    })
  }
  output$done_debt <- done_table()


  onw_name_statistic <- function() {
    output$own_name <- renderValueBox({
      filter_data <- filter(
        dbReadTable(con, "supplier_debt"),
        Customer == input$debtList &
          Phone == input$phoneID &
          CMND == input$CMNDID
      )

      if (isTRUE(filter_data[1, 11]  == "Hết nợ")) {
        iconstuatus = icon("thumbs-up")
        colorStatus = "green"
      } else {
        iconstuatus = icon("id-card")
        colorStatus = "yellow"
      }

      valueBox(filter_data[1, 1],
               "Khách hàng",
               icon = iconstuatus,
               color = colorStatus)
    })
  }

  onw_name_statistic()

  own_amount_statistic <- function() {
    output$own_amount <- renderValueBox({
      filter_data <- filter(
        dbReadTable(con, "supplier_debt"),
        Customer == input$debtList &
          Phone == input$phoneID &
          CMND == input$CMNDID
      )

      if (isTRUE(filter_data[1, 11]  == "Hết nợ")) {
        iconstuatus = icon("thumbs-up")
        colorStatus = "green"
      } else {
        iconstuatus = icon("money-check-alt")
        colorStatus = "yellow"
      }
      valueBox(paste0(format(
        (
          filter(
            dbReadTable(con, "supplier_debt"),
            Customer == input$debtList &
              Phone == input$phoneID &
              CMND == input$CMNDID
          )[1, 5] +   filter(
            dbReadTable(con, "supplier_debt"),
            Customer == input$debtList &
              Phone == input$phoneID &
              CMND == input$CMNDID
          )[1, 6]
        )
        ,
        big.mark = ",",
        scientific = F
      ),
      " VND"),

      "Tổng số nợ",
      icon = iconstuatus,
      color = colorStatus)
    })
  }

  own_amount_statistic()

  own_prepaid_func <- function() {
    output$onw_prepaid <- renderValueBox({
      filter_data <- filter(
        dbReadTable(con, "supplier_debt"),
        Customer == input$debtList &
          Phone == input$phoneID &
          CMND == input$CMNDID
      )

      if (isTRUE(filter_data[1, 11]  == "Hết nợ")) {
        iconstuatus = icon("thumbs-up")
        colorStatus = "green"
      } else {
        iconstuatus = icon("dollar-sign")
        colorStatus = "yellow"
      }
      valueBox(paste0(format(
        filter(
          dbReadTable(con, "supplier_debt"),
          Customer == input$debtList &
            Phone == input$phoneID &
            CMND == input$CMNDID
        )[1, 7],
        big.mark = ",",
        scientific = F
      ),
      " VND"),

      "Đã trả trước",
      icon = iconstuatus,
      color = colorStatus)
    })
  }

  own_prepaid_func()

  own_remain_func <- function() {
    output$onw_remain <- renderValueBox({
      filter_data <- filter(
        dbReadTable(con, "supplier_debt"),
        Customer == input$debtList &
          Phone == input$phoneID &
          CMND == input$CMNDID
      )

      if (isTRUE(filter_data[1, 11]  == "Hết nợ")) {
        iconstuatus = icon("thumbs-up")
        colorStatus = "green"
      } else {
        iconstuatus = icon("comment-dollar")
        colorStatus = "yellow"
      }
      Amount <-
        filter(
          dbReadTable(con, "supplier_debt"),
          Customer == input$debtList &
            Phone == input$phoneID & CMND == input$CMNDID
        )

      valueBox(
        paste0(format(
          Amount[1, 8],
          big.mark = ",",
          scientific = F
        ),
        " VND"),
        "Còn lại (bao gồm thuế)",
        icon = iconstuatus,
        color = colorStatus
      )
    })
  }

  own_remain_func()






  #XOa no dua vao ten khach hang
  observeEvent(input$returnDebt, {
    debtName <- dbReadTable(con, "debt")
    debtSummary <- dbReadTable(con, "supplier_debt")

    filter_name <- filter(
      debtSummary,
      debtSummary$Customer == input$debtList &
        debtSummary$Phone == input$phoneID &
        debtSummary$CMND == input$CMNDID
    )[1, ]


    filter_name$CusReturn <-
      paste("Đã trả ",
            input$own_return_amount,
            " VND vào ngày ",
            format(Sys.time(), "%Y-%m-%d"))



    filter_name$Remain <-
      filter_name$Total[1] - (filter_name$Repaid[1] + input$own_return_amount)


    filter_name$Repaid[1] <-
      filter_name$Repaid[1] + input$own_return_amount


    if (filter_name$Remain <= 0) {
      filter_name$Status[1] <- "Hết nợ"
    }  else{
      filter_name$Status[1] <- "Vẫn còn nợ"
    }


    newData <- rbind(filter_name, debtSummary)

    row <-
      which(
        newData$Customer == input$debtList &
          newData$Phone == input$phoneID &
          newData$CMND == input$CMNDID &
          newData$Status == "Nợ"
      )

    newData[row, 10] <- "Vẫn còn nợ"


    dbWriteTable(conn = con,
                 name = "supplier_debt",
                 value = newData,
                 row.names = FALSE,
                 overwrite = TRUE)

    # Hoan lai tien vao hoa don dang no

    invoice <- dbReadTable(con, "invoice")
    row <- which(
      invoice$Name == input$debtList &
        invoice$Phone == input$phoneID &
        invoice$CMND == input$CMNDID
    )

    own_data <- filter(invoice,
                       Name == input$debtList &
                         Phone == input$phoneID &
                         CMND == input$CMNDID)[14]

    invoice[row, 14] <- as.numeric(own_data) + input$own_return_amount

    dbWriteTable(conn = con,
                 name = "invoice",
                 value = invoice,
                 row.names = FALSE,
                 append = TRUE)


    observe({
      updateSelectInput(session, "debtList", choices = dbReadTable(con, "supplier_debt")[, 1])
    })
    observe({
      updateSelectInput(session,
                        "phoneID",
                        choices = filter(dbReadTable(con, "supplier_debt"),
                                         Customer == input$debtList)[, 2])
    })
    observe({
      updateSelectInput(
        session,
        "CMNDID",
        choices = filter(
          dbReadTable(con, "supplier_debt"),

          Customer == input$debtList &  Phone == input$phoneID
        )[, 3]
      )
    })

    updateNumericInput(session, "own_return_amount", value = "")

    sendSweetAlert(
      session = session,
      title = paste0("Khách hàng đã trả ", input$own_return_amount),
      type = "warning ",
      btn_labels = "Đóng"
    )

    output$individualDebt <- own_table()

    output$own_summaryTable <- own_table_1()

    onw_name_statistic()
    own_amount_statistic()
    own_prepaid_func()
    own_remain_func()

    output$done_debt <- done_table()
  })



  ###############################PHUC HOI DU LIEU ############################
  
  ## SKIN MODIFICATION
  observeEvent(input$theme_confirm, {
    listSkin <- input$theme
    dbWriteTable(conn = con,
                 name = "skin",
                 value = as.data.frame(listSkin),
                 row.names = FALSE,
                 overwrite = TRUE)
    output$note_theme <- renderUI({
      "Vui lòng đóng và mở lại chương trình để thay đổi"
    })
    dbReadTable(conn = con, "skin")
  })
  output$note_theme <- renderUI({
    paste("Màu sắc đang sử dụng là ", listSkin)
  })

  


  observeEvent(input$backupData, {
    withProgress(message = 'Vui lòng chờ ! đang tải dữ liệu lên Google Drive ...', value = 0.6, {

      name <- dbReadTable(con, "active_user")

      tryCatch({
        drive_auth(email = "quan@kkumail.com", path = "data/client_secret_1050780348924-pjk3mne5o382fbbkla7fm48qgedcs9eq.apps.googleusercontent.com.json")

        old <- read.xlsx("data/backupdate.xlsx")
        date <- format(Sys.time(), "%d-%m-%Y")

        upload_file <- function(fileName) {
          drive_upload(
            paste0("data/", fileName, ".xlsx"),
            name = paste0(name[1], "_", fileName, "_", date, ".xlsx"),
            path = paste0("QUAQUEDATVIET/", name[1]),
            overwrite = TRUE
          )
        }

        for (i in 1:length(fileName)) {
          upload_file (fileName[i])
        }

        new <- rbind(date, old)
        write.xlsx(new, file = "data/backupdate.xlsx", overwrite = T)

        sendSweetAlert(
          session = session,
          title = paste("Dữ liệu được cập nhật vào ngày ", date),
          type = "success"

        )
      },
      error = function(e) {
        showModal(
          modalDialog(
            div(
              paste0(
                "Máy tính bạn chưa kết nối Internet hoặc là dữ liệu ngày đang chọn không tồn tại"
              )
            ),
            title = "MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
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
    withProgress(message = 'Vui lòng chờ ! đang phục hồi ...', value = 0.8, {
      # load("data/activeMember.Rda")
      name <- dbReadTable(con, "active_user")
      tryCatch({
        drive_auth(email = "quan@kkumail.com", path = "data/client_secret_1050780348924-pjk3mne5o382fbbkla7fm48qgedcs9eq.apps.googleusercontent.com.json")


        download_file <- function(fileName) {
          drive_download(
            paste0(name[1], "_", fileName, "_", input$dateData, ".xlsx"),
            path = paste0("data/", fileName, ".xlsx"),
            overwrite = TRUE
          )
        }

        for (i in 1:length(fileName)) {
          download_file (fileName[i])
        }

        sendSweetAlert(
          session = session,
          title = paste("Dữ liệu ngày ", input$dateData, " đã được phục hồi"),
          type = "success"

        )
      },
      error = function(e) {
        showModal(
          modalDialog(
            title = "MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
            footer = modalButton("ĐÓNG"),
            easyClose = T
          )
        )
        return()
      })
    })

  })



  output$remindBackup <- renderUI({
    name <- dbReadTable(con, "active_user")
    em(
      paste(
        "LƯU Ý: Dữ liệu backup và cập nhật chỉ áp dụng riêng cho tài khoản ",
        name[1, 3] ,
        " mà thôi. Dữ liệu này sẽ KHÔNG chia sẽ với các tài khoản khác"
      )
    )
  })


  ### TU dong update theo thoi gian
  load("data/settime.rda")
  toStop <- as.POSIXct(timeStop, format = "%H:%M:%S")
  toStop2 <- toStop + 604800

  output$settime <-
    renderUI(paste0("Dữ liệu trên Google sẽ tự động xóa vào lúc ", toStop2))
  output$title_auto_update <-
    renderUI(strong("HOẶC HẸN THỜI GIAN CẬP NHẬT TỰ ĐỘNG"))
  load("data/settime.rda")
  output$auto_update_time <-
    renderUI(paste0(
      "Hệ thống sẽ tự động cập nhật dữ liệu lúc ",
      as.POSIXct(timeStop, format = "%H:%M:%S")
    ))

  observeEvent(input$setup_time, {
    timeStop <-
      paste0(input$hours, ":", input$minutes, ":", input$seconds)
    save(timeStop, file = "data/settime.rda")
    sendSweetAlert(
      session = session,
      title = "KÍCH HOẠT TỰ ĐỘNG CẬP NHẬT",
      text = paste0(
        "Hệ thống sẽ tự động cập nhật dữ liệu lên Google Drive vào lúc ",
        input$hours,
        ":",
        input$minutes,
        ":",
        input$seconds
      ),
      type = "info"
    )

    autoUpdate()

  })


  ## AUTOMATICALLY BACKUP DATABASE
  
  autoUpdate <-  function() {
    observe({
      load("data/settime.rda")
      toStop <- as.POSIXct(timeStop, format = "%H:%M:%S")
      toStop2 <- toStop + 15
      toStop3 <- toStop - 1

      invalidateLater(1000)

      if ((Sys.time() > toStop3) & (Sys.time() < toStop)) {
        sendSweetAlert(
          session = session,
          title = "THÔNG BÁO CẬP NHẬT",
          text =
            "Hệ thống bắt đầu tự động cập nhật dữ liệu lên Google Drive. Vui lòng chờ trong giây lát.",
          type = "info"

        )
      }

      if ((Sys.time() > toStop) & (Sys.time() < toStop2)) {
        withProgress(message = 'Vui lòng chờ ! đang tải dữ liệu lên Google Drive ...', value = 0.6, {
      
          name <- dbReadTable(con, "active_user")
          tryCatch({
            drive_auth(email = "quan@kkumail.com", path = "data/client_secret_1050780348924-pjk3mne5o382fbbkla7fm48qgedcs9eq.apps.googleusercontent.com.json")

            old <- read.xlsx("data/backupdate.xlsx")
            date <- format(Sys.time(), "%d-%m-%Y")
            
            upload_file <- function(fileName) {
              drive_upload(
                paste0("data/", fileName, ".xlsx"),
                name = paste0(name[1], "_", fileName, "_", date, ".xlsx"),
                path = paste0("QUAQUEDATVIET/", name[1]),
                overwrite = TRUE
              )
            }

            for (i in 1:length(fileName)) {
              upload_file (fileName[i])
            }

            new <- rbind(date, old)
            write.xlsx(new, file = "data/backupdate.xlsx", overwrite = T)
            
            sendSweetAlert(
              session = session,
              title = paste("Dữ liệu được cập nhật vào ngày ", date),
              type = "success"

            )

          },
          error = function(e) {
            showModal(
              modalDialog(
                title = "MÁY TÍNH CHƯA KẾT NỐI INTERNET" ,
                footer = modalButton("ĐÓNG"),
                easyClose = T
              )
            )
            return()
          })

        })

      }

    })
  }

  autoUpdate()



  # TU DONG XOA SAU 7 ngay
  observe({
    load("data/settime.rda")
    toStop <- as.POSIXct(timeStop, format = "%H:%M:%S")
    toStop2 <- toStop + 604800


    invalidateLater(1000)

    if ((Sys.time() > toStop2) & (Sys.time() < toStop)) {
      withProgress(message = 'Xóa dữ liệu',
                   detail = 'Vui lòng chờ  ..',
                   value = 0.8,
                   {
                     name <- dbReadTable(con, "active_user")
                     drive_auth(email = "quan@kkumail.com", path = "data/client_secret_1050780348924-pjk3mne5o382fbbkla7fm48qgedcs9eq.apps.googleusercontent.com.json")

                     b <- 1

                     for (i in 7:14) {
                       b[i] <- Sys.Date() - i

                       for (j in 1:length(fileName)) {
                         drive_rm(paste0(name[1], "_", fileName[j], "_", (
                           format(b[i], format = "%Y-%m-%d")
                         ), ".xlsx"))
                       }
                     }


                   })
    }
  })



  #UPDATE CODES TO GG

  observeEvent(input$updateCode, {
    tryCatch({
      withProgress(message = 'Vui lòng chờ ! đang nâng cấp  ...', value = 0.8, {
        drive_auth(email = "quan@kkumail.com", path = "data/client_secret_1050780348924-pjk3mne5o382fbbkla7fm48qgedcs9eq.apps.googleusercontent.com.json")

        drive_download("QUAQUEDATVIET/app.R",
                       path = "app.R",
                       overwrite = TRUE)
        sendSweetAlert(
          session = session,
          title = paste("Phiên bản ", version, " đã được cập nhật ngày", updateDate),
          type = "success"
        )
        refresh()
      })
    },
    error = function(e) {
      showModal(
        modalDialog(
          div(paste0("No internet connection or unauthorisation to the database")),
          title = "CONNECT DETERMINATED" ,
          footer = modalButton("ĐÓNG"),
          easyClose = T
        )
      )
      return()
    })
  })

  output$updateNote <- renderUI({
    em(paste("Phiên bản: ", version, " cập nhật ngày: ", updateDate))
  })

  output$debug <- renderUI({
    paste(change)
  })


  
  
  #$ DOWNLOAD DATA FROM DATABASE
  
    product <- dbReadTable(con, "product")
    output$excel_database <- downloadHandler(
      filename = function(){"product.csv"}, 
      content = function(fname){
        write.csv(product, fname)
      }
    )

    stock <- dbReadTable(con, "stock")
    output$excel_khohang <- downloadHandler(
      filename = function(){"stock.csv"}, 
      content = function(fname){
        write.csv(stock, fname)
      }
    )
    
    receipt <- dbReadTable(con, "supplier_receipt")
    output$excel_donhang <- downloadHandler(
      filename = function(){"supplier_receipt.csv"}, 
      content = function(fname){
        write.csv(receipt, fname)
      }
    )

    supplier_debt <- dbReadTable(con, "supplier_receipt")
    output$excel_debt <- downloadHandler(
      filename = function(){"supplier_debt.csv"}, 
      content = function(fname){
        write.csv(supplier_debt, fname)
      }
    )
    
    debt <- dbReadTable(con, "debt")
    output$excel_debtlist <- downloadHandler(
      filename = function(){"debt.csv"}, 
      content = function(fname){
        write.csv(debt, fname)
      }
    )
 
    invoice <- dbReadTable(con, "invoice")
    output$excel_invoice <- downloadHandler(
      filename = function(){"invoice.csv"}, 
      content = function(fname){
        write.csv(invoice, fname)
      }
    )
    

   ## VIDEO
    
  output$video <- renderUI({
    HTML(
      '<iframe width="100%" height="400" src="https://www.youtube.com/embed/0lwLgeqaV6M" title="QQDV" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
    )
  })
  
  ## DELETE ALL DATABASE
  
  observe(updateSelectInput(
    session,
    "reset_metadata",
    "Chọn tên dữ liệu cần xóa",
    choices = c(
      "product",
      "stock",
      "supplier_receipt",
      "supplier_debt",
      "debt",
      "invoice"
    )
  ))

  observeEvent(input$reset_metadata_button, {
    dbExecute(con, paste0("DELETE FROM ", input$reset_metadata ,";"))
    sendSweetAlert(
      session = session,
      title = paste("Dữ liệu ", input$reset_metadata, " đã xóa"),
      type = "success"
    )
  })
  
}


# Run the application

shinyApp(ui = ui, server = server)
