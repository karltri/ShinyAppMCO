# ShinyAppMCO
App de shiny que se encarga de comparar las respuestas usuario administradas contra los cálculos por MCO de regresiones lineales múltiples.

      library(shiny)
      library(readxl)
      library(lubridate)
      library(ggplot2)
      library(dplyr)
      library(readr)
      library(shinythemes)
      library("car")                   
      library("tidyr")
      library(stargazer)
      library(nortest)
      library(lmtest)



      ui <- fluidPage(
        theme=shinytheme("flatly"),
        navbarPage("RegresiÃ³n lineal",
                   #Inicia DIY
                   tabPanel("Bases pre cargadas",
                            sidebarLayout(
                              ##Panel lateral

                              #Inicia sidebar
                              sidebarPanel(
                                #Titulo
                                titlePanel("SelecciÃ³n de data set"),
                                textInput("u_adm", "Respuesta",width="100%", placeholder = "Ingrese valores separados por una coma."),

                                #Seleccion de data set
                                selectInput("dataset_M", label = "Dataset", 
                                            choices = c("airquality","anscombe","attenu","attitude","beaver1",
                                                        "beaver2","BOD","cars","ChickWeight","DNase","eruptions",
                                                        "Formaldehyde","freeny","Indometh","infert","iris","LifeCycleSavings",
                                                        "Loblolly","longley","morley","mtcars","Orange","pressure",
                                                        "Puromycin","quakes","randu","rock"),"attitude"),

                                ###Explorar variables
                                uiOutput('op_vary_M'),
                                uiOutput('op_varx_M'),
                                ##Descargar datos
                                downloadButton("downloadData", "Descargar"),
                                ##Summary de datos
                                verbatimTextOutput("summary_expl_t1_M"),
                              ),
                              #^termina sidebar
                              #Inicia main
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("ExploraciÃ³n de data sets",
                                           fluidRow(
                                             column(6,
                                                    h4("CorrelaciÃ³n de las variables selecionadas:"),
                                             ),
                                             column(3,
                                                    tableOutput("cor"),
                                             ),
                                           ),
                                           #Tabla del dataset
                                           tableOutput("table_expl"),
                                           #plot exploratorio
                                           plotOutput("plot_exp")
                                          ),

                                  tabPanel("Respuestas",
                                           #verbatimTextOutput("summary_reg"),

                                           fluidRow(
                                             column(5,
                                                    sliderInput("ic_M","Intervalo de confianza",0.5,0.99,0.9),
                                                    uiOutput('var_niv'),
                                             ),
                                             column(7,
                                                    h4("Modelo calculado"),
                                                    tableOutput("beta"),

                                             ),

                                           ),
                                           #comparacion de errores
                                           tableOutput("TablaErrores"),
                                           #plot regresion
                                           plotOutput("plotreg_M"),

                                           #Opcion de hacer predicciones
                                           textInput("pred_M", "Predecir con el modelo ajustado",width="100%", placeholder = "Ingrese valores separados por una coma y vectores separados con una diagonal ej. 4.2, 4.4/ 5, 5.03/12,43 etc."),

                                           DT::DTOutput("dt_pred_M")

                                  ),

                                  tabPanel("Residuales",
                                           h2("GrÃ¡fica de residuales"),
                                           uiOutput("var_ErroresEx"),
                                           plotOutput("plotResiduales"),

                                           h2("Normalidad de residuales"),
                                           fluidRow(

                                             column(8,
                                                    h3("GrÃ¡fica de normalidad"),
                                                    plotOutput("plotNormResi"),
                                                    h3("GrÃ¡fica de varianza"),
                                                    plotOutput("plotVarianza"),
                                             ),

                                             column(4,
                                                    h3("Pruebas de hipÃ³tesis"),
                                                    sliderInput("ic_pr_M","Valor de rechazo",0.001,0.2,0.05),

                                                    h4("Normalidad"),
                                                    textOutput("tnorm_r_M"),
                                                    textOutput("tnorm_r_cad_M"),


                                             )
                                           ),
                                  )

                              ),
                              )
                            #^Termina main
                            )
        ),
        #^Finaliza diy
        tabPanel("Datos propios",
                 sidebarLayout(
                   ##Panel lateral

                   #Inicia sidebar
                   sidebarPanel(
                     #Titulo
                     titlePanel("SelecciÃ³n de data set"),
                     textInput("u_adm_p", "Respuesta",width="100%", placeholder = "Ingrese valores separados por una coma."),
                     fileInput('file1', 'Choose file to upload',
                               accept = c(
                                 'text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                               )
                     ),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Coma=',',
                                    PuntoComa=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Comillas'='"',
                                    'Apostrofe'="'"),
                                  '"'),
                     tags$hr(),

                     ###Explorar variables
                     uiOutput('op_vary_M_p'),
                     uiOutput('op_varx_M_p'),


                   ),
                   #^termina sidebar
                   #Inicia main
                   mainPanel(
                     tabsetPanel(
                       tabPanel("ExploraciÃ³n del data set",

                                #summary de datos
                                verbatimTextOutput("summary_expl_t1_M_p"),
                                fluidRow(
                                  column(6,
                                         h4("CorrelaciÃ³n de las variables selecionadas:"),
                                  ),
                                  column(3,
                                         tableOutput("cor_p"),
                                  ),
                                ),

                                #Tabla del dataset
                                tableOutput("table_expl_p"),
                                #plot exploratorio
                                plotOutput("plot_exp_p")
                       ),

                       tabPanel("Respuestas",
                                #verbatimTextOutput("summary_reg"),

                                fluidRow(
                                  column(5,
                                         sliderInput("ic_M_p","Intervalo de confianza",0.5,0.99,0.9),
                                         uiOutput('var_niv_p'),
                                  ),
                                  column(7,
                                         h4("Modelo calculado"),
                                         tableOutput("beta_p"),


                                  ),

                                ),
                                #comparacion de errores
                                tableOutput("TablaErrores_p"),
                                #plot regresion
                                plotOutput("plotreg_M_p"),

                                #Opcion de hacer predicciones
                                textInput("pred_M_p", "Predecir con el modelo ajustado",width="100%", placeholder = "Ingrese valores separados por una coma y vectores separados con una diagonal ej. 4.2, 4.4/ 5, 5.03/12,43 etc."),

                                DT::DTOutput("dt_pred_M_p")

                       ),
                       # 
                        tabPanel("Residuales",
                                 h2("GrÃ¡fica de residuales"),
                                 uiOutput("var_ErroresEx_p"),
                                 plotOutput("plotResiduales_p"),
                       #          
                                 h2("Normalidad de residuales"),
                                fluidRow(

                                  column(8,
                                         h3("GrÃ¡fica de normalidad"),
                                         plotOutput("plotNormResi_p"),
                                         h3("GrÃ¡fica de varianza"),
                                         plotOutput("plotVarianza_p"),
                                  ),

                                  column(4,
                                         h3("Pruebas de hipÃ³tesis"),
                                         sliderInput("ic_pr_M_p","Valor de rechazo",0.001,0.2,0.05),

                                         h4("Normalidad"),
                                         textOutput("tnorm_r_M_p"),
                                         textOutput("tnorm_r_cad_M_p"),


                                  )
                                ),
                        )

                     ),
                   )
                   #^Termina main
                 )
        ),
        #^Finaliza diy

      ),


      #^Finaliza navBar
      )



      ######
      # Define server
      server <- function(input, output, session) {


        rval_dataset_M<-reactive({
          drop_na(get(input$dataset_M, "package:datasets"))
        })

        output$summary_expl_t1_M <- renderPrint({
          summary(rval_dataset_M())
        })

        output$op_vary_M<-renderUI({
          selectInput("vary_M", label = "Variable dependiente",names(rval_dataset_M()),names(rval_dataset_M())[1])
        })
        output$op_varx_M<-renderUI({
          checkboxGroupInput("varx_M", label = "Variable explicativa",names(rval_dataset_M()),names(rval_dataset_M())[2])
        })

        rval_betauser<-reactive({
          as.numeric(strsplit(input$u_adm, ",")[[1]])
        })

        ##Correlacion (valor)
        correl<-reactive({
          datos<-rval_dataset_M()
          n<-length(input$varx_M)
          a<-array(dim=c(n))

          for (i in seq(n)){
            a[i]<-cor(datos[,input$varx_M[i]],datos[,input$vary_M])
          }

          data.frame(y=input$varx_M,"Correlacion con x"=a)
        })

        output$cor<-renderTable({
          correl()
        })

        ##Algunos datos del data set
        output$table_expl <- renderTable({
          head(rval_dataset_M())
        })

        ##Correlacion (grafica)
        output$plot_exp<-renderPlot({
          datos<-rval_dataset_M()
          pairs(datos)

        })

        ##descargar dataset
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(input$dataset_M, ".csv", sep = "")
          },
          content = function(file) {
            write.csv(rval_dataset_M()[c(input$varx_M,input$vary_M)], file, row.names = FALSE)
          }
        )

        #Seleccionar con respecto a que variable se mostrara la grafica
        output$var_niv<-renderUI({
          selectInput("ig_niv", label = "Mostrar grÃ¡fica con respecto a:",input$varx_M)
        })

        ##Beta



        #Coeficientes de regresion  
        beta<-function(){
          #Variables
          datos<-rval_dataset_M()
          r<-length(input$varx_M)
          nomb<-c("alfa")
          n<-length(datos[,input$vary_M])

          d<-data.frame(u=rep(1,n))

          #Construccion de matriz X, con 1 en una columna y vector y
          for(i in seq(r)){
            d<-data.frame(d,datos[,input$varx_M[i]])
            no<-paste("beta.",input$varx_M[i])
            nomb<-c(nomb,no)
          }

          #convertir tipo de dato
          vec<-as.matrix(datos[,input$vary_M])
          mat<-as.matrix(d)

          #Calculo de beta
          beta<-(solve(t(mat) %*% mat))%*% t(mat) %*% vec

          #Y estimado
          y.est=mat %*% beta

          #Error
          e=vec-y.est

          #suma e cuadrada
          se_c<-as.numeric((t(e) %*% e)/(n-r-1))

          #matriz de var covar
          vec_cov<-se_c*(solve(t(mat) %*% mat))


          #tstudent
          t<-qt((1-input$ic_M)/2,n-r-1, lower.tail = F)

          error.est<-c()
          t.est<-c()
          p.value<-c()
          for(i in seq(dim(vec_cov)[1])){
            error.est<-c(error.est,sqrt(vec_cov[i,i]))
            t.est<-c(t.est,beta[i]/error.est[i])
            p.value<-c(p.value,(1-pt(abs(t.est[i]),dim(mat)[1]-length(beta)))*2)
          }

          #intervalo de confianza
          lim.inf<-error.est-t*beta
          lim.sup<-error.est+t*beta
          param<-data.frame(parametro=nomb,valor=beta,error.est,t.est,p.value,lim.inf,lim.sup)
          return(param)
        }

        #Output de parametros e intervalos de confianza
        output$beta<-renderTable({beta()})

        rval_beta<-reactive({beta()})


        #Valores ajustados 
        adjusted<-function(beta){

          #Variables
          datos<-rval_dataset_M()
          r<-length(input$varx_M)

          n<-length(datos[,input$vary_M])

          d<-data.frame(u=rep(1,n))

          #Construccion de matriz X, con 1 en una columna y vector y
          for(i in seq(r)){
            d<-data.frame(d,datos[,input$varx_M[i]])
          }

          #convertir tipo de dato
          vec<-as.matrix(datos[,input$vary_M])
          mat<-as.matrix(d)

          #Calculo de beta
          beta<-as.matrix(beta)#rval_beta()[,2])

          #Y estimado
          y.est=mat %*% beta

          #Error
          e=vec-y.est

          #suma e cuadrada
          se<-sqrt(as.numeric((t(e) %*% e)/(n-r-1)))

          #tstudent
          t<-qt((1-input$ic_M)/2,n-r-1, lower.tail = F)

          x<-mat[,-1]

          lim.punt.inf<-c()
          lim.punt.sup<-c()
          lim.med.inf<-c()
          lim.med.sup<-c()

          #intervalo de confianza
          for(i in seq(n)){

            if(r>1){xx<-x[i,]}
            else{xx<-x[i]}

            lim.punt.inf<-c(lim.punt.inf,y.est[i]-t*se*sqrt(1+t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.punt.sup<-c(lim.punt.sup,y.est[i]+t*se*sqrt(1+t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.med.inf<-c(lim.med.inf,y.est[i]-t*se*sqrt(t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.med.sup<-c(lim.med.sup,y.est[i]+t*se*sqrt(t(xx) %*% solve(t(x) %*% x)%*%xx))

          }

          #lim.punt.inf<-(-t*se)*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.punt.sup<-y.est+t*se*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))

          #lim.med.inf<-y.est-t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.med.sup<-y.est+t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))

          #data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup)
          param<-data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup,error=e)
          return(param)

        }

        ##Comparacion de errores
        output$TablaErrores<-renderTable({
          data.frame(Modelo=c("Usuario","MCO"),
                     `Suma de errores`=c(sum(adjusted(rval_betauser())[,6]),
                                         sum(adjusted(rval_beta()[,2])[,6])),
                     `Suma de errores^2`=c(sum(t(adjusted(rval_betauser())[,6])%*%adjusted(rval_betauser())[,6]),
                                         sum(t(adjusted(rval_beta()[,2])[,6])%*%adjusted(rval_beta()[,2])[,6]))
                     )
        })

        output$plotreg_M<-renderPlot({
          datos<-rval_dataset_M()
          n<-length(input$varx_M)

          for(i in seq(n)){
            if(input$varx_M[i]==input$ig_niv){
              var<-input$varx_M[i]
              j<-i+1
            }
          }

          if(length(rval_betauser())==length(input$varx_M
                                             )+1){
            ggplot(datos,aes(datos[,var],datos[,input$vary_M]))+
              geom_point()+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,1])) +
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,2]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,3]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,4]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,5]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted(rval_betauser())[,1])) +
              geom_line(aes(datos[,var],adjusted(rval_betauser())[,2]), lty = 2, col = 'green')+
              geom_line(aes(datos[,var],adjusted(rval_betauser())[,3]), lty = 2, col = 'green')+
              geom_line(aes(datos[,var],adjusted(rval_betauser())[,4]), lty = 2, col = 'pink')+
              geom_line(aes(datos[,var],adjusted(rval_betauser())[,5]), lty = 2, col = 'pink')+
              xlab(var)+
              ylab(input$vary_M)+
              theme_bw()
          }else{
            ggplot(datos,aes(datos[,var],datos[,input$vary_M]))+
              geom_point()+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,1])) +
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,2]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,3]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,4]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted(rval_beta()[,2])[,5]), lty = 2, col = 'red')+
              xlab(var)+
              ylab(input$vary_M)+
              theme_bw()
          }


        })

        data_pred_M<-reactive({
          t(as.matrix(data.frame(lapply(sapply(strsplit(input$pred_M, "/")[[1]],function(x) strsplit(x, ",")),as.numeric))))
        })

        predicted<-function(q){
          datos<-rval_dataset_M()
          n<-dim(q)[1]
          r<-length(input$varx_M)
          m<-length(adjusted(rval_beta()[,2])[,1])

          if(n>1){x<-cbind(rep(1,n),q)}
          else{x<-t(as.matrix(c(1,q)))}

          x_modelo<-as.matrix(rep(1,m))

          for(i in seq(r)){
            x_modelo<-cbind(x_modelo,datos[,input$varx_M[i]])
          }

          x_modelo<-x_modelo[,-1]

          #Calculo de beta
          beta<-as.matrix(rval_beta()[,2])

          #Y estimado
          y.est=x %*% beta

          e<-adjusted(rval_beta()[,2])[,6]

          #suma e cuadrada
          se<-sqrt(as.numeric((t(e) %*% e)/(m-r-1)))

          #tstudent
          t<-qt((1-input$ic_M)/2,m-r-1, lower.tail = F)

          x<-x[,-1]


          #intervalo de confianza
          if(n>1){
            lim.punt.inf<-c()
            lim.punt.sup<-c()
            lim.med.inf<-c()
            lim.med.sup<-c()

            for(i in seq(n)){

              if(r>1){xx<-as.matrix(x[i,])}
              else{xx<-x[i]}

              lim.punt.inf<-c(lim.punt.inf,y.est[i]-t*se*sqrt(1+t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.punt.sup<-c(lim.punt.sup,y.est[i]+t*se*sqrt(1+t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.med.inf<-c(lim.med.inf,y.est[i]-t*se*sqrt(t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.med.sup<-c(lim.med.sup,y.est[i]+t*se*sqrt(t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))

            }

          }
          else{
            y.est<-as.numeric(y.est)

            lim.punt.inf<-c(y.est-t*se*sqrt(1+t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.punt.sup<-c(y.est+t*se*sqrt(1+t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.med.inf<-c(y.est-t*se*sqrt(t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.med.sup<-c(y.est+t*se*sqrt(t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))

          }



          param<-data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup)
          return(param)

        }

        rval_data_pred_M<-reactive({
          predicted(data_pred_M())
        })

        output$dt_pred_M<-DT::renderDT({rval_data_pred_M()})

        #Grafica de residuales con respecto a una variable explecativa
        output$var_ErroresEx<-renderUI({
          selectInput("E_v_exp", label = "Mostrar grÃ¡fica con respecto a:",input$varx_M)
        })

        output$plotResiduales<-renderPlot({

          ggplot(data = data.frame(e=adjusted(rval_beta()[,2])[,6],
                                   variable=rval_dataset_M()[,input$E_v_exp]),
                 aes(x = variable, y = e)) +
          geom_point() +
          geom_smooth(color = "firebrick") +
          geom_hline(yintercept = 0) +
          labs(x=input$E_v_exp,y="Errores")+
          theme_bw()
        })

        ##Normalidad de residuales

        output$plotNormResi<-renderPlot({
          qqnorm(adjusted(rval_beta()[,2])[,6])
          qqline(adjusted(rval_beta()[,2])[,6])
        })

        output$tnorm_r_M<-renderText({
          shapiro.test(adjusted(rval_beta()[,2])[,6])$p.value
          })

        output$tnorm_r_cad_M<-renderText({
          ifelse(shapiro.test(adjusted(rval_beta()[,2])[,6])$p.value<input$ic_pr_M,
                 "Rechazamos la hip. nula, por lo que decimos que los residuales no son normales",
                 "No rechazamos la hip. nula, por lo que decimos que los residuales son normales")
        })

        #homocedasticidad

        output$plotVarianza<-renderPlot({
          ggplot(data = data.frame(predict_values = adjusted(rval_beta()[,2])[,1],
                                   residuos = adjusted(rval_beta()[,2])[,6]),
                 aes(x = predict_values, y = residuos)) +
            geom_point() +
            geom_smooth(color = "firebrick", se = FALSE) +
            geom_hline(yintercept = 0) +
            labs(y="Residuales",x="Valores ajustados")+
            theme_bw()
        })

        adjusted_p<-function(beta){

          #Variables
          datos<-rval_dataset_M_p()
          r<-length(input$varx_M_p)

          n<-length(datos[,input$vary_M_p])

          d<-data.frame(u=rep(1,n))

          #Construccion de matriz X, con 1 en una columna y vector y
          for(i in seq(r)){
            d<-data.frame(d,datos[,input$varx_M_p[i]])
          }

          #convertir tipo de dato
          vec<-as.matrix(datos[,input$vary_M_p])
          mat<-as.matrix(d)

          #Calculo de beta
          beta<-as.matrix(beta)#rval_beta()[,2])

          #Y estimado
          y.est=mat %*% beta

          #Error
          e=vec-y.est

          #suma e cuadrada
          se<-sqrt(as.numeric((t(e) %*% e)/(n-r-1)))

          #tstudent
          t<-qt((1-input$ic_M)/2,n-r-1, lower.tail = F)

          x<-mat[,-1]

          lim.punt.inf<-c()
          lim.punt.sup<-c()
          lim.med.inf<-c()
          lim.med.sup<-c()

          #intervalo de confianza
          for(i in seq(n)){

            if(r>1){xx<-x[i,]}
            else{xx<-x[i]}

            lim.punt.inf<-c(lim.punt.inf,y.est[i]-t*se*sqrt(1+t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.punt.sup<-c(lim.punt.sup,y.est[i]+t*se*sqrt(1+t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.med.inf<-c(lim.med.inf,y.est[i]-t*se*sqrt(t(xx) %*% solve(t(x) %*% x)%*%xx))
            lim.med.sup<-c(lim.med.sup,y.est[i]+t*se*sqrt(t(xx) %*% solve(t(x) %*% x)%*%xx))

          }

          #lim.punt.inf<-(-t*se)*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.punt.sup<-y.est+t*se*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))

          #lim.med.inf<-y.est-t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.med.sup<-y.est+t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))

          #data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup)
          param<-data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup,error=e)
          return(param)

        }

        predicted_p<-function(q){
          datos<-rval_dataset_M_p()
          n<-dim(q)[1]
          r<-length(input$varx_M_p)
          m<-length(adjusted_p(rval_beta_p()[,2])[,1])

          if(n>1){x<-cbind(rep(1,n),q)}
          else{x<-t(as.matrix(c(1,q)))}

          x_modelo<-as.matrix(rep(1,m))

          for(i in seq(r)){
            x_modelo<-cbind(x_modelo,datos[,input$varx_M_p[i]])
          }

          x_modelo<-x_modelo[,-1]

          #Calculo de beta
          beta<-as.matrix(rval_beta_p()[,2])

          #Y estimado
          y.est=x %*% beta

          e<-adjusted_p(rval_beta_p()[,2])[,6]

          #suma e cuadrada
          se<-sqrt(as.numeric((t(e) %*% e)/(m-r-1)))

          #tstudent
          t<-qt((1-input$ic_M_p)/2,m-r-1, lower.tail = F)

          x<-x[,-1]


          #intervalo de confianza
          if(n>1){
            lim.punt.inf<-c()
            lim.punt.sup<-c()
            lim.med.inf<-c()
            lim.med.sup<-c()

            for(i in seq(n)){

              if(r>1){xx<-as.matrix(x[i,])}
              else{xx<-x[i]}

              lim.punt.inf<-c(lim.punt.inf,y.est[i]-t*se*sqrt(1+t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.punt.sup<-c(lim.punt.sup,y.est[i]+t*se*sqrt(1+t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.med.inf<-c(lim.med.inf,y.est[i]-t*se*sqrt(t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))
              lim.med.sup<-c(lim.med.sup,y.est[i]+t*se*sqrt(t(xx) %*% solve(t(x_modelo) %*% x_modelo)%*%xx))

            }

          }
          else{
            y.est<-as.numeric(y.est)

            lim.punt.inf<-c(y.est-t*se*sqrt(1+t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.punt.sup<-c(y.est+t*se*sqrt(1+t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.med.inf<-c(y.est-t*se*sqrt(t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))
            lim.med.sup<-c(y.est+t*se*sqrt(t(x) %*% solve(t(x_modelo) %*% x_modelo)%*%x))

          }

          #lim.punt.inf<-(-t*se)*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.punt.sup<-y.est+t*se*sqrt(rep(1,n)+(x) %*% solve(t(x) %*% (x))%*%t(x))

          #lim.med.inf<-y.est-t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))
          #lim.med.sup<-y.est+t*se*sqrt((x) %*% solve(t(x) %*% (x))%*%t(x))

          #data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup)
          param<-data.frame(y.est,lim.punt.inf,lim.punt.sup,lim.med.inf,lim.med.sup)
          return(param)

        }

        rval_dataset_M_p<-reactive({
          inFile <- input$file1

          if (is.null(inFile))
            return(NULL)

          drop_na(read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, quote = input$quote))
        })

        output$op_vary_M_p<-renderUI({
          selectInput("vary_M_p", label = "Variable dependiente",names(rval_dataset_M_p()),names(rval_dataset_M_p())[1])
        })
        output$op_varx_M_p<-renderUI({
          checkboxGroupInput("varx_M_p", label = "Variable explicativa",names(rval_dataset_M_p()),names(rval_dataset_M_p())[2])
        })



        output$summary_expl_t1_M_p <- renderPrint({
          summary(rval_dataset_M_p())
        })


        rval_betauser_p<-reactive({
          as.numeric(strsplit(input$u_adm_p, ",")[[1]])
        })

        output$betauser<-renderText({rval_betauser_p()})

        ##Correlacion (valor)
        correl_p<-reactive({
          datos<-rval_dataset_M_p()
          n<-length(input$varx_M_p)
          a<-array(dim=c(n))

          for (i in seq(n)){
            a[i]<-cor(datos[,input$varx_M_p[i]],datos[,input$vary_M_p])
          }

          data.frame(y=input$varx_M_p,"Correlacion con x"=a)
        })

        output$cor_p<-renderTable({
          correl_p()
        })

        ##Algunos datos del data set
        output$table_expl_p <- renderTable({
          head(rval_dataset_M_p())
        })

        ##Correlacion (grafica)
        output$plot_exp_p<-renderPlot({
          datos<-rval_dataset_M_p()
          pairs(datos)

        })

        #Seleccionar con respecto a que variable se mostrara la grafica
        output$var_niv_p<-renderUI({
          selectInput("ig_niv_p", label = "Mostrar grÃ¡fica con respecto a:",input$varx_M_p)
        })

        ##Beta

        #Coeficientes de regresion  
        beta_p<-function(){
          #Variables
          datos<-rval_dataset_M_p()
          r<-length(input$varx_M_p)
          nomb<-c("alfa")
          n<-length(datos[,input$vary_M_p])

          d<-data.frame(u=rep(1,n))

          #Construccion de matriz X, con 1 en una columna y vector y
          for(i in seq(r)){
            d<-data.frame(d,datos[,input$varx_M_p[i]])
            no<-paste("beta.",input$varx_M_p[i])
            nomb<-c(nomb,no)
          }

          #convertir tipo de dato
          vec<-as.matrix(datos[,input$vary_M_p])
          mat<-as.matrix(d)

          #Calculo de beta
          beta<-(solve(t(mat) %*% mat))%*% t(mat) %*% vec

          #Y estimado
          y.est=mat %*% beta

          #Error
          e=vec-y.est

          #suma e cuadrada
          se_c<-as.numeric((t(e) %*% e)/(n-r-1))

          #matriz de var covar
          vec_cov<-se_c*(solve(t(mat) %*% mat))


          #tstudent
          t<-qt((1-input$ic_M_p)/2,n-r-1, lower.tail = F)

          error.est<-c()
          t.est<-c()
          p.value<-c()
          for(i in seq(dim(vec_cov)[1])){
            error.est<-c(error.est,sqrt(vec_cov[i,i]))
            t.est<-c(t.est,beta[i]/error.est[i])
            p.value<-c(p.value,(1-pt(t.est[i],dim(mat)[1]-length(beta))))
          }

          #intervalo de confianza
          lim.inf<-error.est-t*beta
          lim.sup<-error.est+t*beta
          param<-data.frame(parametro=nomb,valor=beta,error.est,t.est,p.value,lim.inf,lim.sup)
          return(param)
        }

        #Output de parametros e intervalos de confianza
        output$beta_p<-renderTable({beta_p()})

        rval_beta_p<-reactive({beta_p()})


        ##Comparacion de errores
        output$TablaErrores_p<-renderTable({
          data.frame(Modelo=c("Usuario","MCO"),
                     `Suma de errores`=c(sum(adjusted_p(rval_betauser_p())[,6]),
                                         sum(adjusted_p(rval_beta_p()[,2])[,6])),
                     `Suma de errores^2`=c(sum(t(adjusted_p(rval_betauser_p())[,6])%*%adjusted_p(rval_betauser_p())[,6]),
                                           sum(t(adjusted_p(rval_beta_p()[,2])[,6])%*%adjusted_p(rval_beta_p()[,2])[,6]))
          )
        })

        output$plotreg_M_p<-renderPlot({
          datos<-rval_dataset_M_p()
          n<-length(input$varx_M_p)

          for(i in seq(n)){
            if(input$varx_M_p[i]==input$ig_niv_p){
              var<-input$varx_M_p[i]
              j<-i+1
            }
          }

          if(length(rval_betauser_p())==length(input$varx_M_p
          )+1){
            ggplot(datos,aes(datos[,var],datos[,input$vary_M_p]))+
              geom_point()+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,1])) +
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,2]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,3]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,4]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,5]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted_p(rval_betauser_p())[,1])) +
              geom_line(aes(datos[,var],adjusted_p(rval_betauser_p())[,2]), lty = 2, col = 'green')+
              geom_line(aes(datos[,var],adjusted_p(rval_betauser_p())[,3]), lty = 2, col = 'green')+
              geom_line(aes(datos[,var],adjusted_p(rval_betauser_p())[,4]), lty = 2, col = 'pink')+
              geom_line(aes(datos[,var],adjusted_p(rval_betauser_p())[,5]), lty = 2, col = 'pink')+
              xlab(var)+
              ylab(input$vary_M_p)+
              theme_bw()
          }else{
            ggplot(datos,aes(datos[,var],datos[,input$vary_M_p]))+
              geom_point()+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,1])) +
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,2]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,3]), lty = 2, col = 'blue')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,4]), lty = 2, col = 'red')+
              geom_line(aes(datos[,var],adjusted_p(rval_beta_p()[,2])[,5]), lty = 2, col = 'red')+
              xlab(var)+
              ylab(input$vary_M_p)+
              theme_bw()
          }


        })

        data_pred_M_p<-reactive({
          t(as.matrix(data.frame(lapply(sapply(strsplit(input$pred_M_p, "/")[[1]],function(x) strsplit(x, ",")),as.numeric))))
        })


        rval_data_pred_M_p<-reactive({
          predicted_p(data_pred_M_p())
        })

        output$dt_pred_M_p<-DT::renderDT({rval_data_pred_M_p()})

        #Grafica de residuales con respecto a una variable explecativa
        output$var_ErroresEx_p<-renderUI({
          selectInput("E_v_exp_p", label = "Mostrar grÃ¡fica con respecto a:",input$varx_M_p)
        })


        output$plotResiduales_p<-renderPlot({

          ggplot(data = data.frame(e=adjusted_p(rval_beta_p()[,2])[,6],
                                   variable=rval_dataset_M_p()[,input$E_v_exp_p]),
                 aes(x = variable, y = e)) +
            geom_point() +
            geom_smooth(color = "firebrick") +
            geom_hline(yintercept = 0) +
            labs(x=input$E_v_exp_p,y="Errores")+
            theme_bw()
        })

        ##Normalidad de residuales

        output$plotNormResi_p<-renderPlot({
          qqnorm(adjusted_p(rval_beta_p()[,2])[,6])
          qqline(adjusted_p(rval_beta_p()[,2])[,6])
        })

        output$tnorm_r_M_p<-renderText({
          shapiro.test(adjusted_p(rval_beta_p()[,2])[,6])$p.value
        })

        output$tnorm_r_cad_M_p<-renderText({
          ifelse(shapiro.test(adjusted_p(rval_beta_p()[,2])[,6])$p.value<input$ic_pr_M_p,
                 "Rechazamos la hip. nula, por lo que decimos que los residuales no son normales",
                 "No rechazamos la hip. nula, por lo que decimos que los residuales son normales")
        })

        #homocedasticidad

        output$plotVarianza_p<-renderPlot({
          ggplot(data = data.frame(predict_values = adjusted_p(rval_beta_p()[,2])[,1],
                                   residuos = adjusted_p(rval_beta_p()[,2])[,6]),
                 aes(x = predict_values, y = residuos)) +
            geom_point() +
            geom_smooth(color = "firebrick", se = FALSE) +
            geom_hline(yintercept = 0) +
            labs(y="Residuales",x="Valores ajustados")+
            theme_bw()
        })



      }



      # Run the application
      shinyApp(ui = ui, server = server)
