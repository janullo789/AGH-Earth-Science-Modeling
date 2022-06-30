#           Model zasiêgu lodu morskiego wokó³ Antarktydy           #
#   Projekt zaliczeniowy - Modelowanie w naukach o Ziemi 2022 AGH   #
#                       Jan Skwarczeñski                            #



# Biblioteki --------------------------------------------------------------
setwd("~/Studia/Semestr 4/Modelowanie w naukach o Ziemi/lab7_02.06.2022")

library(matrixStats)
library(ggplot2)
library(animation)



# Za³adowanie danych ------------------------------------------------------

granicaLodu <- read.csv("daily_ice_edge.csv")

# Minimalny zakres lodu ---------------------------------------------------

granicaLoduM <- as.matrix(granicaLodu[1:9288,2:362])
granicaLoduMIN <- colMins(granicaLoduM)
dlugosc <- c(-180:180)
granicaLoduDF <- data.frame(dlugoscGeo = dlugosc, szerokoscGeo = granicaLoduMIN)

granicaMapa <- ggplot(granicaLoduDF, aes(x = dlugoscGeo, y = szerokoscGeo, col="Minimalny zakres"))+geom_path()+coord_polar()+ylim(-90,0)
granicaMapa

# Animacja ----------------------------------------------------------------

i <- 30
j <- 0

prog_bar<-txtProgressBar( min=0,max=i,style=3)

saveGIF({
  
  stepi<-(-1)
  
  
  while ( i > 0) {
    
    PETLAGranicaLoduDF <- data.frame(dlugoscGeo = dlugosc, szerokoscGeo = granicaLoduM[i,])
    
    #model matematyczny
    
    xc <- cos(2*pi*PETLAGranicaLoduDF$dlugoscGeo/360)
    xs <- sin(2*pi*PETLAGranicaLoduDF$szerokoscGeo/180)
    
    funLm <- lm(PETLAGranicaLoduDF$szerokoscGeo~xc+xs)
    summary(funLm)
    
    funPredict <- predict(funLm, newdata = data.frame(PETLAGranicaLoduDF$dlugoscGeo))
    PETLAGranicaLoduDF$funPredict <- funPredict
    
    PETLAGranicaLoduModelDF <- data.frame(dlugoscGeo = dlugosc, szerokoscGeo = funPredict)
  
    #koniec modelu
    
    PETLAGranicaMapa <- ggplot(PETLAGranicaLoduDF, aes(x = dlugoscGeo, y = szerokoscGeo, col="Rzeczywisty zasiêg"))+geom_path()+geom_line(data = PETLAGranicaLoduModelDF, aes(x = dlugoscGeo, y = szerokoscGeo, col="Model matematyczny zakres"))+geom_line(data = granicaLoduDF, aes(x = dlugoscGeo, y = szerokoscGeo, col="Minimalny zasiêg"))+coord_polar()+ylim(-90,0)+ggtitle(granicaLodu[j,1])+labs(color='Legenda') 
    
    print(PETLAGranicaMapa)
    
    i = i - 1
    j = j + 1
    
    stepi<-stepi+1
    setTxtProgressBar( prog_bar, stepi)
    
  }
  
}, interval = 0.1)


TimeSeq_1 <- seq(1,3180, 2)
TimeSeq_2 <- seq(3180, 9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

plot(TimeSeq,PETLAGranicaLoduDF$szerokoscGeo, xlim=c(1, 9000))
lines(TimeSeq,predict(funLm), col="red")
