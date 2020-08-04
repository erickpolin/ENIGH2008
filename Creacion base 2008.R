####################################################################
###################  Creacion de la base 2008 #####################
#################################################################

#ENIGH 2018
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data

setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008")
Conc<-read.dbf("NCV_Concentrado_2008_concil_2010_DBF.dbf",as.is = T)

#Keeping Variables of interest
Conc <- Conc %>%
  select(FOLIOVIV,FOLIOHOG,TAM_HOG,INGCOR,INGTRAB,TRABAJO,NEGOCIO,OTROS_TRAB,RENTAS,
         UTILIDAD,ARRENDA,TRANSFER,JUBILA,BECA,DONATIVO,REMESA,BENE_GOB,ESP_HOG,ESP_INST,ESTI,OTROS,FACTOR,UPM,EST_DIS)



################ DEfinir hogares in?genas#################
Poblacion<-read.dbf("NCV_Poblacion_2008_concil_2010_DBF.dbf",as.is = T)

Poblacion<-Poblacion%>%
  select(FOLIOVIV,FOLIOHOG, NUMREN, PARENTESCO,LENGUA6)

#El concepto de hogar ind?gena se ha definido como aquel donde el jefe(a), 
#su c?nyuge o alguno de los ascendientes (madre o padre, madrastra o padrastro, abuelo(a),
#bisabuelo(a), tatarabuelo(a), suegro(a)) declararon hablar alguna lengua ind?gena.
parentescos<-c(101,201,202,203,204,601,602,606,607,608,615,616)

Poblacion<- Poblacion%>%
  mutate(HogarIndigena=ifelse(PARENTESCO%in%parentescos&LENGUA6==1,1,0))

HogaresIndigenas<-Poblacion %>%
  group_by(FOLIOVIV,FOLIOHOG)%>%
  summarize(HogarIndigena=mean(HogarIndigena))

HogaresIndigenas<-data.frame(HogaresIndigenas)

HogaresIndigenas<-HogaresIndigenas%>%
  mutate(HogarIndigena=ifelse(HogarIndigena>0,1,0))

prop.table(table(HogaresIndigenas$HogarIndigena))

Conc<-merge(Conc,HogaresIndigenas,by=c("FOLIOVIV","FOLIOHOG"))

########ya est?n los hogares ind?genas############

#the fist two digits of "folioviv" makes reference to the state
#Let?s create a variable called entidad that contains thos two first digits of the "folioviv" variable
Conc$entidad<-substr(Conc$FOLIOVIV,1,2)

############vamos a deflactar#################
entidad<-c("01","02","03","04","05","06","07","08","09",
           "10","11","12","13","14","15","16","17","18","19","20",
           "21","22","23","24","25","26","27","28","29","30","31","32")

Deflactores<-c(68.43336412,68.76454756,69.70928971,64.51565837,
               68.57802593,66.86016389,65.06062721,69.24456993,65.00373277,
               64.85389685,65.60593852,66.27615338,63.5393076,66.08166233,
               65.54488752,65.77879561,68.87479907,66.09170991,69.58948826,
               67.56498193,66.11267629,64.4169399,69.0632509,67.43017227,
               70.15635311,69.91225279,66.34653907,69.39774369,65.70063392,
               65.28640849,65.79005286,67.04877517)
entidades<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza",
             "Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo",
             "Jalisco","México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
             "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

Deflactores2008<-data.frame(entidad,entidades,Deflactores)

Conc<-merge(Conc,Deflactores2008,by=c("entidad"))

Conc <- Conc%>%
  mutate(INGCOR=(INGCOR/Deflactores)*100,INGTRAB=(INGTRAB/Deflactores)*100,TRABAJO=(TRABAJO/Deflactores)*100,
         NEGOCIO=(NEGOCIO/Deflactores)*100,OTROS_TRAB=(OTROS_TRAB/Deflactores)*100,RENTAS=(RENTAS/Deflactores)*100,
         UTILIDAD=(UTILIDAD/Deflactores)*100,ARRENDA=(ARRENDA/Deflactores)*100,TRANSFER=(TRANSFER/Deflactores)*100,
         JUBILA=(JUBILA/Deflactores)*100,BECA=(BECA/Deflactores)*100,DONATIVO=(DONATIVO/Deflactores)*100,
         REMESA=(REMESA/Deflactores)*100,BENE_GOB=(BENE_GOB/Deflactores)*100,ESP_HOG=(ESP_HOG/Deflactores)*100,
         ESP_INST=(ESP_INST/Deflactores)*100,ESTI=(ESTI/Deflactores)*100,OTROS=(OTROS/Deflactores)*100)

######################################################

#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES ##########################################

#Attaching the data frame
attach(Conc) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
Conc<- orderBy (~+INGCOR+FOLIOVIV+FOLIOHOG, data=Conc) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(Conc$FACTOR,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/10) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
Conc$tam_dec<-tam_dec

############################# Creating Deciles of Income ###########################

Conc$MAXT<-Conc$INGCOR #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$FACTOR) #aqu? creamos una variable de suma acumulada del factor de viviendas.


######################################################################################################
############################Ahora viene la creaci?n de los deciles###################################
######################################################################################################

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:9)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$FACTOR
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$FACTOR<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$FACTOR<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$FACTOR)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:9)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-10


write.dbf(Conc,file="Concnacional2008.dbf")

