############################################################################
###########  Ingresos por fuente por entidad #############################
#########################################################################

library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008")
Conc<-read.dbf("Concnacional2008.dbf",as.is = T)



mydesign <- svydesign(id=~UPM,strata=~EST_DIS,data=Conc,weights=~FACTOR)

#vamos por el ingreso corriente total del pa?s
# ing_ cor se define como La suma de las variables ingtrab, rentas, transfer, estim_alqu y otros_ing.
#te sale que el ingreso trimestra promedio en Mexico es de 49,610.
#notes? que esto no es otra cosa que el ing_cor*factor/34744819
Ming_corTot <- svyratio(~INGCOR,denominator=~Nhog,mydesign) 

#ahora, vamos a hacer lo mismo por entidad
#aqu? cmabia la funci?n a svyby, en by va el entidad que creamos.
#y al final va la funci?n que queremos
Ming_corentidad <- svyby(~INGCOR,denominator=~Nhog,by=~entidad,mydesign,svyratio)

############################################################################################################
###################################        Trabajo       #######################################
############################################################################################################
#El trabajo se divide en tres clasificaciones: subordinado, independiente y otros.
### ingreso del trabajo total###
MingtrabTot <- svyratio(~INGTRAB,denominator=~Nhog,mydesign) # Total promedio
Mingtrabentidad <- svyby(~INGTRAB,denominator=~Nhog,by=~entidad,mydesign,svyratio) # por entidad
###### ingreso del trabajo subordinado
MtrabajoTot <- svyratio(~TRABAJO,denominator=~Nhog,mydesign) # Total promedio
Mtrabajoentidad <- svyby(~TRABAJO,denominator=~Nhog,by=~entidad,mydesign,svyratio) # por entidad
###### ingreso del trabajo independiente
MnegocioTot <- svyratio(~NEGOCIO,denominator=~Nhog,mydesign) # Total promedio
Mnegocioentidad <- svyby(~NEGOCIO,denominator=~Nhog,by=~entidad,mydesign,svyratio) # por entidad
###### ingreso de otros trabajos
Motros_trabTot <- svyratio(~OTROS_TRAB,denominator=~Nhog,mydesign) # Total promedio
Motros_trabentidad <- svyby(~OTROS_TRAB,denominator=~Nhog,by=~entidad,mydesign,svyratio) # por entidad

############################################################################################################
###################################        Rentas de la propiedad      #######################################
############################################################################################################

#la renta de la propiedad se divide en: ingresos de sociedades y arrendamientos.

#ingresos totales por renta de la porpiedad
MrentasTot <- svyratio(~RENTAS,denominator=~Nhog,mydesign) # Total promedio
Mrentasentidad <- svyby(~RENTAS,denominator=~Nhog,by=~entidad,mydesign,svyratio) #Por entidad
###### ingresos de sociedades
MutilidadTot <- svyratio(~UTILIDAD,denominator=~Nhog,mydesign) # Total promedio
Mutilidadentidad <- svyby(~UTILIDAD,denominator=~Nhog,by=~entidad,mydesign,svyratio) # por entidad
###### arrendamiento
MarrendaTot <- svyratio(~ARRENDA,denominator=~Nhog,mydesign) # Total promedio
Marrendaentidad <- svyby(~ARRENDA,denominator=~Nhog,by=~entidad,mydesign,svyratio) # Por entidad

############################################################################################################
###################################        Transferencias   #######################################
#####################################################################################################

#las transferencias totales se definen como la suma de jubilacion, becas, donativos, remesas, bene_gob, transf_hog y trans_inst.

MtransferTot <- svyratio(~TRANSFER,denominator=~Nhog,mydesign) # Total promedio
Mtransferentidad <- svyby(~TRANSFER,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### jubilacion se define como Jubilaciones, pensiones e indemnizaciones por accidente de trabajo despido y retiro voluntario.
#En el cuestionario solo se les pregunta si recibi? jubilaciones. As? que puede ser p?blicas o privadas.

MjubilacionTot <- svyratio(~JUBILA,denominator=~Nhog,mydesign) # Total promedio
Mjubilacionentidad <- svyby(~JUBILA,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### becas que pueden ser, de nuevo, p?blicas privadas. 
MbecasTot <- svyratio(~BECA,denominator=~Nhog,mydesign) # Total promedio
Mbecasentidad <- svyby(~BECA,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### donativos que tambi?n pueden ser p?blicos o privados.
MdonativosTot <- svyratio(~DONATIVO,denominator=~Nhog,mydesign) # Total promedio
Mdonativosentidad <- svyby(~DONATIVO,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### remesas se definen como ingresos provenientes d eotros paises. As? de manera gen?rica.
MremesasTot <- svyratio(~REMESA,denominator=~Nhog,mydesign) # Total promedio
Mremesasentidad <- svyby(~REMESA,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### bene_gob:  aqu? estna los programas p?blicos. Prospera, procampo, 65 y m?s, adultos mayores, sin hambre, empleo tempora y Otros.
Mbene_gobTot <- svyratio(~BENE_GOB,denominator=~Nhog,mydesign) # Total promedio
Mbene_gobentidad <- svyby(~BENE_GOB,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###### transf_hog:  Esto es lo que transfiere otro hogar.
Mtransf_hogTot <- svyratio(~ESP_HOG,denominator=~Nhog,mydesign) # Total promedio
Mtransf_hogentidad <- svyby(~ESP_HOG,denominator=~Nhog,by=~entidad,mydesign,svyratio) #entidad

###### trans_inst: puede venir de institucione sp?blicas o privadas.
Mtrans_instTot <- svyratio(~ESP_INST,denominator=~Nhog,mydesign) # Total promedio
Mtrans_instentidad <- svyby(~ESP_INST,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

####################################################################################################

### estim_alqu ### Aparentemente se le pregunta al entrevistado cu?nto constar?a la renta del lugar donde vive.
Mestim_alquTot <- svyratio(~ESTI,denominator=~Nhog,mydesign) # Total promedio
Mestim_alquentidad <- svyby(~ESTI,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad

###################################################################################################


### otros_ing ### es literalmente ?algo m?s?
Motros_ingTot <- svyratio(~OTROS,denominator=~Nhog,mydesign) # Total promedio
Motros_ingentidad <- svyby(~OTROS,denominator=~Nhog,by=~entidad,mydesign,svyratio) # entidad


#####################################################################################################
######################################### Estimaciones #############################################
##################################################################################################

ES_Ming_corTot <- Ming_corTot[[1]] #lo que estoy haciendo aqu? es extraer el valor de la primera columa que corresponde al c?lculo.
ES_Ming_corentidad <- Ming_corentidad[[2]] #En el caso de las entidades, los c?lculos quedaron en la segunda columna

ES_MingtrabTot <- MingtrabTot[[1]]
ES_Mingtrabentidad <- Mingtrabentidad[[2]]

ES_MtrabajoTot <- MtrabajoTot[[1]]
ES_Mtrabajoentidad <- Mtrabajoentidad[[2]]

ES_MnegocioTot <- MnegocioTot[[1]]
ES_Mnegocioentidad <- Mnegocioentidad[[2]]

ES_Motros_trabTot <- Motros_trabTot [[1]]
ES_Motros_trabentidad <- Motros_trabentidad [[2]]

ES_MrentasTot <- MrentasTot [[1]]
ES_Mrentasentidad <- Mrentasentidad [[2]]

ES_MutilidadTot <- MutilidadTot [[1]]
ES_Mutilidadentidad <- Mutilidadentidad [[2]]

ES_MarrendaTot <- MarrendaTot [[1]]
ES_Marrendaentidad <- Marrendaentidad [[2]]

ES_MtransferTot <- MtransferTot[[1]]
ES_Mtransferentidad <- Mtransferentidad[[2]]

ES_MjubilacionTot <- MjubilacionTot [[1]]
ES_Mjubilacionentidad <- Mjubilacionentidad [[2]]

ES_MbecasTot <- MbecasTot [[1]]
ES_Mbecasentidad <- Mbecasentidad [[2]]

ES_MdonativosTot <- MdonativosTot[[1]]
ES_Mdonativosentidad <- Mdonativosentidad[[2]]

ES_MremesasTot <- MremesasTot[[1]]
ES_Mremesasentidad <- Mremesasentidad[[2]]

ES_Mbene_gobTot <- Mbene_gobTot [[1]]
ES_Mbene_gobentidad <- Mbene_gobentidad [[2]]

ES_Mtransf_hogTot <- Mtransf_hogTot [[1]]
ES_Mtransf_hogentidad <- Mtransf_hogentidad [[2]]

ES_Mtrans_instTot <- Mtrans_instTot[[1]]
ES_Mtrans_instentidad <- Mtrans_instentidad[[2]]

ES_Mestim_alquTot <- Mestim_alquTot [[1]]
ES_Mestim_alquentidad <- Mestim_alquentidad [[2]]

ES_Motros_ingTot <- Motros_ingTot [[1]]
ES_Motros_ingentidad <- Motros_ingentidad [[2]]

########## Error Est?ndar ##########
SE_Ming_corTot <- SE (Ming_corTot)
SE_Ming_corentidad <- SE (Ming_corentidad)

SE_MingtrabTot <- SE (MingtrabTot)
SE_Mingtrabentidad <- SE (Mingtrabentidad)

SE_MtrabajoTot <- SE (MtrabajoTot)
SE_Mtrabajoentidad <- SE (Mtrabajoentidad)

SE_MnegocioTot <- SE (MnegocioTot)
SE_Mnegocioentidad <- SE (Mnegocioentidad)

SE_Motros_trabTot <- SE (Motros_trabTot)
SE_Motros_trabentidad <- SE (Motros_trabentidad)

SE_MrentasTot <- SE (MrentasTot)
SE_Mrentasentidad <- SE (Mrentasentidad)

SE_MutilidadTot <- SE (MutilidadTot)
SE_Mutilidadentidad <- SE (Mutilidadentidad)

SE_MarrendaTot <- SE (MarrendaTot)
SE_Marrendaentidad <- SE (Marrendaentidad)

SE_MtransferTot <- SE (MtransferTot)
SE_Mtransferentidad <- SE (Mtransferentidad)

SE_MjubilacionTot <- SE (MjubilacionTot)
SE_Mjubilacionentidad <- SE (Mjubilacionentidad)

SE_MbecasTot <- SE (MbecasTot)
SE_Mbecasentidad <- SE (Mbecasentidad)

SE_MdonativosTot <- SE (MdonativosTot)
SE_Mdonativosentidad <- SE (Mdonativosentidad)

SE_MremesasTot <- SE (MremesasTot)
SE_Mremesasentidad <- SE (Mremesasentidad)

SE_Mbene_gobTot <- SE (Mbene_gobTot)
SE_Mbene_gobentidad <- SE (Mbene_gobentidad)

SE_Mtransf_hogTot <- SE (Mtransf_hogTot)
SE_Mtransf_hogentidad <- SE (Mtransf_hogentidad)

SE_Mtrans_instTot <- SE (Mtrans_instTot)
SE_Mtrans_instentidad <- SE (Mtrans_instentidad)

SE_Mestim_alquTot <- SE (Mestim_alquTot)
SE_Mestim_alquentidad <- SE (Mestim_alquentidad)

SE_Motros_ingTot <- SE (Motros_ingTot)
SE_Motros_ingentidad <- SE (Motros_ingentidad)

########## Coeficiente de variaci?n ##########
CV_Ming_corTot <- cv(Ming_corTot)
CV_Ming_corentidad <- cv(Ming_corentidad)

CV_MingtrabTot <- cv(MingtrabTot)
CV_Mingtrabentidad <- cv(Mingtrabentidad)

CV_MtrabajoTot <- cv(MtrabajoTot)
CV_Mtrabajoentidad <- cv(Mtrabajoentidad)

CV_MnegocioTot <- cv(MnegocioTot)
CV_Mnegocioentidad <- cv(Mnegocioentidad)

CV_Motros_trabTot <- cv(Motros_trabTot)
CV_Motros_trabentidad <- cv(Motros_trabentidad)

CV_MrentasTot <- cv(MrentasTot)
CV_Mrentasentidad <- cv(Mrentasentidad)

CV_MutilidadTot <- cv(MutilidadTot)
CV_Mutilidadentidad <- cv(Mutilidadentidad)

CV_MarrendaTot <- cv(MarrendaTot)
CV_Marrendaentidad <- cv(Marrendaentidad)

CV_MtransferTot <- cv(MtransferTot)
CV_Mtransferentidad <- cv(Mtransferentidad)

CV_MjubilacionTot <- cv(MjubilacionTot)
CV_Mjubilacionentidad <- cv(Mjubilacionentidad)

CV_MbecasTot <- cv(MbecasTot)
CV_Mbecasentidad <- cv(Mbecasentidad)

CV_MdonativosTot <- cv(MdonativosTot)
CV_Mdonativosentidad <- cv(Mdonativosentidad)

CV_MremesasTot <- cv(MremesasTot)
CV_Mremesasentidad <- cv(Mremesasentidad)

CV_Mbene_gobTot <- cv(Mbene_gobTot)
CV_Mbene_gobentidad <- cv(Mbene_gobentidad)

CV_Mtransf_hogTot <- cv(Mtransf_hogTot)
CV_Mtransf_hogentidad <- cv(Mtransf_hogentidad)

CV_Mtrans_instTot <- cv(Mtrans_instTot)
CV_Mtrans_instentidad <- cv(Mtrans_instentidad)

CV_Mestim_alquTot <- cv(Mestim_alquTot)
CV_Mestim_alquentidad <- cv(Mestim_alquentidad)

CV_Motros_ingTot <- cv(Motros_ingTot)
CV_Motros_ingentidad <- cv(Motros_ingentidad)
########## Limite inferior ##########
LI_Ming_corTot <- confint(Ming_corTot,level=0.90)[,1]
LI_Ming_corentidad <- confint(Ming_corentidad,level=0.90)[,1]

LI_MingtrabTot <- confint(MingtrabTot,level=0.90)[,1]
LI_Mingtrabentidad <- confint(Mingtrabentidad,level=0.90)[,1]

LI_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,1]
LI_Mtrabajoentidad <- confint(Mtrabajoentidad,level=0.90)[,1]

LI_MnegocioTot <- confint(MnegocioTot,level=0.90)[,1]
LI_Mnegocioentidad <- confint(Mnegocioentidad,level=0.90)[,1]

LI_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,1]
LI_Motros_trabentidad <- confint(Motros_trabentidad,level=0.90)[,1]

LI_MrentasTot <- confint(MrentasTot,level=0.90)[,1]
LI_Mrentasentidad <- confint(Mrentasentidad,level=0.90)[,1]

LI_MutilidadTot <- confint(MutilidadTot,level=0.90)[,1]
LI_Mutilidadentidad <- confint(Mutilidadentidad,level=0.90)[,1]

LI_MarrendaTot <- confint(MarrendaTot,level=0.90)[,1]
LI_Marrendaentidad <- confint(Marrendaentidad,level=0.90)[,1]

LI_MtransferTot <- confint(MtransferTot,level=0.90)[,1]
LI_Mtransferentidad <- confint(Mtransferentidad,level=0.90)[,1]

LI_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,1]
LI_Mjubilacionentidad <- confint(Mjubilacionentidad,level=0.90)[,1]

LI_MbecasTot <- confint(MbecasTot,level=0.90)[,1]
LI_Mbecasentidad <- confint(Mbecasentidad,level=0.90)[,1]

LI_MdonativosTot <- confint(MdonativosTot,level=0.90)[,1]
LI_Mdonativosentidad <- confint(Mdonativosentidad,level=0.90)[,1]

LI_MremesasTot <- confint(MremesasTot,level=0.90)[,1]
LI_Mremesasentidad <- confint(Mremesasentidad,level=0.90)[,1]

LI_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,1]
LI_Mbene_gobentidad <- confint(Mbene_gobentidad,level=0.90)[,1]

LI_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,1]
LI_Mtransf_hogentidad <- confint(Mtransf_hogentidad,level=0.90)[,1]

LI_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,1]
LI_Mtrans_instentidad <- confint(Mtrans_instentidad,level=0.90)[,1]

LI_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,1]
LI_Mestim_alquentidad <- confint(Mestim_alquentidad,level=0.90)[,1
                                                            ]
LI_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,1]
LI_Motros_ingentidad <- confint(Motros_ingentidad ,level=0.90)[,1]

########## Limite superior ##########
LS_Ming_corTot <- confint(Ming_corTot,level=0.90)[,2]
LS_Ming_corentidad <- confint(Ming_corentidad,level=0.90)[,2]

LS_MingtrabTot <- confint(MingtrabTot,level=0.90)[,2]
LS_Mingtrabentidad <- confint(Mingtrabentidad,level=0.90)[,2]

LS_MtrabajoTot <- confint(MtrabajoTot,level=0.90)[,2]
LS_Mtrabajoentidad <- confint(Mtrabajoentidad,level=0.90)[,2]

LS_MnegocioTot <- confint(MnegocioTot,level=0.90)[,2]
LS_Mnegocioentidad <- confint(Mnegocioentidad,level=0.90)[,2]

LS_Motros_trabTot <- confint(Motros_trabTot,level=0.90)[,2]
LS_Motros_trabentidad <- confint(Motros_trabentidad,level=0.90)[,2]

LS_MrentasTot <- confint(MrentasTot,level=0.90)[,2]
LS_Mrentasentidad <- confint(Mrentasentidad,level=0.90)[,2]

LS_MutilidadTot <- confint(MutilidadTot,level=0.90)[,2]
LS_Mutilidadentidad <- confint(Mutilidadentidad,level=0.90)[,2]

LS_MarrendaTot <- confint(MarrendaTot,level=0.90)[,2]
LS_Marrendaentidad <- confint(Marrendaentidad,level=0.90)[,2]

LS_MtransferTot <- confint(MtransferTot,level=0.90)[,2]
LS_Mtransferentidad <- confint(Mtransferentidad,level=0.90)[,2]

LS_MjubilacionTot <- confint(MjubilacionTot,level=0.90)[,2]
LS_Mjubilacionentidad <- confint(Mjubilacionentidad,level=0.90)[,2]

LS_MbecasTot <- confint(MbecasTot,level=0.90)[,2]
LS_Mbecasentidad <- confint(Mbecasentidad,level=0.90)[,2]

LS_MdonativosTot <- confint(MdonativosTot,level=0.90)[,2]
LS_Mdonativosentidad <- confint(Mdonativosentidad,level=0.90)[,2]

LS_MremesasTot <- confint(MremesasTot,level=0.90)[,2]
LS_Mremesasentidad <- confint(Mremesasentidad,level=0.90)[,2]

LS_Mbene_gobTot <- confint(Mbene_gobTot,level=0.90)[,2]
LS_Mbene_gobentidad <- confint(Mbene_gobentidad,level=0.90)[,2]

LS_Mtransf_hogTot <- confint(Mtransf_hogTot,level=0.90)[,2]
LS_Mtransf_hogentidad <- confint(Mtransf_hogentidad,level=0.90)[,2]

LS_Mtrans_instTot <- confint(Mtrans_instTot,level=0.90)[,2]
LS_Mtrans_instentidad <- confint(Mtrans_instentidad,level=0.90)[,2]

LS_Mestim_alquTot <- confint(Mestim_alquTot,level=0.90)[,2]
LS_Mestim_alquentidad <- confint(Mestim_alquentidad,level=0.90)[,2]

LS_Motros_ingTot <- confint(Motros_ingTot,level=0.90)[,2]
LS_Motros_ingentidad <- confint(Motros_ingentidad,level=0.90)[,2]


####################################################################################################
#############################      Cuadros       ##################################################
##################################################################################################

#este cuadro, lo ?nico que tiene son todas la estimaciones.
#son 10 filas y 18 columnas.
c_entidad_ES <-
  data.frame(c(ES_Ming_corTot,ES_Ming_corentidad),c(ES_MingtrabTot,ES_Mingtrabentidad),c(ES_MtrabajoTot,ES_Mtrabajoentidad),c(ES_MnegocioTot,ES_Mnegocioentidad)
             ,c(ES_Motros_trabTot,ES_Motros_trabentidad),c(ES_MrentasTot,ES_Mrentasentidad),c(ES_MutilidadTot,ES_Mutilidadentidad)
             ,c(ES_MarrendaTot,ES_Marrendaentidad),c(ES_MtransferTot,ES_Mtransferentidad),c(ES_MjubilacionTot,ES_Mjubilacionentidad),c(ES_MbecasTot,ES_Mbecasentidad),
             c(ES_MdonativosTot,ES_Mdonativosentidad),c(ES_MremesasTot,ES_Mremesasentidad),c(ES_Mbene_gobTot,ES_Mbene_gobentidad),c(ES_Mtransf_hogTot,ES_Mtransf_hogentidad)
             ,c(ES_Mtrans_instTot,ES_Mtrans_instentidad),c(ES_Mestim_alquTot,ES_Mestim_alquentidad),c(ES_Motros_ingTot,ES_Motros_ingentidad))
##### ERROR ESTANDAR
c_entidad_SE <-
  data.frame(c(SE_Ming_corTot,SE_Ming_corentidad),c(SE_MingtrabTot,SE_Mingtrabentidad),c(SE_MtrabajoTot,SE_Mtrabajoentidad),c(SE_MnegocioTot,SE_Mnegocioentidad)
             ,c(SE_Motros_trabTot,SE_Motros_trabentidad),c(SE_MrentasTot,SE_Mrentasentidad),c(SE_MutilidadTot,SE_Mutilidadentidad)
             ,c(SE_MarrendaTot,SE_Marrendaentidad),c(SE_MtransferTot,SE_Mtransferentidad),c(SE_MjubilacionTot,SE_Mjubilacionentidad),c(SE_MbecasTot,SE_Mbecasentidad),
             c(SE_MdonativosTot,SE_Mdonativosentidad),c(SE_MremesasTot,SE_Mremesasentidad),c(SE_Mbene_gobTot,SE_Mbene_gobentidad),c(SE_Mtransf_hogTot,SE_Mtransf_hogentidad),c(SE_Mtrans_instTot,SE_Mtrans_instentidad)
             ,c(SE_Mestim_alquTot,SE_Mestim_alquentidad),c(SE_Motros_ingTot,SE_Motros_ingentidad))

##### COEFICIENTE DE VARIACION
c_entidad_CV <-
  data.frame(c(CV_Ming_corTot,CV_Ming_corentidad),c(CV_MingtrabTot,CV_Mingtrabentidad),c(CV_MtrabajoTot,CV_Mtrabajoentidad),c(CV_MnegocioTot,CV_Mnegocioentidad)
             ,c(CV_Motros_trabTot,CV_Motros_trabentidad),c(CV_MrentasTot,CV_Mrentasentidad),c(CV_MutilidadTot,CV_Mutilidadentidad),
             c(CV_MarrendaTot,CV_Marrendaentidad),c(CV_MtransferTot,CV_Mtransferentidad),c(CV_MjubilacionTot,CV_Mjubilacionentidad),c(CV_MbecasTot,CV_Mbecasentidad)
             ,c(CV_MdonativosTot,CV_Mdonativosentidad),c(CV_MremesasTot,CV_Mremesasentidad),c(CV_Mbene_gobTot,CV_Mbene_gobentidad),c(CV_Mtransf_hogTot,CV_Mtransf_hogentidad),c(CV_Mtrans_instTot,CV_Mtrans_instentidad)
             ,c(CV_Mestim_alquTot,CV_Mestim_alquentidad),c(CV_Motros_ingTot,CV_Motros_ingentidad))

##### LIMITE INFERIOR AL 90%
c_entidad_LI <-
  data.frame(c(LI_Ming_corTot,LI_Ming_corentidad),c(LI_MingtrabTot,LI_Mingtrabentidad),c(LI_MtrabajoTot,LI_Mtrabajoentidad),
             c(LI_MnegocioTot,LI_Mnegocioentidad),c(LI_Motros_trabTot,LI_Motros_trabentidad),c(LI_MrentasTot,LI_Mrentasentidad),c(LI_MutilidadTot,LI_Mutilidadentidad),c(LI_MarrendaTot,LI_Marrendaentidad)
             ,c(LI_MtransferTot,LI_Mtransferentidad),c(LI_MjubilacionTot,LI_Mjubilacionentidad),c(LI_MbecasTot,LI_Mbecasentidad),c(LI_MdonativosTot,LI_Mdonativosentidad)
             ,c(LI_MremesasTot,LI_Mremesasentidad),c(LI_Mbene_gobTot,LI_Mbene_gobentidad),c(LI_Mtransf_hogTot,LI_Mtransf_hogentidad),c(LI_Mtrans_instTot,LI_Mtrans_instentidad)
             ,c(LI_Mestim_alquTot,LI_Mestim_alquentidad),c(LI_Motros_ingTot,LI_Motros_ingentidad))

### LIMITE SUPERIOR AL 90%
c_entidad_LS <-
  data.frame(c(LS_Ming_corTot,LS_Ming_corentidad),c(LS_MingtrabTot,LS_Mingtrabentidad),c(LS_MtrabajoTot,LS_Mtrabajoentidad),c(LS_MnegocioTot,LS_Mnegocioentidad)
             ,c(LS_Motros_trabTot,LS_Motros_trabentidad),c(LS_MrentasTot,LS_Mrentasentidad),c(LS_MutilidadTot,LS_Mutilidadentidad),
             c(LS_MarrendaTot,LS_Marrendaentidad),c(LS_MtransferTot,LS_Mtransferentidad),c(LS_MjubilacionTot,LS_Mjubilacionentidad),c(LS_MbecasTot,LS_Mbecasentidad),
             c(LS_MdonativosTot,LS_Mdonativosentidad),c(LS_MremesasTot,LS_Mremesasentidad),c(LS_Mbene_gobTot,LS_Mbene_gobentidad),c(LS_Mtransf_hogTot,LS_Mtransf_hogentidad),c(LS_Mtrans_instTot,LS_Mtrans_instentidad)
             ,c(LS_Mestim_alquTot,LS_Mestim_alquentidad),c(LS_Motros_ingTot,LS_Motros_ingentidad))

# se agregan los nombres de las entidades a las filas
#esta cadena est? bien loca, no?
#Definimos un vector con la entidad federativa
Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur",
             "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "M?xico", "Durango",
             "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de M?xico", "Michoac?n de Ocampo",
             "Morelos", "Nayarit", "Nuevo Le?n", "Oaxaca", "Puebla", "Quer?taro", "Quintana Roo", "San Luis Potos?",
             "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucat?n",
             "Zacatecas")

row.names(c_entidad_ES)<-row.names(c_entidad_SE)<-row.names(c_entidad_CV)<-row.names(c_entidad_LI)<-row.names(c_entidad_LS)<-Entidades

#ahora vamos a ponerle nombre a las columnas
names(c_entidad_ES)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_entidad_SE)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_entidad_CV)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_entidad_LI)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

names(c_entidad_LS)=c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS","OTROS TRAB", "RENTAS","UTILIDAD", "ARRENDA", "TRANSFER","JUBILACION", "BECAS", "DONATIVOS", "REMESAS", "BENEGOBIERNO", "TRANS HOG", "TRANS INST", "ESTIM ALQU", "OTROS INGRESOS")

#ahora, lo que podemos hacer es mostrar los cuadros en la consola redondeados
# el comando round, redondea las cifra para mostrar, en el caso del coeficiente de variaci?n redondea a 4 decimales y luego multiplica por cien.
# Mostramos el resultado en pantalla #####
round(c_entidad_ES)
round(c_entidad_SE)
round(c_entidad_CV,4)*100
round(c_entidad_LI)
round(c_entidad_LS)


write.dbf(c_entidad_ES,file = "Ingresos por fuente por entidad estimaciones 2008.dbf")
write.dbf(c_entidad_SE,file = "Ingresos por fuente por entidad errores standard 2008.dbf")
write.dbf(c_entidad_CV,file = "Ingresos por fuente por entidad CV 2008.dbf")
write.dbf(c_entidad_LI,file = "Ingresos por fuente por entidad LI 2008.dbf")
write.dbf(c_entidad_ES,file = "Ingresos por fuente por entidad LS 2008.dbf")