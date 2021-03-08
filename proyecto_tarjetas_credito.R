#PROYECTO: PREDICCIONES DE ABANDONO DE TARJETAS DE CRÉDITO

getwd()

bank <- read.csv("BankChurners2.csv")
class(bank)
View(bank)
names(bank)

#analizar monto de deuda minimo
bank_monto_deuda <- select(bank, monto_deuda, meses_inactivos_12meses)
bank_monto_deuda <- sort(bank_monto_deuda$monto_deuda, decreasing = FALSE)
View(bank_monto_deuda)

#convertir a una tabla de frecuencias
bank_h1<- table(bank_monto_meses)
class(bank_monto_meses)
str(bank_h1)
class(bank_h1)

#analizar meses_inactivos_12meses
bank_meses_inac <- select(bank, meses_inactivos_12meses)
bank_meses_inac <- sort(bank_meses_inac$meses_inactivos_12meses, decreasing = TRUE)

#SELECCION DE REGISTROS EN BANK
#HIPOTESIS 1: CLIENTES CON DEUDA MINIMA CON MAYOR INACTIVIDAD DURANTE MESES
#DEUDA MINIMA: QUE SEA MENOR A $10,000
#INACTVIDAD: MAYOR A 5 MESES

bank2 <- subset(x=bank, subset= monto_deuda < 10000 & meses_inactivos_12meses >= 5, select = c("monto_deuda", "meses_inactivos_12meses"))
View(bank2)

bank3 <- table(bank2)
ggplot(bank2, aes(x= monto_deuda, y= meses_inactivos_12meses, fill= "blank")) + geom_density2d() +
  ggtitle("Clientes con deuda mínima y máxima inactividad") +
  xlab("Deuda") +
  ylab("Inactividad")

#HIPOTESIS 2: CLIENTES CON INGRESOS BAJOS Y DEUDA ALTA

View(bank)
bank4<- select(bank, num_cliente, nivel_ingresos, monto_deuda)
View(bank4)

bank5<-gsub("Less than", "", bank4$nivel_ingresos, ignore.case = FALSE)
bank5<- gsub("\\$60K \\- \\$80K", "70", bank5, ignore.case = FALSE)
bank5<- gsub("\\$80K \\- \\$120K", "100", bank5, ignore.case = FALSE)
bank5<- gsub("\\$120K ", "120", bank5, ignore.case = FALSE)
bank5<- gsub("\\$40K \\- \\$60K", "50", bank5, ignore.case = FALSE)
bank5<- gsub("\\$40K", "40", bank5, ignore.case = FALSE)
bank5<- gsub("120\\+", "120", bank5, ignore.case = FALSE)
bank5<- gsub("Unknown", "na", bank5, ignore.case = FALSE)
bank5<- gsub(" 40", "40", bank5, ignore.case = FALSE)
bank5 <- gsub("na", "80", bank5, ignore.case = FALSE)

bank_ingbaj_deu_al<- cbind(bank5, bank4$num_cliente, bank4$monto_deuda)

#pregunta: ordena los registros como estaban originalmente, en el mismo orden?
class(bank_ingbaj_deu_al)

bank_ingbaj_deu_al<- as.data.frame(bank_ingbaj_deu_al)
bank_ingbaj_deu_al <- subset(x=bank_ingbaj_deu_al, subset= bank5 <= 50 & V3 >= 50000, select = c("bank5", "V3"))

ggplot(bank_ingbaj_deu_al, aes(x= V3, y= bank5, fill= "blank")) + geom_col() +
  ggtitle("Clientes con sueldo bajo y deuda alta") +
  xlab("Sueldo") +
  ylab("Deuda")

ggplot(bank_ingbaj_deu_al, aes(x= V3, y= bank5, fill= "blank")) + geom_point() +
  ggtitle("Clientes con sueldo bajo y deuda alta") +
  xlab("Sueldo") +
  ylab("Deuda")

#como agrupar por rangos los sueldos y las deudas para hacer mas facil las graficas