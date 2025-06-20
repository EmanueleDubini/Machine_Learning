#scaricare il dataset da https://www.kaggle.com/datasets/779de8cc626fb0c43e6083e957a3a045463962637600765c74dd400a268430d3?resource=download
#inserire il file nella cartella di lavoro e rinominarlo come "Hotel_Reservations.csv"
#comando per caricare il dataset nell'ambiente di lavoro
data <- read.csv("Hotel_Reservations.csv", sep = ",")


#è possibile utilizzare la funzione str() per visualizzare le informazioni di base sulle variabili
str(data)

# Identificazione dei valori mancanti
missing_values <- colSums(is.na(data))
print(missing_values)

#aggiungere rimozione del booking_id 
data = data[, 2:19]


#converto alcune covariate tra cui il target in un tipo target
data$booking_status = factor(data$booking_status)
data$type_of_meal_plan= factor(data$type_of_meal_plan)
data$room_type_reserved= factor(data$room_type_reserved)
data$market_segment_type= factor(data$market_segment_type)
data$required_car_parking_space= factor(data$required_car_parking_space)
data$repeated_guest= factor(data$repeated_guest)


str(data)

################################################### Definizione training e test set ################################################### 
#installiamo e importiamo la libreria caret
install.packages("caret")
library(caret)

#dividiamo il dataset in train e test set
set.seed(1)
splitIndex <- createDataPartition(data$booking_status, p = .70, list = FALSE, times = 1)
train_set <- data[splitIndex,]
test_set <- data[-splitIndex,]

################################################### ANALISI UNIVARIATA ###################################################

# utilizzare la funzione summary() per visualizzare le statistiche di base delle variabili quantitative
summary(train_set)

#diagramma a barre della distribuzione dei valori della variabile target
plot(train_set[,18],col=c(4,6))
#grafico a torta della distribuzione dei valori del target
pie(table(train_set$booking_status))


#Diagramma a barre per visualizzare la distribuzione delle variabili quantitative e qualitative, è possibile utilizzare la funzione ggplot()
#lo facciamo per ogni covariata 
print(ggplot(data = train_set, aes(x = no_of_adults)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of adults"))
print(ggplot(data = train_set, aes(x = no_of_children)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of children") + scale_x_discrete(limits=c(min(train_set$no_of_children),max(train_set$no_of_children))))
print(ggplot(data = train_set, aes(x = no_of_weekend_nights)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of weekend nights"))
print(ggplot(data = train_set, aes(x = no_of_week_nights)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of week nights"))
print(ggplot(data = train_set, aes(x = type_of_meal_plan)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("type of meal plan"))
print(ggplot(data = train_set, aes(x = required_car_parking_space)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("required car parking space"))
print(ggplot(data = train_set, aes(x = room_type_reserved)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("room type reserved"))
print(ggplot(data = train_set, aes(x = lead_time)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("lead time"))  
print(ggplot(data = train_set, aes(x = arrival_year)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("arrival year") + scale_x_discrete(limits=c(min(train_set$arrival_year),max(train_set$arrival_year))))
print(ggplot(data = train_set, aes(x = arrival_month)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("arrival month") + scale_x_discrete(limits=c(min(train_set$arrival_month),max(train_set$arrival_month))))
print(ggplot(data = train_set, aes(x = arrival_date)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("arrival date"))
print(ggplot(data = train_set, aes(x = market_segment_type)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("market segment type"))
print(ggplot(data = train_set, aes(x = repeated_guest)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("repeated guest"))
print(ggplot(data = train_set, aes(x = no_of_previous_cancellations)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of previous cancellations"))
print(ggplot(data = train_set, aes(x = no_of_previous_bookings_not_canceled)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("number of previous bookings not canceled"))
print(ggplot(data = train_set, aes(x = avg_price_per_room)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("average price per room"))

#Una alternativa sarebbe utilizzare un box plot, che mostra la distribuzione dei dati attraverso quartili. Il box plot mostra la mediana, i quartili inferiore e superiore e gli estremi dei dati, e può essere utilizzato per individuare eventuali valori anomali o outlier.
print(ggplot(data = train_set, aes(x = no_of_special_requests)) + geom_bar(fill = "blue", alpha = 0.8) + ggtitle("no_of_special_requests"))


#generiamo i boxplot di ogni covariata quantitativa
print(ggplot(data = train_set, aes(x = "", y = no_of_weekend_nights)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("no_of_weekend_nights"))
print(ggplot(data = train_set, aes(x = "", y = no_of_week_nights)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("no_of_week_nights"))
print(ggplot(data = train_set, aes(x = "", y = arrival_month)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("arrival_month"))
print(ggplot(data = train_set, aes(x = "", y = arrival_date)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("arrival_date"))
print(ggplot(data = train_set, aes(x = "", y = repeated_guest)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("repeated_guest"))
print(ggplot(data = train_set, aes(x = "", y = no_of_previous_cancellations)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("no_of_previous_cancellations"))
print(ggplot(data = train_set, aes(x = "", y = no_of_previous_bookings_not_canceled)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("no_of_previous_bookings_not_canceled"))
print(ggplot(data = train_set, aes(x = "", y = avg_price_per_room)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("Average price per room"))
print(ggplot(data = train_set, aes(x = "", y = no_of_special_requests)) + geom_boxplot(fill = "blue", alpha = 0.8) + ggtitle("no_of_special_requests"))


################################################### ANALISI MULTIVARIATA ###################################################

#confronto per ogni valore di ciascuna covariata quante istanze hanno target "Canceled" o "Not_canceled"

#covariata = no_of_adults
ggplot(data = train_set, aes(x = no_of_adults, fill = booking_status)) +
  geom_bar(position = "dodge") +
  xlab("number of adults") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of adults")

#covariata = no_of_children
ggplot(data = train_set, aes(x = factor(no_of_children), fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of children") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of children")

#covvariata = no_of_weekend_nights
ggplot(data = train_set, aes(x = no_of_weekend_nights, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of weekend nights") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of weekend nights")

#covariata = no_of_week_nights
ggplot(data = train_set, aes(x = no_of_week_nights, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of week nights") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of week nights")

#covariata = type_of_meal_plan
ggplot(data = train_set, aes(x = type_of_meal_plan, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("Type of Meal Plan") +
  ylab("Count") +
  ggtitle("Count of Reservations by Type of Meal Plan")

#covariata = required_car_parking_space
ggplot(data = train_set, aes(x = required_car_parking_space, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("required car parking space") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of required car parking space")

#covariata == room_type_reserved
ggplot(data = train_set, aes(x = room_type_reserved, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("room type reserved") +
  ylab("Count") +
  ggtitle("Count of Reservations by room type reserved")

#covariata = lead_time
ggplot(data = train_set, aes(x = lead_time, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("lead time") +
  ylab("Count") +
  ggtitle("Count of Reservations by lead time")

#covariata = arrival_year
ggplot(data = train_set, aes(x = factor(arrival_year), fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("arrival year") +
  ylab("Count") +
  ggtitle("Count of Reservations by arrival year")

#covariata = arrival_month
ggplot(data = train_set, aes(x = factor(arrival_month), fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("arrival month") +
  ylab("Count") +
  ggtitle("Count of Reservations by arrival month")

#covariata = arrival_date
ggplot(data = train_set, aes(x = arrival_date, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("arrival date") +
  ylab("Count") +
  ggtitle("Count of Reservations by arrival date")

#covariata = market_segment_type
ggplot(data = train_set, aes(x = market_segment_type, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("market segment type") +
  ylab("Count") +
  ggtitle("Count of Reservations by market segment type")

#covariata = repeated_guest
ggplot(data = train_set, aes(x = factor(repeated_guest), fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("repeated guest") +
  ylab("Count") +
  ggtitle("Count of Reservations by repeated guest")

#covariata = no_of_previous_cancellations
ggplot(data = train_set, aes(x = no_of_previous_cancellations, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of previous cancellations") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of previous cancellations")

#covariata = no_of_previous_bookings_not_canceled
ggplot(data = train_set, aes(x = no_of_previous_bookings_not_canceled, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of previous bookings not canceled") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of previous bookings not canceled")

#covariata = avg_price_per_room   !!!!!!!!!!questo si potrebbe togliere, poco rilevante!!!!!!!!!
ggplot(data = train_set, aes(x = avg_price_per_room, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("average price per room") +
  ylab("Count") +
  ggtitle("Count of Reservations by average price per room")

#covariata = no_of_special_requests
ggplot(data = train_set, aes(x = no_of_special_requests, fill = booking_status)) + 
  geom_bar(position = "dodge") +
  xlab("number of special requests") +
  ylab("Count") +
  ggtitle("Count of Reservations by number of special requests")

#Andiamo ora alla ricerca di un buon attributo in grado di poter classificare bene le nostre istanze: 

#Cerchiamo se esiste qualche correlazione tra l'attributo bambini e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione al fatto che si hanno bambini o meno:
cancelled_table <- table(train_set$booking_status, train_set$no_of_children > 0) #true= almeno 1 bambino FALSE=niente bambini
#Rinominiamo le colonne:
colnames(cancelled_table) <- c("0 bambini", "Con bambini")
#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(cancNoChildren <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(cancWithChildren <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
#Confroniamo i risultati delle variabili cancNoChildren e cancWithChildren

#Cerchiamo se esiste qualche correlazione tra l'attributo vecchio/nuovo cliente e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione all'attributo repeated_guest:
counts = table(train_set$booking_status, train_set$repeated_guest)
#Rinominiamo le colonne:
colnames(counts) <- c("Nuovo cliente", "Cliente Abituale")
#Creiamo il barplot:
barplot(counts, col=c("red","green"), legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(counts[,1])
cancNewClient <- (counts[1,1]/sommaPrimaColonna)*100
sommaSecondaColonna <- sum(counts[,2])
cancOldClient <- (counts[1,2]/sommaSecondaColonna)*100
#Confroniamo i risultati delle variabili cancNewClient e cancOldClient


#Cerchiamo se esiste qualche correlazione tra l'attributo arrival month e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione al periodo di prenotazione:
cancelled_table <- table(train_set$booking_status, train_set$arrival_month>4 & train_set$arrival_month<11) #true= prenotato tra maggio e ottobre(alta stagione) FALSE=prenotato a Novembre,Dicembre,Gennaio,Febbraio,Marzo e Aprile (bassa stagione)
#Rinominiamo le colonne:
colnames(cancelled_table) <- c("Bassa Stagione", "Alta stagione")
#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(cancBassaStagione <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(cancAltaStagione <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
#Confroniamo i risultati delle variabili cancBassaStagione e cancAltaStagione

#Cerchiamo se esiste qualche correlazione tra l'attributo arrival year e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione all'anno di prenotazione:
cancelled_table <- table(train_set$booking_status, train_set$arrival_year) #true= prenotato nel 2017 FALSE=prenotato nel 2018

#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(canc2017 <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(canc2018 <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
#Confroniamo i risultati delle variabili canc2017 e canc2018

#Cerchiamo se esiste qualche correlazione tra l'attributo market segment type e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione al market segment type:
cancelled_table <- table(train_set$booking_status, train_set$market_segment_type)

#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(cancAviation <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(cancComplementary <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
sommaTerzaColonna <- sum(cancelled_table[,3])
print(cancCorporate <- (cancelled_table[1,3]/sommaTerzaColonna)*100)
sommaQuartaColonna <- sum(cancelled_table[,4])
print(cancOffline <- (cancelled_table[1,4]/sommaQuartaColonna)*100)
sommaQuintaColonna <- sum(cancelled_table[,5])
print(cancOnlline <- (cancelled_table[1,5]/sommaQuintaColonna)*100)

#Cerchiamo se esiste qualche correlazione tra l'attributo required_car_parking_space e il target
#Creiamo prima di tutto una tabella che contiene il numero di prenotazioni cancellate e non in relazione alla richiesta di parcheggio privato durante il soggiorno:
cancelled_table <- table(train_set$booking_status, train_set$required_car_parking_space) 

#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(cancNoparking <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(cancParking <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
#Confroniamo i risultati delle variabili cancNoparking e cancParking


cancelled_table <- table(train_set$booking_status, train_set$no_of_special_requests)

#Creiamo il barplot:
barplot(cancelled_table, col=c("red","green"),legend = c("cancellata","mantenuta"))
#Calcoliamo la percentuale e confrontiamola:
sommaPrimaColonna <- sum(cancelled_table[,1])
print(canc0sr <- (cancelled_table[1,1]/sommaPrimaColonna)*100)
sommaSecondaColonna <- sum(cancelled_table[,2])
print(canc1sr <- (cancelled_table[1,2]/sommaSecondaColonna)*100)
sommaTerzaColonna <- sum(cancelled_table[,3])
print(canc2sr <- (cancelled_table[1,3]/sommaTerzaColonna)*100)
sommaQuartaColonna <- sum(cancelled_table[,4])
print(canc3sr <- (cancelled_table[1,4]/sommaQuartaColonna)*100)
sommaQuintaColonna <- sum(cancelled_table[,5])
print(canc4sr <- (cancelled_table[1,5]/sommaQuintaColonna)*100)
sommaSestaColonna <- sum(cancelled_table[,5])
print(canc5sr <- (cancelled_table[1,5]/sommaSestaColonna)*100)



################################################### Decision tree ################################################### 

#creiamo il modello di albero di decisione
library(rpart)
model <- rpart(booking_status ~ no_of_adults + no_of_children + no_of_weekend_nights + no_of_week_nights + type_of_meal_plan + required_car_parking_space + room_type_reserved + arrival_year + arrival_month + arrival_date + market_segment_type + repeated_guest + no_of_previous_cancellations + no_of_previous_bookings_not_canceled + no_of_special_requests , data = train_set, method = "class")


install.packages("rattle")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model)

#effettuiamo le predizioni sul test set
predictions <- predict(model, test_set, type = "class")


#valutiamo la performance del modello
confusionMatrix(predictions, test_set$booking_status)

dt_confusionMatrix_Canceled = confusionMatrix(predictions, test_set$booking_status, mode = "prec_recall", positive = "Canceled")
dt_confusionMatrix_Not_Canceled = confusionMatrix(predictions, test_set$booking_status, mode = "prec_recall", positive = "Not_Canceled")

dt_confusionMatrix_Canceled
dt_confusionMatrix_Not_Canceled




#grafico complexity parameter
plotcp(model)



#impostiamo cp=0.015 osservando un minimo cambiamento nei valori di specificità,accuratezza e sensitività rispetto al valore originale 0.11
pruned_model <- prune(model, 0.015)
fancyRpartPlot(pruned_model)

predictions <- predict(pruned_model, test_set, type = "class")


dt_cp15_confusionMatrix_Canceled = confusionMatrix(predictions, test_set$booking_status, mode = "prec_recall", positive = "Canceled")
dt_cp15_confusionMatrix_Not_Canceled = confusionMatrix(predictions, test_set$booking_status, mode = "prec_recall", positive = "Not_Canceled")

dt_cp15_confusionMatrix_Canceled
dt_cp15_confusionMatrix_Not_Canceled

#precision calcolata con macro average
precision_Canceled_cp15 <- dt_confusionMatrix_Canceled$byClass[["Precision"]]
precision_Not_Canceled_cp15 <- dt_confusionMatrix_Not_Canceled$byClass[["Precision"]]

precision_macro_average_cp15 = mean(c(precision_Canceled_cp15, precision_Not_Canceled_cp15))


#recall calcolata con macro average
recall_Canceled_cp15 <- dt_confusionMatrix_Canceled$byClass[["Recall"]]
recall_Not_Canceled_cp15 <- dt_confusionMatrix_Not_Canceled$byClass[["Recall"]]

recall_macro_average_cp15 = mean(c(recall_Canceled_cp15, recall_Not_Canceled_cp15))

#f-measure calcolata con macro average
F1_Canceled_cp15 <- dt_confusionMatrix_Canceled$byClass[["F1"]]
F1_Not_Canceled_cp15 <- dt_confusionMatrix_Not_Canceled$byClass[["F1"]]

F1_macro_average_cp15 = mean(c(F1_Canceled_cp15, F1_Not_Canceled_cp15))

##Calcolo overall accuracy
library(caret)
install.packages("ROCR")
library(ROCR)

predictions <- predict(pruned_model, test_set)[,2]
# Creiamo un oggetto di tipo "prediction" che contiene le predizioni e il target vero
pred_obj <- prediction(predictions, test_set$booking_status)

# Calcoliamo la performance con la metrica "acc"

perf_obj <- performance(pred_obj, measure = "acc")

plot(perf_obj)

# Estraiamo il valore di overall accuracy
overall_accuracy <- attr(perf_obj, "y.value")[1]

#####ROC#####

#Calcoliamo le predizioni sul test_set
predictions <- predict(pruned_model, test_set, type = "class")

# Calcola le probabilità che la prenotazione non venga cancellata
probs <- predict(pruned_model, newdata = test_set, type = "prob")[,2]

# Crea la curva ROC
pred <- prediction(probs, test_set$booking_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for Decision Tree Model", col="blue")
abline(0, 1, col="red")

# Calcola l'AUC utilizzando la funzione performance
perf <- performance(pred, "auc")
# Visualizza l'AUC
print(perf@y.values[[1]])

#impostiamo cp=0.025 osservando un minimo cambiamento nei valori di specificità,accuratezza e sensitività rispetto al valore originale 0.11
pruned_model <- prune(model, 0.025)
predictions <- predict(pruned_model, test_set, type = "class")
confusionMatrix(predictions, test_set$booking_status)
fancyRpartPlot(pruned_model)



################################################### Random Forest ################################################### 


#Non è necessario normalizzare le variabili che rappresentano la data dell'arrivo (arrival_year, arrival_month, arrival_date) poiché non hanno valori molto diversi tra loro e la loro scala non influirà sulla performance del modello. Inoltre, la normalizzazione delle date non è un'operazione convenzionale poiché le date hanno un ordine temporale che non può essere alterato dalla normalizzazione.
#Le variabili no_of_previous_cancellations e no_of_previous_bookings_not_cancelled non sono state normalizzate poiché possono avere valori molto diversi tra loro. Normalizzare queste variabili può influire negativamente sul modello e sulla sua capacità di prevedere correttamente. Tuttavia, in alcuni casi, può essere utile normalizzare anche queste variabili per evitare che abbiano un peso eccessivo rispetto alle altre. La decisione di normalizzare o meno queste variabili dipende dalla situazione specifica e dalle esigenze del modello.
#Sì, anche se i valori di no_of_special_requests vanno da 0 a 5, normalizzare le variabili quantitative può essere utile per evitare che una variabile abbia un peso eccessivo rispetto alle altre e influisca negativamente sul modello. La normalizzazione non è obbligatoria, ma può migliorare l'accuratezza e la stabilità del modello.
# Normalizzazione delle variabili quantitative
rf_data = data
rf_data[, c("no_of_adults", "no_of_children", "no_of_weekend_nights", "no_of_week_nights", "lead_time", "avg_price_per_room", "no_of_special_requests")] <- scale(data[, c("no_of_adults", "no_of_children", "no_of_weekend_nights", "no_of_week_nights", "lead_time", "avg_price_per_room", "no_of_special_requests")])

library(caret)
set.seed(123)
index <- createDataPartition(rf_data$booking_status, p = 0.7, list = FALSE)
rf_train_set <- rf_data[index, ]
rf_test_set <- rf_data[-index, ]

install.packages("randomForest")
# Load the randomForest library
library(randomForest)

# Build the random forest model
rf_model <- randomForest(booking_status ~ ., data = rf_train_set, ntree = 500, mtry = 3, importance = TRUE)
#Spiegazione:     
#library(randomForest) carica la libreria randomForest.
#rf_model <- randomForest(booking_status ~ ., data = rf_train_set, ntree = 500, mtry = 3, importance = TRUE) costruisce il modello di random forest utilizzando il train set rf_train_set.
#booking_status ~ . indica che la variabile da prevedere è booking_status e che tutte le altre variabili nel train set sono usate come predictor.
#ntree = 500 indica che il modello deve essere costruito utilizzando 500 alberi.
#mtry = 3 indica che ogni albero deve utilizzare 3 variabili a caso per dividere i nodi.
#importance = TRUE calcola l'importanza delle variabili nel modello.

#Si può utilizzare la funzione "importance()" per visualizzare l'importanza delle variabili. Questo fornisce una misura della qualità delle variabili nel modello.
varImp <- importance(rf_model)
varImp

#Si può utilizzare la funzione "table()" per creare una matrice di confusione che mostra la quantità di casi previsti correttamente e incorrettamente.
# Matrice di confusione
rf_prediction <- predict(rf_model, rf_test_set[, -18])

confusion_matrix_table <- table(rf_test_set$booking_status, rf_prediction)
confusion_matrix_table

confusionMatrix(rf_prediction, rf_test_set$booking_status)
#le performance vanno misurate per ogni classe della covariata target
confusionMatrix(rf_prediction, rf_test_set$booking_status, positive = "Not_Canceled")

#calcoliamo precision, recall e f-measure di ogni classe del target

rf_confusionMatrix_Canceled = confusionMatrix(rf_prediction, rf_test_set$booking_status, mode = "prec_recall", positive = "Canceled")
rf_confusionMatrix_Not_Canceled = confusionMatrix(rf_prediction, rf_test_set$booking_status, mode = "prec_recall", positive = "Not_Canceled")

rf_confusionMatrix_Canceled
rf_confusionMatrix_Not_Canceled

#precision calcolata con macro average
rf_precision_Canceled <- rf_confusionMatrix_Canceled$byClass[["Precision"]]
rf_precision_Not_Canceled <- rf_confusionMatrix_Not_Canceled$byClass[["Precision"]]

rf_precision_macro_average = mean(c(rf_precision_Canceled, rf_precision_Not_Canceled))


#recall calcolata con macro average
rf_recall_Canceled <- rf_confusionMatrix_Canceled$byClass[["Recall"]]
rf_recall_Not_Canceled <- rf_confusionMatrix_Not_Canceled$byClass[["Recall"]]

rf_recall_macro_average = mean(c(rf_recall_Canceled, rf_recall_Not_Canceled))

#f-measure calcolata con macro average
rf_F1_Canceled <- rf_confusionMatrix_Canceled$byClass[["F1"]]
rf_F1_Not_Canceled <- rf_confusionMatrix_Not_Canceled$byClass[["F1"]]

rf_F1_macro_average = mean(c(rf_F1_Canceled, rf_F1_Not_Canceled))


##Calcolo overall accuracy

rf_predictions <- predict(rf_model, rf_test_set, type = "prob")
# Creare un oggetto predizione
rf_pred_obj <- prediction(rf_predictions[,2], rf_test_set$booking_status)

# Calcolare l'accuratezza complessiva in funzione del cutoff
rf_perf_obj <- performance(rf_pred_obj, "acc")

# Visualizzare il grafico
plot(rf_perf_obj)


#Si può utilizzare la funzione "plot()" per visualizzare un grafico che mostra la distribuzione delle predizioni rispetto alle osservazioni reali.
plot(rf_test_set$booking_status, predict(rf_model, rf_test_set[, -18]))

#####ROC#####

#Calcoliamo le predizioni sul test_set
predictions <- predict(rf_model, rf_test_set, type = "class")

# Calcola le probabilità che la prenotazione non venga cancellata
probs <- predict(rf_model, newdata = rf_test_set, type = "prob")[,2]

# Crea la curva ROC
pred <- prediction(probs, rf_test_set$booking_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for Random Forest Model", col="blue")
abline(0, 1, col="red")

# Calcola l'AUC utilizzando la funzione performance
perf <- performance(pred, "auc")
# Visualizza l'AUC
print(perf@y.values[[1]])


################################################### Neural network ################################################### 
nn_data <- data[1:2000,]# vengono usate 2000 istanze del dataset
nn_data$no_of_adults <- as.numeric(nn_data$no_of_adults)
nn_data$no_of_children <- as.numeric(nn_data$no_of_children)
nn_data$no_of_weekend_nights <- as.numeric(nn_data$no_of_weekend_nights)
nn_data$no_of_week_nights <- as.numeric(nn_data$no_of_week_nights)
nn_data$lead_time <- as.numeric(nn_data$lead_time)
nn_data$arrival_year <- as.numeric(nn_data$arrival_year)
nn_data$arrival_month <- as.numeric(nn_data$arrival_month)
nn_data$arrival_date <- as.numeric(nn_data$arrival_date)
nn_data$no_of_previous_bookings_not_canceled <- as.numeric(nn_data$no_of_previous_bookings_not_canceled)
nn_data$no_of_previous_cancellations <- as.numeric(nn_data$no_of_previous_cancellations)
nn_data$avg_price_per_room <- as.numeric(nn_data$avg_price_per_room) #!!!!! è gia numeric e viene riconvertito a numeric
nn_data$no_of_special_requests <- as.numeric(nn_data$no_of_special_requests)

#la libreria neuralnet di R non supporta direttamente la modellizzazione di variabili target categoriali. 
#È necessario convertire la variabile target in una forma numerica
nn_data$booking_status <- ifelse(nn_data$booking_status == "Canceled", 0, 1)
#assegna il valore 0 a tutte le osservazioni con "Canceled" come valore di "booking_status", e il valore 1 a tutte le osservazioni con "Not_Canceled"

str(nn_data)


#La normalizzazione consiste nel trasformare i valori dei dati in una scala uniforme. 
#Ciò aiuta a evitare che alcune caratteristiche abbiano un maggiore impatto sul modello rispetto ad altre a causa di scale di valori diverse. 
#Ci sono diversi modi di normalizzare i dati, come la normalizzazione Z-score. 
#Questo processo aiuta a migliorare la convergenza e la prestazione del modello.
#Questo codice normalizza tutte le colonne dalla 1 alla 17 esclusa la colonna "booking_status". 
#nn_data[,1:17] <- scale(nn_data[,1:17])

nn_data[, c("no_of_adults", "no_of_children", "no_of_weekend_nights", "no_of_week_nights", "lead_time", "avg_price_per_room", "no_of_special_requests")] <- scale(nn_data[, c("no_of_adults", "no_of_children", "no_of_weekend_nights", "no_of_week_nights", "lead_time", "avg_price_per_room", "no_of_special_requests")])

#La funzione scale() calcolerà la media e la deviazione standard per ogni colonna e trasformerà i valori di ogni colonna in valori normalizzati (z-score).
str(nn_data)
#La standardizzazione delle variabili numeriche consiste nel trasformare i valori in modo che abbiano media 0 e deviazione standard 1. Ciò è utile perché molti algoritmi di machine learning funzionano meglio quando le variabili hanno la stessa scala.
#La formula per standardizzare una variabile numerica X è:

#Z = (X - media(X)) / deviazione_standard(X)

#Dove Z è la variabile standardizzata, media(X) è la media della variabile X e deviazione_standard(X) è la deviazione standard della variabile X.
#È importante standardizzare le variabili numeriche prima di utilizzare alcuni algoritmi di machine learning, come ad esempio le reti neurali o i modelli di regressione logistica, in quanto questi algoritmi tendono ad essere sensibili alla scala delle variabili.




install.packages("mltools")
library(mltools)

install.packages("data.table")
library(data.table)

nn_data <- one_hot(as.data.table(nn_data))

str(nn_data)

#nn_data <- nn_data[, -1]
colnames(nn_data)[5] <- "type_of_mealPlan_MealPlan1"
colnames(nn_data)[6] <- "type_of_mealPlan_MealPlan2"
colnames(nn_data)[7] <- "type_of_mealPlan_MealPlan3"
colnames(nn_data)[8] <- "type_of_mealPlan_NotSelected"
colnames(nn_data)[11] <- "room_type_reservedRoom_Type1"
colnames(nn_data)[12] <- "room_type_reservedRoom_Type2"
colnames(nn_data)[13] <- "room_type_reservedRoom_Type3"
colnames(nn_data)[14] <- "room_type_reservedRoom_Type4"
colnames(nn_data)[15] <- "room_type_reservedRoom_Type5"
colnames(nn_data)[16] <- "room_type_reservedRoom_Type6"
colnames(nn_data)[17] <- "room_type_reservedRoom_Type7"

str(nn_data)



# utilizziamo la funzione sample() per selezionare un campione casuale del dataset per l'addestramento
set.seed(123)
train_index <- sample(1:nrow(nn_data), 0.7 * nrow(nn_data))
nn_train_data <- nn_data[train_index, ]
nn_test_data <- nn_data[-train_index, ]


#package neauralnet
install.packages("neuralnet")
library(neuralnet)

#è necessario fornire tutte le covariate come input al modello di rete neurale perché queste possono influire sulla previsione del target. In questo caso, tutte le 17 covariate presenti nel dataset data_numeric verranno utilizzate come input per la previsione della variabile target booking_status.
#Non esiste un numero specifico di strati nascosti che sia universalmente considerato il migliore per tutti i dataset. Il numero di strati nascosti dipende dalle dimensioni e dalla complessità del dataset, e dalle prestazioni desiderate. In genere, si inizia con un numero relativamente basso di strati nascosti (ad esempio, 1 o 2) e si aumenta gradualmente se le prestazioni non sono soddisfacenti. È importante anche evitare di utilizzare troppi strati nascosti poiché ciò potrebbe portare a un overfitting dei dati di allenamento.
start_time <- Sys.time()
nn_model = neuralnet(booking_status ~ no_of_adults + no_of_children + no_of_weekend_nights + no_of_week_nights + type_of_mealPlan_MealPlan1 + type_of_mealPlan_MealPlan2 + type_of_mealPlan_MealPlan3 + type_of_mealPlan_NotSelected + 
                       required_car_parking_space_0 + required_car_parking_space_1 + room_type_reservedRoom_Type1 + room_type_reservedRoom_Type2 + room_type_reservedRoom_Type3 +room_type_reservedRoom_Type4 +room_type_reservedRoom_Type5 + room_type_reservedRoom_Type6 + room_type_reservedRoom_Type7
                       + lead_time + arrival_year + arrival_month + arrival_date + market_segment_type_Aviation + market_segment_type_Complementary + market_segment_type_Corporate + market_segment_type_Offline + 
                       market_segment_type_Online + repeated_guest_0 + repeated_guest_1 + no_of_previous_cancellations + no_of_previous_bookings_not_canceled + avg_price_per_room + no_of_special_requests, data = nn_train_data, hidden = c(16,4) , act.fct="logistic", stepmax = 1e7, linear.output = FALSE ) #-> !!!!!!!!!!!!Il modello neuralnet potrebbe avere difficoltà a convergere con grandi quantità di dati, in questo caso 25,000 istanze. Potresti provare a ridurre le dimensioni del train set, oppure a modificare i parametri di addestramento della rete neurale (ad esempio, aumentare il numero di ripetizioni o il numero massimo di passi). Potresti anche provare a utilizzare un algoritmo diverso di addestramento di reti neurali, come ad esempio quello offerto dalla libreria caret.



#nn_model <- neuralnet(input_data, output, hidden=c(10, 5), linear.output=TRUE)
#Quando l'algoritmo di una rete neurale non converge, significa che non è stato in grado di trovare una soluzione ottimale per i pesi della rete entro un certo numero di passaggi. Ciò può accadere a causa di molte ragioni, come ad esempio la scelta dei parametri non ottimali, la presenza di rumore o di una distribuzione sbilanciata dei dati di addestramento, o la mancanza di un numero sufficiente di dati di addestramento. In questi casi, può essere necessario modificare i parametri dell'algoritmo, raccogliere più dati di addestramento o utilizzare un altro tipo di modello di rete neurale.
#L'uscita della rete lineare significa che la funzione di attivazione utilizzata nell'ultimo strato della rete neurale è una funzione lineare, come ad esempio la funzione identità. Questo significa che la rete neurale restituisce una previsione lineare in base ai pesi delle connessioni tra i nodi della rete. Questo può essere utile in alcune applicazioni, come la regressione, in cui si desidera che la previsione sia un valore continuo, ma potrebbe non essere adatto per altre applicazioni, come la classificazione, in cui si desidera che la previsione sia una classe discreta.


end_time <- Sys.time()
print(time_elapsed <- end_time - start_time)

plot(nn_model)




#CALCOLO MATRICE DI CONFUSIONE
library(caret)
nn_pred <- predict(nn_model, nn_test_data[,-33])
nn_pred <- ifelse(nn_pred[,1] > 0.5, 1, 0)
nn_pred <- as.factor(nn_pred)
nn_test_data$booking_status <- as.factor(nn_test_data$booking_status)

#confusion_matrix <- confusionMatrix(nn_pred, nn_test_data$booking_status)
#le performance vanno misurate per ogni classe della covariata target

#calcoliamo precision, recall e f-measure di ogni classe del target
nn_confusionMatrix_Canceled = confusionMatrix(nn_pred, nn_test_data$booking_status, mode = "prec_recall", positive = "0")
nn_confusionMatrix_Not_Canceled = confusionMatrix(nn_pred, nn_test_data$booking_status, mode = "prec_recall", positive = "1")

nn_confusionMatrix_Canceled
nn_confusionMatrix_Not_Canceled

#precision calcolata con macro average
nn_precision_Canceled <- nn_confusionMatrix_Canceled$byClass[["Precision"]]
nn_precision_Not_Canceled <- nn_confusionMatrix_Not_Canceled$byClass[["Precision"]]

nn_precision_macro_average = mean(c(nn_precision_Canceled, nn_precision_Not_Canceled))


#recall calcolata con macro average
nn_recall_Canceled <- nn_confusionMatrix_Canceled$byClass[["Recall"]]
nn_recall_Not_Canceled <- nn_confusionMatrix_Not_Canceled$byClass[["Recall"]]

nn_recall_macro_average = mean(c(nn_recall_Canceled, nn_recall_Not_Canceled))

#f-measure calcolata con macro average
nn_F1_Canceled <- nn_confusionMatrix_Canceled$byClass[["F1"]]
nn_F1_Not_Canceled <- nn_confusionMatrix_Not_Canceled$byClass[["F1"]]

nn_F1_macro_average = mean(c(nn_F1_Canceled, nn_F1_Not_Canceled))




####Calcolo overall accuracy####

nn_predictions <- predict(nn_model, nn_test_data, type= "prob")
# Creare un oggetto predizione
nn_pred_obj <- prediction(nn_predictions[,1], nn_test_data$booking_status)

# Calcolare l'accuratezza complessiva in funzione del cutoff
nn_perf_obj <- performance(nn_pred_obj, measure = "acc")

# Visualizzare il grafico
plot(nn_perf_obj)




#####ROC#####

# Calcola le probabilità che la prenotazione non venga cancellata
probs <- predict(nn_model, newdata = nn_test_data, type = "prob")

# Crea la curva ROC
pred <- prediction(probs, nn_test_data$booking_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for Random Forest Model", col="blue")
abline(0, 1, col="red")

# Calcola l'AUC utilizzando la funzione performance
perf <- performance(pred, "auc")
# Visualizza l'AUC
print(perf@y.values[[1]])

