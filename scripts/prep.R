#IMPORTATION DU DATASET
# On remonte d'un cran pour choper le CSV à la racine

churn<-read.csv("./WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE,sep=",")
str(churn)
summary(churn)

#Valeurs manquantes

naChurn<-colSums(is.na(churn))
print(naChurn)

##Suppression de lignes avec NA

churn<-na.omit(churn)

#Verification

na_churn<-colSums(is.na(churn))
print(na_churn)

# Detection de valeurs aberrrantes



##Visualisation du dataset

head(churn)
tail(churn)



##ANALYSE UNIVARIEE

vars_pure_quali_orig <- c("gender", "Partner", "Dependents", "MultipleLines", 
                          "InternetService", "OnlineSecurity", "OnlineBackup", 
                          "DeviceProtection", "TechSupport", "StreamingTV", 
                          "StreamingMovies", "Contract", "PaperlessBilling", 
                          "PaymentMethod")




library(ggplot2)

# 1. Ton vecteur de variables (on garde les "pures quali")
vars_pure_quali_orig <- c("gender", "Partner", "Dependents", "MultipleLines", 
                          "InternetService", "OnlineSecurity", "OnlineBackup", 
                          "DeviceProtection", "TechSupport", "StreamingTV", 
                          "StreamingMovies", "Contract", "PaperlessBilling", 
                          "PaymentMethod")

# 2. La fonction (Bien s'assurer que data est ton dataframe churn)
diag_univ <- function(data, var) {
  ggplot(data, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar() +
    # Utilisation de after_stat(count) pour les versions récentes de ggplot2
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = paste("Répartition de", var), 
         x = var, 
         y = "Nombre de clients")
}

# 3. La boucle avec print()
for (v in vars_pure_quali_orig) {
  p <- diag_univ(churn, v)
  print(p)
}