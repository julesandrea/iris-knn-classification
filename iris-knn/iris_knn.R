data("iris")
head(iris)
table(iris$Species)

#Representation de la Longueur du sépale selon l'espèce
boxplot(Sepal.Length ~ Species, data = iris, col = c("red", "blue", "orange"),
        main = "Longueur du sépale selon l'espèce")

#Résumé par espèce selon la longueur des sépales  
m1=aggregate(Sepal.Length ~ Species, data = iris, summary)


#Representation de la Largeur du sépale selon l'espèce
boxplot(Sepal.Width ~ Species, data = iris, col = c("red", "blue", "orange"),
        main = "Largeur du sépale selon l'espèce")

#Résumé par espèce selon la largeur des sépales  
m2=aggregate(Sepal.Width ~ Species, data = iris, summary)

#Representation de la Longueur du pétale selon l'espèce
boxplot(Petal.Length ~ Species, data = iris, col = c("red", "blue", "orange"),
        main = "Longueur du pétale selon l'espèce")

#Résumé par espèce selon la longueur des pétale  
m3=aggregate(Petal.Length ~ Species, data = iris, summary)

#Representation de la Largeur du pétale selon l'espèce
boxplot(Petal.Width ~ Species, data = iris, col = c("red", "blue", "orange"),
        main = "Largeur du pétale selon l'espèce")

#Résumé par espèce selon la largeur des pétale  
m4=aggregate(Petal.Width ~ Species, data = iris, summary)


# Fonction pour calculer la distance entre deux fleurs
distance <- function(fleur1, fleur2) {
  sqrt(sum((fleur1 - fleur2)^2))
}

# Fonction pour prédire l'espèce d'une fleur
predire_espece <- function(nouvelle_fleur, fleurs_connues, k=5){
  
  # Calcule des distances entre la nouvelle fleur et les fleurs connues
  distances <- apply(fleurs_connues[,1:4], 1, function(row)distance(nouvelle_fleur, row))
  
  # Prend les indices des k plus proches voisins
  indices <- order(distances)[1:k]
  
  # Récupère leurs espèces
  nearest_species <- fleurs_connues$Species[indices]
  
  # Renvoie l’espèce la plus fréquente
  prediction_espece <- names(sort(table(nearest_species), decreasing = TRUE))[1]
  return(prediction_espece)
}

d=iris[1,1:4]
f=iris[2,1:4]
#test
g=predire_espece(d,iris)
h=predire_espece(f,iris)

print(g)

# Fixer la graine aléatoire pour reproductibilité
#set.seed(123)

# Indices aléatoires pour l'ensemble d'apprentissage (80%)
indices <- sample(1:150, 120)

# Création des deux sous-ensembles
ensemble_apprentissage <- iris[indices, ]
ensemble_test <- iris[-indices, ]


# Prédiction des espèces pour les fleurs du test
predictions <- sapply(1:30, function(i) {
  predire_espece(ensemble_test[i, 1:4], ensemble_apprentissage, k = 5)
})


# Espèces réelles du test
Especes_reelles <- ensemble_test$Species

# Nombre de bonnes prédictions
succes <- sum(predictions == Especes_reelles)

# Taux de succès (en %)
taux_succes <- succes / length(Especes_reelles) * 100

# Affichage
cat("Le taux de succès :", round(taux_succes, 2), "%\n")

# Matrice de confusion
table(Predicted = predictions, Actual = test_data$Species)

