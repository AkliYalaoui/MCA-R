
# Présentation du dataset : 

# Importer les données
data <- read.csv("International students Time management data.csv")

# Nombre de variables
print(c("nombre de variable",ncol(data)))

# Nombre d'individus
print(c("nombre de ligne",nrow(data)))

print("#######################################")
# Les noms et type des variables
for(i in 1:ncol(data)){
  print(c(colnames(data)[i],typeof(data[,i])))
}


# Prétraitement des données : 


data$Number <- NULL

for (i in 1: ncol(data)){
  print(sum(is.na(data[,i])))
}


for (i in 1: ncol(data)){
  print(c(colnames(data)[i],sum((data[,i] == ""))/nrow(data)*100))
}

Mode <- function(u){
  tmp <- unique(u)
  tmp[which.max(tabulate(match(u,tmp)))]
}
# Le mode de la colonne académique
Mode(data$Academic)


for (i in 1: ncol(data)){
  data[data[,i] == "",i] <- Mode(data[,i])
}

for (i in 1: ncol(data)){
  print(c(colnames(data)[i],sum((data[,i] == ""))/nrow(data)*100))
}

# Etude et visualisation des statistiques des données :


library(ggplot2)

Cat_plot <- function(y,title){
  
  ggplot(data = data, aes(x=reorder(y, -table(y)[y]))) +
    geom_bar(fill="#31708f") +
    geom_text(stat='count', aes(label=..count..), vjust=1,colour="white") +
    ggtitle(title) +
    xlab("") +
    ylab("Nombre d'étudiant")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}


## La variable Age : 
#En regardant le graphe ci-dessous, il est clair que la majorité des personnes interrogées appartenaient au groupe d'âge 21-25 ans. Il y avait 6 personnes de moins de 18 ans et 4 de plus de 36 ans.


Cat_plot(data$Age,"La répartiton de l'age dans cette étude")

## La variable Gender : 

#Il y avait une répartition presque égale entre les deux sexes dans l'enquête, 63 femmes et 62 hommes.
Cat_plot(data$Gender,"La répartiton du sexe dans cette étude")

## La variable Nationality 

#La majorité (77) des étudiants interrogés étaient chinois, suivis par l'Arabie saoudite et la Corée à 6. La majorité des pays avaient 2 ou 1 étudiant interrogé.


Cat_plot(data$Nationality,"La répartiton de la nationalité dans cette étude")


## La variable Program : 

#La majorité des répondants au sondage étaient dans le programme PM, suivi du programme FC à 31 étudiants. Le programme de langue n'avait qu'un seul étudiant interrogé.


Cat_plot(data$Program,"La répartiton du Program dans cette étude")

## La variable Course : 

#La plupart des étudiants interrogés suivaient le cours du Business  (80), suivis de ceux du cours de Law/Legal Studies (14). Le cours sur Media and Communications a le plus petit nombre d'étudiants interrogés à 3.

Cat_plot(data$Course,"La répartiton des Cours dans cette étude")


## La variable English 

#Seuls 15 étudiants interrogés avaient des notes supérieures à 70%, la majorité des étudiants se situant entre 60 et 70%. Il y avait  une seule personne qui était inférieure à 40 %.


Cat_plot(data$English,"La répartiton du English  dans cette étude")

## La variable Academic : 

#Les notes des cours académiques suivent un modèle similaire à ce que nous avons vu avec les notes des cours d'anglais, la majorité des étudiants interrogés se situent entre 60 et 70%, 13 se situant entre 70% et plus et2 avaient des notes inférieures à 40%.

#Cat_plot(data$Academic,"La répartiton du Academic dans cette étude")


## La variable Attendance :

#La majorité des étudiants interrogés appartenaient au groupe S0 (63), cela signifie que la plupart des étudiants avaient le plus de présence.

Cat_plot(data$Attendance,"La répartiton de l'Attendance dans cette étude")

## La variable question 6 : 

#50 étudiants interrogés sont en désaccord avec l'affirmation selon laquelle leur vie est sans but, sans but précis, c'est la majorité. 23 étudiants interrogés étaient d'accord avec la déclaration, tandis que 27 étudiants n'avaient aucun sentiment dans les deux sens.

Cat_plot(data$X6,"La répartiton des réponse sur la question 6 dans cette étude")

## La variable question 7 :

#40 étudiants interrogés ont déclaré qu'ils avaient du mal à organiser les choses qu'ils ont à faire et 35 ont dit qu'ils n'avaient pas de mal. Un total de 14 étudiants ont des sentiments forts dans les deux sens et c'est une répartition égale dans les deux sens.

Cat_plot(data$X7,"La répartiton des réponse sur la question 7 dans cette étude")

## La variable question 8 : 

#61 étudiants interrogés ont déclaré qu'une fois qu'ils ont commencé quelque chose, ils le finiront. Au total, 28 élèves ont déclaré qu'ils étaient soit en désaccord soit fortement en désaccord quant à la fin des activités qu'ils ont commencées.

Cat_plot(data$X8,"La répartiton des réponse sur la question 8 dans cette étude")

## La variable question 9 : 

#44 étudiants interrogés étaient d'accord ou fortement d'accord qu'ils ont parfois l'impression que les choses qu'ils doivent faire pendant la journée n'ont tout simplement pas d'importance et 26 étudiants étaient en désaccord ou fortement en désaccord. La majorité des étudiants (48) n'ont répondu ni l'un ni l'autre.

Cat_plot(data$X9,"La répartiton des réponse sur la question 9 dans cette étude")

## La variable question 10 : 

#Au total, 57 élèves ont déclaré être fortement d'accord ou d'accord avec l'énoncé selon lequel ils planifient leurs activités. 34 élèves n'ont répondu ni l'un ni l'autre.

Cat_plot(data$X10,"La répartiton des réponse sur la question 10 dans cette étude")
## La variable question 11 :

#La plupart des étudiants (38) ont déclaré qu'ils n'étaient pas d'accord avec l'affirmation selon laquelle ils avaient laissé les choses à la dernière minute. 31 ont dit qu'ils étaient d'accord pour laisser les choses à la dernière minute.

Cat_plot(data$X11,"La répartiton des réponse sur la question 11 dans cette étude")

## La variable question 12 : 

#52 étudiants ont déclaré qu'ils n'avaient pas tendance à passer sans but précis d'une activité à l'autre au cours de la journée, c'est le nombre le plus élevé parmi tous les choix possibles. Plus d'élèves étaient d'accord/tout à fait d'accord qu'en désaccord/pas du tout d'accord avec l'énoncé selon lequel ils ont tendance à passer sans but précis d'une activité à l'autre au cours de la journée.

Cat_plot(data$X12,"La répartiton des réponse sur la question 12 dans cette étude")

## La variable question 13 : 

#Plus d'étudiants (42 + 39 = 81) ont dit qu'ils étaient en désaccord/pas du tout d'accord que d'accord/tout à fait d'accord (13 + 5 = 18) lorsqu'il s'agissait d'abandonner les choses qu'ils avaient prévu de faire simplement parce que leur ami avait dit non.


Cat_plot(data$X13,"La répartiton des réponse sur la question 13 dans cette étude")

## La variable question 14 : 

#47 étudiants étaient d'accord/tout à fait d'accord qu'ils avaient fait assez de leur temps, et c'était la majorité des étudiants interrogés. 41 étudiants ont fait le chemin inverse avec leurs réponses et 37 n'ont dit ni l'un ni l'autre.

Cat_plot(data$X14,"La répartiton des réponse sur la question 14 dans cette étude")

## La variable question 15 : 

#La réponse la plus élevée à la question si les étudiants s'ennuient facilement avec leurs activités quotidiennes d'aujourd'hui était la réponse Neither avec 39 étudiants et est suivie par ceux qui étaient d'accord (36) qu'ils s'ennuient facilement avec leurs activités quotidiennes.


Cat_plot(data$X15,"La répartiton des réponse sur la question 15 dans cette étude")

## La variable question 16 :

#47 étudiants ont déclaré que leurs intérêts/activités importants dans leur vie avaient tendance à changer fréquemment. Cette réponse comptait le plus grand nombre d'étudiants et était suivie par ceux qui n'étaient pas d'accord (31).

Cat_plot(data$X16,"La répartiton des réponse sur la question 16 dans cette étude")

## La variable question 17 : 

#Plus d'élèves étaient d'accord (45) que pas d'accord (29) avec l'énoncé selon lequel ils savent combien de temps ils consacrent aux devoirs qu'ils font.

Cat_plot(data$X17,"La répartiton des réponse sur la question 17 dans cette étude")

# Préparation à l'AFCm

library(FactoMineR)
library(factoextra)


data$Nationality <- NULL

data$English <- paste("English",data$English,sep ="_")
data$Academic <- paste("Academic",data$Academic,sep ="_")
data$X6 <- paste("X6",data$X6,sep = "_")
data$X7 <- paste("X7",data$X7,sep ="_")
data$X8 <- paste("X8",data$X8,sep ="_")
data$X9 <- paste("X9",data$X9,sep ="_")
data$X10 <- paste("X10",data$X10,sep ="_")
data$X11 <- paste("X11",data$X11,sep ="_")
data$X12 <- paste("X12",data$X12,sep ="_")
data$X13 <- paste("X13",data$X13,sep ="_")
data$X14 <- paste("X14",data$X14,sep ="_")
data$X15 <- paste("X15",data$X15,sep ="_")
data$X16 <- paste("X16",data$X16,sep ="_")
data$X17 <- paste("X17",data$X17,sep ="_")

J <- 0

for(i in 1: ncol(data)){
  data[,i] <- as.factor(data[,i])
  J <- J + length(levels(data[,i]))
}

print(c("Nombre total de modaliés : " ,J))


tdc <- tab.disjonctif(data)
df <- as.data.frame(tdc)

n <- nrow(df)

i <- 1

while( i <= ncol(df) ){
  
  ns <- sum(df[,i])
  
  
  if( ns < 5  ){
    df[,i] <- NULL
    i <- 1
    next
  }
  
  i <- i + 1
}

print(ncol(df))


# AFCM sur le questionnaire : 

acm <- CA(df,graph = F)

## valeurs propres :


fviz_screeplot(acm, addlabels = TRUE)
acm$eig[,3]

## le biplot individus-variables :

fviz_ca_biplot(acm,repel = TRUE,ggtheme = theme_minimal())

## Etude du tableau des contributions: 

fviz_contrib(acm, choice = "col", axes = 1, top = 10)
fviz_contrib(acm, choice = "row", axes = 1, top = 10)


contribCol <- as.data.frame(acm$col$contrib)
coordCol <- as.data.frame(acm$col$coord)

contribRow <- as.data.frame(acm$row$contrib)
coordRow <- as.data.frame(acm$row$coord)

print("######### Les variables qui contribuent le plus à la construction du premier axe ######### ")

for(i in 1:ncol(df)){
  
  ns <- sum(df[,i])
  fs <- ns /n
  
  if(contribCol$`Dim 1`[i] >= fs*100 && coordCol$`Dim 1`[i] > 0){
    print(c(colnames(df)[i],"+"))
  }
  if(contribCol$`Dim 1`[i] >= 100*fs && coordCol$`Dim 1`[i] < 0){
    print(c(colnames(df)[i],"-"))
  }
}

print("######### Les individus qui contribuent le plus à la construction du premier axe ######### ")


for(i in 1:nrow(df)){
  
  
  if(contribRow$`Dim 1`[i] > 100/n && coordRow$`Dim 1`[i] > 0){
    print(c(i,"+"))
  }
  if(contribRow$`Dim 1`[i] > 100/n && coordRow$`Dim 1`[i] < 0){
    print(c(i,"-"))
  }
}


fviz_contrib(acm, choice = "col", axes = 2, top = 10)
fviz_contrib(acm, choice = "row", axes = 2, top = 10)

print("######### Les variables qui contribuent le plus à la construction du 2ème axe ######### ")

for(i in 1:ncol(df)){
  
  ns <- sum(df[,i])
  fs <- ns /n
  
  if(contribCol$`Dim 2`[i] > fs*100 && coordCol$`Dim 2`[i] > 0){
    print(c(colnames(df)[i],"+"))
  }
  if(contribCol$`Dim 2`[i] > fs*100 && coordCol$`Dim 2`[i] < 0){
    print(c(colnames(df)[i],"-"))
  }
  
}

print("######### Les individus qui contribuent le plus à la construction du 2ème axe ######### ")


for(i in 1:nrow(df)){
  
  
  if(contribRow$`Dim 2`[i] > 100/n && coordRow$`Dim 2`[i] > 0){
    print(c(i,"+"))
  }
  if(contribRow$`Dim 2`[i] > 100/n && coordRow$`Dim 2`[i] < 0){
    print(c(i,"-"))
  }
}

## Les fortes association qu'on peut ressortir de l'AFCM :


col.sum <- apply(df, 2, sum)
row.avg <- apply(df, 1, sum)/sum(df)

k <- as.integer(ncol(df)/2)

for( i in 1:k){
  for(j in (k+1):ncol(df)){
    
    col1 <- df[, i]
    col2 <- df[, j]
    d2 <- sum(((col1/col.sum[i] - col2/col.sum[j])^2) / row.avg)
    
    if( d2 < 2 ){
      print(c(colnames(df)[i],colnames(df)[j]))
    }
  }
}

# On remarque que : 
#   
#   - Ceux qui sont d'accord avec la question 10 [ ils sont d'accord à planifier leurs activités] suivent le programme PM et ils ont une assiduité S0 [ toujours présents].
# - Ceux qui sont d'accord avec la question 17 [ ils savent combien de temps qu'ils prennent pour faire leurs devoirs maisons] suivent le cours  Business.
# - Ceux qui ne sont ni d'accord ni en désaccord  avec la question 9 [ ils ont parfois l'impression que les choses qu'ils doivent faire pendant la journée n'ont tout simplement pas d'importance] suivent le cours  Business et ils ont une trés bonne assiduité.
 
## Les questions les mieux représentées par l'AFCM :
  
cosCol <- as.data.frame(acm$col$cos2)

print("######### Les variables les mieux représentées par le  premier plan ######### ")

for(i in 1:ncol(df)){
  
  if(cosCol$`Dim 1`[i] + cosCol$`Dim 2`[i] > 0.3 ){
    print(rownames(cosCol)[i])
  }
}

fviz_cos2(acm, choice = "col", axes = 1:2,top=10)

# AFC sur deux questions : 
## Croiser 2 questions : 

col1 <- -1
col2 <- -1
tc_afc <- NULL
p_value <- 1

for( i in 1:ncol(data)){
  for(j in 1:ncol(data)){
    if( i != j){
      tc <- table(data[,i],data[,j])
      a <- chisq.test(tc)
      
      if( a$p.value < p_value){
        p_value <- a$p.value
        col1 <- i
        col2 <- j
        tc_afc <- tc
      }
    }
  }
}

print(colnames(data)[col1])
print(colnames(data)[col2])
print(p_value)
tc_afc

afc <- CA(tc_afc,graph = F)

## valeurs propres :


fviz_screeplot(afc, addlabels = TRUE)

fviz_ca_biplot(afc, repel = TRUE)

## Etude du tableau des contributions: 


contribCol <- as.data.frame(afc$col$contrib)
coordCol <- as.data.frame(afc$col$coord)

contribRow <- as.data.frame(afc$row$contrib)
coordRow <- as.data.frame(afc$row$coord)

print("######### Les profile colonnes qui contribuent le plus à la construction du premier axe ######### ")

for(i in 1:ncol(tc_afc)){
  
      ns <- sum(tc_afc[,i])
      fs <- ns /n
      
      if(contribCol$`Dim 1`[i] >= fs*100 && coordCol$`Dim 1`[i] > 0){
        print(c(rownames(contribCol)[i],"+"))
      }
      if(contribCol$`Dim 1`[i] >= fs*100 && coordCol$`Dim 1`[i] < 0){
        print(c(rownames(contribCol)[i],"-"))
      }
}

print("######### Les profiles lignes qui contribuent le plus à la construction du premier axe ######### ")

      
for(i in 1:nrow(tc_afc)){
  
      ns <- sum(tc_afc[i,])
      fs <- ns /n

      if(contribRow$`Dim 1`[i] > fs*100 && coordRow$`Dim 1`[i] > 0){
        print(c(rownames(contribRow)[i],"+"))
      }
      if(contribRow$`Dim 1`[i] > fs*100 && coordRow$`Dim 1`[i] < 0){
        print(c(rownames(contribRow)[i],"-"))
      }
}

# Contributions of rows to dimension 1
fviz_contrib(afc, choice = "row", axes = 2, top = 10)
# Contributions of cols to dimension 1
fviz_contrib(afc, choice = "col", axes = 2, top = 10)

print("######### Les profile colonnes qui contribuent le plus à la construction du 2ème axe ######### ")

for(i in 1:ncol(tc_afc)){
  
      ns <- sum(tc_afc[,i])
      fs <- ns /n
      
      if(contribCol$`Dim 2`[i] >= fs*100 && coordCol$`Dim 2`[i] > 0){
        print(c(rownames(contribCol)[i],"+"))
      }
      if(contribCol$`Dim 2`[i] >= fs*100 && coordCol$`Dim 2`[i] < 0){
        print(c(colnames(contribCol)[i],"-"))
      }
}

print("######### Les profiles lignes qui contribuent le plus à la construction du 2ème axe ######### ")

      
for(i in 1:nrow(tc_afc)){
  
      ns <- sum(tc_afc[i,])
      fs <- ns /n

      if(contribRow$`Dim 2`[i] > fs*100 && coordRow$`Dim 2`[i] > 0){
        print(c(rownames(contribRow)[i],"+"))
      }
      if(contribRow$`Dim 2`[i] > fs*100 && coordRow$`Dim 2`[i] < 0){
        print(c(rownames(contribRow)[i],"-"))
      }
}


# Conclusion : 
# 
# D’après les résultats obtenus par l'AFCM on conclut que :
# 
# - Le type du programme suivi ainsi que l'assiduité ont un effet sur la gestion du temps des étudiants.
# - Ceux qui suivent le cours Business savent combien de temps qu'ils prennent pour faire leurs devoirs maisons .
# 
# D’après les résultats obtenus par l'AFC on conclut que :
# 
# - Les étudiants qui ont plus de 36 ans suivent le programme language.
# - Les étudiants moins de 20 ans sont soit dans le programme FC soit dans le programme IYO.
 
 
 