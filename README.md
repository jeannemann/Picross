# Picross
Ce projet consiste en la création d'un jeu de Picross interactif utilisant le framework Shiny de R. Le code sera mis sous forme de package R. 
Le Picross, également connu sous le nom de Nonogram, est un jeu de puzzle où le joueur doit colorier certaines cases d'une grille selon les indices fournis sur les bords de la grille. Les indices indiquent le nombre de cases à colorier dans chaque ligne et colonne. Le but du jeu est de colorier les bonnes cases pour révéler un dessin caché.

## Fonctionnalités
- [x] Sélection de la taille de la grille : Permet au joueur de choisir parmi différentes tailles de grille pour jouer.
- [x] Choix de la difficulté : Le joueur peut sélectionner la difficulté du puzzle parmi plusieurs niveaux.
- [x] Affichage des règles du Picross : Explique les règles du jeu pour guider les joueurs.
- [x] Grille interactive : Le joueur peut remplir les cases de la grille en cliquant dessus pour résoudre le puzzle. Puis les effacer en recliquant dessus.
- [x] Vérification de la solution : Un bouton "Check" permet au joueur de vérifier si sa solution est correcte.

## Installation
Pour installer le package Picross, exécutez les commandes suivantes dans R :
```R
install.packages("devtools")
devtools::install_github("jeannemann/Picross")
Picross::play_MJ()
```

## Contributeurs
- Mario LAPI
- Jeanne MANNEQUIN
