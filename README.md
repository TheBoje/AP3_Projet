# Algorithme et Programmation 3 </br> Compte rendu de projet

M.Laurent Fuchs</br>
Groupe 1, Yann Berthelot, Louis Leenart & Alexis Louail

## Réponse à la section 1.4

### Expérimentation sur les ARB

Dans notre projet, les ABR sont créés à partir d’une liste générée de manière différente selon si l’on veut un arbre construit à partir de valeur au hasard où si nous voulons un arbre contenant des sous-suites ordonnées mais en ne prenant toujours qu’un argument ; la taille de la liste voulue. Cela facilite grandement la fiabilité des expérimentations en permettant de pratiquer sur des échantillons semblables.

Afin d’estimer une moyenne des déséquilibres entre les arbres nous avons construit et utilisé plusieurs fonctions : une fonction `unbalance(tree)` qui calcule le déséquilibre d’un arbre, une fonction `unbalance_avg(tSample, treesSize)` qui calcule la moyenne des déséquilibre d’un échantillon d’arbres générés par la fonction (`tSample` étant la taille de l’échantillon et `treesSize` la taille des arbres à créer et évaluer) et enfin une fonction `unbalance_avgs_avg(avgSample, treeSample, treesSize)` qui calcule une moyenne des moyennes des déséquilibres (`avgSample` étant le nombre de moyennes à évaluer).

Cette organisation permet lancer peu fois la dernière fonction tout en évaluant un grand nombre d’arbres, nous avons décidé de lancer l’expérimentation 10 fois sur 1000 moyennes de déséquilibre entre 100 arbres de taille 100 afin d’avoir une expérimentation
