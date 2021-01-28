# Algorithme et Programmation 3 </br> Compte rendu de projet

M.Laurent Fuchs</br>
Groupe 1, Yann Berthelot, Louis Leenart & Alexis Louail
## Partie 1 : Arbres binaires de recherche
### Question 1
// TODO
### Question 2
// TODO
### Question 3
// TODO
### Question 4
Dans notre projet, les ABR sont créés à partir d’une liste générée de manière différente selon si l’on veut un arbre construit à partir de valeurs au hasard où si nous voulons un arbre contenant des sous-suites ordonnées mais en ne prenant toujours qu’un argument ; la taille de la liste voulue. Cela facilite grandement la fiabilité des expérimentations en permettant de pratiquer sur des échantillons semblables.

Afin d’estimer une moyenne des déséquilibres entre les arbres nous avons construit et utilisé plusieurs fonctions : une fonction `unbalance(tree)` qui calcule le déséquilibre d’un arbre, une fonction `unbalance_avg(tSample, treesSize)` qui calcule la moyenne des déséquilibre d’un échantillon d’arbres générés par la fonction (`tSample` étant la taille de l’échantillon et `treesSize` la taille des arbres à créer et évaluer) et enfin une fonction `unbalance_avgs_avg(avgSample, treeSample, treesSize)` qui calcule une moyenne des moyennes des déséquilibres (`avgSample` étant le nombre de moyennes à évaluer).

Cette organisation permet de lancer peu de fois la dernière fonction tout en évaluant un grand nombre d’arbres, nous avons décidé de lancer l’expérimentation 10 fois sur 1000 moyennes de déséquilibre entre 100 arbres de taille 100 afin d’avoir une expérimentation produisant des résultats fiables sur des échantillons similaires (changer les paramètres pourrait fausser les résultats).

|	Expérimentation	|	Résultat pour les arbres au hasard	|	Résultat pour les arbres aves sous-suites	|
| :-------------------------:	|:------------------------------------------:	| :------------------------------------------------:	|
|		1		|		    -0.01604			|			24.45545			|
|		2		|		    -0.00133			|			24.41074			|
|		3		|		     0.00472			|			24.51547			|
|		4		|		    -0.00033			|			24.45549			|
|		5		|		     0.01846			|			24.45782			|
|		6		|		     0.00623			|			24.44097			|
|		7		|		     0.01629			|			24.50553			|
|		8		|		    -0.00629			|			24.47762			|
|		9		|		     0.04008			|			24.55913			|
|	       10		|		     0.02023			|			24.51649			|
|	     Moyenne		|		     0.00820			|			24.47947			|

La complexité des fonctions générant les résultats ci-dessus est en O(n)

On peut clairement conclure que les abr sont bien plus équilibrés lorsqu'ils sont construits à partir de valeurs au hasard.


## Arbres AVL
### Implantation d'un module `Avl`
#### Question 1
Notre implantation du type `Avl` reprend la structure `t_btree` via la déclaration `type 'a t_avltree = 'a bst`. Nous avons aussi implémenté les opérations suivante : 
- `rd(avl : 'a t_avltree) : 'a t_avltree` effectue la `Rotation Droite` de l'AVL. Pour ne pas rencontrer d'erreur, il faut que `avl` et `avl.lson` existent.
- `rg(avl : 'a t_avltree) : 'a t_avltree` effectue la `Rotation Gauche` de l'AVL
- ``
#### Question 2
// TODO
#### Question 3
// TODO
#### Question 4
// TODO
### Expérimentations avec les arbres AVL
#### Question 1
// TODO
#### Question 2
// TODO