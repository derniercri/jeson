# LDC-Json

## Module json


### Fonction
- gen_decoder(Type_list, Field_name, Record_name) :
    Génère une fonction permettant de convertir une chaîne json en un record erlang.
	La structure du record doit être spécifiée pour générer la fonction :
	
    - Type_list est la liste des types de chaque champs du record. (voir plus bas)
    - Field_name est la liste des noms des champs du record. Chaque élément de la liste des noms doit correspondre à un type dans Type_list
	- Record_name est le nom du record utilisé pour stocker les données extraites de la chaine json

- gen_encoder(Type_list, Field_name) :
    Génère une fonction permetant de convertir un record erlang en une chaine json.
	La structure du record doit être spécifiée pour générer la fonction :
	
    - Type_list est la liste des types de chaque champ du record. (voir plus bas)
    - Field_name est la liste des noms des champs du record. Chaque élément de la liste des nom doit correspondre à un type dans la liste Type_list

### Spécification de type
Les types de chaque champs du record doivent être spécifiés pour pouvoir traiter correctement la chaine json. Chaque type est une valeur erlang.

- int : un nombre entier
- float : un nombre flottant
- atom : une valeur atomique (booléens, etc ...)
- string : une chaîne de caractère
  Attention : Les guillemets délimitant les chaînes de caractère doivent être échappées par une contre oblique. Les guillemets à l'intérieur d'une chaîne de caractère doivent être échappées par deux contre oblique

- {object, F} : permet de décrire le type d'un autre objet json
  Dans le cas de la fonction gen_decoder, F doit être une fonction permettant de transformer l'objet du champ correspondant en une valeur erlang  
  Dans le cas de la fonction gen_encoder, F doit être une fonction permettant de transformer le record du champ correspondant en une chaîne de caractère.

- {pure_list, Type} : une liste ne contenant qu'un seul type. Celui-ci est spécifié dans Type(il peut prendre tout les types décris dans cette section)
- {impure_list, [Type1, Type2, ...]} : une liste contenant plusieurs types, les types  de chaque élément doivent être indiqués dans le deuxième élément du couple.

### Exemples

- convertir la chaîne "{"c1":[1,2,3], "c2":["toto", 1], "c3":{"c1":123}}" en record erlang :
Il faut d'abord générer la fonction permettant de convertir l'objet c3 en record :

		F = json:gen_decoder([int], ["c1"], record1).

	Puis il faut générer la fonction permettant de convertir la chaîne :

		F2 = json:gen_decoder([{pure_list, int}, {impure_list, [string, int]},{object, F}],
		["c1","c2","c3"],
		record2).

	La fonction F2 peut maintenant être utilisée pour convertir la chaîne précédente (stocké dans S) :

		F2(S).

	Résultat :

		{record2, [1,2,3], ["toto", 1], {record1, 123}}

- convertir le record précédent en chaîne json
  On utilisera les mêmes valeurs pour convertir un record erlang en json.
  Il faut aussi générer la fonction permettant de convertir le record c3 en chaîne json
  
		F = json:gen_encoder([int], ["c1"]).
  
	  On génère ensuite la fonction général :
  
		F2 = json:gen_decoder([{pure_list, int}, {impure_list, [string, int]},{object, F}],
		["c1","c2","c3"]).
