# Guide d'utilisation - Formulaire de soumission de recettes

## Vue d'ensemble

Le site web de recettes dispose maintenant d'un formulaire de soumission qui permet aux utilisateurs de proposer de nouvelles recettes directement via l'interface web. Ce formulaire génère automatiquement un fichier YAML et crée une pull request sur GitHub.

## Accès au formulaire

Le formulaire est accessible via le menu de navigation principal du site sous "Soumettre une recette" ou directement à l'URL :
`/soumettre-recette.html`

## Comment soumettre une recette

### 1. Informations de base

Remplissez les champs obligatoires :
- **Nom complet** : Le nom complet de la recette (ex: "Poulet au beurre à la mijoteuse")
- **Nom court** : Version abrégée pour la table des matières (ex: "Poulet beurre mijoteuse")
- **Source** : D'où vient la recette (ex: "Ricardo Cuisine", "Grand-mère", etc.)
- **Nombre de portions** : Combien de portions la recette produit
- **Se congèle** : Indiquer si la recette peut être congelée
- **Catégorie principale** : Choisir entre Accompagnements, Repas ou Desserts
- **Tags/Catégories** : Ajouter des mots-clés séparés par des virgules (ex: "Poulet, Mijoteuse, Rapide")

### 2. Temps de préparation

Indiquez les temps en minutes :
- **Préparation** : Temps de préparation des ingrédients
- **Cuisson** : Temps de cuisson
- **Réfrigération** : Temps de réfrigération (laisser vide si non applicable)

### 3. Préparation

Le formulaire permet d'organiser la recette en sections avec plusieurs étapes :

1. **Cliquez sur "+ Ajouter une section"** pour créer une nouvelle section (ex: "Pâte", "Garniture", "Préparation")
2. Pour chaque section, **cliquez sur "+ Ajouter une étape"** pour décrire une étape de la recette
3. Pour chaque étape, vous pouvez :
   - Décrire l'étape en détail
   - Ajouter des **ingrédients** avec quantités et unités (utilisez le menu déroulant pour les unités courantes)
   - Ajouter l'**équipement** nécessaire

### 4. Commentaires

Ajoutez des notes ou conseils optionnels sur la recette.

### 5. Informations sur le soumetteur

Indiquez votre nom pour que nous sachions qui a partagé cette recette. Cela nous permet de vous remercier!

### 6. Soumission

Cliquez sur **"Soumettre la recette"**. Le formulaire va :
1. Valider les données
2. Générer un fichier YAML
3. Créer une nouvelle branche sur GitHub
4. Créer un fichier dans le dossier approprié
5. Créer une pull request

Une fois la pull request créée, un lien vous sera fourni pour la consulter.

## Que se passe-t-il après la soumission ?

1. Une **pull request** est créée sur le dépôt GitHub
2. Le workflow GitHub Actions **génère automatiquement** le fichier `.qmd` correspondant
3. Les mainteneurs du site **révisent** la recette
4. Si approuvée, la recette est **fusionnée** et apparaît sur le site web

## Sécurité

Le formulaire utilise un token GitHub configuré par les administrateurs du site. Vous n'avez pas besoin de compte GitHub pour soumettre une recette.

**Note pour les administrateurs** : Consultez le fichier `TOKEN_SETUP.md` pour la configuration du token.

## Support technique

En cas de problème :
1. Vérifiez que tous les champs obligatoires sont remplis
2. Vérifiez les messages d'erreur affichés dans le formulaire
3. Contactez les administrateurs du site si le problème persiste

**Pour les administrateurs** : Consultez `README_CONFIG.md` et `TOKEN_SETUP.md` pour la configuration du token.

## Structure technique

Le formulaire génère un fichier YAML conforme au template existant avec les champs suivants :
- `nom`, `nom_court`, `source`
- `portions`, `se_congele`
- `categories[]`
- `temps` (preparation, cuisson, refrigeration)
- `preparation[]` avec sections et étapes
- `commentaires[]`

Le fichier est créé dans : `recettes/{categorie}/{nom-de-la-recette}.yaml`
