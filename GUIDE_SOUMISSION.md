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

### 5. Informations GitHub

Pour créer la pull request, vous devez fournir :

- **Token GitHub Personnel (PAT)** : Un token d'accès GitHub
  - Créez-le sur [github.com/settings/tokens](https://github.com/settings/tokens/new?scopes=public_repo&description=Recipe%20Submission)
  - Permissions nécessaires : `public_repo`
  - ⚠️ **Important** : Pour des raisons de sécurité, révoquez ce token après utilisation
- **Nom d'utilisateur GitHub** : Votre nom d'utilisateur GitHub

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

- Le token GitHub n'est **jamais sauvegardé** et est utilisé uniquement pour cette soumission
- Utilisez un token avec des **permissions limitées** (`public_repo` uniquement)
- **Révoquez le token** après utilisation pour plus de sécurité
- Toutes les données sont **validées et échappées** avant la génération du YAML

## Support technique

En cas de problème :
1. Vérifiez que votre token GitHub a les bonnes permissions
2. Assurez-vous que tous les champs obligatoires sont remplis
3. Vérifiez les messages d'erreur affichés dans le formulaire
4. Contactez les mainteneurs du dépôt si le problème persiste

## Structure technique

Le formulaire génère un fichier YAML conforme au template existant avec les champs suivants :
- `nom`, `nom_court`, `source`
- `portions`, `se_congele`
- `categories[]`
- `temps` (preparation, cuisson, refrigeration)
- `preparation[]` avec sections et étapes
- `commentaires[]`

Le fichier est créé dans : `recettes/{categorie}/{nom-de-la-recette}.yaml`
