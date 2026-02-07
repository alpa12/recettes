# Configuration du formulaire de soumission de recettes

## 🎯 Changements apportés

Le formulaire a été modifié pour simplifier l'expérience utilisateur :

**Avant :**
- Les utilisateurs devaient fournir leur propre token GitHub
- Les utilisateurs devaient avoir un compte GitHub
- Processus complexe et peu accessible

**Après :**
- ✅ **Token hardcodé** géré par l'administrateur
- ✅ **Aucun compte GitHub requis** pour les utilisateurs
- ✅ **Champ simple** : "Soumis par (votre nom)"
- ✅ **Processus simplifié** pour tous

## 🔧 Configuration pour l'administrateur

### Étape 1 : Créer un token GitHub

Suivez le guide complet dans **`TOKEN_SETUP.md`** qui explique :
- Comment créer un token avec les permissions minimales
- Les permissions exactes nécessaires (`public_repo`)
- Les bonnes pratiques de sécurité
- Comment surveiller et renouveler le token

**Résumé rapide :**
1. Allez sur https://github.com/settings/tokens/new
2. Nom : "Recipe Submission Form"
3. Expiration : 90 jours (recommandé)
4. Permissions : ✅ `public_repo` uniquement
5. Générez et copiez le token (commence par `ghp_`)

### Étape 2 : Configurer le fichier config.js

1. Copiez le fichier exemple :
   ```bash
   cp config.js.example config.js
   ```

2. Éditez `config.js` et remplacez `'VOTRE_TOKEN_ICI'` :
   ```javascript
   const GITHUB_CONFIG = {
       token: 'ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx', // Votre token ici
       owner: 'alpa12',
       repo: 'recettes',
       baseBranch: 'dev'
   };
   ```

3. **Important** : Le fichier `config.js` est dans `.gitignore` et ne sera jamais commité

### Étape 3 : Déployer

Assurez-vous que `config.js` est présent sur votre serveur web avec le reste des fichiers.

## 📋 Fichiers

- **`config.js.example`** - Template de configuration (commité dans git)
- **`config.js`** - Configuration réelle avec token (ignoré par git)
- **`TOKEN_SETUP.md`** - Guide détaillé pour créer et gérer le token
- **`soumettre-recette.html`** - Formulaire mis à jour
- **`.gitignore`** - Configuré pour exclure `config.js`

## 🔐 Sécurité

### ⚠️ Avertissement important

Le token sera visible dans le code source de la page car le formulaire est 100% client-side (pas de backend). C'est un compromis acceptable pour cette utilisation car :

**Avantages :**
- ✅ Pas de serveur backend à maintenir
- ✅ Hébergement simple (GitHub Pages, serveur statique)
- ✅ Aucun coût supplémentaire
- ✅ Expérience utilisateur simplifiée

**Limitations :**
- ⚠️ Le token peut être vu par n'importe qui
- ⚠️ Quelqu'un pourrait créer des PRs malveillantes

**Mitigations :**
1. **Permissions minimales** : Token avec `public_repo` uniquement
2. **Pas de fusion automatique** : Les PRs doivent être approuvées manuellement
3. **Surveillance** : Surveillez l'activité du token sur GitHub
4. **Renouvellement régulier** : Changez le token tous les 3-6 mois
5. **Protection des branches** : La branche `dev` peut avoir des protections

### Ce que quelqu'un PEUT faire avec ce token :
- Créer des branches dans votre dépôt public
- Créer des pull requests
- Créer des issues
- Lire le contenu public

### Ce que quelqu'un NE PEUT PAS faire :
- ❌ Fusionner des pull requests (nécessite permissions de maintainer)
- ❌ Modifier directement les branches protégées
- ❌ Supprimer le dépôt
- ❌ Modifier les paramètres du dépôt
- ❌ Accéder aux dépôts privés (avec `public_repo`)

## 🔄 Workflow de soumission

1. **Utilisateur** remplit le formulaire avec son nom
2. **JavaScript** valide et génère le YAML
3. **API GitHub** crée une branche `recipe-submission-{nom}-{timestamp}`
4. **API GitHub** ajoute le fichier YAML
5. **API GitHub** crée une PR vers `dev`
   - Titre : "Nouvelle recette: {nom}"
   - Description inclut : "**Soumis par:** {nom de l'utilisateur}"
6. **GitHub Action** génère automatiquement le `.qmd`
7. **Vous** révisez et fusionnez la PR

## 📊 Exemple de PR créée

```markdown
## Nouvelle recette soumise

**Nom:** Tarte aux pommes de grand-mère
**Source:** Grand-mère Marie
**Catégorie:** desserts
**Portions:** 8

**Soumis par:** Marie Tremblay

Recette ajoutée via le formulaire de soumission de recettes.
```

## 🚀 Alternatives plus sécurisées (futur)

Si vous souhaitez améliorer la sécurité à l'avenir :

### Option 1 : Backend simple
- Créez une fonction serverless (Netlify/Vercel Functions)
- Le token reste côté serveur
- Le formulaire envoie les données au backend
- Coût : gratuit jusqu'à un certain volume

### Option 2 : GitHub App
- Créez une GitHub App au lieu d'un token personnel
- Permissions plus granulaires
- Meilleure traçabilité
- Complexité : moyenne

### Option 3 : OAuth
- Les utilisateurs s'authentifient avec GitHub
- Utilisent leurs propres permissions
- Complexité : élevée

## ❓ FAQ

**Q: Que faire si le token expire ?**  
R: Créez un nouveau token (même processus) et mettez à jour `config.js`

**Q: Comment savoir si le token est compromis ?**  
R: Vérifiez les PRs récentes. Si vous voyez des PRs suspectes, révoquez immédiatement le token.

**Q: Puis-je utiliser ce setup en production ?**  
R: Oui, c'est acceptable pour un site de recettes communautaire. Pour une application critique, considérez les alternatives ci-dessus.

**Q: Le token fonctionne-t-il sans serveur ?**  
R: Oui! Le formulaire fonctionne entièrement côté client avec n'importe quel hébergement statique.

## 📞 Support

- **Documentation complète** : Voir `TOKEN_SETUP.md`
- **Guide utilisateur** : Voir `GUIDE_SOUMISSION.md`
- **Issues** : Créez une issue sur GitHub

---

**Dernière mise à jour** : Février 2026
