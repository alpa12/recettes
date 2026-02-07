# Configuration du token GitHub pour le formulaire de soumission

## Vue d'ensemble

Le formulaire de soumission de recettes (`soumettre-recette.html`) utilise l'API GitHub pour créer automatiquement des pull requests. Pour fonctionner, il a besoin d'un token d'accès GitHub (Personal Access Token ou PAT).

## ⚠️ Note de sécurité importante

**Le token sera visible dans le code source de la page web** puisque le formulaire fonctionne entièrement côté client (pas de serveur backend). Cela signifie que **n'importe qui peut voir votre token** en consultant le code source de la page.

### Recommandations de sécurité:

1. **Utilisez UNIQUEMENT les permissions minimales requises** (voir ci-dessous)
2. **Surveillez l'activité de votre compte GitHub** pour détecter toute utilisation abusive
3. **Révoquez et recréez le token régulièrement** (ex: tous les 3-6 mois)
4. **Considérez les alternatives** pour un environnement de production (voir section ci-dessous)

## 📝 Étapes pour créer votre token GitHub

### 1. Accéder aux paramètres de tokens

Allez sur: https://github.com/settings/tokens/new

Ou manuellement:
1. Connectez-vous à GitHub
2. Cliquez sur votre photo de profil (coin supérieur droit)
3. Sélectionnez **Settings**
4. Dans le menu de gauche, tout en bas, cliquez sur **Developer settings**
5. Cliquez sur **Personal access tokens** → **Tokens (classic)**
6. Cliquez sur **Generate new token** → **Generate new token (classic)**

### 2. Configurer le token

Remplissez les informations suivantes:

#### Note (description)
Donnez un nom descriptif pour vous rappeler à quoi sert ce token:
```
Recipe Submission Form - Recettes Website
```

#### Expiration
Choisissez une durée d'expiration raisonnable:
- **Recommandé**: 90 jours (vous devrez le renouveler tous les 3 mois)
- Alternative: 1 an (mais n'oubliez pas de le renouveler!)

#### Permissions (Scopes)

**✅ Cochez UNIQUEMENT cette permission:**

```
☑️ public_repo
   Access public repositories
```

**OU si votre dépôt est privé:**

```
☑️ repo
   Full control of private repositories
```

**❌ NE cochez PAS:**
- ❌ workflow
- ❌ write:packages
- ❌ delete_repo
- ❌ admin:org
- ❌ Toute autre permission

#### Pourquoi seulement `public_repo` ou `repo`?

Le formulaire a besoin de:
- Lire la branche `dev` (pour obtenir le dernier commit)
- Créer une nouvelle branche
- Créer/modifier des fichiers dans le dépôt
- Créer une pull request

La permission `public_repo` (ou `repo` pour un dépôt privé) permet exactement cela, rien de plus.

### 3. Générer et copier le token

1. Cliquez sur **Generate token** en bas de la page
2. **IMPORTANT**: Copiez immédiatement le token qui s'affiche
   - Il commence généralement par `ghp_`
   - Vous ne pourrez plus le voir après avoir quitté cette page!
3. Conservez-le temporairement dans un endroit sûr (gestionnaire de mots de passe recommandé)

### 4. Configurer le fichier config.js

1. Copiez le fichier exemple:
   ```bash
   cp config.js.example config.js
   ```

2. Ouvrez `config.js` et remplacez `'VOTRE_TOKEN_ICI'` par votre token:
   ```javascript
   const GITHUB_CONFIG = {
       token: 'ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',  // ← Collez votre token ici
       owner: 'alpa12',
       repo: 'recettes',
       baseBranch: 'dev'
   };
   ```

3. **NE COMMITEZ PAS** ce fichier! Le `.gitignore` est configuré pour l'exclure automatiquement.

### 5. Déployer

Une fois configuré:
1. Le fichier `config.js` doit être présent sur votre serveur web
2. Le formulaire `soumettre-recette.html` chargera automatiquement la configuration
3. Les utilisateurs pourront soumettre des recettes sans avoir besoin de token

## 🔄 Renouvellement du token

Quand votre token expire:
1. Créez un nouveau token (suivez les mêmes étapes)
2. Mettez à jour `config.js` avec le nouveau token
3. Redéployez le fichier

## 🛡️ Alternatives plus sécurisées (pour production)

Si vous voulez une solution plus robuste et sécurisée:

### Option 1: GitHub App
Créez une GitHub App au lieu d'utiliser un token personnel. Les GitHub Apps ont:
- Des permissions plus granulaires
- Une meilleure traçabilité
- Pas besoin d'être liées à un compte utilisateur personnel

### Option 2: Backend avec API
Implémentez un backend simple (ex: Netlify Functions, Vercel Functions, ou simple Express.js):
- Le token est stocké côté serveur (variable d'environnement)
- Le formulaire envoie les données au backend
- Le backend crée la pull request
- Le token n'est jamais exposé au client

### Option 3: OAuth Flow
Implémentez un flux OAuth GitHub:
- Les utilisateurs s'authentifient avec leur propre compte GitHub
- Utilise leurs propres permissions
- Plus complexe à implémenter mais plus sécurisé

## 🔍 Surveillance et audit

### Vérifier l'utilisation du token

1. Allez sur https://github.com/settings/tokens
2. Cliquez sur votre token
3. Consultez la section "Last used" pour voir quand il a été utilisé

### Révoquer un token compromis

Si vous pensez que votre token a été compromis:
1. Allez sur https://github.com/settings/tokens
2. Cliquez sur **Delete** à côté du token
3. Créez immédiatement un nouveau token

## 📊 Que peut faire quelqu'un avec ce token?

Avec un token ayant uniquement la permission `public_repo`, une personne malveillante pourrait:
- ✅ Créer des branches et pull requests dans vos dépôts publics
- ✅ Lire le contenu de vos dépôts publics
- ✅ Créer des issues et commenter

Mais NE PEUT PAS:
- ❌ Fusionner des pull requests (nécessite des permissions de maintainer)
- ❌ Supprimer des branches protégées
- ❌ Modifier directement les branches protégées
- ❌ Accéder à vos dépôts privés (si vous utilisez `public_repo`)
- ❌ Supprimer le dépôt
- ❌ Modifier les paramètres du dépôt

## ❓ Questions fréquentes

### Q: Dois-je vraiment renouveler le token?
**R**: Oui! C'est une bonne pratique de sécurité. GitHub permet de définir une date d'expiration automatique.

### Q: Que se passe-t-il si le token expire?
**R**: Le formulaire cessera de fonctionner et affichera une erreur. Les utilisateurs ne pourront plus soumettre de recettes jusqu'à ce que vous mettiez à jour le token.

### Q: Puis-je utiliser le même token pour plusieurs sites?
**R**: Techniquement oui, mais ce n'est pas recommandé. Créez un token différent pour chaque usage afin de pouvoir les révoquer indépendamment.

### Q: Le token fonctionne-t-il même si je change mon mot de passe?
**R**: Oui, les tokens sont indépendants de votre mot de passe. Ils restent valides même si vous changez votre mot de passe.

### Q: Puis-je limiter le token à un seul dépôt?
**R**: Malheureusement non avec les tokens classiques. C'est une limitation de GitHub. Vous devez utiliser une GitHub App pour des permissions aussi granulaires.

## 📧 Support

Si vous rencontrez des problèmes:
1. Vérifiez que le token n'a pas expiré
2. Vérifiez que les permissions sont correctes
3. Consultez les logs de votre navigateur (F12 → Console) pour des erreurs spécifiques
4. Créez une issue sur le dépôt GitHub

---

**Dernière mise à jour**: Février 2026
