# 🚀 Quick Start - Configuration du formulaire

## Pour démarrer en 3 minutes

### 1️⃣ Créer le token GitHub (2 minutes)

1. Ouvrez : https://github.com/settings/tokens/new
2. Remplissez :
   - **Note** : `Recipe Submission Form`
   - **Expiration** : `90 days`
   - **Permissions** : Cochez ✅ `public_repo`
3. Cliquez **Generate token**
4. **Copiez le token** (commence par `ghp_`)

### 2️⃣ Configurer le fichier (30 secondes)

```bash
cp config.js.example config.js
# Éditez config.js et collez votre token
```

Dans `config.js` :
```javascript
const GITHUB_CONFIG = {
    token: 'ghp_votre_token_ici',  // ← Collez votre token ici
    owner: 'alpa12',
    repo: 'recettes',
    baseBranch: 'dev'
};
```

### 3️⃣ Déployer (30 secondes)

Uploadez `config.js` sur votre serveur web avec les autres fichiers.

**C'est tout !** 🎉 Le formulaire est prêt à l'emploi.

---

## ⚠️ Important

- `config.js` est dans `.gitignore` - il ne sera jamais commité
- Le token doit être renouvelé tous les 90 jours
- Seule permission nécessaire : `public_repo`

## 📖 Documentation complète

- **Configuration** : Voir `README_CONFIG.md`
- **Token détaillé** : Voir `TOKEN_SETUP.md`
- **Guide utilisateur** : Voir `GUIDE_SOUMISSION.md`

## 🆘 Aide rapide

**Le formulaire ne fonctionne pas ?**
1. Vérifiez que `config.js` existe sur le serveur
2. Vérifiez que le token n'a pas expiré
3. Consultez la console du navigateur (F12) pour les erreurs

**Le token est compromis ?**
1. Révoquez-le sur https://github.com/settings/tokens
2. Créez-en un nouveau
3. Mettez à jour `config.js`

---

**Besoin d'aide ?** Consultez les guides complets ou créez une issue GitHub.
