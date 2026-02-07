#!/usr/bin/env Rscript

library(yaml)
library(httr)
library(rvest)
library(jsonlite)
library(glue)
library(ellmer)

# Lire le fichier URL
url_file <- Sys.getenv("RECIPE_URL_FILE")
if (!file.exists(url_file)) {
  stop("Fichier URL introuvable: ", url_file)
}

url_data <- yaml::read_yaml(url_file)
recipe_url <- url_data$url
submitted_by <- url_data$submitted_by %||% "Import automatique"

cat("📥 Import de la recette depuis:", recipe_url, "\n")

# Scraper la page web
cat("🌐 Téléchargement de la page...\n")
page <- tryCatch({
  read_html(recipe_url)
}, error = function(e) {
  stop("Erreur lors du téléchargement de l'URL: ", e$message)
})

# Extraire le contenu texte de la page
page_text <- page |> 
  html_text() |> 
  trimws()

# Tronquer si trop long (pour éviter de dépasser les limites du LLM)
if (nchar(page_text) > 50000) {
  page_text <- substr(page_text, 1, 50000)
  cat("⚠️ Contenu tronqué à 50000 caractères\n")
}

# Charger le template
template <- yaml::read_yaml("recettes/template.yaml")

# Créer le prompt pour le LLM
prompt <- glue::glue('
Tu es un assistant qui extrait des recettes depuis des pages web.

Voici le contenu d\'une page web contenant une recette :

---
{page_text}
---

SOURCE URL: {recipe_url}

Extrais les informations de cette recette et génère un fichier YAML avec la structure suivante :

- titre: Le nom de la recette
- description: Une courte description (1-2 phrases)
- temps_preparation: Temps de préparation en minutes (nombre entier, ex: 30)
- temps_cuisson: Temps de cuisson en minutes (nombre entier, ex: 45)
- portions: Nombre de portions (nombre entier, ex: 4)
- difficulte: Niveau de difficulté (Facile/Moyen/Difficile)
- categorie: Catégorie principale (Repas/Dessert/Accompagnement)
- ingredients: Liste des ingrédients (tableau YAML avec champs "nom" et "quantite")
- instructions: Liste des étapes (tableau de chaînes)
- tags: Liste de mots-clés pertinents (tableau)
- source_url: URL de la source ({recipe_url})
- soumis_par: "{submitted_by}"

IMPORTANT:
1. Réponds UNIQUEMENT avec le YAML valide, sans texte d\'introduction ni conclusion
2. Ne mets PAS le YAML entre des triple backticks
3. Commence directement par "titre:"
4. Utilise des tirets (-) pour les listes
5. Les valeurs numériques (temps, portions) doivent être des nombres entiers sans guillemets
6. Si une information est manquante, utilise une valeur par défaut raisonnable

Exemple de format attendu:

titre: Nom de la recette
description: Description courte
temps_preparation: 30
temps_cuisson: 45
portions: 4
difficulte: Moyen
categorie: Repas
ingredients:
  - nom: Ingrédient 1
    quantite: 200g
  - nom: Ingrédient 2
    quantite: 3 unités
instructions:
  - Étape 1
  - Étape 2
tags:
  - tag1
  - tag2
source_url: {recipe_url}
soumis_par: {submitted_by}
')

# Appeler le LLM avec ellmer (GitHub Copilot)
cat("🤖 Extraction des informations avec GitHub Copilot...\n")

chat <- chat_github(
  system_prompt = "Tu es un expert en extraction de recettes. Tu réponds uniquement avec du YAML valide, sans texte additionnel.",
  api_key = Sys.getenv("GITHUB_TOKEN")
)

response <- chat$chat(prompt)
yaml_content <- response

cat("📝 Réponse du LLM reçue\n")

# Nettoyer la réponse (enlever les backticks si présents)
yaml_content <- gsub("^```yaml\\n?", "", yaml_content)
yaml_content <- gsub("^```\\n?", "", yaml_content)
yaml_content <- gsub("\\n?```$", "", yaml_content)
yaml_content <- trimws(yaml_content)

# Valider et parser le YAML
recipe_data <- tryCatch({
  yaml::yaml.load(yaml_content)
}, error = function(e) {
  cat("❌ Erreur lors du parsing YAML:\n")
  cat(yaml_content, "\n")
  stop("YAML invalide généré par le LLM: ", e$message)
})

cat("✅ YAML valide généré\n")

# Générer le nom de fichier
filename_base <- gsub("[^a-z0-9]+", "-", tolower(recipe_data$titre))
filename_base <- gsub("^-|-$", "", filename_base)

yaml_file <- glue("recettes/{filename_base}.yaml")
qmd_file <- glue("recettes/{filename_base}.qmd")

# Sauvegarder le YAML
cat("💾 Sauvegarde de", yaml_file, "\n")
yaml::write_yaml(recipe_data, yaml_file)

# Générer le fichier QMD
cat("💾 Génération de", qmd_file, "\n")
qmd_content <- glue('---
title: "{recipe_data$titre}"
---

{{{{< include _recette.qmd >}}}}
')

writeLines(qmd_content, qmd_file)

cat("✅ Import terminé avec succès!\n")
cat("📄 Fichiers générés:\n")
cat("  -", yaml_file, "\n")
cat("  -", qmd_file, "\n")
