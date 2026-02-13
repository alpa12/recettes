#!/usr/bin/env Rscript

library(yaml)
library(httr)
library(rvest)
library(jsonlite)
library(glue)
library(ellmer)
library(gargle)

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
  rvest::read_html(recipe_url)
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

cat("📋 Template chargé avec structure:\n")
str(template, max.level = 2)

# Convertir le template en exemple YAML pour le prompt (avec indentation correcte)
template_example <- yaml::as.yaml(template)

# Créer le prompt pour le LLM de manière dynamique basé sur le template
prompt <- glue::glue('
Tu es un assistant qui extrait des recettes depuis des pages web.

Voici le contenu d\'une page web contenant une recette :

---
{page_text}
---

SOURCE URL: {recipe_url}

Extrais les informations de cette recette et génère un fichier YAML avec EXACTEMENT la même structure que cet exemple de template :

{template_example}

RÈGLES IMPORTANTES:
1. Réponds UNIQUEMENT avec le YAML valide, sans texte d\'introduction ni conclusion
2. Ne mets PAS le YAML entre des triple backticks  
3. Commence directement par le premier champ du template
4. Respecte EXACTEMENT la même structure que le template fourni ci-dessus
5. Remplace les valeurs d\'exemple du template par les vraies informations extraites de la recette
6. Les valeurs numériques doivent être des nombres entiers sans guillemets
7. Utilise des tirets (-) pour les listes avec indentation correcte (2 espaces)
8. Si une information est manquante, utilise null ou une valeur par défaut raisonnable
9. Pour nom_court: enlève les accents, articles, et garde maximum 5 mots
10. Pour la source: utilise {recipe_url}
11. Le champ soumis_par sera ajouté automatiquement après, ne l\'inclus PAS dans ta réponse

Extrais maintenant les informations de la recette et génère le YAML complet.
')

# Appeler le LLM avec ellmer (GitHub Copilot)
cat("🤖 Extraction des informations avec GitHub Copilot...\n")

chat <- chat_google_gemini(
  system_prompt = paste(
    "Tu es un expert en extraction de recettes.",
    "Tu réponds uniquement avec du YAML valide, sans texte additionnel, à moins qu'on demande explicitement autre chose."
  )
)

response <- chat$chat(prompt)
yaml_content <- response

cat("📝 Réponse du LLM reçue\n")

# Nettoyer la réponse (enlever les backticks si présents)
yaml_content <- gsub("^```yaml\\s*", "", yaml_content)
yaml_content <- gsub("^```\\s*", "", yaml_content)
yaml_content <- gsub("\\s*```$", "", yaml_content)
yaml_content <- trimws(yaml_content)

cat("📄 YAML nettoyé:\n")
cat(substr(yaml_content, 1, 500), "...\n")

# Valider et parser le YAML
recipe_data <- tryCatch({
  yaml::yaml.load(yaml_content)
}, error = function(e) {
  cat("❌ Erreur lors du parsing YAML:\n")
  cat(yaml_content, "\n")
  stop("YAML invalide généré par le LLM: ", e$message)
})

cat("✅ YAML valide généré\n")

# Ajouter le champ soumis_par
recipe_data$soumis_par <- submitted_by

# Demander la catégorie de la recette au LLM
recipe_category <- chat$chat("Dans quelle catégorie classerais-tu cette recette? Réponds en un seul mot. Choix : Accompagnements, Repas, Desserts.") |>
  trimws() |> 
  tolower()

# Générer le nom de fichier
filename_base <- gsub("[^a-z0-9]+", "-", tolower(recipe_data$nom_court))
filename_base <- gsub("^-|-$", "", filename_base)

yaml_file <- glue("recettes/{recipe_category}/{filename_base}.yaml")

# Sauvegarder le fichier YAML
cat("💾 Sauvegarde de", yaml_file, "\n")
yaml::write_yaml(recipe_data, yaml_file)

# Charger la fonction yaml_to_qmd depuis le projet
cat("📦 Chargement de la fonction yaml_to_qmd...\n")
source("R/yaml_to_qmd.R")

# Générer le fichier QMD avec la fonction existante
cat("💾 Génération du qmd avec yaml_to_qmd()...\n")
yaml_recipe_to_qmd(yaml_path = yaml_file)

cat("✅ Import terminé avec succès!\n")
cat("📄 Fichier yaml généré:\n")
cat("  -", yaml_file, "\n")
