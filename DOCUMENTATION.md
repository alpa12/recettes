# Documentation: Recipe Submission Form

This document explains the implementation of the recipe submission form feature for the Quarto recipe cookbook website.

## Overview

The recipe submission form allows users to submit new recipes directly through the website. When a user completes and submits the form, it automatically:
1. Converts the form data into a YAML file matching the recipe schema
2. Creates a new branch in the GitHub repository
3. Commits the YAML file to that branch
4. Opens a pull request to merge the new recipe into the `dev` branch
5. A GitHub Action workflow then automatically generates the corresponding `.qmd` file

## Components

### 1. Recipe Submission Page (`soumettre.qmd`)

A Quarto document containing an HTML form with embedded JavaScript that handles:
- Dynamic form fields for all recipe components
- Client-side validation
- YAML file generation
- GitHub API integration for PR creation

#### Form Fields

The form includes all fields from the recipe YAML schema:

**General Information:**
- `nom`: Full recipe name (required)
- `nom_court`: Short name for URL (required, lowercase with hyphens only)
- `source`: Recipe source/author (required)
- `portions`: Number of servings (optional)

**Timing:**
- `temps.preparation`: Preparation time in minutes (optional)
- `temps.cuisson`: Cooking time in minutes (optional)
- `temps.refrigeration`: Refrigeration time in minutes (optional)

**Other:**
- `se_congele`: Can be frozen? (boolean, default: yes)

**Dynamic Sections:**
- **Ingredients**: Multiple sections, each containing multiple ingredients with name, quantity, and unit
- **Preparation**: Multiple sections, each containing multiple preparation steps
- **Comments**: Optional comments/notes (multiple allowed)

#### User Experience Features

- **Dynamic field management**: Users can add/remove ingredient sections, ingredients, preparation sections, steps, and comments
- **Validation**: Client-side validation ensures required fields are filled and formats are correct
- **Visual feedback**: Success/error messages displayed after submission
- **Section numbering**: Automatic renumbering when sections are added/removed

### 2. GitHub Action Workflow (`.github/workflows/convert-recipe-yaml-to-qmd.yml`)

Automatically triggered when:
- A pull request is opened, synchronized, or reopened
- Changes include files in `recettes/*.yaml`

**Workflow steps:**
1. Checks out the PR branch
2. Sets up R environment
3. Installs required R packages (`yaml`, `fs`, `stringr`)
4. Sources the conversion function from `R/yaml_to_qmd.R`
5. Identifies changed/new YAML files
6. Converts each YAML file to a `.qmd` file using the existing R function
7. Commits and pushes the generated `.qmd` files back to the PR

### 3. Navigation Integration

Added to `_quarto.yml` navbar:
```yaml
- href: soumettre.qmd
  text: Soumettre une recette
```

## How to Use

### For Users Submitting Recipes

1. **Navigate to the submission page**: Click "Soumettre une recette" in the site navigation
2. **Fill in the recipe details**: Complete all required fields (marked with *)
3. **Add ingredients**: 
   - Use the "+ Ajouter un ingrédient" button to add more ingredients to a section
   - Use "+ Ajouter une section d'ingrédients" to create separate ingredient groups
4. **Add preparation steps**:
   - Use "+ Ajouter une étape" to add more steps
   - Use "+ Ajouter une section de préparation" for different preparation phases
5. **Add comments** (optional): Use "+ Ajouter un commentaire" to add notes
6. **GitHub authentication**:
   - Create a GitHub Personal Access Token (PAT) at https://github.com/settings/tokens
   - The PAT needs `repo` scope permissions
   - Enter your PAT and GitHub username in the form
7. **Submit**: Click "Soumettre la recette"

**Important Notes:**
- The `nom_court` field must use lowercase letters, numbers, and hyphens only
- This becomes the filename and URL slug for the recipe
- The form creates a PR automatically; the recipe won't appear on the site until the PR is reviewed and merged

### For Repository Maintainers

When a PR is created via the form:

1. **Review the YAML file**: Check `recettes/[nom_court].yaml` for correctness
2. **Verify the auto-generated QMD**: The GitHub Action should automatically commit `recettes/[nom_court].qmd`
3. **Review the rendered recipe**: Check that the recipe displays correctly
4. **Merge the PR**: Once approved, merge to add the recipe to the site
5. **Deploy**: The site will need to be re-rendered and deployed to Posit Connect Cloud

## Technical Implementation Details

### YAML Generation

The JavaScript function `convertToYAML()` creates a YAML string that matches the expected schema:
- Handles null values appropriately
- Escapes single quotes in text content
- Maintains proper indentation for nested structures
- Follows the exact format of existing recipe YAML files

### GitHub API Integration

The form uses the GitHub REST API v3 to:
1. Get the base branch SHA
2. Create a new branch
3. Create a blob for the YAML file content
4. Create a tree with the new file
5. Create a commit
6. Update the branch reference
7. Create a pull request

All operations use the user's provided PAT for authentication.

### Security Considerations

- **PATs are never stored**: The GitHub token is only used client-side and never sent to any server other than GitHub's API
- **Client-side only**: All form processing happens in the browser
- **User authentication**: Users must authenticate with their own GitHub credentials
- **PR review process**: All submissions go through PR review before being merged

## Limitations and Future Enhancements

### Current Limitations
1. Users must have a GitHub account and create a PAT
2. No server-side validation or storage
3. No preview of the rendered recipe before submission
4. Form doesn't persist data if user navigates away

### Potential Future Enhancements
1. Add OAuth integration to eliminate the need for manual PAT entry
2. Implement local storage to save form progress
3. Add a preview feature to show how the recipe will render
4. Support image uploads for recipe photos
5. Add unit conversion helpers (e.g., cups to ml)
6. Implement recipe import from URLs (e.g., scrape from popular recipe sites)
7. Add more comprehensive validation (e.g., detect duplicate recipe names)

## Troubleshooting

### Common Issues

**"Impossible de créer la nouvelle branche"**
- The branch name might already exist (unlikely with timestamp)
- PAT might not have `repo` permissions
- Repository might be private and PAT doesn't have access

**"Impossible de créer la pull request"**
- A PR with the same title might already exist
- The base branch (`dev`) might not exist
- User might not have permission to create PRs

**"Le nom court doit contenir uniquement des minuscules, chiffres et tirets"**
- The `nom_court` field contains invalid characters
- Use only: a-z, 0-9, and hyphens (-)
- Example: "poulet-au-beurre" not "Poulet au Beurre"

**GitHub Action doesn't run**
- Check that the YAML file was added to `recettes/` directory
- Verify the workflow file exists in `.github/workflows/`
- Check GitHub Actions tab for workflow run status

## File Structure

```
recettes/
├── .github/
│   └── workflows/
│       └── convert-recipe-yaml-to-qmd.yml  # Workflow to convert YAML to QMD
├── R/
│   └── yaml_to_qmd.R                       # R function for conversion
├── recettes/
│   ├── *.yaml                              # Recipe YAML files
│   └── *.qmd                               # Generated recipe QMD files
├── _quarto.yml                             # Quarto site configuration
├── soumettre.qmd                           # Recipe submission form page
└── DOCUMENTATION.md                        # This file
```

## Maintenance

### Updating the Form
If the recipe schema changes:
1. Update the form fields in `soumettre.qmd`
2. Update the `convertToYAML()` function to match the new schema
3. Test with a sample submission
4. Update this documentation

### Updating the Workflow
If the R conversion function changes:
1. Test the function locally with sample YAML files
2. Update the workflow if needed (e.g., different package dependencies)
3. Test with a sample PR

## Credits

- Form implementation: Using vanilla HTML/CSS/JavaScript for broad compatibility
- YAML conversion: Leverages existing `yaml_to_qmd.R` function
- CI/CD: GitHub Actions for automated workflow
- Hosting: Posit Connect Cloud
