# Recipe Submission Form - Implementation Summary

## Overview
This PR implements a complete recipe submission system that allows website users to submit new recipes directly through a web form. The submitted recipes are automatically converted to the proper format and opened as pull requests for review.

## What Was Added

### 1. Recipe Submission Page (`soumettre.qmd`)
- **770 lines** of HTML, CSS, and JavaScript
- Complete form covering all recipe YAML schema fields
- Dynamic sections for ingredients, preparation steps, and comments
- Client-side YAML generation with proper security escaping
- GitHub API integration for automated PR creation

### 2. GitHub Actions Workflow (`.github/workflows/convert-recipe-yaml-to-qmd.yml`)
- **80 lines** of workflow configuration
- Automatically triggered when YAML files are added/modified in PRs
- Sets up R environment and dependencies
- Converts YAML recipes to Quarto (.qmd) format
- Commits generated files back to the PR branch
- Follows security best practices with explicit permissions

### 3. Documentation (`DOCUMENTATION.md`)
- **219 lines** of comprehensive documentation
- User guide for submitting recipes
- Maintainer guide for reviewing submissions
- Technical implementation details
- Troubleshooting guide
- Future enhancement suggestions

### 4. Navigation Update (`_quarto.yml`)
- Added "Soumettre une recette" link to navbar
- Makes form easily accessible from all pages

## How It Works

### User Workflow
```
1. User navigates to "Soumettre une recette" page
2. Fills out form with recipe details
3. Creates GitHub Personal Access Token (PAT)
4. Submits form
   ↓
5. JavaScript converts form data to YAML
6. Creates new branch via GitHub API
7. Commits YAML file
8. Opens pull request
   ↓
9. GitHub Action triggers automatically
10. Converts YAML to QMD
11. Commits QMD back to PR
   ↓
12. Maintainers review and merge
13. Recipe appears on website
```

### Technical Architecture

#### Frontend (soumettre.qmd)
- **Pure HTML/CSS/JavaScript** - No build step required
- **Progressive enhancement** - Works without additional dependencies
- **Client-side processing** - No backend server needed
- **GitHub API integration** - Direct communication with GitHub

#### Backend (GitHub Actions)
- **Event-driven** - Triggers on PR changes
- **R-based conversion** - Leverages existing `yaml_to_qmd.R` function
- **Automated commits** - Pushes generated files back to PR
- **Secure** - Explicit permissions, minimal scope

## Security Features

### YAML Injection Prevention
- Proper escaping of special YAML characters (`:`, `#`, `-`, `|`, etc.)
- Double-quote wrapping for strings with special characters
- Backslash and quote escaping within quoted strings
- Validated with js-yaml parser

### GitHub Authentication
- User provides their own Personal Access Token
- PAT never stored or sent to third parties
- Only used client-side for GitHub API calls
- Uses modern Bearer token authentication format

### Workflow Security
- Explicit permissions: `contents: write`, `pull-requests: read`
- Minimal scope following principle of least privilege
- No secrets exposed in workflow
- CodeQL security scan passed with zero alerts

### Input Validation
- Required field validation
- Format validation (nom_court must be lowercase with hyphens)
- Client-side sanitization
- YAML structure validation

## Form Features

### Dynamic Content Management
- **Add/Remove Ingredient Sections** - Support for multiple ingredient groups (e.g., "Sauce", "Garniture")
- **Add/Remove Ingredients** - Unlimited ingredients per section
- **Add/Remove Preparation Sections** - Support for multi-phase preparations
- **Add/Remove Steps** - Unlimited preparation steps per section
- **Add/Remove Comments** - Optional notes and tips

### User Experience
- Inline error messages (no disruptive alerts)
- Visual feedback on success/failure
- Automatic section numbering
- Clear field labels and placeholders
- Responsive layout

### Data Fields
All standard recipe fields are supported:
- General: nom, nom_court, source, portions
- Timing: preparation, cuisson, refrigeration
- Freezability: se_congele
- Structured: ingredients (with sections), preparation (with sections), commentaires

## Testing

### YAML Generation Test
Created and executed comprehensive test (`/tmp/test-yaml-generation.js`):
- ✓ All required fields present
- ✓ Proper escaping of colons, quotes, apostrophes
- ✓ Valid YAML structure
- ✓ Parseable by js-yaml library
- ✓ Special characters handled correctly

### Security Validation
- ✓ CodeQL analysis: 0 alerts
- ✓ Proper YAML escaping prevents injection
- ✓ Modern authentication methods used
- ✓ Minimal workflow permissions

## Files Modified/Created

```
✓ soumettre.qmd                                        [NEW] 770 lines
✓ .github/workflows/convert-recipe-yaml-to-qmd.yml    [NEW]  80 lines  
✓ DOCUMENTATION.md                                     [NEW] 219 lines
✓ _quarto.yml                                          [MODIFIED] +2 lines
```

## Integration with Existing System

### Leverages Existing Code
- Uses existing `R/yaml_to_qmd.R` function for conversion
- Follows existing recipe YAML schema exactly
- Integrates seamlessly with Quarto site structure
- Maintains consistency with current recipes

### No Breaking Changes
- Existing recipes unaffected
- Existing workflows continue to work
- Additive only - no deletions or modifications to core functionality

## Deployment Notes

### User Requirements
- GitHub account
- Ability to create Personal Access Token
- Basic understanding of recipe structure

### Repository Requirements
- Dev branch must exist (target for PRs)
- GitHub Actions must be enabled
- R packages: yaml, fs, stringr (installed by workflow)

### Hosting Requirements
- Static hosting (Posit Connect Cloud)
- No server-side processing needed
- No database required
- No API endpoints needed

## Why This Approach?

### Pure Client-Side Processing
**Pros:**
- No backend infrastructure needed
- No hosting costs
- Scales infinitely (GitHub handles the load)
- Simple deployment (just commit files)

**Cons:**
- Users need GitHub account and PAT
- No server-side validation

### GitHub Actions for Conversion
**Pros:**
- Automated workflow
- Consistent conversion using existing R code
- No manual steps for maintainers
- Free for public repositories

**Cons:**
- Depends on GitHub Actions availability
- R environment setup time (~2 minutes per PR)

### Choice of Technologies
- **No frameworks** - Maximum compatibility and minimal dependencies
- **Vanilla JavaScript** - No build step, works everywhere
- **GitHub API v3** - Stable, well-documented, widely supported
- **Existing R function** - Reuses proven conversion logic

## Future Enhancements

See DOCUMENTATION.md for full list, but key opportunities:
1. OAuth integration (eliminate manual PAT entry)
2. Local storage (save form progress)
3. Recipe preview (show rendered output before submit)
4. Image upload support
5. Unit conversion helpers
6. Recipe import from URLs

## Testing Recommendations

Before merging, test:
1. ✓ YAML generation with special characters
2. ✓ Security scan (CodeQL)
3. [ ] Form submission with test PAT (requires manual test)
4. [ ] GitHub Action execution on real PR (requires manual test)
5. [ ] QMD generation from submitted YAML (requires manual test)
6. [ ] Full workflow: submit → PR → review → merge → render

## Support and Maintenance

### For Users
- See DOCUMENTATION.md for complete usage guide
- PAT creation: https://github.com/settings/tokens
- Required scope: `repo`

### For Maintainers  
- Review YAML files in PRs before merging
- Check that auto-generated QMD looks correct
- Merge PR to add recipe to site
- Re-deploy site to Posit Connect Cloud

## Summary

This implementation provides:
- ✅ Complete recipe submission form
- ✅ All YAML schema fields supported
- ✅ Dynamic sections for complex recipes
- ✅ Secure YAML generation with proper escaping
- ✅ Automated PR creation via GitHub API
- ✅ Automated QMD generation via GitHub Actions
- ✅ Zero security alerts from CodeQL
- ✅ Comprehensive documentation
- ✅ No infrastructure costs
- ✅ Seamless integration with existing code

Total changes: **4 files, ~1,071 new lines of code + documentation**
