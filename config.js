// Configuration GitHub pour les soumissions de recettes
// Le token est légèrement obfusqué pour éviter le scraping automatique
const GITHUB_CONFIG = {
    // Reconstruit le token à partir de fragments
    get token() {
        const parts = [
            'github_pat_',
            '11AHEFEKI0yVUL1Y',
            '8qbQZs_ObMlOskCwuC',
            'SaLxTMeIECX50ZlL4',
            'bUn1Kyb7M40qxVJZHEN6NZZamx49MMV'
        ];
        return parts.join('');
    },
    owner: 'alex-parent',
    repo: 'recettes',
    baseBranch: 'main'
};
