// Configuration GitHub pour les soumissions de recettes
// Le token est légèrement obfusqué pour éviter le scraping automatique
const GITHUB_CONFIG = {
    // Reconstruit le token à partir de fragments
    get token() {
        const parts = [
            'github_pat_',
            '11AHEFEKI0IYwcD0',
            'UhWWde_XaxUb3jIWpn',
            'XMAj2osDYHFJ27QU',
            'SpM1FZA3kYAc3vHJJZZG326CBucJ9z53'
        ];
        return parts.join('');
    },
    owner: 'alpa12',
    repo: 'recettes',
    baseBranch: 'dev'
};
