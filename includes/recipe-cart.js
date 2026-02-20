(function () {
  if (window.__RECIPE_CART_BOOTSTRAPPED__) return;
  window.__RECIPE_CART_BOOTSTRAPPED__ = true;

  const CART_KEY = "recipe_cart_v1";
  const CART_CHECKS_KEY = "recipe_cart_checks_v1";

  const AILLES = [
    "Fruits et legumes",
    "Viandes et substituts",
    "Produits laitiers et oeufs",
    "Boulangerie",
    "Epicerie seche",
    "Conserves et bocaux",
    "Surgeles",
    "Epices et condiments",
    "Boissons",
    "Autres"
  ];

  const AILLE_RULES = [
    { key: "Fruits et legumes", terms: ["poivron", "oignon", "echalot", "ail", "carotte", "champignon", "laitue", "pomme", "ananas", "persil", "basilic", "origan", "ciboulette", "citron", "lime", "tomate", "brocoli", "epinard"] },
    { key: "Viandes et substituts", terms: ["boeuf", "porc", "poulet", "jambon", "saucisse", "dinde", "tofu", "pois chiche", "f eve", "lentille", "haricot"] },
    { key: "Produits laitiers et oeufs", terms: ["lait", "creme", "fromage", "beurre", "oeuf", "yogourt", "yogourt"] },
    { key: "Boulangerie", terms: ["pain", "pita", "tortilla", "bagel", "croissant"] },
    { key: "Conserves et bocaux", terms: ["boite", "conserve", "tomates", "pate de tomate", "jus de tomate"] },
    { key: "Surgeles", terms: ["surgele", "congele"] },
    { key: "Epices et condiments", terms: ["sel", "poivre", "cayenne", "moutarde", "sauce", "vinaigre", "epice", "cannelle", "paprika", "cari", "hoisin", "worcestershire", "sriracha"] },
    { key: "Boissons", terms: ["eau", "jus", "vin", "biere"] },
    { key: "Epicerie seche", terms: ["farine", "sucre", "cassonade", "riz", "pate", "spaghetti", "huile", "f ecule", "cacao", "chocolat", "amande", "noix", "levure"] }
  ];

  function safeParse(json, fallback) {
    try {
      return JSON.parse(json);
    } catch (_e) {
      return fallback;
    }
  }

  function loadCart() {
    return safeParse(localStorage.getItem(CART_KEY) || "[]", []);
  }

  function saveCart(cart) {
    localStorage.setItem(CART_KEY, JSON.stringify(cart));
  }

  function normText(value) {
    return String(value || "")
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .toLowerCase()
      .replace(/\s+/g, " ")
      .trim();
  }

  function formatQty(value) {
    const n = Number(value);
    if (!Number.isFinite(n)) {
      return String(value || "").trim();
    }
    const rounded = Math.round(n * 100) / 100;
    if (Math.abs(rounded - Math.round(rounded)) < 1e-9) {
      return String(Math.round(rounded));
    }
    return String(rounded).replace(".", ",");
  }

  function defaultRecipeId() {
    return window.location.pathname;
  }

  function defaultRecipeUrl() {
    return window.location.pathname;
  }

  function parseRecipeDataScript() {
    const node = document.getElementById("recipe-cart-data");
    if (!node) return null;
    return safeParse(node.textContent || "{}", null);
  }

  function extractIngredientsFromDom() {
    const items = [];
    document.querySelectorAll(".recipe-grocery-list .recipe-ingredient").forEach((li) => {
      const name = (li.querySelector(".ingredient-nom") || {}).textContent || "";
      if (!name.trim()) return;
      const unit = (li.querySelector(".ingredient-uni") || {}).textContent || "";
      const qteNode = li.querySelector(".ingredient-qte");
      const base = qteNode ? qteNode.getAttribute("data-base") : null;
      const qte = base !== null && base !== "" ? Number(base) : (qteNode ? qteNode.textContent : "");
      const rangee = li.getAttribute("data-rangee") || "";
      items.push({ nom: name.trim(), uni: unit.trim(), qte: qte, rangee: rangee.trim() });
    });
    return items;
  }

  function currentRecipePayload() {
    const fromScript = parseRecipeDataScript() || {};
    const title = fromScript.title || document.querySelector("h1")?.textContent || "Recette";
    const ingredients = Array.isArray(fromScript.ingredients) && fromScript.ingredients.length > 0
      ? fromScript.ingredients
      : extractIngredientsFromDom();
    const basePortions = Number(fromScript.portions_base);
    const portionsBase = Number.isFinite(basePortions) && basePortions > 0 ? basePortions : null;
    const portionsTarget = Number(fromScript.portions_target);

    return {
      id: fromScript.id || defaultRecipeId(),
      title: String(title).trim(),
      url: fromScript.url || defaultRecipeUrl(),
      portions_base: portionsBase,
      portions_target: Number.isFinite(portionsTarget) && portionsTarget > 0
        ? portionsTarget
        : portionsBase,
      ingredients: ingredients.map((ing) => ({
        nom: String(ing.nom || "").trim(),
        uni: String(ing.uni || "").trim(),
        qte: ing.qte,
        rangee: String(ing.rangee || ing.rayon || "").trim()
      })).filter((ing) => ing.nom)
    };
  }

  function isRecipeInCart(recipeId) {
    return loadCart().some((x) => x.id === recipeId);
  }

  function upsertRecipeInCart(recipe) {
    const cart = loadCart();
    const idx = cart.findIndex((x) => x.id === recipe.id);
    if (idx >= 0) {
      cart[idx] = recipe;
    } else {
      cart.push(recipe);
    }
    saveCart(cart);
  }

  function removeRecipeFromCart(recipeId) {
    const next = loadCart().filter((x) => x.id !== recipeId);
    saveCart(next);
  }

  function isPanierHref(href) {
    try {
      const url = new URL(String(href || ""), window.location.href);
      const normalized = url.pathname
        .replace(/index\.html$/, "")
        .replace(/\/+$/, "");
      return normalized === "/recettes/panier";
    } catch (_e) {
      return false;
    }
  }

  function updateNavbarBadge() {
    const count = loadCart().length;
    const links = Array.from(document.querySelectorAll("a[href]")).filter((a) => isPanierHref(a.getAttribute("href")));
    links.forEach((link) => {
      let badge = link.querySelector(".cart-nav-badge");
      if (!badge) {
        badge = document.createElement("span");
        badge.className = "cart-nav-badge";
        badge.setAttribute("aria-live", "polite");
        link.appendChild(badge);
      }
      badge.textContent = String(count);
      badge.classList.toggle("d-none", count < 1);
    });
  }

  function resolveAisle(ingredient) {
    const explicit = String(ingredient.rangee || ingredient.rayon || "").trim();
    if (explicit) return explicit;

    const n = normText(ingredient.nom);
    for (const rule of AILLE_RULES) {
      if (rule.terms.some((term) => n.includes(normText(term)))) {
        return rule.key;
      }
    }
    return "Autres";
  }

  function normalizeIngredient(ingredient, sourceRecipeTitle) {
    return {
      nom: String(ingredient.nom || "").trim(),
      uni: String(ingredient.uni || "").trim(),
      qte: ingredient.qte,
      rangee: resolveAisle(ingredient),
      recipeTitle: sourceRecipeTitle
    };
  }

  function recipeScaleRatio(recipe) {
    const base = Number(recipe?.portions_base);
    const target = Number(recipe?.portions_target);
    if (!Number.isFinite(base) || base <= 0) return 1;
    if (!Number.isFinite(target) || target <= 0) return 1;
    return target / base;
  }

  function mergeIngredientsFromCart(cart) {
    const map = new Map();

    cart.forEach((recipe) => {
      const ratio = recipeScaleRatio(recipe);
      const list = Array.isArray(recipe.ingredients) ? recipe.ingredients : [];
      list.forEach((rawIng) => {
        const ing = normalizeIngredient(rawIng, recipe.title);
        if (!ing.nom) return;
        const rawQty = Number(ing.qte);
        const scaledQty = Number.isFinite(rawQty) ? rawQty * ratio : ing.qte;

        const key = [normText(ing.rangee), normText(ing.nom), normText(ing.uni)].join("||");
        const existing = map.get(key);

        if (!existing) {
          map.set(key, {
            key,
            rangee: ing.rangee,
            nom: ing.nom,
            uni: ing.uni,
            qte: scaledQty,
            recipeTitles: [recipe.title]
          });
          return;
        }

        if (!existing.recipeTitles.includes(recipe.title)) {
          existing.recipeTitles.push(recipe.title);
        }

        const a = Number(existing.qte);
        const b = Number(ing.qte);
        const scaled = Number.isFinite(b) ? b * ratio : ing.qte;
        if (Number.isFinite(a) && Number.isFinite(b)) {
          existing.qte = a + scaled;
        } else if (!existing.qte && scaled) {
          existing.qte = scaled;
        }
      });
    });

    const entries = Array.from(map.values());
    const aisleOrder = new Map(AILLES.map((v, i) => [normText(v), i]));
    entries.sort((a, b) => {
      const ia = aisleOrder.has(normText(a.rangee)) ? aisleOrder.get(normText(a.rangee)) : 999;
      const ib = aisleOrder.has(normText(b.rangee)) ? aisleOrder.get(normText(b.rangee)) : 999;
      if (ia !== ib) return ia - ib;
      return a.nom.localeCompare(b.nom, "fr");
    });
    return entries;
  }

  function makeIngredientLabel(item) {
    const qty = formatQty(item.qte);
    const base = [qty, item.uni, item.nom].filter(Boolean).join(" ");
    return base || item.nom;
  }

  function loadChecks() {
    return safeParse(localStorage.getItem(CART_CHECKS_KEY) || "{}", {});
  }

  function saveChecks(state) {
    localStorage.setItem(CART_CHECKS_KEY, JSON.stringify(state));
  }

  function renderRecipeButton() {
    const btn = document.getElementById("recipe-cart-toggle");
    if (!btn) return;

    const recipe = currentRecipePayload();
    const refresh = () => {
      const inCart = isRecipeInCart(recipe.id);
      btn.textContent = inCart ? "Retirer du panier" : "Ajouter au panier";
      btn.classList.toggle("btn-success", inCart);
      btn.classList.toggle("btn-outline-success", !inCart);
      btn.setAttribute("aria-pressed", inCart ? "true" : "false");
      updateNavbarBadge();
    };

    btn.addEventListener("click", () => {
      if (isRecipeInCart(recipe.id)) {
        removeRecipeFromCart(recipe.id);
      } else {
        upsertRecipeInCart(recipe);
      }
      refresh();
    });

    refresh();
  }

  function renderCartPage() {
    const page = document.querySelector("[data-page='recipe-cart']");
    if (!page) return;

    const recipesWrap = page.querySelector("#cart-recipes");
    const ingredientsWrap = page.querySelector("#cart-ingredients");
    const countWrap = page.querySelector("#cart-recipes-count");
    const copyBtn = page.querySelector("#cart-copy-remaining");
    const resetBtn = page.querySelector("#cart-reset-checks");
    const feedback = page.querySelector("#cart-copy-feedback");

    function getUncheckedLines() {
      return Array.from(ingredientsWrap.querySelectorAll(".cart-ingredient-item"))
        .filter((li) => !(li.querySelector(".cart-ingredient-check") || {}).checked)
        .map((li) => {
          const label = li.querySelector(".cart-ingredient-label");
          return label ? label.textContent.replace(/\s+/g, " ").trim() : "";
        })
        .filter(Boolean);
    }

    function render() {
      const cart = loadCart();
      const checks = loadChecks();
      const merged = mergeIngredientsFromCart(cart);

      countWrap.textContent = String(cart.length);
      updateNavbarBadge();

      if (cart.length === 0) {
        recipesWrap.innerHTML = "<p class='text-muted mb-0'>Aucune recette dans le panier.</p>";
        ingredientsWrap.innerHTML = "<p class='text-muted mb-0'>Ajoute des recettes pour generer la liste d'epicerie.</p>";
        return;
      }

      recipesWrap.innerHTML = cart.map((recipe) => {
        const title = recipe.title || recipe.id;
        const href = recipe.url || "#";
        const count = Array.isArray(recipe.ingredients) ? recipe.ingredients.length : 0;
        const basePortions = Number(recipe.portions_base);
        const hasBasePortions = Number.isFinite(basePortions) && basePortions > 0;
        const targetPortions = Number(recipe.portions_target);
        const wanted = Number.isFinite(targetPortions) && targetPortions > 0 ? targetPortions : basePortions;
        const portionsHtml = hasBasePortions ? [
          "<div class='cart-portions-controls'>",
          `<span class='text-muted small'>Base: ${formatQty(basePortions)} portion(s)</span>`,
          "<label class='cart-portions-label'>Voulu</label>",
          `<input class='form-control form-control-sm cart-portions-input' type='number' min='1' step='1' value='${wanted}' data-recipe-id='${recipe.id}'>`,
          "</div>"
        ].join("") : "<div class='text-muted small'>Portions non definies</div>";
        return [
          "<article class='cart-recipe-card'>",
          `<div><a class='cart-recipe-link' href='${href}'>${title}</a><div class='text-muted small'>${count} ingredient(s)</div>${portionsHtml}</div>`,
          `<button class='btn btn-sm btn-outline-danger cart-remove-recipe' type='button' data-recipe-id='${recipe.id}'>Retirer</button>`,
          "</article>"
        ].join("");
      }).join("");

      let currentAisle = null;
      const lines = [];
      merged.forEach((item) => {
        if (item.rangee !== currentAisle) {
          if (currentAisle !== null) lines.push("</ul>");
          currentAisle = item.rangee;
          lines.push(`<h3 class='cart-aisle-title'>${item.rangee}</h3>`);
          lines.push("<ul class='recipe-ingredients cart-ingredients-list'>");
        }

        const checked = checks[item.key] ? "checked" : "";
        const label = makeIngredientLabel(item);
        lines.push([
          `<li class='recipe-ingredient cart-ingredient-item' data-item-key='${item.key}'>`,
          "<label class='recipe-check-row'>",
          `<input type='checkbox' class='recipe-check cart-ingredient-check' ${checked}>`,
          `<span class='ingredient-label cart-ingredient-label'>${label}</span>`,
          "</label>",
          "</li>"
        ].join(""));
      });
      if (currentAisle !== null) lines.push("</ul>");

      ingredientsWrap.innerHTML = lines.join("");

      ingredientsWrap.querySelectorAll(".cart-ingredient-check").forEach((cb) => {
        cb.addEventListener("change", () => {
          const li = cb.closest(".cart-ingredient-item");
          if (!li) return;
          const key = li.getAttribute("data-item-key");
          const next = loadChecks();
          if (cb.checked) {
            next[key] = 1;
          } else {
            delete next[key];
          }
          saveChecks(next);
        });
      });

      recipesWrap.querySelectorAll(".cart-remove-recipe").forEach((btn) => {
        btn.addEventListener("click", () => {
          removeRecipeFromCart(btn.getAttribute("data-recipe-id"));
          render();
        });
      });

      recipesWrap.querySelectorAll(".cart-portions-input").forEach((input) => {
        input.addEventListener("change", () => {
          const recipeId = input.getAttribute("data-recipe-id");
          const wanted = Number(input.value);
          if (!Number.isFinite(wanted) || wanted <= 0) return;
          const nextCart = loadCart().map((recipe) => {
            if (recipe.id !== recipeId) return recipe;
            return { ...recipe, portions_target: wanted };
          });
          saveCart(nextCart);
          render();
        });
      });
    }

    copyBtn?.addEventListener("click", async () => {
      const lines = getUncheckedLines();
      const txt = lines.length ? lines.map((x) => "- " + x).join("\n") : "(Aucun ingredient restant)";

      try {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(txt);
        } else {
          window.prompt("Copie la liste:", txt);
        }
        if (feedback) feedback.textContent = "Liste copié.";
      } catch (_e) {
        window.prompt("Copie la liste:", txt);
        if (feedback) feedback.textContent = "Liste prête a copier.";
      }
    });

    resetBtn?.addEventListener("click", () => {
      saveChecks({});
      render();
    });

    render();
  }

  document.addEventListener("DOMContentLoaded", function () {
    updateNavbarBadge();
    renderRecipeButton();
    renderCartPage();
  });
})();
