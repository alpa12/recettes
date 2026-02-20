# yaml_to_qmds.R
# ------------------------------------------------------------------------------
# Utilities to convert recipe YAML files into Quarto (.qmd) pages.
# Adds an "Edit" button on every recipe page that links to an edit form page
# with the YAML path as a query parameter (static-site friendly).
#
# Comments upgrade:
# - recipe$commentaires can be:
#   (A) old format: character vector
#   (B) new format: list of dict with optional fields:
#       commentaire (string), evaluation (int 1..5), auteur (string), date (YYYY-MM-DD)
#   Backward compatibility:
#       note -> evaluation, nom -> auteur
#
# In the generated recipe page, we display:
# - Average stars (from available evaluations)
# - Total comment count
# - Count of comments written by "Alexandre Parent" and "Élodie Bourgeois"
#   (case-insensitive, accents-insensitive best-effort)
# ------------------------------------------------------------------------------

EDIT_PAGE_HREF <- "ajouter_recette/"
EDIT_PARAM_NAME <- "yaml"

escape_html <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

`%||%` <- function(a, b) if (is.null(a)) b else a

fmt_number <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n)) return(as.character(x))
  if (abs(n - round(n)) < 1e-9) return(as.character(as.integer(round(n))))
  s <- format(round(n, 2), nsmall = 2, trim = TRUE, scientific = FALSE)
  s <- sub("0+$", "", s)
  sub("\\.$", "", s)
}

render_ingredient_li <- function(ing, list_kind = "generic") {
  q_raw <- ing$qte %||% ""
  q_num <- suppressWarnings(as.numeric(q_raw))
  q_is_num <- !is.na(q_num)
  q_label <- escape_html(fmt_number(q_raw))
  uni <- escape_html(stringr::str_trim(as.character(ing$uni %||% "")))
  nom <- escape_html(stringr::str_trim(as.character(ing$nom %||% "")))
  rangee <- escape_html(stringr::str_trim(as.character(ing$rangee %||% ing$rayon %||% "")))

  content <- if (q_is_num) {
    paste0(
      "<span class=\"ingredient-qte\" data-base=\"", q_num, "\">", q_label, "</span>",
      if (nzchar(uni)) paste0(" <span class=\"ingredient-uni\">", uni, "</span>") else "",
      if (nzchar(nom)) paste0(" <span class=\"ingredient-nom\">", nom, "</span>") else ""
    )
  } else {
    paste0(
      if (nzchar(q_label)) q_label else "",
      if (nzchar(uni)) paste0(" ", uni) else "",
      if (nzchar(nom)) paste0(" ", nom) else ""
    )
  }
  paste0(
    "<li class=\"recipe-ingredient\" data-list-kind=\"", list_kind,
    "\" data-rangee=\"", rangee,
    "\" data-ingredient-name=\"", nom,
    "\" data-ingredient-unit=\"", uni, "\">",
    "<label class=\"recipe-check-row\">",
    "<input type=\"checkbox\" class=\"recipe-check recipe-ingredient-check\">",
    "<span class=\"ingredient-label\">", content, "</span>",
    "</label>",
    "</li>"
  )
}

render_ingredient_inline <- function(ing) {
  q_raw <- ing$qte %||% ""
  q_num <- suppressWarnings(as.numeric(q_raw))
  q_is_num <- !is.na(q_num)
  q_label <- escape_html(fmt_number(q_raw))
  uni <- escape_html(stringr::str_trim(as.character(ing$uni %||% "")))
  nom <- escape_html(stringr::str_trim(as.character(ing$nom %||% "")))
  rangee <- escape_html(stringr::str_trim(as.character(ing$rangee %||% ing$rayon %||% "")))

  content <- if (q_is_num) {
    paste0(
      "<span class=\"ingredient-qte\" data-base=\"", q_num, "\">", q_label, "</span>",
      if (nzchar(uni)) paste0(" <span class=\"ingredient-uni\">", uni, "</span>") else "",
      if (nzchar(nom)) paste0(" <span class=\"ingredient-nom\">", nom, "</span>") else ""
    )
  } else {
    paste0(
      if (nzchar(q_label)) q_label else "",
      if (nzchar(uni)) paste0(" ", uni) else "",
      if (nzchar(nom)) paste0(" ", nom) else ""
    )
  }
  paste0(
    "<label class=\"recipe-check-row\" data-rangee=\"", rangee, "\">",
    "<input type=\"checkbox\" class=\"recipe-check recipe-ingredient-check\">",
    "<span class=\"ingredient-label\">", content, "</span>",
    "</label>"
  )
}

build_fact_box <- function(label, value) {
  paste0(
    "<div class=\"recipe-fact\">",
    "<span>", escape_html(label), "</span>",
    "<strong>", escape_html(value), "</strong>",
    "</div>"
  )
}

extract_step_timers <- function(step_text) {
  txt <- as.character(step_text %||% "")
  if (!nzchar(txt)) return(list())
  m <- gregexpr("(\\d+)\\s*(h|heure|heures|hr|hrs|min|minute|minutes)", txt, perl = TRUE, ignore.case = TRUE)
  vals <- regmatches(txt, m)[[1]]
  if (length(vals) == 1 && vals[1] == "-1") return(list())
  out <- list()
  for (v in vals) {
    mm <- regexec("(\\d+)\\s*(h|heure|heures|hr|hrs|min|minute|minutes)", v, perl = TRUE, ignore.case = TRUE)
    parts <- regmatches(v, mm)[[1]]
    if (length(parts) < 3) next
    n <- suppressWarnings(as.numeric(parts[2]))
    u <- tolower(parts[3])
    if (!is.finite(n)) next
    sec <- if (u %in% c("h", "heure", "heures", "hr", "hrs")) n * 3600 else n * 60
    if (sec > 0) out[[length(out) + 1]] <- list(label = trimws(v), seconds = as.integer(sec))
  }
  out
}

#' @importFrom yaml read_yaml
#' @importFrom fs path_ext_set path_file path_rel
#' @importFrom stringr str_trim str_to_lower
#' @export
yaml_recipe_to_qmd <- function(yaml_path, qmd_path = NULL) {
  stopifnot(file.exists(yaml_path))

  if (is.null(qmd_path)) {
    qmd_path <- fs::path_ext_set(yaml_path, "qmd")
  }

  recipe <- yaml::read_yaml(yaml_path)
  lines <- character()

  # ---- Quarto front-matter ----
  image_line <- if (!is.null(recipe$image_guid) && nzchar(as.character(recipe$image_guid))) {
    guid <- as.character(recipe$image_guid)
    thumb_local <- file.path("images", "thumbs", paste0(guid, ".jpg"))
    img_path <- if (file.exists(thumb_local)) {
      paste0("/images/thumbs/", guid, ".jpg")
    } else {
      paste0("/images/", guid, ".jpg")
    }
    paste0("image: ", img_path)
  } else {
    ""
  }
  lines <- c(
    lines,
    "---",
    paste0("title: ", recipe$nom),
    image_line
  )

  if (!is.null(recipe$categories)) {
    cats <- unlist(recipe$categories, use.names = FALSE)
    lines <- c(lines, "categories:", paste0("  - ", cats))
  }
  lines <- c(lines, "---", "")

  # ---- Edit button ----
  yaml_rel_to_root <- fs::path_rel(yaml_path, start = ".")
  yaml_rel_to_root <- gsub("\\\\", "/", yaml_rel_to_root)
  if (!startsWith(yaml_rel_to_root, "/")) yaml_rel_to_root <- paste0("/", yaml_rel_to_root)
  recipe_url <- sub("\\.ya?ml$", ".qmd", yaml_rel_to_root, ignore.case = TRUE)

  yaml_qp <- utils::URLencode(yaml_rel_to_root, reserved = TRUE)
  edit_href <- paste0("../", EDIT_PAGE_HREF, "?", EDIT_PARAM_NAME, "=", yaml_qp)
  base_portions <- suppressWarnings(as.numeric(recipe$portions))
  base_portions <- if (!is.na(base_portions) && base_portions > 0) base_portions else NULL

  cart_ingredients <- list()
  for (section in recipe$preparation %||% list()) {
    for (step in section$etapes %||% list()) {
      for (ing in step$ingredients %||% list()) {
        nom <- as.character(ing$nom %||% "")
        if (!nzchar(stringr::str_trim(nom))) next
        cart_ingredients[[length(cart_ingredients) + 1]] <- list(
          nom = nom,
          uni = as.character(ing$uni %||% ""),
          qte = ing$qte %||% NULL,
          rangee = as.character(ing$rangee %||% ing$rayon %||% "")
        )
      }
    }
  }
  cart_payload <- list(
    id = recipe_url,
    title = as.character(recipe$nom %||% ""),
    url = recipe_url,
    portions_base = base_portions,
    portions_target = base_portions,
    ingredients = cart_ingredients
  )
  cart_json <- jsonlite::toJSON(cart_payload, auto_unbox = TRUE, null = "null")
  cart_json <- gsub("</", "<\\\\/", cart_json, fixed = TRUE)

  # ---- Quick facts and tools ----
  facts <- character()
  if (!is.null(recipe$source) && nzchar(as.character(recipe$source))) {
    facts <- c(facts, build_fact_box("Source", recipe$source))
  }
  if (!is.null(recipe$se_congele)) {
    txt <- if (isTRUE(recipe$se_congele)) "Oui" else "Non"
    facts <- c(facts, build_fact_box("Se congèle", txt))
  }
  t <- if (is.list(recipe$temps)) recipe$temps else list()
  prep <- if (!is.null(t$preparation) && nzchar(fmt_number(t$preparation))) paste0(fmt_number(t$preparation), " min") else "-"
  cook <- if (!is.null(t$cuisson) && nzchar(fmt_number(t$cuisson))) paste0(fmt_number(t$cuisson), " min") else "-"
  facts <- c(facts, build_fact_box("Temps préparation", prep))
  facts <- c(facts, build_fact_box("Temps cuisson", cook))
  if (!is.null(t$refrigeration) && nzchar(fmt_number(t$refrigeration))) {
    cool <- paste0(fmt_number(t$refrigeration), " min")
    facts <- c(facts, build_fact_box("Temps réfrigération", cool))
  }

  lines <- c(
    lines,
    "```{=html}",
    "<div class=\"recipe-toolbar\">",
    paste0("<a href=\"", edit_href, "\" class=\"btn btn-outline-primary btn-sm\">✏️ Modifier cette recette</a>"),
    "<div class=\"recipe-toolbar-actions\">",
    "<button id=\"recipe-cart-toggle\" type=\"button\" class=\"btn btn-outline-success btn-sm\">Ajouter au panier</button>",
    "<button id=\"recipe-reading-mode\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">🍳 Mode cuisson</button>",
    "<button type=\"button\" class=\"btn btn-outline-secondary btn-sm\" onclick=\"window.print()\">🖨️ Imprimer</button>",
    "<button type=\"button\" class=\"btn btn-outline-secondary btn-sm\" onclick=\"navigator.clipboard && navigator.clipboard.writeText(window.location.href)\">🔗 Copier le lien</button>",
    "</div>",
    "</div>",
    if (length(facts) > 0) paste0("<div class=\"recipe-facts-grid\">", paste(facts, collapse = ""), "</div>") else "",
    "```",
    ""
  )

  lines <- c(
    lines,
    "```{=html}",
    paste0("<script id=\"recipe-cart-data\" type=\"application/json\">", cart_json, "</script>"),
    "<script src=\"/includes/recipe-cart.js\"></script>",
    "```",
    ""
  )

  meta_badges <- character()
  if (!is.null(recipe$difficulte) && nzchar(as.character(recipe$difficulte))) {
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Difficulté: ", escape_html(recipe$difficulte), "</span>"))
  }
  if (!is.null(recipe$cout) && nzchar(as.character(recipe$cout))) {
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Coût: ", escape_html(recipe$cout), "</span>"))
  }
  if (is.list(recipe$allergenes) && length(recipe$allergenes) > 0) {
    allg <- paste(escape_html(unlist(recipe$allergenes, use.names = FALSE)), collapse = ", ")
    meta_badges <- c(meta_badges, paste0("<span class=\"recipe-meta-badge\">Allergènes: ", allg, "</span>"))
  }
  if (length(meta_badges) > 0) {
    lines <- c(
      lines,
      "```{=html}",
      paste0("<div class=\"recipe-meta-badges\">", paste(meta_badges, collapse = ""), "</div>"),
      "```",
      ""
    )
  }

  has_scaler <- !is.null(base_portions) && !is.na(base_portions) && base_portions > 0
  has_step_images <- any(vapply(recipe$preparation %||% list(), function(section) {
    any(vapply(section$etapes %||% list(), function(step) {
      !is.null(step$image_guid) && nzchar(as.character(step$image_guid))
    }, logical(1)))
  }, logical(1)))

  if (has_scaler) {
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<div class=\"recipe-servings-control\">",
        "<label for=\"servings-input\" class=\"form-label mb-1\">Ajuster les portions</label>",
        "<div class=\"d-flex gap-2 align-items-center\">",
        "<input id=\"servings-input\" class=\"form-control form-control-sm\" type=\"number\" min=\"1\" step=\"1\" value=\"", base_portions, "\" style=\"max-width: 110px;\">",
        "<button id=\"servings-reset\" class=\"btn btn-outline-secondary btn-sm\" type=\"button\">Réinitialiser</button>",
        "<span class=\"text-muted small\">Base: ", base_portions, "</span>",
        "</div>",
        "</div>"
      ),
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/glightbox/dist/css/glightbox.min.css\">",
      "<script src=\"https://cdn.jsdelivr.net/npm/glightbox/dist/js/glightbox.min.js\"></script>",
      "```",
      ""
    )
  }

  lines <- c(
    lines,
    "```{=html}",
    "<script>(function(){",
    "const key='recipe_reading_mode';",
    "let wakeLock=null;",
    "const apply=(on)=>{document.body.classList.toggle('reading-mode', on);};",
    "const requestWake=async()=>{try{if('wakeLock' in navigator){wakeLock=await navigator.wakeLock.request('screen');}}catch(e){}};",
    "const releaseWake=async()=>{try{if(wakeLock){await wakeLock.release(); wakeLock=null;}}catch(e){}};",
    "document.addEventListener('DOMContentLoaded', ()=>{",
    "const btn=document.getElementById('recipe-reading-mode');",
    "const saved=localStorage.getItem(key)==='1'; apply(saved); if(saved) requestWake();",
    "if(!btn) return;",
    "const refresh=()=>{btn.classList.toggle('active', document.body.classList.contains('reading-mode'));}; refresh();",
    "btn.addEventListener('click', async()=>{const next=!document.body.classList.contains('reading-mode'); apply(next); localStorage.setItem(key, next?'1':'0'); refresh(); if(next){await requestWake();} else {await releaseWake();}});",
    "});",
    "})();</script>",
    "```",
    ""
  )

  # ---- Ingredients (grouped by section) ----
  lines <- c(lines, "## Ingrédients", "")
  lines <- c(
    lines,
    "```{=html}",
    "<div class=\"recipe-grocery-actions\">",
    "<button id=\"copy-grocery-list\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">📋 Générer la liste d'épicerie</button>",
    "<button id=\"reset-recipe-checks\" type=\"button\" class=\"btn btn-outline-secondary btn-sm\">↺ Réinitialiser les cases</button>",
    "<span id=\"grocery-copy-feedback\" class=\"text-muted small\"></span>",
    "</div>",
    "```",
    ""
  )

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    ing_list <- list()
    for (step in section$etapes) {
      if (!is.null(step$ingredients)) {
        for (ing in step$ingredients) {
          key <- paste(ing$nom, ing$uni)
          if (!key %in% names(ing_list)) {
            ing_list[[key]] <- ing
          }
        }
      }
    }
    lines <- c(lines, "```{=html}", "<ul class=\"recipe-ingredients recipe-grocery-list\">")
    for (item in ing_list) lines <- c(lines, render_ingredient_li(item, list_kind = "grocery"))
    lines <- c(lines, "</ul>", "```")
    lines <- c(lines, "")
  }

  # ---- Equipment (grouped by section) ----
  lines <- c(lines, "## Équipements", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "")

    equip_list <- character()
    for (step in section$etapes) {
      if (!is.null(step$equipements)) {
        equip_list <- c(equip_list, step$equipements)
      }
    }
    equip_list <- unique(equip_list)

    for (eq in equip_list) {
      lines <- c(lines, paste0("- ", stringr::str_trim(eq)))
    }
    lines <- c(lines, "")
  }

  # ---- Preparation steps (2-column cooking layout) ----
  lines <- c(lines, "## Préparation", "")

  for (section in recipe$preparation) {
    lines <- c(lines, paste0("### ", section$section), "", "```{=html}", "<div class=\"recipe-prep-grid\">")

    for (i in seq_along(section$etapes)) {
      step <- section$etapes[[i]]
      step_text <- escape_html(step$etape %||% "")

      ing_html <- ""
      if (is.list(step$ingredients) && length(step$ingredients) > 0) {
        ing_lines <- vapply(step$ingredients, function(x) render_ingredient_li(x, list_kind = "step"), character(1))
        ing_html <- paste0(
          "<div class=\"recipe-prep-ingredients\">",
          "<div class=\"recipe-prep-label\">Ingrédients</div>",
          "<ul class=\"recipe-ingredients recipe-step-ingredients\">",
          paste(ing_lines, collapse = ""),
          "</ul>",
          "</div>"
        )
      } else {
        ing_html <- "<div class=\"recipe-prep-ingredients\"><div class=\"recipe-prep-label\">Ingrédients</div><div class=\"text-muted small\">Aucun ingrédient spécifique.</div></div>"
      }

      img_html <- ""
      if (!is.null(step$image_guid) && nzchar(as.character(step$image_guid))) {
        src <- paste0("/images/", step$image_guid, ".jpg")
        img_html <- paste0(
          "<a href=\"", src, "\" class=\"glightbox recipe-step-image-link\" data-gallery=\"recipe-steps\">",
          "<img src=\"", src, "\" alt=\"Image étape ", i, "\" class=\"recipe-step-thumb\">",
          "</a>"
        )
      }
      timers <- extract_step_timers(step$etape)
      timer_html <- ""
      if (length(timers) > 0) {
        timer_buttons <- vapply(timers, function(t) {
          paste0("<button type=\"button\" class=\"btn btn-outline-secondary btn-sm recipe-timer-btn\" data-seconds=\"", t$seconds, "\">⏱️ ", escape_html(t$label), "</button>")
        }, character(1))
        timer_html <- paste0("<div class=\"recipe-step-timers\">", paste(timer_buttons, collapse = ""), "</div>")
      }

      lines <- c(
        lines,
        paste0(
          "<article class=\"recipe-prep-step\">",
          ing_html,
          "<div class=\"recipe-prep-instruction\">",
          "<div class=\"recipe-prep-stepno\"><label class=\"recipe-check-row\"><input type=\"checkbox\" class=\"recipe-check recipe-step-check\"><span>Étape ", i, "</span></label></div>",
          "<p>", step_text, "</p>",
          timer_html,
          img_html,
          "</div>",
          "</article>"
        )
      )
    }

    lines <- c(lines, "</div>", "```", "")
  }

  if (has_scaler) {
    lines <- c(
      lines,
      "```{=html}",
      paste0(
        "<script>",
        "(function(){",
        "const input=document.getElementById('servings-input');",
        "const reset=document.getElementById('servings-reset');",
        "if(!input) return;",
        "const base=", base_portions, ";",
        "const format=(n)=>{const r=Math.round(n*100)/100; return (Math.abs(r-Math.round(r))<1e-9)?String(Math.round(r)):String(r).replace('.',',');};",
        "const update=()=>{",
        "const current=parseFloat(input.value);",
        "if(!Number.isFinite(current)||current<=0) return;",
        "const ratio=current/base;",
        "document.querySelectorAll('.ingredient-qte[data-base]').forEach((el)=>{",
        "const b=parseFloat(el.getAttribute('data-base'));",
        "if(Number.isFinite(b)){el.textContent=format(b*ratio);} });",
        "};",
        "input.addEventListener('input', update);",
        "if(reset){reset.addEventListener('click', ()=>{input.value=String(base); update();});}",
        "update();",
        "})();",
        "</script>"
      ),
      "```",
      ""
    )
  }

  if (has_step_images) {
    lines <- c(
      lines,
      "```{=html}",
      "<script>document.addEventListener('DOMContentLoaded', function(){ if (window.GLightbox) { window.GLightbox({ selector: '.glightbox', touchNavigation: true, loop: true }); } });</script>",
      "```",
      ""
    )
  }

  lines <- c(
    lines,
    "```{=html}",
    "<div id=\"recipe-timer-dock\" class=\"recipe-timer-dock d-none\"><strong>Minuteur</strong> <span id=\"recipe-timer-label\"></span> <span id=\"recipe-timer-remaining\"></span> <button id=\"recipe-timer-stop\" class=\"btn btn-sm btn-outline-light\" type=\"button\">Arrêter</button></div>",
    "<script>(function(){",
    "let timer=null, target=0, audioCtx=null, alarmInterval=null, alarmTimeout=null;",
    "const pad=(n)=>String(n).padStart(2,'0');",
    "const fmt=(s)=>{const h=Math.floor(s/3600), m=Math.floor((s%3600)/60), sec=s%60; return h>0?`${h}:${pad(m)}:${pad(sec)}`:`${m}:${pad(sec)}`;};",
    "const ensureAudio=async()=>{const Ctx=window.AudioContext||window.webkitAudioContext; if(!Ctx) return null; if(!audioCtx) audioCtx=new Ctx(); if(audioCtx.state==='suspended'){try{await audioCtx.resume();}catch(e){}} return audioCtx;};",
    "const stopAlarm=()=>{if(alarmInterval){clearInterval(alarmInterval); alarmInterval=null;} if(alarmTimeout){clearTimeout(alarmTimeout); alarmTimeout=null;}};",
    "const playBeep=(ctx,freq)=>{const t=ctx.currentTime+0.01; const osc=ctx.createOscillator(); const gain=ctx.createGain(); osc.type='sawtooth'; osc.frequency.value=freq; gain.gain.setValueAtTime(0.0001,t); gain.gain.exponentialRampToValueAtTime(0.42,t+0.015); gain.gain.exponentialRampToValueAtTime(0.0001,t+0.19); osc.connect(gain); gain.connect(ctx.destination); osc.start(t); osc.stop(t+0.2);};",
    "const startAlarm=async()=>{const ctx=await ensureAudio(); if(!ctx) return; stopAlarm(); const notes=[1760,1480,2093,1760,2349,1760]; let i=0; const ring=()=>{playBeep(ctx,notes[i%notes.length]); i+=1;}; ring(); alarmInterval=setInterval(ring,250); alarmTimeout=setTimeout(stopAlarm,15000);};",
    "const dock=document.getElementById('recipe-timer-dock'); const rem=document.getElementById('recipe-timer-remaining'); const lab=document.getElementById('recipe-timer-label'); const stop=document.getElementById('recipe-timer-stop');",
    "const hide=()=>{if(timer){clearInterval(timer); timer=null;} stopAlarm(); if(dock) dock.classList.add('d-none');};",
    "document.addEventListener('click',async(e)=>{const b=e.target.closest('.recipe-timer-btn'); if(!b) return; const secs=parseInt(b.dataset.seconds||'0',10); if(!secs||secs<1) return; await ensureAudio(); stopAlarm(); if(timer) clearInterval(timer); target=Date.now()+secs*1000; if(lab) lab.textContent=b.textContent.replace('⏱️','').trim(); if(dock) dock.classList.remove('d-none'); timer=setInterval(()=>{const left=Math.max(0, Math.round((target-Date.now())/1000)); if(rem) rem.textContent=fmt(left); if(left<=0){if(timer){clearInterval(timer); timer=null;} if(rem) rem.textContent='Terminé!'; if(dock) dock.classList.remove('d-none'); startAlarm();}},250);});",
    "if(stop) stop.addEventListener('click', hide);",
    "})();</script>",
    "```",
    ""
  )

  lines <- c(
    lines,
    "```{=html}",
    "<script>(function(){",
    "const pageKey='recipe_check_state:'+window.location.pathname;",
    "const save=()=>{const state={}; document.querySelectorAll('.recipe-check').forEach((cb,i)=>{state[i]=cb.checked?1:0;}); localStorage.setItem(pageKey, JSON.stringify(state));};",
    "const load=()=>{try{const s=JSON.parse(localStorage.getItem(pageKey)||'{}'); document.querySelectorAll('.recipe-check').forEach((cb,i)=>{cb.checked=!!s[i];});}catch(e){}};",
    "const reset=()=>{document.querySelectorAll('.recipe-check').forEach(cb=>cb.checked=false); save();};",
    "const textFromLi=(li)=>{const label=li.querySelector('.ingredient-label'); return label?label.innerText.replace(/\\s+/g,' ').trim():li.innerText.replace(/\\s+/g,' ').trim();};",
    "const copyGroceries=async()=>{const items=[...document.querySelectorAll('.recipe-grocery-list .recipe-ingredient')].filter(li=>!(li.querySelector('.recipe-ingredient-check')||{}).checked).map(textFromLi).filter(Boolean); const txt=items.length?items.map(x=>'- '+x).join('\\n'):'(Aucun ingrédient restant)'; const fb=document.getElementById('grocery-copy-feedback'); try{if(navigator.clipboard&&navigator.clipboard.writeText){await navigator.clipboard.writeText(txt);} else {window.prompt('Copie la liste:', txt);} if(fb) fb.textContent='Liste copiée.';}catch(e){window.prompt('Copie la liste:', txt); if(fb) fb.textContent='Liste prête à copier.';}};",
    "document.addEventListener('DOMContentLoaded',()=>{load(); document.querySelectorAll('.recipe-check').forEach(cb=>cb.addEventListener('change',save)); const c=document.getElementById('copy-grocery-list'); if(c) c.addEventListener('click',copyGroceries); const r=document.getElementById('reset-recipe-checks'); if(r) r.addEventListener('click',reset);});",
    "})();</script>",
    "```",
    ""
  )

  # ---- Comments / Notes ----
  comments_norm <- normalize_comments(recipe$commentaires)

  if (length(comments_norm) > 0) {
    lines <- c(lines, "## Notes", "")

    # Summary: stars + counts
    avg <- mean(vapply(comments_norm, function(x) if (!is.null(x$evaluation)) x$evaluation else NA_real_, numeric(1)), na.rm = TRUE)
    if (is.nan(avg)) avg <- NA_real_
    stars <- if (is.na(avg)) "" else stars_string(avg)

    n_total <- length(comments_norm)
    n_alex <- count_by_author(comments_norm, "Alexandre Parent")
    n_elodie <- count_by_author(comments_norm, "Élodie Bourgeois")

    summary_parts <- c()
    if (stars != "") summary_parts <- c(summary_parts, paste0(stars, " (moyenne ", format(avg, digits = 2), "/5)"))
    summary_parts <- c(summary_parts, paste0(n_total, " commentaire(s)"))
    summary_parts <- c(summary_parts, paste0("Alexandre Parent: ", n_alex))
    summary_parts <- c(summary_parts, paste0("Élodie Bourgeois: ", n_elodie))

    lines <- c(lines, paste0("_", paste(summary_parts, collapse = " · "), "_"), "")

    # Each comment
    for (cmt in comments_norm) {
      lines <- c(lines, paste0("- ", format_comment_line(cmt)))
    }
  }

  writeLines(enc2utf8(lines), qmd_path)
  invisible(qmd_path)
}

# --- Comment helpers ----------------------------------------------------------

normalize_comments <- function(commentaires) {
  if (is.null(commentaires) || length(commentaires) == 0) return(list())

  # Old format: vector of strings
  if (is.character(commentaires)) {
    return(lapply(commentaires, function(s) list(commentaire = s, evaluation = NULL, auteur = NULL, date = NULL)))
  }

  # Some YAML parsers may give a list with unnamed entries
  if (is.list(commentaires)) {
    out <- list()
    for (i in seq_along(commentaires)) {
      x <- commentaires[[i]]
      if (is.character(x)) {
        out[[length(out) + 1]] <- list(commentaire = x, evaluation = NULL, auteur = NULL, date = NULL)
      } else if (is.list(x)) {
        raw_evaluation <- if (!is.null(x$evaluation)) x$evaluation else x$note
        raw_author <- if (!is.null(x$auteur)) x$auteur else x$nom
        out[[length(out) + 1]] <- list(
          commentaire = if (!is.null(x$commentaire)) x$commentaire else NULL,
          evaluation = if (!is.null(raw_evaluation)) as.numeric(raw_evaluation) else NULL,
          auteur = if (!is.null(raw_author)) raw_author else NULL,
          date = if (!is.null(x$date)) x$date else NULL
        )
      }
    }
    return(out)
  }

  list()
}

stars_string <- function(avg) {
  # round to nearest integer for display
  n <- round(avg)
  n <- max(0, min(5, n))
  paste0(strrep("★", n), strrep("☆", 5 - n))
}

strip_accents <- function(x) {
  # best effort: convert to ASCII
  iconv(x, from = "", to = "ASCII//TRANSLIT")
}

norm_name <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return("")
  x <- strip_accents(x)
  x <- stringr::str_to_lower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

count_by_author <- function(comments, author) {
  a <- norm_name(author)
  sum(vapply(comments, function(cmt) norm_name(cmt$auteur) == a, logical(1)))
}

format_comment_line <- function(cmt) {
  parts <- c()

  if (!is.null(cmt$evaluation) && !is.na(cmt$evaluation)) {
    ev <- as.integer(cmt$evaluation)
    if (!is.na(ev) && ev >= 1 && ev <= 5) {
      parts <- c(parts, paste0(strrep("★", ev), strrep("☆", 5 - ev)))
    }
  }

  if (!is.null(cmt$auteur) && cmt$auteur != "") parts <- c(parts, cmt$auteur)
  if (!is.null(cmt$date) && cmt$date != "") parts <- c(parts, cmt$date)

  prefix <- if (length(parts) > 0) paste0("[", paste(parts, collapse = " · "), "] ") else ""
  txt <- if (!is.null(cmt$commentaire) && cmt$commentaire != "") cmt$commentaire else "(sans texte)"
  paste0(prefix, txt)
}

#' Regenerate all recipe QMDs from YAMLs
#' @export
regenerate_recipe_qmds <- function(recipes_dir = "recettes", pattern = "\\.ya?ml$") {
  yaml_files <- list.files(recipes_dir, pattern = pattern, full.names = TRUE)
  yaml_files <- yaml_files[!grepl("template\\.ya?ml$", yaml_files, ignore.case = TRUE)]

  for (y in yaml_files) {
    yaml_recipe_to_qmd(y)
  }

  invisible(yaml_files)
}
