# =============================================================================
# Film Neighbours: a Shiny front end for the MovieLens tag-genome recommender
#
# Save this file as app.R in its own folder, then run:
#   shiny::runApp("path/to/that/folder")
#
# Packages:
#   install.packages(c("shiny", "bslib", "tidyverse", "ggrepel", "DT", "jsonlite"))
#
# Data: point data_dir (below) at the folder film_neighbors.R already downloaded
# into, so nothing is downloaded twice. The first launch builds genome_cache.rds
# in that folder; every launch after that just loads the cache.
#
# Your seeds / vetoes / skips and settings save automatically to
# film_lists.json next to this file, and reload the next time you open the app.
#
# Sections
#   1. SETTINGS    data folder and starting lists
#   2. DATA        download, load and cache the genome
#   3. ENGINE      tag merging, recommend(), diagnostics
#   4. PLOTS       map and pulls heatmap
#   5. LISTS       loading and saving your lists
#   6. UI
#   7. SERVER
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)


# =============================================================================
# 1. SETTINGS
# =============================================================================

# Folder holding ml-25m.zip / ml-25m/. Relative paths are relative to this
# app's folder. e.g. data_dir <- "~/Documents/films/data"
data_dir <- "data"

# Where your lists are saved
lists_file <- "film_lists.json"

# Starting lists, used only on the very first launch (before film_lists.json
# exists). Typed the same way as in film_neighbors.R.
default_seeds <- c(
  "The Perks of Being a Wallflower (2012)", "Juno (2007)", "Moonrise Kingdom (2012)",
  "The Dangerous Lives of Altar Boys (2002)", "The Blue Lagoon (1980)", "Saved! (2004)",
  "Fast Times at Ridgemont High", "Almost Famous (2000)", "Romeo and Juliet (1968)",
  "My Girl (1991)"
)
default_vetoes <- c(
  "American Pie (1999)", "American Pie 2", "American Wedding", "EuroTrip",
  "Road Trip (2000)", "Superbad", "Porky's", "Sex Drive", "The Girl Next Door (2004)",
  "Not Another Teen Movie", "The Inbetweeners Movie", "Eighth Grade"
)
default_skips <- c(
  "Bridge to Terabithia (2007)", "The Fall (2006)", "Igby Goes Down", "Donnie Darko",
  "Little Miss Sunshine", "Garden State", "Dazed and Confused", "High Fidelity",
  "Harold and Maude", "The Squid and the Whale"
)
default_settings <- list(gamma = 0.25, veto_mode = "protect", n = 15, threshold = 0.76)


# =============================================================================
# 2. DATA  (download once, then cached)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

load_genome <- function(data_dir) {
  cache <- file.path(data_dir, "genome_cache.rds")
  if (file.exists(cache)) {
    message("Loading cached genome from ", cache)
    return(readRDS(cache))
  }

  zip_url  <- "https://files.grouplens.org/datasets/movielens/ml-25m.zip"
  zip_path <- file.path(data_dir, "ml-25m.zip")
  ml_dir   <- file.path(data_dir, "ml-25m")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  needed <- c("genome-scores.csv", "genome-tags.csv", "movies.csv")
  if (!all(file.exists(file.path(ml_dir, needed)))) {
    if (!file.exists(zip_path)) {
      message("Downloading MovieLens 25M (~250 MB)...")
      old_timeout <- getOption("timeout")
      options(timeout = max(1800, old_timeout))
      part_path <- paste0(zip_path, ".part")
      tryCatch({
        download.file(zip_url, part_path, mode = "wb")
        file.rename(part_path, zip_path)
      }, finally = {
        options(timeout = old_timeout)
        if (file.exists(part_path)) file.remove(part_path)
      })
    }
    unzip(zip_path, files = file.path("ml-25m", needed), exdir = data_dir)
  }

  message("Building the genome matrix (first launch only)...")
  scores <- read_csv(file.path(ml_dir, "genome-scores.csv"),
                     col_types = cols(movieId = col_integer(), tagId = col_integer(),
                                      relevance = col_double()))
  tags   <- read_csv(file.path(ml_dir, "genome-tags.csv"),
                     col_types = cols(tagId = col_integer(), tag = col_character()))
  movies <- read_csv(file.path(ml_dir, "movies.csv"),
                     col_types = cols(movieId = col_integer(), title = col_character(),
                                      genres = col_character()))

  wide <- scores |>
    left_join(tags, by = "tagId") |>
    select(-tagId) |>
    pivot_wider(names_from = tag, values_from = relevance)
  genome_mat <- as.matrix(select(wide, -movieId))
  rownames(genome_mat) <- wide$movieId

  # Raw tag correlations across films and the merge tree, computed once
  tag_cor_raw <- suppressWarnings(cor(genome_mat))
  tag_cor_raw[is.na(tag_cor_raw)] <- 0
  diag(tag_cor_raw) <- 1
  hc <- hclust(as.dist(1 - tag_cor_raw), method = "complete")

  G <- list(genome_mat = genome_mat, movies = movies, tag_cor_raw = tag_cor_raw,
            hc = hc, tag_means = colMeans(genome_mat), ml_dir = ml_dir, zip_path = zip_path)
  saveRDS(G, cache)
  message("Cached to ", cache)
  G
}

G <- load_genome(data_dir)
message("Genome: ", nrow(G$genome_mat), " films x ", ncol(G$genome_mat), " tags")

# Rating counts per film: optional, read from ratings.csv once, then cached
rating_counts <- local({
  mem <- NULL
  function() {
    if (!is.null(mem)) return(mem)
    cache <- file.path(data_dir, "rating_counts.rds")
    if (file.exists(cache)) return(mem <<- readRDS(cache))
    path <- file.path(G$ml_dir, "ratings.csv")
    if (!file.exists(path)) unzip(G$zip_path, files = "ml-25m/ratings.csv", exdir = data_dir)
    mem <<- read_csv(path, col_types = cols_only(movieId = col_integer()), progress = FALSE) |>
      count(movieId, name = "n_ratings")
    saveRDS(mem, cache)
    mem
  }
})

# ---- Title catalogue ----
# "Science of Sleep, The (La science des rêves) (2006)" -> "The Science of Sleep (2006)"
articles <- "The|A|An|Le|La|Les|L'|Il|El|Los|Las|Der|Die|Das|Den|Det"

clean_main_title <- function(x) {
  x |>
    str_remove("\\s*\\(\\d{4}\\)\\s*$") |>
    str_remove("\\s+\\([^()]*\\)$") |>
    str_replace(str_c("^(.*), (", articles, ")$"), "\\2 \\1") |>
    str_replace("^L' ", "L'")
}

normalise_title <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

genome_ids <- as.integer(rownames(G$genome_mat))

catalogue <- G$movies |>
  mutate(
    year      = as.integer(str_match(title, "\\((\\d{4})\\)\\s*$")[, 2]),
    main      = clean_main_title(title),
    display   = if_else(is.na(year), main, str_c(main, " (", year, ")")),
    key       = normalise_title(main),
    in_genome = movieId %in% genome_ids
  )

display_of <- function(ids) catalogue$display[match(ids, catalogue$movieId)]
main_of    <- function(ids) catalogue$main[match(ids, catalogue$movieId)]

# Choices for the film pickers: label = title, value = movieId
film_choices <- with(filter(catalogue, in_genome) |> arrange(display),
                     setNames(as.character(movieId), display))

# Typed titles -> movieIds (used for the starting lists). Ambiguous or unknown
# titles are reported and skipped.
match_titles <- function(titles) {
  ids <- integer(0)
  for (t in str_squish(titles)) {
    year <- as.integer(str_match(t, "\\((\\d{4})\\)$")[, 2])
    key  <- normalise_title(clean_main_title(t))
    hits <- catalogue |> filter(key == !!key, is.na(!!year) | year == !!year, in_genome)
    if (nrow(hits) == 1) ids <- c(ids, hits$movieId)
    else message("Starting lists: couldn't place \"", t, "\" (",
                 if (nrow(hits) == 0) "no match" else "ambiguous, add the year", ")")
  }
  unique(ids)
}


# =============================================================================
# 3. ENGINE
# =============================================================================

# ---- The tag space at a given merge threshold ----
# Tags correlated at r >= threshold across films are averaged into one column,
# so a concept gets one vote however many ways users spelled it. Complete
# linkage: every pair inside a group clears the threshold. 1 = no merging.
# Stores unit-length rows plus each film's length, which is all the engine needs.
build_space <- function(threshold) {
  M <- G$genome_mat
  if (threshold >= 1) {
    mat    <- M
    groups <- tibble(group = character(0), n_tags = integer(0), tags = character(0))
  } else {
    cluster  <- cutree(G$hc, h = 1 - threshold)
    tag_info <- tibble(tag = colnames(M), cluster = cluster, mean_rel = G$tag_means,
                       position = seq_along(cluster)) |>
      group_by(cluster) |>
      arrange(desc(mean_rel), .by_group = TRUE) |>
      summarise(n_tags = n(), tags = list(tag), position = min(position), .groups = "drop") |>
      arrange(position) |>
      mutate(group = map2_chr(tags, n_tags, \(t, k) {
        if (k <= 3) str_c(t, collapse = " / ")
        else str_c(str_c(t[1:2], collapse = " / "), " (+", k - 2, ")")
      }))
    mat <- vapply(tag_info$tags, \(t) rowMeans(M[, t, drop = FALSE]), numeric(nrow(M)))
    colnames(mat) <- tag_info$group
    rownames(mat) <- rownames(M)
    groups <- tag_info |>
      filter(n_tags > 1) |>
      transmute(group, n_tags, tags = map_chr(tags, str_c, collapse = ", ")) |>
      arrange(desc(n_tags))
  }

  # Centre each tag (value = more / less than the average film), then unit rows
  X   <- sweep(mat, 2, colMeans(mat))
  len <- sqrt(rowSums(X^2))
  list(X_unit = X / len, len = len, ids = as.integer(rownames(X)), groups = groups,
       n_tags = ncol(mat), threshold = threshold, median_len = median(len))
}

# Centred (not unit) rows for some films
x_rows <- function(sp, ids) {
  ids <- as.character(ids)
  sp$X_unit[ids, , drop = FALSE] * sp$len[ids]
}

# ---- recommend() ----
# Query = centroid of the seeds, pushed away from the vetoes.
# veto_mode "protect" only pushes down tags the vetoes are above average on AND
# the seeds don't favour; "full" is classic Rocchio (subtract the whole film).
recommend <- function(sp, seed_ids, veto_ids = integer(0), skip_ids = integer(0),
                      gamma = 0.25, n = 15, veto_mode = "protect") {
  seed_ids <- intersect(seed_ids, sp$ids)
  veto_ids <- intersect(veto_ids, sp$ids)
  skip_ids <- intersect(skip_ids, sp$ids)
  sid <- as.character(seed_ids)

  centroid <- colMeans(x_rows(sp, seed_ids))
  query    <- centroid
  note     <- NULL
  if (length(veto_ids) > 0) {
    veto_vec <- colMeans(x_rows(sp, veto_ids))
    if (veto_mode == "protect") {
      veto_vec <- pmax(veto_vec, 0)
      veto_vec[centroid > 0] <- 0
      if (all(veto_vec == 0)) {
        note <- "Vetoes had nothing to push on: every tag they're strong on, your seeds are too."
      }
    }
    query <- centroid - gamma * veto_vec
  }
  q_unit <- query / sqrt(sum(query^2))

  seed_sims <- sp$X_unit %*% t(sp$X_unit[sid, , drop = FALSE])
  results <- tibble(
    movieId      = sp$ids,
    title        = display_of(sp$ids),
    similarity   = drop(sp$X_unit %*% q_unit),
    nearest_seed = main_of(seed_ids)[max.col(seed_sims, ties.method = "first")]
  )
  picks <- results |>
    filter(!movieId %in% c(seed_ids, veto_ids, skip_ids)) |>
    slice_max(similarity, n = n, with_ties = FALSE) |>
    mutate(rank = row_number(), .before = 1)

  list(picks = picks, results = results, centroid = centroid, query = query,
       q_unit = q_unit, seed_ids = seed_ids, veto_ids = veto_ids, skip_ids = skip_ids,
       pick_ids = picks$movieId, note = note, gamma = gamma, veto_mode = veto_mode)
}

query_tags <- function(rec, n = 15) {
  tibble(tag = names(rec$query), above_average = rec$query) |>
    slice_max(above_average, n = n)
}

pushed_tags <- function(rec, n = 10) {
  tibble(tag = names(rec$query), shift = rec$query - rec$centroid) |>
    filter(shift < 0) |>
    slice_min(shift, n = n, with_ties = FALSE)
}

# Cosine similarity splits additively by tag: contributions sum to the score
explain_film <- function(sp, rec, id, n = 12) {
  id <- as.character(id)
  contrib <- sp$X_unit[id, ] * rec$q_unit
  list(similarity = sum(contrib), length = unname(sp$len[id]),
       table = tibble(tag = names(contrib), contribution = contrib) |>
         slice_max(contribution, n = n))
}

explain_pair <- function(sp, a, b, n = 12) {
  contrib <- sp$X_unit[as.character(a), ] * sp$X_unit[as.character(b), ]
  list(similarity = sum(contrib),
       table = tibble(tag = names(contrib), contribution = contrib) |>
         slice_max(contribution, n = n))
}

# Seeds as items, centroid as total score:
#   similarity      uncorrected item-total (each seed is part of the centroid)
#   loo_similarity  corrected: vs. the centroid of the OTHER seeds
#   vector_length   how strongly the genome characterises the film
seed_report <- function(sp, rec, ratings = FALSE) {
  sid <- as.character(rec$seed_ids)
  loo <- vapply(sid, function(s) {
    others <- setdiff(sid, s)
    if (length(others) == 0) return(NA_real_)
    c_loo <- colMeans(x_rows(sp, others))
    sum(sp$X_unit[s, ] * c_loo) / sqrt(sum(c_loo^2))
  }, numeric(1))
  pair <- sp$X_unit[sid, , drop = FALSE] %*% t(sp$X_unit[sid, , drop = FALSE])

  out <- tibble(
    movieId        = rec$seed_ids,
    title          = display_of(rec$seed_ids),
    similarity     = rec$results$similarity[match(rec$seed_ids, rec$results$movieId)],
    loo_similarity = loo,
    vector_length  = unname(sp$len[sid])
  )
  if (ratings) out <- left_join(out, rating_counts(), by = "movieId")
  list(table = arrange(out, desc(loo_similarity)),
       mean_pair = if (length(sid) > 1) mean(pair[upper.tri(pair)]) else NA_real_)
}

# ---- Map layout: PCA of just the films on the map (+ the query) ----
map_layout <- function(sp, rec, n_context = 40) {
  shown <- c(rec$seed_ids, rec$pick_ids, rec$veto_ids)
  context_ids <- rec$results |>
    filter(!movieId %in% shown) |>
    slice_max(similarity, n = n_context, with_ties = FALSE) |>
    pull(movieId)

  films <- tibble(
    movieId = c(rec$seed_ids, rec$pick_ids, rec$veto_ids, context_ids),
    role    = rep(c("Seed", "Pick", "Veto", "Context"),
                  c(length(rec$seed_ids), length(rec$pick_ids),
                    length(rec$veto_ids), length(context_ids)))
  )
  ids <- as.character(films$movieId)

  U  <- rbind(sp$X_unit[ids, , drop = FALSE], query = rec$q_unit)
  pc <- prcomp(U, center = TRUE, scale. = FALSE, rank. = 2)
  pad <- function(v) range(v) + c(-1, 1) * 0.05 * diff(range(v))

  list(films = films, ids = ids, context_ids = context_ids,
       center = pc$center, loadings = pc$rotation, points = pc$x,
       kept = sum(pc$sdev[1:2]^2) / sum(pc$sdev^2),
       xlim = pad(pc$x[, 1]), ylim = pad(pc$x[, 2]))
}

# Every film scored on the map's two axes.
# quality = share of the film's distance from the map's centre that lies along
# these two axes (cos^2). Low = the axes say little about it.
project_catalogue <- function(sp, lay) {
  ctr   <- lay$center
  S     <- sweep(sp$X_unit %*% lay$loadings, 2, drop(ctr %*% lay$loadings))
  total <- pmax(1 - 2 * drop(sp$X_unit %*% ctr) + sum(ctr^2), 1e-12)
  q     <- lay$points["query", ]
  tibble(
    movieId  = sp$ids,
    x        = S[, 1],
    y        = S[, 2],
    quality  = (S[, 1]^2 + S[, 2]^2) / total,
    dist_2d  = sqrt((S[, 1] - q[1])^2 + (S[, 2] - q[2])^2),
    in_frame = between(S[, 1], lay$xlim[1], lay$xlim[2]) &
               between(S[, 2], lay$ylim[1], lay$ylim[2])
  )
}

map_frame <- function(sp, rec, lay) {
  project_catalogue(sp, lay) |>
    filter(in_frame) |>
    mutate(
      title      = display_of(movieId),
      role       = coalesce(lay$films$role[match(movieId, lay$films$movieId)], "New"),
      similarity = rec$results$similarity[match(movieId, rec$results$movieId)],
      skipped    = movieId %in% rec$skip_ids
    ) |>
    arrange(dist_2d) |>
    select(movieId, title, role, dist_2d, quality, similarity, skipped)
}


# =============================================================================
# 4. PLOTS
# =============================================================================

viz <- list(
  surface  = "#fcfcfb",
  ink      = "#0b0b0b",
  ink_2    = "#52514e",
  muted    = "#b4b3ad",
  faint    = "#dcdbd5",
  seed     = "#2a78d6",
  pick     = "#eb6834",
  veto     = "#1baf7a",
  div_low  = "#e34948",
  div_mid  = "#f0efec",
  div_high = "#2a78d6"
)

theme_films <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background      = element_rect(fill = viz$surface, colour = NA),
      panel.background     = element_rect(fill = viz$surface, colour = NA),
      panel.grid           = element_blank(),
      text                 = element_text(colour = viz$ink),
      axis.text            = element_text(colour = viz$ink_2, size = 10),
      axis.title           = element_text(colour = viz$ink_2, size = 10),
      plot.title           = element_text(face = "bold", size = 14),
      plot.subtitle        = element_text(colour = viz$ink_2, size = 10),
      plot.title.position  = "plot",
      legend.position      = "top",
      legend.justification = "left",
      legend.text          = element_text(colour = viz$ink_2)
    )
}

axis_poles <- function(loadings, k = 3) {
  short <- \(t) str_remove(t, " / .*$")
  low  <- short(names(sort(loadings))[1:k])
  high <- short(names(sort(loadings, decreasing = TRUE))[1:k])
  str_c("← ", str_c(low, collapse = ", "), "        ·        ",
        str_c(high, collapse = ", "), " →")
}

plot_map <- function(sp, rec, lay, label_width = 28,
                     catalogue = FALSE, min_quality = 0.25, n_new = 12) {
  P <- lay$points
  pts <- lay$films |>
    mutate(x = P[lay$ids, 1], y = P[lay$ids, 2],
           role = factor(role, levels = c("Seed", "Pick", "Veto", "Context")))
  marks <- filter(pts, role != "Context") |> droplevels()

  labels <- marks |>
    transmute(x, y, label = str_trunc(main_of(movieId), label_width),
              face = "plain", ink = viz$ink) |>
    add_row(x = P["query", 1], y = P["query", 2], label = "query",
            face = "bold", ink = viz$ink)

  extra <- tibble(x = numeric(0), y = numeric(0))
  near  <- extra
  if (catalogue) {
    extra <- project_catalogue(sp, lay) |>
      filter(in_frame, quality >= min_quality, !movieId %in% lay$films$movieId)
    near <- slice_min(extra, dist_2d, n = n_new, with_ties = FALSE)
    labels <- bind_rows(labels, near |>
      transmute(x, y, label = str_trunc(main_of(movieId), label_width),
                face = "italic", ink = viz$ink_2))
  }

  subtitle <- str_c(
    "Closer = more alike. Axes are the two main directions of difference among these films (PCA), ",
    "capturing ~", round(100 * lay$kept), "% of it.\nGrey = the next ",
    length(lay$context_ids), " nearest films.",
    if (catalogue) str_c(" Faint dots = ", nrow(extra), " more films in frame with quality ≥ ",
                         min_quality, "; italic = the ", nrow(near), " nearest the query.") else ""
  )

  ggplot(pts, aes(x, y)) +
    geom_point(data = extra, shape = 16, size = 0.8, colour = viz$faint) +
    geom_point(data = filter(pts, role == "Context"), shape = 21, size = 1.8,
               fill = viz$muted, colour = viz$surface, stroke = 0.4) +
    geom_point(data = near, shape = 21, size = 2.2,
               fill = viz$surface, colour = viz$ink_2, stroke = 0.6) +
    geom_point(data = marks, aes(fill = role, shape = role),
               size = 3.4, colour = viz$surface, stroke = 0.7) +
    annotate("point", x = P["query", 1], y = P["query", 2],
             shape = 8, size = 4, colour = viz$ink, stroke = 1) +
    ggrepel::geom_text_repel(
      data = labels, aes(label = label, fontface = face, colour = ink),
      size = 3.3, seed = 1, max.overlaps = Inf,
      box.padding = 0.4, point.padding = 0.25, min.segment.length = 0.2,
      segment.colour = viz$muted, segment.size = 0.3
    ) +
    scale_colour_identity() +
    scale_fill_manual(values = c(Seed = viz$seed, Pick = viz$pick, Veto = viz$veto), name = NULL) +
    scale_shape_manual(values = c(Seed = 21, Pick = 24, Veto = 25), name = NULL) +
    coord_equal(xlim = lay$xlim, ylim = lay$ylim, expand = FALSE) +
    labs(title = "Seeds, picks and the neighbourhood around them", subtitle = subtitle,
         x = axis_poles(lay$loadings[, 1]), y = axis_poles(lay$loadings[, 2])) +
    theme_films() +
    theme(axis.text = element_blank())
}

plot_pulls <- function(sp, rec, label_width = 34) {
  sid <- as.character(rec$seed_ids)
  pid <- as.character(rec$pick_ids)

  seed_order <- rec$results |>
    filter(movieId %in% rec$seed_ids) |>
    arrange(desc(similarity)) |>
    pull(movieId)
  seed_lab <- str_trunc(main_of(seed_order), label_width)
  pick_lab <- str_c(seq_along(pid), ". ", str_trunc(main_of(rec$pick_ids), label_width))
  sims <- sp$X_unit[pid, , drop = FALSE] %*% t(sp$X_unit[sid, , drop = FALSE])

  cells <- expand_grid(p = seq_along(pid), s = seq_along(sid)) |>
    mutate(similarity = sims[cbind(p, s)],
           pick = factor(pick_lab[p], levels = rev(pick_lab)),
           seed = factor(str_trunc(main_of(rec$seed_ids[s]), label_width), levels = seed_lab))
  strongest <- cells |> group_by(pick) |> slice_max(similarity, n = 1, with_ties = FALSE)
  lim <- max(abs(cells$similarity))

  ggplot(cells, aes(seed, pick, fill = similarity)) +
    geom_tile(colour = viz$surface, linewidth = 0.8) +
    geom_point(data = strongest, shape = 21, size = 1.8,
               fill = viz$surface, colour = viz$ink, stroke = 0.4) +
    scale_fill_gradient2(low = viz$div_low, mid = viz$div_mid, high = viz$div_high,
                         midpoint = 0, limits = c(-lim, lim), name = "Similarity") +
    scale_x_discrete(position = "top") +
    labs(title = "Which seeds each pick draws from",
         subtitle = str_c("Blue = alike, grey = unrelated, red = opposite. ",
                          "Dot = each pick's strongest pull. Seeds run from most to least central."),
         x = NULL, y = NULL) +
    theme_films() +
    theme(axis.text.x.top = element_text(angle = 40, hjust = 0, vjust = 0),
          legend.position = "right", legend.justification = "center",
          legend.title = element_text(colour = viz$ink_2, size = 10))
}


# =============================================================================
# 5. LISTS  (saved to film_lists.json)
# =============================================================================

load_lists <- function() {
  if (file.exists(lists_file)) {
    s <- jsonlite::read_json(lists_file, simplifyVector = TRUE)
    keep <- \(x) intersect(as.integer(x %||% integer(0)), genome_ids)
    return(list(seeds = keep(s$seeds), vetoes = keep(s$vetoes), skips = keep(s$skips),
                gamma = s$gamma %||% default_settings$gamma,
                veto_mode = s$veto_mode %||% default_settings$veto_mode,
                n = s$n %||% default_settings$n,
                threshold = s$threshold %||% default_settings$threshold))
  }
  c(list(seeds = match_titles(default_seeds), vetoes = match_titles(default_vetoes),
         skips = match_titles(default_skips)), default_settings)
}

save_lists <- function(state) {
  jsonlite::write_json(
    c(state, list(seed_titles = display_of(state$seeds), veto_titles = display_of(state$vetoes),
                  skip_titles = display_of(state$skips))),
    lists_file, auto_unbox = TRUE, pretty = TRUE)
}

# The lists as R code, ready to paste into film_neighbors.R
lists_as_r <- function(ids, name) {
  if (length(ids) == 0) return(str_c(name, " <- character(0)"))
  t <- str_replace_all(display_of(ids), '"', '\\\\"')
  str_c(name, " <- c(\n", str_c('  "', t, '"', collapse = ",\n"), "\n)")
}


# =============================================================================
# 6. UI
# =============================================================================

film_picker <- function(id, label, placeholder) {
  selectizeInput(id, label, choices = NULL, multiple = TRUE, width = "100%",
                 options = list(placeholder = placeholder, plugins = list("remove_button")))
}

add_buttons <- function(prefix) {
  div(class = "d-flex flex-wrap gap-2 align-items-center my-2",
      span(class = "text-muted small", "Select rows in the table, then move them to:"),
      actionButton(str_c(prefix, "_seeds"),  "Seeds",  class = "btn-sm btn-outline-primary"),
      actionButton(str_c(prefix, "_vetoes"), "Vetoes", class = "btn-sm btn-outline-success"),
      actionButton(str_c(prefix, "_skips"),  "Skips",  class = "btn-sm btn-outline-secondary"))
}

# Tables size to their content (older DT versions don't have the fill argument)
dt_output <- function(id) {
  if ("fill" %in% names(formals(DT::DTOutput))) DT::DTOutput(id, fill = FALSE)
  else DT::DTOutput(id)
}

# A function, so every page load reads the latest saved settings
ui <- function(request) {
  init <- load_lists()
  page_sidebar(
    title = "Film neighbours",
    fillable = FALSE,
    theme = bs_theme(version = 5, bg = viz$surface, fg = viz$ink, primary = viz$seed,
                     success = "#148a60", "font-size-base" = "0.95rem"),

    sidebar = sidebar(
      width = 380,
      film_picker("seeds",  "Seeds: films that define the taste", "Type a title..."),
      film_picker("vetoes", "Vetoes: push the query away from", "Type a title..."),
      film_picker("skips",  "Skips: seen it, or not interested", "Type a title..."),
      sliderInput("gamma", "Veto strength (gamma)", min = 0, max = 1, step = 0.05,
                  value = init$gamma),
      radioButtons("veto_mode", "Veto mode",
                   choices = c("Protect seeds' tags" = "protect", "Classic Rocchio" = "full"),
                   selected = init$veto_mode, inline = TRUE),
      sliderInput("n", "Number of picks", min = 5, max = 50, step = 1, value = init$n),
      sliderInput("threshold", "Merge tags correlated at r ≥ (1 = off)",
                  min = 0.6, max = 1, step = 0.01, value = init$threshold),
      div(class = "text-muted small", textOutput("saved_status"))
    ),

    navset_card_tab(
      nav_panel(
        "Picks",
        textOutput("summary_line"),
        uiOutput("veto_note"),
        add_buttons("pick"),
        dt_output("picks")
      ),

      nav_panel(
        "Map",
        layout_columns(
          col_widths = c(3, 3, 2, 2, 2),
          checkboxInput("catalogue", "Pour in the rest of the catalogue", FALSE),
          checkboxInput("hide_vetoes", "Leave vetoes off the map", FALSE),
          numericInput("n_context", "Grey context films", 40, min = 0, max = 200, step = 10),
          sliderInput("min_quality", "Min quality", 0, 1, 0.25, step = 0.05),
          numericInput("n_new", "Label nearest", 12, min = 0, max = 50, step = 1)
        ),
        plotOutput("map", height = "780px"),
        downloadButton("map_png", "Save map as PNG", class = "btn-sm")
      ),

      nav_panel(
        "Pulls",
        plotOutput("pulls", height = "auto")
      ),

      nav_panel(
        "In frame",
        p(class = "text-muted small",
          "Every film in the genome scored on the map's two axes, keeping those that land inside ",
          "the frame. Nearest the query first. ",
          strong("quality"), " = how much of a film's position these two axes capture; ",
          "low-quality films only sit near the middle because they score ~0 on both. ",
          "Uses the Map tab's settings (including min quality)."),
        checkboxInput("frame_new_only", "Only films not already on the map", TRUE),
        textOutput("frame_summary"),
        add_buttons("frame"),
        dt_output("frame")
      ),

      nav_panel(
        "Diagnostics",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Tags that define the query"), tableOutput("query_tags")),
          card(card_header("Tags pushed down by the vetoes"), tableOutput("pushed_tags"))
        ),
        card(
          card_header("Seed report"),
          p(class = "text-muted small",
            "loo_similarity = each seed vs. the centroid of the other seeds (corrected item-total). ",
            "Low = the seed doesn't share the group's direction. vector_length = how strongly the ",
            "genome characterises the film; short = shrunk toward average."),
          checkboxInput("with_ratings", "Include MovieLens rating counts (reads ratings.csv once)", FALSE),
          textOutput("seed_summary"),
          tableOutput("seed_report")
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Why is a film where it is?"),
               selectizeInput("explain_id", NULL, choices = NULL, width = "100%",
                              options = list(placeholder = "Pick any film...")),
               textOutput("explain_summary"),
               tableOutput("explain_table")),
          card(card_header("Why are two films close?"),
               selectizeInput("pair_a", NULL, choices = NULL, width = "100%",
                              options = list(placeholder = "First film...")),
               selectizeInput("pair_b", NULL, choices = NULL, width = "100%",
                              options = list(placeholder = "Second film...")),
               textOutput("pair_summary"),
               tableOutput("pair_table"))
        )
      ),

      nav_panel(
        "Tags",
        layout_columns(
          col_widths = c(7, 5),
          card(card_header(textOutput("groups_header", inline = TRUE)), dt_output("tag_groups")),
          card(card_header("Raw tag correlations across films"),
               p(class = "text-muted small",
                 "To choose a merge threshold: set it just below the lowest pair you want merged."),
               selectizeInput("cor_tags", NULL, multiple = TRUE, width = "100%",
                              choices = sort(colnames(G$genome_mat)),
                              selected = intersect(c("coming of age", "coming-of-age", "adolescence",
                                                     "teen", "teens", "teen movie", "teenager"),
                                                   colnames(G$genome_mat)),
                              options = list(plugins = list("remove_button"))),
               div(style = "overflow-x: auto;", tableOutput("tag_cor")))
        )
      ),

      nav_panel(
        "Export",
        p(class = "text-muted small",
          "Your current lists as R code, ready to paste into section 5 of film_neighbors.R."),
        verbatimTextOutput("export")
      )
    )
  )
}


# =============================================================================
# 7. SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---- Fill the pickers from the saved lists ----
  init <- load_lists()
  set_list <- function(id, ids) {
    updateSelectizeInput(session, id, choices = film_choices,
                         selected = as.character(ids), server = TRUE)
  }
  set_list("seeds",  init$seeds)
  set_list("vetoes", init$vetoes)
  set_list("skips",  init$skips)
  for (id in c("explain_id", "pair_a", "pair_b")) {
    updateSelectizeInput(session, id, choices = film_choices, selected = character(0),
                         server = TRUE)
  }

  ids_of <- function(x) as.integer(x %||% character(0))
  seeds  <- reactive(ids_of(input$seeds))
  vetoes <- reactive(ids_of(input$vetoes))
  skips  <- reactive(ids_of(input$skips))

  # Move films into one list, taking them out of the other two
  move_to <- function(target, ids) {
    req(length(ids) > 0)
    lists <- list(seeds = seeds(), vetoes = vetoes(), skips = skips())
    for (nm in names(lists)) {
      lists[[nm]] <- if (nm == target) union(lists[[nm]], ids) else setdiff(lists[[nm]], ids)
      set_list(nm, lists[[nm]])
    }
  }

  # ---- Saving: only once the pickers have loaded the saved lists ----
  # (otherwise the empty pickers at startup would overwrite film_lists.json)
  loaded <- reactiveVal(FALSE)
  observe({
    req(!loaded())
    if (setequal(seeds(), init$seeds) && setequal(vetoes(), init$vetoes) &&
        setequal(skips(), init$skips)) loaded(TRUE)
  })

  state <- reactive(list(seeds = seeds(), vetoes = vetoes(), skips = skips(),
                         gamma = input$gamma, veto_mode = input$veto_mode,
                         n = input$n, threshold = input$threshold))
  state_d <- debounce(state, 800)
  last_saved <- reactiveVal(NULL)
  observeEvent(state_d(), {
    req(loaded())
    save_lists(state_d())
    last_saved(format(Sys.time(), "%H:%M:%S"))
  })
  output$saved_status <- renderText({
    if (is.null(last_saved())) str_c("Lists save automatically to ", lists_file)
    else str_c("Saved to ", lists_file, " at ", last_saved())
  })

  # ---- The space and the recommendation ----
  threshold_d <- debounce(reactive(input$threshold), 800)
  space <- reactive({
    withProgress(message = "Merging tags...", value = 0.5, build_space(threshold_d()))
  })

  rec <- reactive({
    validate(need(length(seeds()) > 0, "Add at least one seed film to get started."))
    clash <- intersect(seeds(), vetoes())
    validate(need(length(clash) == 0,
                  str_c("In both seeds and vetoes: ", str_c(display_of(clash), collapse = ", "))))
    recommend(space(), seeds(), vetoes(), skips(), gamma = input$gamma,
              n = input$n, veto_mode = input$veto_mode)
  })

  # ---- Picks ----
  output$summary_line <- renderText({
    r <- rec()
    str_c(length(r$seed_ids), " seeds | ", length(r$veto_ids), " vetoes (", r$veto_mode,
          ") | ", length(r$skip_ids), " skips | gamma = ", r$gamma, " | ",
          space()$n_tags, " tags")
  })
  output$veto_note <- renderUI({
    note <- rec()$note
    if (!is.null(note)) div(class = "text-warning small mb-2", note)
  })

  dt <- function(df, ...) {
    DT::datatable(df, rownames = FALSE, selection = "multiple",
                  options = list(pageLength = 25, dom = "ftip", ...))
  }

  output$picks <- DT::renderDT({
    rec()$picks |>
      mutate(similarity = round(similarity, 3)) |>
      select(rank, title, similarity, nearest_seed) |>
      dt()
  })
  pick_selected <- reactive(rec()$picks$movieId[input$picks_rows_selected])
  observeEvent(input$pick_seeds,  move_to("seeds",  pick_selected()))
  observeEvent(input$pick_vetoes, move_to("vetoes", pick_selected()))
  observeEvent(input$pick_skips,  move_to("skips",  pick_selected()))

  # ---- Map ----
  map_rec <- reactive({
    r <- rec()
    if (isTRUE(input$hide_vetoes)) r$veto_ids <- integer(0)
    r
  })
  layout_r <- reactive(map_layout(space(), map_rec(), input$n_context %||% 40))

  map_plot <- reactive({
    plot_map(space(), map_rec(), layout_r(), catalogue = input$catalogue,
             min_quality = input$min_quality, n_new = input$n_new %||% 12)
  })
  output$map <- renderPlot(map_plot(), res = 96)
  output$map_png <- downloadHandler(
    filename = function() str_c("film_map_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content  = function(file) {
      ggsave(file, map_plot(), width = 12, height = 10, dpi = 150, bg = viz$surface)
    }
  )

  # ---- Pulls ----
  output$pulls <- renderPlot(plot_pulls(space(), rec()), res = 96,
                             height = function() 220 + 24 * length(rec()$pick_ids))

  # ---- In frame ----
  frame_tbl <- reactive({
    f <- map_frame(space(), map_rec(), layout_r()) |>
      filter(role != "New" | quality >= input$min_quality)
    if (isTRUE(input$frame_new_only)) f <- filter(f, role == "New")
    f
  })
  output$frame_summary <- renderText({
    all <- map_frame(space(), map_rec(), layout_r())
    str_c(nrow(all), " of ", length(space()$ids), " films land in the frame; ",
          sum(all$role == "New" & all$quality >= input$min_quality),
          " new ones reach quality ≥ ", input$min_quality, ".")
  })
  output$frame <- DT::renderDT({
    frame_tbl() |>
      mutate(across(c(dist_2d, quality, similarity), \(v) round(v, 3))) |>
      select(title, role, dist_2d, quality, similarity, skipped) |>
      dt()
  })
  frame_selected <- reactive(frame_tbl()$movieId[input$frame_rows_selected])
  observeEvent(input$frame_seeds,  move_to("seeds",  frame_selected()))
  observeEvent(input$frame_vetoes, move_to("vetoes", frame_selected()))
  observeEvent(input$frame_skips,  move_to("skips",  frame_selected()))

  # ---- Diagnostics ----
  output$query_tags <- renderTable(query_tags(rec()), digits = 3)
  output$pushed_tags <- renderTable({
    validate(need(length(rec()$veto_ids) > 0, "No vetoes."))
    pushed_tags(rec())
  }, digits = 3)

  seed_rep <- reactive({
    if (isTRUE(input$with_ratings)) {
      withProgress(message = "Counting ratings (first time only)...", value = 0.5,
                   seed_report(space(), rec(), ratings = TRUE))
    } else {
      seed_report(space(), rec())
    }
  })
  output$seed_summary <- renderText({
    str_c(length(rec()$seed_ids), " seeds | mean pairwise similarity ",
          round(seed_rep()$mean_pair, 3), " | median film vector length ",
          round(space()$median_len, 2))
  })
  output$seed_report <- renderTable(select(seed_rep()$table, -movieId), digits = 3)

  output$explain_summary <- renderText({
    req(input$explain_id)
    e <- explain_film(space(), rec(), input$explain_id)
    str_c(display_of(as.integer(input$explain_id)), ": similarity to query ",
          round(e$similarity, 3), ", vector length ", round(e$length, 2),
          " (median film ", round(space()$median_len, 2), ")")
  })
  output$explain_table <- renderTable({
    req(input$explain_id)
    explain_film(space(), rec(), input$explain_id)$table
  }, digits = 4)

  pair_ok <- reactive({
    req(input$pair_a, input$pair_b)
    validate(need(input$pair_a != input$pair_b, "Pick two different films."))
    explain_pair(space(), input$pair_a, input$pair_b)
  })
  output$pair_summary <- renderText({
    str_c(display_of(as.integer(input$pair_a)), " vs ", display_of(as.integer(input$pair_b)),
          ": similarity ", round(pair_ok()$similarity, 3))
  })
  output$pair_table <- renderTable(pair_ok()$table, digits = 4)

  # ---- Tags ----
  output$groups_header <- renderText({
    sp <- space()
    str_c(ncol(G$genome_mat), " tags → ", sp$n_tags, " after merging at r ≥ ",
          sp$threshold, " (", nrow(sp$groups), " merged groups)")
  })
  output$tag_groups <- DT::renderDT({
    DT::datatable(space()$groups, rownames = FALSE, selection = "none",
                  options = list(pageLength = 15, dom = "ftip"))
  })
  output$tag_cor <- renderTable({
    req(length(input$cor_tags) > 1)
    m <- round(G$tag_cor_raw[input$cor_tags, input$cor_tags, drop = FALSE], 2)
    as.data.frame(m)
  }, rownames = TRUE)

  # ---- Export ----
  output$export <- renderText({
    str_c(lists_as_r(seeds(), "seed_films"), "\n\n",
          lists_as_r(vetoes(), "veto_films"), "\n\n",
          lists_as_r(skips(), "skip_films"), "\n\n",
          "rec <- recommend(seed_films, veto_films, skip_films, gamma = ", input$gamma,
          ", n = ", input$n, ', veto_mode = "', input$veto_mode, '")')
  })
}

shinyApp(ui, server)
