# jack-davison.github.io

My personal website, built with [Quarto](https://quarto.org) and published to GitHub Pages at <https://jack-davison.github.io/>.

## Where things live

| Path | What it is |
|---|---|
| `index.qmd` | Home page: hero, intro, download stats, skills, selected work, experience |
| `work.qmd` | Work page: the full work list plus publications |
| `work.yaml` | Every work entry. `featured: "yes"` also shows it under Selected work on the home page |
| `img/` | Thumbnails for the work entries (keep them around 800px wide) |
| `references.bib` | Publications listed on the Work page |
| `style/` | Theme: `light.scss` and `dark.scss` set the colours and fonts, `styles.css` the layout |
| `404.qmd` | Page shown for broken links |
| `_scripts/cran-downloads.ts` | Runs before each render and writes CRAN download counts to `_variables.yml` |
| `_scripts/publications.lua` | Sorts the publications newest first and puts my name in bold |

## Building

```sh
quarto preview   # live preview while editing
quarto render    # full build into docs/ (not committed)
```

No R is needed. The pre-render script uses the Deno runtime bundled with Quarto.

## Publishing

Pushing to `main` runs `.github/workflows/publish.yml`, which renders the site and publishes it to the `gh-pages` branch. The same workflow also runs every Monday so the download counts stay current.
