// Pre-render step: fetch all-time CRAN download counts for my packages and
// write them to _variables.yml for use with {{< var downloads.* >}}.
// Counts come from cranlogs (Posit's CRAN mirror only, from Oct 2012).
// If the API can't be reached, the existing _variables.yml is left as-is.

const packages = ["openair", "openairmaps", "worldmet"];
const today = new Date().toISOString().slice(0, 10);
const url =
  `https://cranlogs.r-pkg.org/downloads/total/2012-10-01:${today}/${packages.join(",")}`;

// 686270 -> "686k", 1234567 -> "1.2M"
function compact(n: number): string {
  if (n >= 1e6) return `${(Math.floor(n / 1e5) / 10).toFixed(1)}M`;
  if (n >= 1e3) return `${Math.floor(n / 1e3)}k`;
  return `${n}`;
}

try {
  const res = await fetch(url, { signal: AbortSignal.timeout(15000) });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  const rows: { package: string; downloads: number }[] = await res.json();

  const counts: Record<string, number> = {};
  for (const row of rows) counts[row.package] = row.downloads;
  if (!packages.every((p) => counts[p] > 0)) throw new Error("missing counts");

  const total = packages.reduce((sum, p) => sum + counts[p], 0);
  const lines = [
    "# Written by _scripts/cran-downloads.ts on each render. Do not edit by hand.",
    "downloads:",
    ...packages.map((p) => `  ${p}: "${compact(counts[p])}"`),
    `  total: "${compact(total)}"`,
    `  updated: "${new Date().toLocaleDateString("en-GB", { month: "long", year: "numeric" })}"`,
  ];
  await Deno.writeTextFile("_variables.yml", lines.join("\n") + "\n");
  console.log(`CRAN downloads: ${packages.map((p) => `${p}=${counts[p]}`).join(", ")}`);
} catch (err) {
  console.warn(`CRAN downloads not updated (${err}); keeping existing _variables.yml`);
}
