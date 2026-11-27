-- Work page publications: builds the bibliography, sorts it newest first
-- and puts my name in bold wherever it appears in an author list.

local function year_of(entry)
  return tonumber(pandoc.utils.stringify(entry):match("(%d%d%d%d)")) or 0
end

-- Ways citation styles write my name, as two words split by a space:
-- APA "Davison, J.", Chicago "Davison, Jack" and "Jack Davison".
local name_forms = {
  { "Davison,", "J." },
  { "Davison,", "Jack" },
  { "Jack", "Davison" },
}

-- Bold my name wherever it appears, keeping any trailing punctuation
-- (e.g. the comma in "Davison, J.,") outside the bold.
local function bold_name(inlines)
  local out, i = pandoc.List(), 1
  while i <= #inlines do
    local a, sp, b = inlines[i], inlines[i + 1], inlines[i + 2]
    local matched = false
    if a and sp and b and a.t == "Str" and sp.t == "Space" and b.t == "Str" then
      for _, form in ipairs(name_forms) do
        if a.text == form[1] and b.text:sub(1, #form[2]) == form[2] then
          out:insert(pandoc.Strong({ pandoc.Str(form[1]), pandoc.Space(), pandoc.Str(form[2]) }))
          local rest = b.text:sub(#form[2] + 1)
          if rest ~= "" then out:insert(pandoc.Str(rest)) end
          i, matched = i + 3, true
          break
        end
      end
    end
    if not matched then
      out:insert(inlines[i])
      i = i + 1
    end
  end
  return out
end

local function reorder_refs(div)
  if div.identifier ~= "refs" then return nil end
  local entries = pandoc.List()
  for idx, block in ipairs(div.content) do
    entries:insert({ block = block, year = year_of(block), idx = idx })
  end
  table.sort(entries, function(x, y)
    if x.year ~= y.year then return x.year > y.year end
    return x.idx < y.idx
  end)
  div.content = entries:map(function(e)
    return pandoc.walk_block(e.block, { Inlines = bold_name })
  end)
  return div
end

-- Pandoc only builds the bibliography after all Lua filters, so build it
-- here instead, then drop the bibliography metadata so it isn't built twice.
function Pandoc(doc)
  doc = pandoc.utils.citeproc(doc)
  doc.meta.bibliography = nil
  doc.meta.references = nil
  doc.meta.nocite = nil
  return doc:walk({ Div = reorder_refs })
end
