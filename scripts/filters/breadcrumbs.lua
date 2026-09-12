-- Static navigation on internal pages, including articles and Portuguese aliases.
local function escape(value)
  return value:gsub('&', '&amp;'):gsub('<', '&lt;'):gsub('>', '&gt;'):gsub('"', '&quot;')
end

function Pandoc(doc)
  if not FORMAT:match('html') then return doc end
  if doc.meta.breadcrumbs == false then return doc end

  local title = pandoc.utils.stringify(doc.meta.title or '')
  if title == '' then title = 'Sobre' end
  local breadcrumb = '<nav class="site-breadcrumbs" aria-label="Navegação estrutural">'
    .. '<ol><li><a href="/index.html">Início</a></li>'
    .. '<li aria-current="page">' .. escape(title) .. '</li></ol></nav>'
  doc.blocks:insert(1, pandoc.RawBlock('html', breadcrumb))
  return doc
end
