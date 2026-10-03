-- Static navigation on internal pages, including articles and Portuguese aliases.
local function escape(value)
  return value:gsub('&', '&amp;'):gsub('<', '&lt;'):gsub('>', '&gt;'):gsub('"', '&quot;')
end

-- Same encoding as urllib.parse.quote in scripts/build-writing.py, so tag links match the archive's.
local function quote(value)
  return (value:gsub('[^%w%-%._~/]', function(char) return string.format('%%%02X', char:byte()) end))
end

-- Articles belong to a section; every other page hangs directly under the home.
local sections = {
  posts = { label = 'Blog', href = '/writing.html', back = 'Todos os textos' },
  portfolio = { label = 'Portfólio', href = '/portfolio.html', back = 'Todo o portfólio' },
}

local function section()
  local path = (quarto.doc.input_file or ''):gsub('\\', '/')
  return sections[path:match('/([^/]+)/[^/]+/index%.qmd$')]
end

-- Only blog categories filter the archive; portfolio categories have no destination.
local function tags(meta)
  local links = {}
  for _, item in ipairs(meta.categories or {}) do
    local category = pandoc.utils.stringify(item)
    links[#links + 1] = '<a class="post-tag" href="/writing.html?tema=' .. quote(category) .. '">#'
      .. escape(category:gsub('%s+', '-')) .. '</a>'
  end
  if #links == 0 then return '' end
  return '<p class="post-tags">' .. table.concat(links) .. '</p>'
end

function Pandoc(doc)
  if not FORMAT:match('html') then return doc end
  if doc.meta.breadcrumbs == false then return doc end

  local title = pandoc.utils.stringify(doc.meta.title or '')
  if title == '' then title = 'Sobre' end
  local parent = section()
  local breadcrumb = '<nav class="site-breadcrumbs" aria-label="Navegação estrutural">'
    .. '<ol><li><a href="/index.html">Início</a></li>'
    .. (parent and '<li><a href="' .. parent.href .. '">' .. parent.label .. '</a></li>' or '')
    .. '<li aria-current="page">' .. escape(title) .. '</li></ol></nav>'
  doc.blocks:insert(1, pandoc.RawBlock('html', breadcrumb))
  if parent then
    local ending = '<nav class="article-end" aria-label="Continuar lendo">'
      .. (parent.label == 'Blog' and tags(doc.meta) or '')
      .. '<a class="utility-link" href="' .. parent.href .. '">← ' .. parent.back .. '</a></nav>'
    doc.blocks:insert(pandoc.RawBlock('html', ending))
  end
  return doc
end
