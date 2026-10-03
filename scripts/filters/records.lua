-- Courses and certifications are written as one paragraph per record: logo, [MM/YY], title.
-- Consecutive records become one list, so the page reads as rows instead of loose paragraphs.
local function record(block)
  if block.t ~= 'Para' then return nil end
  local inlines = block.content
  if #inlines < 5 or inlines[1].t ~= 'Image' or inlines[3].t ~= 'Str' then return nil end
  local date = inlines[3].text:match('^%[(%d%d/%d%d)%]$')
  if not date then return nil end
  local title = pandoc.Inlines({})
  for index = 5, #inlines do title:insert(inlines[index]) end
  return { pandoc.Plain({
    inlines[1],
    pandoc.Span({ pandoc.Str(date) }, pandoc.Attr('', { 'record-date' })),
    pandoc.Span(title, pandoc.Attr('', { 'record-title' })),
  }) }
end

function Pandoc(doc)
  if not FORMAT:match('html') then return doc end
  local blocks, items = pandoc.Blocks({}), {}
  local function flush()
    if #items == 0 then return end
    blocks:insert(pandoc.Div({ pandoc.BulletList(items) }, pandoc.Attr('', { 'record-list' })))
    items = {}
  end
  for _, block in ipairs(doc.blocks) do
    local item = record(block)
    if item then
      items[#items + 1] = item
    else
      flush()
      blocks:insert(block)
    end
  end
  flush()
  doc.blocks = blocks
  return doc
end
