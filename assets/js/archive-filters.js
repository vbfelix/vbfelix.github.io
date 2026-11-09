(() => {
  const entries = Array.from(document.querySelectorAll('.writing-page .archive-entry[data-language]'));
  if (!entries.length) return;

  const months = Array.from(document.querySelectorAll('.writing-page .archive-month'));
  const tags = Array.from(document.querySelectorAll('.writing-page .post-tag[data-category]'));
  const categories = [...new Set(entries.flatMap((entry) =>
    (entry.dataset.categories || '').split(',').map((category) => category.trim()).filter(Boolean),
  ))].sort((left, right) => left.localeCompare(right, 'pt-BR'));

  // Accents and case do not count: "estatistica" finds "Estatística".
  const plain = (text) => text.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  const haystacks = new Map(entries.map((entry) =>
    [entry, plain(`${entry.querySelector('a').textContent} ${entry.dataset.categories || ''}`)]));

  const controls = document.createElement('div');
  controls.className = 'archive-filters';
  controls.setAttribute('role', 'group');
  controls.setAttribute('aria-label', 'Filtrar textos');
  controls.innerHTML = '<label>Buscar <input type="search" class="archive-filter-search" placeholder="Título ou tema" autocomplete="off"></label><label>Idioma <select class="archive-filter-language"><option value="">Todos os idiomas</option><option value="pt">Português</option><option value="en">Inglês</option></select></label><label>Tema <select class="archive-filter-category"><option value="">Todos os temas</option></select></label><p class="archive-filter-count" role="status" aria-live="polite"></p>';
  const search = controls.querySelector('.archive-filter-search');
  const language = controls.querySelector('.archive-filter-language');
  const category = controls.querySelector('.archive-filter-category');
  const count = controls.querySelector('.archive-filter-count');
  for (const topic of categories) category.add(new Option(topic, topic));
  (months[0] || entries[0]).before(controls);

  // Tag links carry the theme in ?tema=, so a tag clicked on another page lands here already filtered.
  const params = new URLSearchParams(location.search);
  const requested = params.get('tema');
  if (categories.includes(requested)) category.value = requested;
  search.value = params.get('busca') || '';

  function update() {
    const terms = plain(search.value).split(/\s+/).filter(Boolean);
    let visible = 0;
    for (const entry of entries) {
      const topics = (entry.dataset.categories || '').split(',').map((item) => item.trim());
      const text = haystacks.get(entry);
      const match = (!language.value || entry.dataset.language === language.value) &&
        (!category.value || topics.includes(category.value)) &&
        terms.every((term) => text.includes(term));
      entry.hidden = !match;
      if (match) visible += 1;
    }
    for (const month of months) month.hidden = !month.querySelector('.archive-entry:not([hidden])');
    for (const tag of tags) {
      const active = tag.dataset.category === category.value;
      tag.classList.toggle('is-active', active);
      if (active) tag.setAttribute('aria-current', 'true');
      else tag.removeAttribute('aria-current');
    }
    count.textContent = visible ? `${visible} ${visible === 1 ? 'texto' : 'textos'}` : 'Nenhum texto encontrado';

    const url = new URL(location.href);
    if (category.value) url.searchParams.set('tema', category.value);
    else url.searchParams.delete('tema');
    if (search.value.trim()) url.searchParams.set('busca', search.value.trim());
    else url.searchParams.delete('busca');
    history.replaceState(null, '', url);
  }

  for (const tag of tags) {
    tag.addEventListener('click', (event) => {
      event.preventDefault();
      // Clicking the active tag clears the theme filter.
      category.value = category.value === tag.dataset.category ? '' : tag.dataset.category;
      update();
    });
  }
  controls.addEventListener('change', update);
  search.addEventListener('input', update);
  update();
})();
