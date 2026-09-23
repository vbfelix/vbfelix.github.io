(() => {
  const entries = Array.from(document.querySelectorAll('.writing-page .archive-entry[data-language]'));
  if (!entries.length) return;

  const categories = [...new Set(entries.flatMap((entry) =>
    (entry.dataset.categories || '').split(',').map((category) => category.trim()).filter(Boolean),
  ))].sort((left, right) => left.localeCompare(right, 'pt-BR'));

  const controls = document.createElement('div');
  controls.className = 'archive-filters';
  controls.setAttribute('role', 'group');
  controls.setAttribute('aria-label', 'Filtrar textos');
  controls.innerHTML = '<label>Idioma <select class="archive-filter-language"><option value="">Todos os idiomas</option><option value="pt">Português</option><option value="en">Inglês</option></select></label><label>Tema <select class="archive-filter-category"><option value="">Todos os temas</option></select></label><p class="archive-filter-count" role="status" aria-live="polite"></p>';
  const language = controls.querySelector('.archive-filter-language');
  const category = controls.querySelector('.archive-filter-category');
  const count = controls.querySelector('.archive-filter-count');
  for (const topic of categories) category.add(new Option(topic, topic));
  entries[0].before(controls);

  function update() {
    let visible = 0;
    for (const entry of entries) {
      const topics = (entry.dataset.categories || '').split(',').map((item) => item.trim());
      const match = (!language.value || entry.dataset.language === language.value) &&
        (!category.value || topics.includes(category.value));
      entry.hidden = !match;
      if (match) visible += 1;
    }
    count.textContent = `${visible} ${visible === 1 ? 'texto' : 'textos'}`;
  }
  controls.addEventListener('change', update);
  update();
})();
