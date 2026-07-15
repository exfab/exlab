/* src/server/static/js/navigation.js */

function generateBackArrow(defaultUrl) {
  const urlParams = new URLSearchParams(window.location.search);
  const from = urlParams.get('from');
  const contextId = urlParams.get('context_id');
  const contextName = urlParams.get('context_name');

  let backUrl = defaultUrl;
  let backText = 'Back';

  /* Generate backlinks for specific instances */
  if (from && contextId) {
    if (from === 'project') {
      backUrl = `/projects/${contextId}`;
      backText = contextName ? `Back to ${contextName}` : 'Back to Project';
    } else if (from === 'sample') {
      backUrl = `/samples/${contextId}`;
      backText = contextName ? `Back to ${contextName}` : 'Back to Sample';
    } else if (from === 'strain') {
      backUrl = `/strains/${contextId}`;
      backText = contextName ? `Back to ${contextName}` : 'Back to Strain';
    } else if (from === 'community') {
      backUrl = `/communities/${contextId}`;
      backText = contextName ? `Back to ${contextName}` : 'Back to Community';
    } else if (from === 'plate') {
      backUrl = `/plates/${contextId}`;
      backText = contextName ? `Back to ${contextName}` : 'Back to Plate';
    }
  } else if (from === 'samples-list') {
    backUrl = '/samples';
    backText = 'Back to Samples';
  } else if (from === 'plates') {
    backUrl = '/plates';
    backText = 'Back to Plates';
  }

  return `<a href="${backUrl}" class="back-arrow">← ${backText}</a>`;
}

/* Theme toggle */
(function () {
  const STORAGE_KEY = 'exlab-theme';
  const LIGHT_THEME = 'light';
  const DARK_THEME = 'dark';

  function setTheme(theme) {
    if (theme === LIGHT_THEME) {
      document.documentElement.setAttribute('data-theme', 'light');
    } else {
      document.documentElement.removeAttribute('data-theme');
    }
    localStorage.setItem(STORAGE_KEY, theme);
    const btn = document.getElementById('theme-toggle');
    if (btn) {
      btn.textContent = theme === LIGHT_THEME ? '☾' : '☀';
    }
  }

  function toggleTheme() {
    const current = document.documentElement.getAttribute('data-theme') === 'light'
      ? LIGHT_THEME
      : DARK_THEME;
    setTheme(current === LIGHT_THEME ? DARK_THEME : LIGHT_THEME);
  }

  const saved = localStorage.getItem(STORAGE_KEY);
  if (saved) {
    setTheme(saved);
  }

  const btn = document.getElementById('theme-toggle');
  if (btn) {
    btn.addEventListener('click', toggleTheme);
  }
})();
