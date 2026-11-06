/* src/server/static/js/app.js */

// Page Loading Functions

/**
 * Draws the Home page with Global ID/Barcode Lookup and Quick Action Launcher
 */
function loadHomePage(mainElement) {
  const resourceMeta = {
    sample: { label: "Sample", emoji: "🧪" },
    plate: { label: "Plate", emoji: "🧫" },
    project: { label: "Project", emoji: "📂" },
    strain: { label: "Strain", emoji: "🦠" },
    community: { label: "Community", emoji: "🌐" },
    product: { label: "Product", emoji: "📦" },
    result_definition: { label: "Data Definition", emoji: "📈" },
  };

  mainElement.innerHTML = `
    <div class="home-container">
      <div class="page-header home-header">
        <div>
          <h2>ExLAB</h2>
          <p class="home-subtitle">Scan a barcode, jump directly to any exact ID, or launch a lab workflow.</p>
        </div>
      </div>

      <section class="card home-lookup-card">
        <div class="home-lookup-header">
          <h3><span class="emoji">🔍</span> Global ID &amp; Barcode Lookup</h3>
          <span class="home-lookup-hint">Exact match by Short ID, UUID, Strain Accession, or numeric ID</span>
        </div>
        <form id="home-lookup-form" class="home-lookup-form" autocomplete="off">
          <select id="home-lookup-scope" class="home-lookup-scope" aria-label="Resource scope">
            <option value="all">All Resources</option>
            <option value="sample">Sample</option>
            <option value="plate">Plate</option>
            <option value="project">Project</option>
            <option value="strain">Strain</option>
            <option value="community">Community</option>
            <option value="result_definition">Data Definition</option>
            <option value="product">Product</option>
          </select>
          <input
            type="text"
            id="home-lookup-input"
            class="home-lookup-input"
            placeholder="Scan barcode or enter exact ID (e.g. UCR-003-0001, S-3-0001, UCR-003)..."
            required
          />
          <button type="submit" id="home-lookup-btn" class="button">Lookup &rarr;</button>
        </form>
        <div id="home-lookup-feedback" class="home-lookup-feedback" style="display: none;"></div>
      </section>

      <section class="home-actions-section">
        <h3 class="home-section-title">Quick Actions</h3>
        <div class="home-actions-grid">
          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">📂</span> Projects</div>
              <p class="home-action-desc">Organize experiments, source stocks, plates, and project-wide data matrices.</p>
            </div>
            <div class="home-action-links">
              <a href="/projects/new" class="button home-nav-link">+ New Project</a>
              <a href="/projects" class="button secondary-button home-nav-link">Browse Projects</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">🧪</span> Samples</div>
              <p class="home-action-desc">Register source or experimental samples and trace sample lineage.</p>
            </div>
            <div class="home-action-links">
              <a href="/samples/new" class="button home-nav-link">+ Create Sample</a>
              <a href="/samples" class="button secondary-button home-nav-link">All Samples</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">🧫</span> Plates</div>
              <p class="home-action-desc">Inspect well layouts, plan transfers, and record plate-level measurements.</p>
            </div>
            <div class="home-action-links">
              <a href="/plates" class="button home-nav-link">Browse &amp; Plan Plates</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">🦠</span> Strains</div>
              <p class="home-action-desc">Manage biological isolates, taxonomy, genotypes, and external accessions.</p>
            </div>
            <div class="home-action-links">
              <a href="/strains/new" class="button home-nav-link">+ Register Strain</a>
              <a href="/strains" class="button secondary-button home-nav-link">Browse Strains</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">🌐</span> Communities</div>
              <p class="home-action-desc">Define multi-strain synthetic communities (SynComs) and member compositions.</p>
            </div>
            <div class="home-action-links">
              <a href="/communities/new" class="button home-nav-link">+ New Community</a>
              <a href="/communities" class="button secondary-button home-nav-link">Browse Communities</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">📈</span> Data Definitions</div>
              <p class="home-action-desc">Configure standardized measurement types, units, series schemas, and categories.</p>
            </div>
            <div class="home-action-links">
              <a href="/result-definitions/create" class="button home-nav-link">+ New Data Definition</a>
              <a href="/result-categories" class="button secondary-button home-nav-link">Data Categories</a>
            </div>
          </div>

          <div class="card home-action-card">
            <div class="home-action-top">
              <div class="home-action-title"><span class="emoji">📦</span> Products</div>
              <p class="home-action-desc">Track lab reagents, media formulations, kits, and catalog inventory.</p>
            </div>
            <div class="home-action-links">
              <a href="/products/new" class="button home-nav-link">+ New Product</a>
              <a href="/products" class="button secondary-button home-nav-link">Browse Products</a>
            </div>
          </div>
        </div>
      </section>
    </div>
  `;

  const lookupInput = document.getElementById("home-lookup-input");
  const lookupScope = document.getElementById("home-lookup-scope");
  const lookupForm = document.getElementById("home-lookup-form");
  const lookupBtn = document.getElementById("home-lookup-btn");
  const feedbackEl = document.getElementById("home-lookup-feedback");

  if (lookupInput) {
    lookupInput.focus();
  }

  mainElement.querySelectorAll(".home-nav-link").forEach(link => {
    link.addEventListener("click", e => {
      e.preventDefault();
      const url = new URL(link.href);
      history.pushState({}, "", url.pathname);
      handleRouting();
    });
  });

  if (lookupForm) {
    lookupForm.addEventListener("submit", async e => {
      e.preventDefault();
      const q = (lookupInput.value || "").trim();
      const scope = lookupScope.value || "all";
      if (!q) return;

      lookupBtn.disabled = true;
      lookupBtn.textContent = "Searching...";
      feedbackEl.style.display = "none";
      feedbackEl.innerHTML = "";

      try {
        const params = new URLSearchParams({ q, scope });
        const res = await ApiUtils.request(`/api/v1/lookup?${params.toString()}`);
        const matches = (res && res.matches) ? res.matches : [];

        if (matches.length === 1) {
          history.pushState({}, "", matches[0].url);
          handleRouting();
          return;
        }

        feedbackEl.style.display = "block";
        if (matches.length === 0) {
          feedbackEl.innerHTML = `
            <div class="home-lookup-empty">
              No resource found matching exact ID <code></code>.
            </div>
          `;
          feedbackEl.querySelector("code").textContent = q;
        } else {
          const listHtml = matches.map(m => {
            const meta = resourceMeta[m.resource_type] || { label: m.resource_type, emoji: "🔎" };
            const codeDisplay = m.short_id || `#${m.id}`;
            return `
              <a href="${m.url}" class="home-lookup-match-item">
                <span class="badge"><span class="emoji">${meta.emoji}</span> ${meta.label}</span>
                <code class="home-lookup-match-code">${codeDisplay}</code>
                <span class="home-lookup-match-name">${m.name}</span>
                <span class="home-lookup-match-arrow">&rarr;</span>
              </a>
            `;
          }).join("");

          feedbackEl.innerHTML = `
            <div class="home-lookup-matches-title">Multiple resources matched <code></code> — select one:</div>
            <div class="home-lookup-matches-list">${listHtml}</div>
          `;
          feedbackEl.querySelector("code").textContent = q;
          feedbackEl.querySelectorAll(".home-lookup-match-item").forEach(item => {
            item.addEventListener("click", ev => {
              ev.preventDefault();
              const url = new URL(item.href);
              history.pushState({}, "", url.pathname);
              handleRouting();
            });
          });
        }
      } catch (err) {
        feedbackEl.style.display = "block";
        feedbackEl.innerHTML = `<div class="home-lookup-empty">Lookup failed: ${err.message || "Unknown error"}</div>`;
      } finally {
        lookupBtn.disabled = false;
        lookupBtn.textContent = "Lookup →";
      }
    });
  }
}

// Basic Client-Side Router
// This function decides what to draw based on the URL
async function handleRouting() {
  const main = document.getElementById("content-area");
  if (!main) {
    console.error("Main content area not found!");
    return;
  }

  const path = window.location.pathname;
  const body = document.body;
  let currentUser = null;

  try {
    const response = await fetch('/api/v1/me');
    if (!response.ok && path !== '/login') {
      body.classList.remove("logged-in");
      document.getElementById("login-menu-item").style.display = "block";
      document.getElementById("logout-menu-item").style.display = "none";
      document.getElementById("profile-menu-item").style.display = "none";
      document.getElementById("admin-menu-item").style.display = "none";
      document.getElementById("users-menu-item").style.display = "none";
      history.pushState({}, "", "/login");
      loadLoginPage(main);
      return;
    } else if (response.ok) {
      currentUser = await response.json();
      body.classList.add("logged-in");
      document.getElementById("login-menu-item").style.display = "none";
      document.getElementById("logout-menu-item").style.display = "block";
      document.getElementById("profile-menu-item").style.display = "block";
      if (currentUser.role === 'admin') {
        document.getElementById("admin-menu-item").style.display = "block";
        document.getElementById("users-menu-item").style.display = "block";
      } else {
        document.getElementById("admin-menu-item").style.display = "none";
        document.getElementById("users-menu-item").style.display = "none";
      }
      if (path.startsWith("/login")) {
        history.pushState({}, "", "/home");
        loadHomePage(main);
        return;
      }
    }
  } catch (error) {
    body.classList.remove("logged-in");
    document.getElementById("logout-menu-item").style.display = "none";
    document.getElementById("profile-menu-item").style.display = "none";
    document.getElementById("admin-menu-item").style.display = "none";
    document.getElementById("users-menu-item").style.display = "none";
    if (path !== '/login') {
      history.pushState({}, "", "/login");
      loadLoginPage(main);
      return;
    }
  }

  if (path.startsWith("/login")) {
    body.classList.add("login-page");
  } else {
    body.classList.remove("login-page");
  }

  const sampleRouteMatch = path.match(/^\/samples\/([a-zA-Z0-9_.\-]+)$/);
  const strainRouteMatch = path.match(/^\/strains\/([a-zA-Z0-9_.\-]+)$/);
  const communityRouteMatch = path.match(/^\/communities\/([a-zA-Z0-9_.\-]+)$/);
  const platePlanRouteMatch = path.match(/^\/plates\/([a-zA-Z0-9_.\-]+)\/plan$/);
  const plateRouteMatch = path.match(/^\/plates\/([a-zA-Z0-9_.\-]+)$/);
  const resultDefinitionRouteMatch = path.match(/^\/result-definitions\/([a-zA-Z0-9_.\-]+)$/);

  const productRouteMatch = path.match(/^\/products\/([a-zA-Z0-9_.\-]+)$/);
  const projectRouteMatch = path.match(/^\/projects\/([a-zA-Z0-9_.\-]+)$/);

  // Sidebar Sub-menus visibility logic
  const categoriesSubMenu = document.getElementById('result-categories-menu-item');
  if (categoriesSubMenu) {
    if (path.startsWith("/result-definitions") || path.startsWith("/result-categories")) {
      categoriesSubMenu.style.display = "block";
    } else {
      categoriesSubMenu.style.display = "none";
    }
  }

  const biologySubMenus = document.querySelectorAll('.biology-sub-menu-item');
  if (biologySubMenus.length > 0) {
    if (path.startsWith("/strains") || path.startsWith("/communities")) {
      biologySubMenus.forEach(el => el.style.display = "block");
    } else {
      biologySubMenus.forEach(el => el.style.display = "none");
    }
  }

  if (path.startsWith("/home")) {
    loadHomePage(main);
  } else if (path.startsWith("/projects/new")) {
    await loadCreateProjectPage(main);
  } else if (projectRouteMatch) {
    const projectId = projectRouteMatch[1];
    await loadProjectDetailsPage(main, projectId);
  } else if (path.startsWith("/projects")) {
    loadProjectsPage(main);
  } else if (path.startsWith("/samples/new")) {
    await loadCreateSamplePage(main);
  } else if (sampleRouteMatch) {
    const sampleId = sampleRouteMatch[1];
    await loadSampleDetailsPage(main, sampleId);
  } else if (path.startsWith("/samples")) {
    await loadSamplesPage(main);
  } else if (path.startsWith("/strains/new")) {
    await loadCreateStrainPage(main);
  } else if (strainRouteMatch) {
    const strainId = strainRouteMatch[1];
    await loadStrainDetailsPage(main, strainId);
  } else if (path.startsWith("/strains")) {
    await loadStrainsPage(main);
  } else if (path.startsWith("/communities/new")) {
    await loadCreateCommunityPage(main);
  } else if (communityRouteMatch) {
    const communityId = communityRouteMatch[1];
    await loadCommunityDetailsPage(main, communityId);
  } else if (path.startsWith("/communities")) {
    await loadCommunitiesPage(main);
  } else if (platePlanRouteMatch) {
    const plateId = platePlanRouteMatch[1];
    await loadPlatePlannerPage(main, plateId);
  } else if (plateRouteMatch) {
    const plateId = plateRouteMatch[1];
    await loadWellViewerPage(main, plateId);
  } else if (path.startsWith("/plates")) {
    await loadPlatesPage(main);
  } else if (path.startsWith("/products/new")) {
    await loadCreateProductPage(main);

  } else if (productRouteMatch) {
    const productId = productRouteMatch[1];
    await loadProductDetailsPage(main, productId);
  } else if (path.startsWith("/products")) {
    loadProductsPage(main);
  } else if (path.startsWith("/result-definitions/create")) {
    await loadCreateResultDefinitionPage(main);
  } else if (resultDefinitionRouteMatch) {
    const resultDefinitionId = resultDefinitionRouteMatch[1];
    await loadResultDefinitionDetailsPage(main, resultDefinitionId);
  } else if (path.startsWith("/result-definitions")) {
    loadResultDefinitionsPage(main);
  } else if (path.startsWith("/result-categories")) {
    loadResultCategoriesPage(main);
  } else if (path.startsWith("/profile")) {
    await loadProfilePage(main);
  } else if (path.startsWith("/admin")) {
    if (currentUser && currentUser.role === 'admin') {
      loadAdminPage(main);
    } else {
      loadHomePage(main);
    }
  } else if (path.startsWith("/users")) {
    if (currentUser && currentUser.role === 'admin') {
      loadUsersPage(main);
    } else {
      loadHomePage(main);
    }
  } else if (path.startsWith("/login")) {
    loadLoginPage(main);
  } else {
    loadHomePage(main);
  }
}

// Event Listeners

// This runs when the page is fully loaded and ready
window.addEventListener("load", () => {
  const main = document.getElementById("content-area");
  const body = document.body;

  // Set the copyright year
  const yearElement = document.getElementById("copyright-year");
  if (yearElement) {
    yearElement.textContent = new Date().getFullYear();
  }

  // Set up the router
  handleRouting();

  // Intercept navigation clicks
  document.addEventListener("click", e => {
    // Check if the click was on a link (<a>) *inside* the <nav>
    const link = e.target.closest("nav a");
    if (link && link.target !== "_blank") {
      e.preventDefault(); // Stop the browser from navigating
      const url = new URL(link.href);
      // Change the URL in the browser bar *without* reloading
      history.pushState({}, "", url.pathname);
      // Manually run our router to load the new content
      handleRouting();
    }
  });

  // Handle Back/Forward buttons
  window.addEventListener("popstate", handleRouting);

  const logoutMenuItem = document.getElementById("logout-menu-item");
  if (logoutMenuItem) {
    logoutMenuItem.addEventListener("click", async (e) => {
      e.preventDefault();
      try {
        const response = await fetch('/api/v1/logout', { method: 'POST' });
        if (response.ok) {
          body.classList.remove("logged-in");
          history.pushState({}, "", "/login");
          loadLoginPage(main);
        }
      } catch (error) {
        console.error('Error during logout:', error);
      }
    });
  }

  // Nav Toggle Logic
  const toggleButton = document.getElementById("nav-toggle");

  // Check if user had it collapsed before
  if (localStorage.getItem("nav-collapsed") === "true") {
    body.classList.add("nav-collapsed");
  }

  // Add click listener for the toggle
  if (toggleButton) {
    toggleButton.addEventListener("click", () => {
      body.classList.toggle("nav-collapsed");
      // Save the preference
      localStorage.setItem(
        "nav-collapsed",
        body.classList.contains("nav-collapsed")
      );
    });
  }
});
