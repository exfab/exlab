/* src/server/static/js/app.js */

// Page Loading Functions

/**
 * Draws a simple home page
 */
function loadHomePage(mainElement) {
  mainElement.innerHTML = `
    <h2>Welcome to exlab!</h2>
    <p>This is the LIMS dashboard. Select a view from the navigation bar.</p>
  `;
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
