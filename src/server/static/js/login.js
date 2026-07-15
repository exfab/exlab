/* src/server/static/js/login.js */

function loadLoginPage(mainElement) {
  mainElement.innerHTML = `
    <div class="login-container">
      <h1>Login</h1>
      <form id="login-form">
        <div class="form-group">
          <label for="email">Email</label>
          <input type="email" id="email" name="email" required>
        </div>
        <div class="form-group">
          <label for="password">Password</label>
          <input type="password" id="password" name="password" required>
        </div>
        <button type="submit">Login</button>
      </form>
    </div>
  `;

  const loginForm = document.getElementById('login-form');

  if (loginForm) {
    loginForm.addEventListener('submit', async (event) => {
      event.preventDefault();

      const email = document.getElementById('email').value;
      const password = document.getElementById('password').value;

      try {
        const response = await fetch('/api/v1/login', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ email, password }),
        });

        if (response.ok) {
          window.location.href = '/home';
        } else {
          const errorData = await response.json();
          UIUtils.showToast(`Login failed: ${errorData.error}`, "error");
        }
      } catch (error) {
        console.error('Error during login:', error);
        UIUtils.showToast('An error occurred during login. Please try again.', "error");
      }
    });
  }
}
