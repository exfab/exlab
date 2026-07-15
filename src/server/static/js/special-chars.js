/**
 * special-chars.js
 * Provides a UI helper to easily insert special mathematical/scientific 
 * characters into text inputs or textareas.
 */

const SPECIAL_CHARS = [
  // Greek & Math
  'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ',
  'ν', 'ξ', 'ο', 'π', 'ρ', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω',
  'Δ', 'Σ', 'Ω', 'μ', '°', '℃', '℉', '±', '×', '÷', '≤', '≥', '≠', '≈', '∞',
  // Superscripts
  '⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹', '⁺', '⁻', 'ⁿ',
  // Subscripts
  '₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉', '₊', '₋'
];

/**
 * Attaches a special character helper button to a given input field.
 * @param {string} inputId - The ID of the input or textarea element.
 */
window.attachSpecialCharHelper = function(inputId) {
  const inputEl = document.getElementById(inputId);
  if (!inputEl) return;

  // Ensure the input has a parent that can hold our absolute positioned button
  // If the parent isn't an input-with-action or similar, we might need to wrap it.
  let wrapper = inputEl.parentNode;
  
  // Create the helper button
  const btn = document.createElement('button');
  btn.type = 'button';
  btn.className = 'char-helper-btn';
  btn.title = 'Insert Special Character';
  btn.innerHTML = 'Ω'; // Use Omega as the icon

  // Create the popover
  const popover = document.createElement('div');
  popover.className = 'char-helper-popover';
  popover.style.display = 'none';

  // Populate popover with characters
  SPECIAL_CHARS.forEach(char => {
    const charBtn = document.createElement('button');
    charBtn.type = 'button';
    charBtn.className = 'char-btn';
    charBtn.textContent = char;
    charBtn.addEventListener('click', (e) => {
      e.preventDefault();
      insertTextAtCursor(inputEl, char);
      popover.style.display = 'none';
      inputEl.focus();
    });
    popover.appendChild(charBtn);
  });

  // Toggle popover on button click
  btn.addEventListener('click', (e) => {
    e.preventDefault();
    const isVisible = popover.style.display === 'grid';
    popover.style.display = isVisible ? 'none' : 'grid';
    
    if (!isVisible) {
      // Basic positioning logic (can be refined via CSS)
      const btnRect = btn.getBoundingClientRect();
      // Ensure it stays within screen bounds roughly
      popover.style.top = (btn.offsetTop + btn.offsetHeight + 4) + 'px';
      popover.style.right = '0'; 
    }
  });

  // Close popover when clicking outside
  document.addEventListener('click', (e) => {
    if (!btn.contains(e.target) && !popover.contains(e.target)) {
      popover.style.display = 'none';
    }
  });

  // Create a relative container if not already in a wrapper that supports it
  const container = document.createElement('div');
  container.className = 'char-helper-wrapper';
  
  wrapper.insertBefore(container, inputEl);
  container.appendChild(inputEl);
  container.appendChild(btn);
  container.appendChild(popover);
};

function insertTextAtCursor(el, text) {
  const startPos = el.selectionStart;
  const endPos = el.selectionEnd;
  const value = el.value;
  
  el.value = value.substring(0, startPos) + text + value.substring(endPos, value.length);
  el.selectionStart = startPos + text.length;
  el.selectionEnd = startPos + text.length;
}
