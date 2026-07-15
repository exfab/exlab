class AutocompleteComponent {
    constructor(containerElement, options) {
        this.container = containerElement;
        this.options = Object.assign({
            placeholder: 'Search...',
            debounceMs: 300,
            onSelect: (item) => {},
            fetchItems: async (query) => [], // Returns array of {id, label}
            initialValue: null // {id, label}
        }, options);

        this.items = [];
        this.selectedItem = this.options.initialValue || null;
        this.debounceTimer = null;
        this.isOpen = false;

        this.render();
        this.bindEvents();
    }

    render() {
        this.container.style.position = 'relative';
        const initialLabel = this.selectedItem ? this.selectedItem.label : '';
        
        this.container.innerHTML = `
            <div class="autocomplete-wrapper" style="position: relative;">
                <input type="text" class="autocomplete-input" placeholder="${this.options.placeholder}" value="${initialLabel}">
                <div class="autocomplete-dropdown" style="display: none; position: absolute; top: 100%; left: 0; right: 0; background: var(--bg-light); border: 1px solid var(--border-color); border-radius: 4px; z-index: 1000; max-height: 200px; overflow-y: auto;">
                </div>
            </div>
        `;

        this.inputElement = this.container.querySelector('.autocomplete-input');
        this.dropdownElement = this.container.querySelector('.autocomplete-dropdown');
    }

    bindEvents() {
        this.inputElement.addEventListener('input', (e) => {
            const query = e.target.value.trim();
            this.selectedItem = null; // Clear selection if typing
            this.options.onSelect(null);

            clearTimeout(this.debounceTimer);
            if (query.length === 0) {
                this.closeDropdown();
                return;
            }

            this.debounceTimer = setTimeout(() => this.performSearch(query), this.options.debounceMs);
        });

        this.inputElement.addEventListener('focus', () => {
            if (this.inputElement.value.trim().length > 0) {
                this.openDropdown();
            }
        });

        // Close dropdown when clicking outside
        document.addEventListener('click', (e) => {
            if (!this.container.contains(e.target)) {
                this.closeDropdown();
                // Restore label if an item was selected, otherwise keep input
                if (this.selectedItem) {
                    this.inputElement.value = this.selectedItem.label;
                }
            }
        });
    }

    async performSearch(query) {
        try {
            this.dropdownElement.innerHTML = '<div style="padding: 8px; color: var(--text-secondary);">Searching...</div>';
            this.openDropdown();

            this.items = await this.options.fetchItems(query);
            this.renderDropdown();
        } catch (error) {
            console.error('Autocomplete search failed:', error);
            this.dropdownElement.innerHTML = '<div style="padding: 8px; color: var(--error-color);">Error searching</div>';
        }
    }

    renderDropdown() {
        if (this.items.length === 0) {
            this.dropdownElement.innerHTML = '<div style="padding: 8px; color: var(--text-secondary);">No matches found</div>';
            return;
        }

        this.dropdownElement.innerHTML = this.items.map((item, index) => `
            <div class="autocomplete-item" data-index="${index}" style="padding: 8px; cursor: pointer; border-bottom: 1px solid var(--border-color);">
                ${UIUtils.escapeHTML(item.label)}
            </div>
        `).join('');

        const itemElements = this.dropdownElement.querySelectorAll('.autocomplete-item');
        itemElements.forEach(el => {
            el.addEventListener('click', (e) => {
                const index = parseInt(e.currentTarget.getAttribute('data-index'), 10);
                this.selectItem(this.items[index]);
            });
            el.addEventListener('mouseenter', (e) => {
                e.currentTarget.style.backgroundColor = 'var(--bg-dark-1)';
            });
            el.addEventListener('mouseleave', (e) => {
                e.currentTarget.style.backgroundColor = 'transparent';
            });
        });
    }

    selectItem(item) {
        this.selectedItem = item;
        this.inputElement.value = item ? item.label : '';
        this.closeDropdown();
        this.options.onSelect(item);
    }

    openDropdown() {
        this.isOpen = true;
        this.dropdownElement.style.display = 'block';
    }

    closeDropdown() {
        this.isOpen = false;
        this.dropdownElement.style.display = 'none';
    }
    
    getValue() {
        return this.selectedItem ? this.selectedItem.id : null;
    }
}

window.AutocompleteComponent = AutocompleteComponent;
