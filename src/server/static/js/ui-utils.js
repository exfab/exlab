/* src/server/static/js/ui-utils.js */

const UIUtils = {
    /**
     * Shows a non-blocking toast notification.
     * @param {string} message - The message to display.
     * @param {string} type - 'info', 'success', 'warning', 'error'
     */
    showToast(message, type = 'info') {
        // Create container if it doesn't exist
        let container = document.getElementById('toast-container');
        if (!container) {
            container = document.createElement('div');
            container.id = 'toast-container';
            Object.assign(container.style, {
                position: 'fixed',
                bottom: '20px',
                right: '20px',
                zIndex: '9999',
                display: 'flex',
                flexDirection: 'column',
                gap: '10px'
            });
            document.body.appendChild(container);
        }

        const toast = document.createElement('div');
        toast.className = `toast toast-${type}`;
        
        // Basic styling for the toast, relies on CSS variables standard to the app
        Object.assign(toast.style, {
            padding: '12px 20px',
            borderRadius: '4px',
            color: '#fff',
            backgroundColor: this._getToastColor(type),
            boxShadow: '0 4px 6px rgba(0,0,0,0.1)',
            opacity: '0',
            transform: 'translateY(20px)',
            transition: 'all 0.3s ease',
            minWidth: '250px',
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center'
        });

        toast.innerHTML = `<span>${message}</span>`;
        
        container.appendChild(toast);

        // Trigger animation
        setTimeout(() => {
            toast.style.opacity = '1';
            toast.style.transform = 'translateY(0)';
        }, 10);

        // Remove after delay
        setTimeout(() => {
            toast.style.opacity = '0';
            toast.style.transform = 'translateY(20px)';
            setTimeout(() => toast.remove(), 300); // Wait for fade out
        }, 4000);
    },

    _getToastColor(type) {
        switch(type) {
            case 'error': return '#dc3545'; // Bootstrap danger
            case 'success': return '#28a745'; // Bootstrap success
            case 'warning': return '#ffc107'; // Bootstrap warning
            default: return 'var(--bg-dark-3, #333)'; // Default to app dark bg
        }
    },

    /**
     * Escapes HTML string to prevent XSS.
     */
    escapeHTML(str) {
        if (!str) return '';
        const div = document.createElement('div');
        div.textContent = String(str);
        return div.innerHTML;
    },

    /**
     * Standardized error handler that extracts backend error messages
     */
    handleError(error, defaultMessage = "An unexpected error occurred.") {
        console.error(error);
        const msg = error.message || defaultMessage;
        this.showToast(msg, 'error');
    },

    /**
     * Renders a standardized Chart.js line chart for time series data
     */
    renderTimeSeriesChart(canvasId, plotData, label = 'Time Series') {
        if (typeof Chart === 'undefined') {
            console.warn('Chart.js is not loaded.');
            return null;
        }

        const canvas = document.getElementById(canvasId);
        if (!canvas) return null;

        return new Chart(canvas, {
            type: 'line',
            data: {
                datasets: [{
                    label: label,
                    data: plotData,
                    borderColor: '#82b1ff',
                    backgroundColor: 'rgba(130, 177, 255, 0.1)',
                    borderWidth: 2,
                    pointRadius: 3,
                    fill: true,
                    tension: 0.1
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: { legend: { display: false } },
                scales: {
                    x: { 
                        type: 'linear', 
                        position: 'bottom', 
                        title: { display: true, text: 'Time' } 
                    },
                    y: { 
                        title: { display: true, text: 'Value' } 
                    }
                }
            }
        });
    },

    /**
     * Binds standard modal behaviors (open on trigger click, close on close-button or outside click)
     * @param {string} modalId - The ID of the modal element
     * @param {string} [openBtnId] - Optional ID of the button that opens the modal
     * @returns {Object} Control functions: { open(), close() }
     */
    bindModal(modalId, openBtnId = null) {
        const modal = document.getElementById(modalId);
        if (!modal) return { open: () => {}, close: () => {} };

        const closeBtn = modal.querySelector('.close-button');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => modal.style.display = 'none');
        }

        if (openBtnId) {
            const openBtn = document.getElementById(openBtnId);
            if (openBtn) {
                openBtn.addEventListener('click', () => modal.style.display = 'block');
            }
        }

        // Attach global window click listener if not already attached
        if (!window._modalListenerAttached) {
            window.addEventListener('click', (e) => {
                if (e.target.classList.contains('modal')) {
                    e.target.style.display = 'none';
                }
            });
            window._modalListenerAttached = true;
        }

        return {
            open: () => modal.style.display = 'block',
            close: () => modal.style.display = 'none'
        };
    },

    _createLineageDrawer(btn, activeStep) {
        const drawer = document.createElement('div');
        drawer.className = 'lineage-inline-drawer';
        drawer.innerHTML = `
          <button type="button" class="lineage-drawer-close" title="Close">&times;</button>
          <div class="lineage-strip">
              <div class="lineage-node ${activeStep === 'strain' ? 'active-step' : ''}">
                  <div class="lineage-node-header">
                      <span class="lineage-node-title"><span class="emoji">🧬</span> 1. Strain / Community</span>
                      <span class="lineage-node-code">BY4741</span>
                  </div>
                  <div class="lineage-node-desc">
                      Global catalog record describing the biological organism or consortium. Shared across all projects.
                  </div>
              </div>
              <div class="lineage-arrow">
                  <span>1:N</span>
                  <span>&rarr;</span>
              </div>
              <div class="lineage-node ${activeStep === 'source' ? 'active-step' : ''}">
                  <div class="lineage-node-header">
                      <span class="lineage-node-title"><span class="emoji">🧪</span> 2. Source Sample</span>
                      <span class="lineage-node-code">UCR-003-0001</span>
                  </div>
                  <div class="lineage-node-desc">
                      Links a Strain or Community to a project as a physical stock item (glycerol tube, isolate, or source well).
                  </div>
              </div>
              <div class="lineage-arrow">
                  <span>1:N</span>
                  <span>&rarr;</span>
              </div>
              <div class="lineage-node ${activeStep === 'experimental' ? 'active-step' : ''}">
                  <div class="lineage-node-header">
                      <span class="lineage-node-title"><span class="emoji">🧫</span> 3. Experimental Sample</span>
                      <span class="lineage-node-code">UCR-003-0001-01</span>
                  </div>
                  <div class="lineage-node-desc">
                      An instance of a Source Sample occurring in an experiment (such as a replicate well on an assay plate).
                  </div>
              </div>
          </div>
        `;

        drawer.querySelector('.lineage-drawer-close').addEventListener('click', () => {
            this.closeAllSampleLineageBanners();
        });

        return drawer;
    },

    closeAllSampleLineageBanners() {
        document.querySelectorAll('.lineage-inline-drawer').forEach(el => el.remove());
        document.querySelectorAll('.workflow-info-btn.active').forEach(b => b.classList.remove('active'));
    },

    /**
     * Toggles a non-blocking inline lineage callout drawer directly below the
     * nearest .table-header or .page-header, syncing across tabs on the page.
     */
    toggleSampleLineageBanner(triggerBtn, activeStep = 'source') {
        const isAlreadyOpen = triggerBtn.classList.contains('active');
        if (isAlreadyOpen) {
            this.closeAllSampleLineageBanners();
            return;
        }

        const allBtns = document.querySelectorAll('.workflow-info-btn');
        allBtns.forEach(btn => {
            const headerEl = btn.closest('.table-header, .page-header');
            if (!headerEl) return;
            const nextEl = headerEl.nextElementSibling;
            if (nextEl && nextEl.classList.contains('lineage-inline-drawer')) {
                nextEl.remove();
            }
            const step = btn.getAttribute('data-step') || activeStep;
            const drawer = this._createLineageDrawer(btn, step);
            headerEl.insertAdjacentElement('afterend', drawer);
            btn.classList.add('active');
        });
    }
};

window.UIUtils = UIUtils;
