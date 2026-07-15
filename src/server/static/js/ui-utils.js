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
    }
};

window.UIUtils = UIUtils;
