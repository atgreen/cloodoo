// Cloodoo Gmail Extension - Popup Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

document.addEventListener('DOMContentLoaded', async () => {
  const form = document.getElementById('todo-form');
  const titleInput = document.getElementById('title');
  const descriptionInput = document.getElementById('description');
  const prioritySelect = document.getElementById('priority');
  const dueDateInput = document.getElementById('due-date');
  const tagsInput = document.getElementById('tags');
  const submitBtn = document.getElementById('submit-btn');
  const statusEl = document.getElementById('status');
  const errorEl = document.getElementById('error-message');
  const successEl = document.getElementById('success-message');
  const successText = document.getElementById('success-text');
  const optionsLink = document.getElementById('options-link');
  const pendingBanner = document.getElementById('pending-banner');
  const pendingCountEl = document.getElementById('pending-count');
  const syncBtn = document.getElementById('sync-btn');

  let serverOnline = false;
  let pageUrl = null;  // Store current page URL

  // Check server health and update UI
  async function checkHealth() {
    try {
      const response = await chrome.runtime.sendMessage({ action: 'checkHealth' });
      serverOnline = response.healthy;
      if (response.healthy) {
        // Show connection method
        if (response.method === 'native') {
          statusEl.textContent = 'Native';
          statusEl.className = 'status native';
        } else {
          statusEl.textContent = 'HTTP';
          statusEl.className = 'status connected';
        }
        hideError();
      } else {
        statusEl.textContent = 'Offline';
        statusEl.className = 'status offline';
        // Don't show error - we can still add TODOs offline
      }
      // Always enable submit - we can store locally
      submitBtn.disabled = false;
    } catch (error) {
      statusEl.textContent = 'Offline';
      statusEl.className = 'status offline';
      serverOnline = false;
      submitBtn.disabled = false; // Can still add locally
    }
  }

  // Check for pending TODOs
  async function checkPending() {
    try {
      const response = await chrome.runtime.sendMessage({ action: 'getPendingCount' });
      if (response.count > 0) {
        pendingCountEl.textContent = response.count;
        pendingBanner.classList.remove('hidden');
      } else {
        pendingBanner.classList.add('hidden');
      }
    } catch (error) {
      console.log('Could not check pending TODOs:', error);
    }
  }

  // Sync pending TODOs
  async function syncPending() {
    syncBtn.disabled = true;
    syncBtn.textContent = '...';
    try {
      const response = await chrome.runtime.sendMessage({ action: 'syncPending' });
      if (response.synced > 0) {
        showSuccess(`Synced ${response.synced} TODO(s)!`);
      }
      await checkPending();
      await checkHealth();
    } catch (error) {
      showError('Sync failed: ' + error.message);
    } finally {
      syncBtn.disabled = false;
      syncBtn.innerHTML = '&#x21bb;';
    }
  }

  // Capture current page URL and optionally extract email data from Gmail
  async function capturePageData() {
    try {
      const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
      if (tab && tab.url) {
        // Always capture the current page URL
        pageUrl = tab.url;

        // If on Gmail, try to extract email-specific data
        if (tab.url.includes('mail.google.com')) {
          try {
            const response = await chrome.tabs.sendMessage(tab.id, { action: 'getEmailData' });
            if (response) {
              // Pre-fill form with email data
              if (response.subject) {
                titleInput.value = response.subject;
              }
              if (response.snippet) {
                descriptionInput.value = `Email from: ${response.sender || 'Unknown'}\n\n${response.snippet}`;
              }
              if (response.sender) {
                // Add sender as a tag
                tagsInput.value = 'email';
              }
            }
          } catch (e) {
            // Content script might not be loaded
            console.log('Could not extract email data:', e);
          }
        }
      }
    } catch (error) {
      console.log('Could not capture page data:', error);
    }
  }

  // Show error message
  function showError(message) {
    errorEl.textContent = message;
    errorEl.classList.remove('hidden');
    form.classList.remove('hidden');
    successEl.classList.add('hidden');
  }

  // Hide error message
  function hideError() {
    errorEl.classList.add('hidden');
  }

  // Show success message
  function showSuccess(message) {
    successText.textContent = message || 'TODO added successfully!';
    form.classList.add('hidden');
    successEl.classList.remove('hidden');
    setTimeout(() => {
      window.close();
    }, 1500);
  }

  // Handle form submission
  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    hideError();

    const title = titleInput.value.trim();
    if (!title) {
      showError('Please enter a title');
      return;
    }

    // Parse tags
    const tags = tagsInput.value
      .split(',')
      .map(t => t.trim())
      .filter(t => t.length > 0);

    // Build TODO object
    const todo = {
      title: title,
      description: descriptionInput.value.trim() || null,
      priority: prioritySelect.value,
      tags: tags.length > 0 ? tags : null
    };

    // Add due date if set
    if (dueDateInput.value) {
      todo.due_date = dueDateInput.value + 'T00:00:00Z';
    }

    // Add page URL if available
    if (pageUrl) {
      todo.url = pageUrl;
    }

    // Disable button and show loading
    submitBtn.disabled = true;
    submitBtn.classList.add('loading');

    try {
      const response = await chrome.runtime.sendMessage({
        action: 'createTodo',
        todo: todo
      });

      if (response.success) {
        if (response.pending) {
          // Saved locally, will sync later
          showSuccess('TODO saved! Will sync when server is available.');
        } else {
          showSuccess('TODO added successfully!');
        }
      } else {
        showError(response.error || 'Failed to create TODO');
        submitBtn.disabled = false;
        submitBtn.classList.remove('loading');
      }
    } catch (error) {
      showError('Error: ' + error.message);
      submitBtn.disabled = false;
      submitBtn.classList.remove('loading');
    }
  });

  // Handle sync button
  syncBtn.addEventListener('click', syncPending);

  // Handle options link
  optionsLink.addEventListener('click', (e) => {
    e.preventDefault();
    chrome.runtime.openOptionsPage();
  });

  // Initialize
  await checkHealth();
  await checkPending();
  await capturePageData();
  titleInput.focus();
});
