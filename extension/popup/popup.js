// Cluedo Gmail Extension - Popup Script
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
  const optionsLink = document.getElementById('options-link');

  // Check server health
  async function checkHealth() {
    try {
      const response = await chrome.runtime.sendMessage({ action: 'checkHealth' });
      if (response.healthy) {
        statusEl.textContent = 'Connected';
        statusEl.className = 'status connected';
        submitBtn.disabled = false;
      } else {
        statusEl.textContent = 'Offline';
        statusEl.className = 'status disconnected';
        showError('Cannot connect to Cluedo server. Make sure "cluedo server" is running.');
      }
    } catch (error) {
      statusEl.textContent = 'Error';
      statusEl.className = 'status disconnected';
      showError('Extension error: ' + error.message);
    }
  }

  // Try to extract email data from Gmail
  async function extractEmailData() {
    try {
      const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
      if (tab && tab.url && tab.url.includes('mail.google.com')) {
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
      }
    } catch (error) {
      // Silently fail - user might not be on Gmail
      console.log('Could not extract email data:', error);
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
  function showSuccess() {
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

    // Disable button and show loading
    submitBtn.disabled = true;
    submitBtn.classList.add('loading');

    try {
      const response = await chrome.runtime.sendMessage({
        action: 'createTodo',
        todo: todo
      });

      if (response.success) {
        showSuccess();
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

  // Handle options link
  optionsLink.addEventListener('click', (e) => {
    e.preventDefault();
    chrome.runtime.openOptionsPage();
  });

  // Initialize
  await checkHealth();
  await extractEmailData();
  titleInput.focus();
});
