// Cloodoo Browser Extension - Popup Script
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

  // Set due date to today by default
  const today = new Date().toISOString().split('T')[0];
  dueDateInput.value = today;

  let serverOnline = false;
  let availableTags = [];  // Tags for autocomplete

  // Create autocomplete dropdown
  const autocompleteList = document.createElement('div');
  autocompleteList.className = 'autocomplete-list hidden';
  tagsInput.parentNode.style.position = 'relative';
  tagsInput.parentNode.appendChild(autocompleteList);

  // Fetch available tags from cloodoo
  async function fetchTags() {
    try {
      const response = await chrome.runtime.sendMessage({ action: 'getTags' });
      if (response.success && response.tags) {
        availableTags = response.tags;
        console.log('Cloodoo: Loaded', availableTags.length, 'tags for autocomplete');
      }
    } catch (error) {
      console.log('Could not fetch tags:', error);
    }
  }

  // Get the current word being typed (after last comma/space)
  function getCurrentWord(input) {
    const value = input.value;
    const cursorPos = input.selectionStart;
    const beforeCursor = value.substring(0, cursorPos);
    const match = beforeCursor.match(/(?:^|[,\s])([^,\s]*)$/);
    return match ? match[1] : '';
  }

  // Replace current word with selected tag
  function selectTag(tag) {
    const value = tagsInput.value;
    const cursorPos = tagsInput.selectionStart;
    const beforeCursor = value.substring(0, cursorPos);
    const afterCursor = value.substring(cursorPos);

    // Find where current word starts
    const match = beforeCursor.match(/(?:^|[,\s])([^,\s]*)$/);
    if (match) {
      const wordStart = beforeCursor.length - match[1].length;
      const newValue = value.substring(0, wordStart) + tag + ', ' + afterCursor.replace(/^[,\s]*/, '');
      tagsInput.value = newValue.replace(/,\s*$/, '');  // Remove trailing comma
      tagsInput.focus();
    }
    hideAutocomplete();
  }

  // Show autocomplete suggestions
  function showAutocomplete(suggestions) {
    if (suggestions.length === 0) {
      hideAutocomplete();
      return;
    }
    autocompleteList.innerHTML = '';
    suggestions.slice(0, 8).forEach((tag, index) => {
      const item = document.createElement('div');
      item.className = 'autocomplete-item';
      item.textContent = tag;
      if (index === 0) item.classList.add('selected');
      item.addEventListener('click', () => selectTag(tag));
      autocompleteList.appendChild(item);
    });
    autocompleteList.classList.remove('hidden');
  }

  function hideAutocomplete() {
    autocompleteList.classList.add('hidden');
    autocompleteList.innerHTML = '';
  }

  // Handle tags input for autocomplete
  tagsInput.addEventListener('input', () => {
    const currentWord = getCurrentWord(tagsInput).toLowerCase();
    if (currentWord.length === 0) {
      hideAutocomplete();
      return;
    }
    // Filter tags that match and aren't already in the input
    const existingTags = tagsInput.value.toLowerCase().split(/[,\s]+/).filter(t => t);
    const suggestions = availableTags.filter(tag =>
      tag.toLowerCase().startsWith(currentWord) &&
      !existingTags.includes(tag.toLowerCase())
    );
    showAutocomplete(suggestions);
  });

  // Handle keyboard navigation in autocomplete
  tagsInput.addEventListener('keydown', (e) => {
    if (autocompleteList.classList.contains('hidden')) return;

    const items = autocompleteList.querySelectorAll('.autocomplete-item');
    const selected = autocompleteList.querySelector('.autocomplete-item.selected');
    const selectedIndex = Array.from(items).indexOf(selected);

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (selected) selected.classList.remove('selected');
      const next = items[(selectedIndex + 1) % items.length];
      next.classList.add('selected');
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (selected) selected.classList.remove('selected');
      const prev = items[(selectedIndex - 1 + items.length) % items.length];
      prev.classList.add('selected');
    } else if (e.key === 'Enter' || e.key === 'Tab') {
      if (selected) {
        e.preventDefault();
        selectTag(selected.textContent);
      }
    } else if (e.key === 'Escape') {
      hideAutocomplete();
    }
  });

  // Hide autocomplete when clicking outside
  document.addEventListener('click', (e) => {
    if (!tagsInput.contains(e.target) && !autocompleteList.contains(e.target)) {
      hideAutocomplete();
    }
  });
  let pageUrl = null;  // Store current page URL

  // Check server health and update UI
  async function checkHealth() {
    try {
      const response = await chrome.runtime.sendMessage({ action: 'checkHealth' });
      serverOnline = response.healthy;
      if (response.healthy) {
        statusEl.textContent = 'Connected';
        statusEl.className = 'status native';
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

  // Known email sites where we inject content script
  const EMAIL_SITES = [
    'mail.google.com',
    'outlook.live.com',
    'outlook.office.com',
    'outlook.office365.com',
    'mail.yahoo.com',
    'mail.proton.me',
    'mail.zoho.com'
  ];

  // Check if URL is a known email site
  function isEmailSite(url) {
    return EMAIL_SITES.some(site => url.includes(site));
  }

  // Sanitize text by removing invisible Unicode characters that can break TUI rendering
  // Removes: zero-width spaces, combining marks, non-breaking spaces, figure spaces, etc.
  function sanitizeText(text) {
    if (!text) return text;
    return text
      // Remove zero-width characters
      .replace(/[\u034F\u200B\u200C\u200D\uFEFF]/g, '')
      // Replace non-breaking spaces and figure spaces with regular spaces
      .replace(/[\u00A0\u2007]/g, ' ')
      // Collapse multiple spaces
      .replace(/  +/g, ' ')
      .trim();
  }

  // Pre-fill the form from email data (used by both pending stash and content-script paths)
  function prefillFromEmailData(data) {
    if (data.subject) {
      titleInput.value = sanitizeText(data.subject);
    }
    if (data.snippet) {
      const sender = sanitizeText(data.sender) || 'Unknown';
      const snippet = sanitizeText(data.snippet);
      descriptionInput.value = `Email from: ${sender}\n\n${snippet}`;
    }
    if (data.sender) {
      tagsInput.value = 'email';
    }
    if (data.url) {
      pageUrl = data.url;
    }
  }

  // Capture current page URL and optionally extract email data from email sites.
  // If pendingEmailData was stashed by the quick-add button, use that instead.
  async function capturePageData() {
    try {
      // Check for data stashed by the Gmail quick-add button (via background.js openPopupWithData)
      const { pendingEmailData } = await chrome.storage.local.get('pendingEmailData');
      if (pendingEmailData) {
        console.log('Cloodoo popup: using pending email data from quick-add button');
        prefillFromEmailData(pendingEmailData);
        await chrome.storage.local.remove('pendingEmailData');
        return;
      }

      const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
      if (tab && tab.url) {
        // Always capture the current page URL
        pageUrl = tab.url;

        // If on a known email site, try to extract email-specific data
        console.log('Cloodoo popup: checking URL', tab.url, 'isEmailSite:', isEmailSite(tab.url));
        if (isEmailSite(tab.url)) {
          console.log('Cloodoo popup: requesting email data from content script');
          try {
            const response = await chrome.tabs.sendMessage(tab.id, { action: 'getEmailData' });
            if (response) {
              prefillFromEmailData(response);
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

    // Add due date if set - use current time with local timezone offset
    if (dueDateInput.value) {
      const now = new Date();
      const hours = String(now.getHours()).padStart(2, '0');
      const minutes = String(now.getMinutes()).padStart(2, '0');
      const seconds = String(now.getSeconds()).padStart(2, '0');
      const offset = -now.getTimezoneOffset();
      const offsetSign = offset >= 0 ? '+' : '-';
      const offsetHours = String(Math.floor(Math.abs(offset) / 60)).padStart(2, '0');
      const offsetMins = String(Math.abs(offset) % 60).padStart(2, '0');
      todo.due_date = `${dueDateInput.value}T${hours}:${minutes}:${seconds}${offsetSign}${offsetHours}:${offsetMins}`;
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
  await fetchTags();
  await capturePageData();
  titleInput.focus();
});
