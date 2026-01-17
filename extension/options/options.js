// Cluedo Gmail Extension - Options Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

const DEFAULT_SERVER_URL = 'http://127.0.0.1:9876';

document.addEventListener('DOMContentLoaded', async () => {
  const form = document.getElementById('settings-form');
  const serverUrlInput = document.getElementById('server-url');
  const resetBtn = document.getElementById('reset-btn');
  const testBtn = document.getElementById('test-btn');
  const statusEl = document.getElementById('status');
  const connectionDot = document.getElementById('connection-dot');
  const connectionText = document.getElementById('connection-text');

  // Load saved settings
  async function loadSettings() {
    const result = await chrome.storage.sync.get(['serverUrl']);
    serverUrlInput.value = result.serverUrl || DEFAULT_SERVER_URL;
  }

  // Save settings
  async function saveSettings() {
    const serverUrl = serverUrlInput.value.trim() || DEFAULT_SERVER_URL;
    await chrome.storage.sync.set({ serverUrl });
    return serverUrl;
  }

  // Show status message
  function showStatus(message, isError = false) {
    statusEl.textContent = message;
    statusEl.className = 'status ' + (isError ? 'error' : 'success');
    setTimeout(() => {
      statusEl.className = 'status';
    }, 3000);
  }

  // Test connection
  async function testConnection() {
    connectionDot.className = 'connection-dot testing';
    connectionText.textContent = 'Testing...';

    const serverUrl = serverUrlInput.value.trim() || DEFAULT_SERVER_URL;

    try {
      const response = await fetch(`${serverUrl}/api/health`, {
        method: 'GET',
        headers: { 'Accept': 'application/json' }
      });

      if (response.ok) {
        connectionDot.className = 'connection-dot connected';
        connectionText.textContent = 'Connected';
      } else {
        connectionDot.className = 'connection-dot disconnected';
        connectionText.textContent = 'Server error: ' + response.status;
      }
    } catch (error) {
      connectionDot.className = 'connection-dot disconnected';
      connectionText.textContent = 'Connection failed';
    }
  }

  // Handle form submission
  form.addEventListener('submit', async (e) => {
    e.preventDefault();
    await saveSettings();
    showStatus('Settings saved successfully!');
  });

  // Handle reset button
  resetBtn.addEventListener('click', async () => {
    serverUrlInput.value = DEFAULT_SERVER_URL;
    await saveSettings();
    showStatus('Settings reset to default');
  });

  // Handle test button
  testBtn.addEventListener('click', testConnection);

  // Initialize
  await loadSettings();
});
