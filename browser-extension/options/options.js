// Cloodoo Browser Extension - Options Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

document.addEventListener('DOMContentLoaded', async () => {
  const testBtn = document.getElementById('test-btn');
  const connectionDot = document.getElementById('connection-dot');
  const connectionText = document.getElementById('connection-text');
  const installHelp = document.getElementById('install-help');

  // Test native messaging connection
  async function testConnection() {
    connectionDot.className = 'connection-dot testing';
    connectionText.textContent = 'Testing...';
    installHelp.classList.remove('visible');

    try {
      const response = await chrome.runtime.sendMessage({ action: 'checkNativeMessaging' });
      if (response && response.available) {
        connectionDot.className = 'connection-dot connected';
        connectionText.textContent = 'Connected';
      } else {
        connectionDot.className = 'connection-dot disconnected';
        connectionText.textContent = 'Not connected';
        installHelp.classList.add('visible');
      }
    } catch (error) {
      connectionDot.className = 'connection-dot disconnected';
      connectionText.textContent = 'Connection failed';
      installHelp.classList.add('visible');
    }
  }

  testBtn.addEventListener('click', testConnection);
});
