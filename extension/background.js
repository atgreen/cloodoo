// Cluedo Gmail Extension - Background Service Worker
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

const DEFAULT_SERVER_URL = 'http://127.0.0.1:9876';

// Get server URL from storage
async function getServerUrl() {
  const result = await chrome.storage.sync.get(['serverUrl']);
  return result.serverUrl || DEFAULT_SERVER_URL;
}

// Check if server is healthy
async function checkServerHealth() {
  try {
    const serverUrl = await getServerUrl();
    const response = await fetch(`${serverUrl}/api/health`, {
      method: 'GET',
      headers: { 'Accept': 'application/json' }
    });
    return response.ok;
  } catch (error) {
    console.error('Cluedo server health check failed:', error);
    return false;
  }
}

// Create a TODO via the API
async function createTodo(todoData) {
  const serverUrl = await getServerUrl();
  const response = await fetch(`${serverUrl}/api/todos`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    },
    body: JSON.stringify(todoData)
  });

  if (!response.ok) {
    throw new Error(`Server error: ${response.status}`);
  }

  return await response.json();
}

// Handle messages from popup and content script
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  if (request.action === 'checkHealth') {
    checkServerHealth().then(healthy => {
      sendResponse({ healthy });
    });
    return true; // Will respond asynchronously
  }

  if (request.action === 'createTodo') {
    createTodo(request.todo)
      .then(result => {
        sendResponse({ success: true, todo: result });
      })
      .catch(error => {
        sendResponse({ success: false, error: error.message });
      });
    return true; // Will respond asynchronously
  }

  if (request.action === 'getServerUrl') {
    getServerUrl().then(url => {
      sendResponse({ url });
    });
    return true;
  }
});

// Show notification on successful TODO creation
function showNotification(title) {
  chrome.notifications.create({
    type: 'basic',
    iconUrl: 'icons/icon48.png',
    title: 'TODO Added',
    message: title
  });
}
