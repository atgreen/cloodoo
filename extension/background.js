// Cloodoo Browser Extension - Background Service Worker
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

const NATIVE_HOST_NAME = 'com.cloodoo.native';
const PENDING_TODOS_KEY = 'pendingTodos';

// Track if native messaging is available
let nativeMessagingAvailable = null;

// Get pending TODOs from local storage
async function getPendingTodos() {
  const result = await chrome.storage.local.get([PENDING_TODOS_KEY]);
  return result[PENDING_TODOS_KEY] || [];
}

// Save pending TODOs to local storage
async function savePendingTodos(todos) {
  await chrome.storage.local.set({ [PENDING_TODOS_KEY]: todos });
}

// Generate a stable client ID for idempotent creation
function generateClientId() {
  const ts = Date.now().toString(36);
  const rnd = Math.random().toString(36).substr(2, 9);
  return `ext-${ts}-${rnd}`;
}

// Add a TODO to the pending queue
async function addPendingTodo(todo) {
  const pending = await getPendingTodos();
  const pendingTodo = {
    ...todo,
    _pendingId: Date.now().toString() + Math.random().toString(36).substr(2, 9),
    _clientId: todo._clientId || generateClientId(),
    _createdAt: new Date().toISOString()
  };
  pending.push(pendingTodo);
  await savePendingTodos(pending);
  return pendingTodo;
}

// Remove a TODO from the pending queue
async function removePendingTodo(pendingId) {
  const pending = await getPendingTodos();
  const filtered = pending.filter(t => t._pendingId !== pendingId);
  await savePendingTodos(filtered);
}

// Send message via native messaging
function sendNativeMessage(message) {
  return new Promise((resolve, reject) => {
    chrome.runtime.sendNativeMessage(NATIVE_HOST_NAME, message, (response) => {
      if (chrome.runtime.lastError) {
        reject(new Error(chrome.runtime.lastError.message));
      } else {
        resolve(response);
      }
    });
  });
}

// Check if native messaging is available
async function checkNativeMessaging() {
  // Only use cache for positive results - retry on failure
  if (nativeMessagingAvailable === true) {
    return true;
  }
  try {
    console.log('Checking native messaging...');
    const response = await sendNativeMessage({ action: 'ping' });
    console.log('Native messaging response:', response);
    nativeMessagingAvailable = response && response.success;
    return nativeMessagingAvailable;
  } catch (error) {
    console.log('Native messaging not available:', error.message);
    nativeMessagingAvailable = false;
    return false;
  }
}

// Check health via native messaging
async function checkHealth() {
  const nativeAvailable = await checkNativeMessaging();
  return { healthy: nativeAvailable, method: nativeAvailable ? 'native' : 'none' };
}

// Create a TODO via native messaging
async function createTodoNative(todoData) {
  console.log('createTodoNative called with:', todoData);
  const response = await sendNativeMessage({
    action: 'createTodo',
    todo: todoData
  });
  console.log('createTodoNative response:', response);
  if (response && response.success) {
    return response;
  } else {
    throw new Error(response?.error || 'Native messaging failed');
  }
}

// Try to create a TODO - uses native messaging, falls back to local queue
async function createTodo(todoData) {
  console.log('createTodo called with:', todoData);
  // Generate a stable client ID so that retries are idempotent
  const clientId = generateClientId();
  const todoWithClientId = { ...todoData, client_id: clientId };

  const nativeAvailable = await checkNativeMessaging();
  if (nativeAvailable) {
    try {
      const result = await createTodoNative(todoWithClientId);
      console.log('Native messaging succeeded:', result);
      return { success: true, todo: result.todo, synced: true, method: 'native' };
    } catch (error) {
      console.log('Native messaging failed, storing locally:', error.message);
    }
  }

  // Store locally when native messaging is unavailable
  const pendingTodo = await addPendingTodo({ ...todoData, _clientId: clientId });
  return { success: true, todo: pendingTodo, synced: false, pending: true };
}

// Sync a single pending TODO via native messaging
async function syncOneTodo(todo) {
  const { _pendingId, _createdAt, _clientId, ...todoData } = todo;
  // Include client_id for idempotent creation on the backend
  if (_clientId) {
    todoData.client_id = _clientId;
  }

  const nativeAvailable = await checkNativeMessaging();
  if (nativeAvailable) {
    try {
      await createTodoNative(todoData);
      return true;
    } catch (error) {
      return false;
    }
  }

  return false;
}

// Sync all pending TODOs
// Uses atomic drain: clears queue first, then re-adds failures.
// This prevents Manifest V3 service worker kills from leaving stale
// entries that get re-sent every 5 minutes, creating duplicates.
async function syncPendingTodos() {
  const pending = await getPendingTodos();
  if (pending.length === 0) {
    return { synced: 0, failed: 0, remaining: 0 };
  }

  // Atomically clear the queue BEFORE processing.
  // If the service worker dies mid-sync, we lose pending items rather
  // than creating duplicates on every alarm cycle.
  await savePendingTodos([]);

  let synced = 0;
  const failedTodos = [];

  for (const todo of pending) {
    const success = await syncOneTodo(todo);
    if (success) {
      synced++;
    } else {
      failedTodos.push(todo);
    }
  }

  // Re-add any that failed back to the queue
  if (failedTodos.length > 0) {
    const current = await getPendingTodos();
    await savePendingTodos([...current, ...failedTodos]);
  }

  return { synced, failed: failedTodos.length, remaining: failedTodos.length };
}

// Handle messages from popup and content script
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  if (request.action === 'checkHealth') {
    checkHealth().then(result => {
      sendResponse(result);
    });
    return true;
  }

  if (request.action === 'createTodo') {
    createTodo(request.todo)
      .then(result => {
        sendResponse(result);
      })
      .catch(error => {
        sendResponse({ success: false, error: error.message });
      });
    return true;
  }

  if (request.action === 'getPendingCount') {
    getPendingTodos().then(todos => {
      sendResponse({ count: todos.length });
    });
    return true;
  }

  if (request.action === 'syncPending') {
    syncPendingTodos().then(result => {
      sendResponse(result);
    });
    return true;
  }

  if (request.action === 'getPendingTodos') {
    getPendingTodos().then(todos => {
      sendResponse({ todos });
    });
    return true;
  }

  if (request.action === 'checkNativeMessaging') {
    checkNativeMessaging().then(available => {
      sendResponse({ available });
    });
    return true;
  }

  if (request.action === 'openPopupWithData') {
    // Stash email data so the popup can pre-fill from it
    chrome.storage.local.set({ pendingEmailData: request.emailData }, () => {
      chrome.windows.create({
        url: 'popup/popup.html',
        type: 'popup',
        width: 420,
        height: 520
      });
    });
    return false;
  }

  if (request.action === 'recordDom') {
    // Record DOM for analysis when extraction fails on known email sites.
    // Gated behind the domRecordingEnabled flag (disabled by default).
    (async () => {
      try {
        const { domRecordingEnabled } = await chrome.storage.local.get('domRecordingEnabled');
        if (!domRecordingEnabled) {
          console.log('Cloodoo: DOM recording disabled, skipping');
          sendResponse({ success: false, error: 'DOM recording disabled' });
          return;
        }
        const nativeAvailable = await checkNativeMessaging();
        if (nativeAvailable) {
          const response = await sendNativeMessage({
            action: 'recordDom',
            url: request.url,
            html: request.html,
            site: request.site,
            extractionResult: request.extractionResult
          });
          console.log('Cloodoo: DOM recorded for analysis:', response);
          sendResponse(response);
        } else {
          console.log('Cloodoo: Cannot record DOM - native messaging unavailable');
          sendResponse({ success: false, error: 'Native messaging unavailable' });
        }
      } catch (error) {
        console.log('Cloodoo: Failed to record DOM:', error);
        sendResponse({ success: false, error: error.message });
      }
    })();
    return true;
  }

  if (request.action === 'getTags') {
    // Get list of all unique tags for autocomplete
    (async () => {
      try {
        const nativeAvailable = await checkNativeMessaging();
        if (nativeAvailable) {
          const response = await sendNativeMessage({ action: 'getTags' });
          sendResponse(response);
        } else {
          sendResponse({ success: false, error: 'Native messaging unavailable', tags: [] });
        }
      } catch (error) {
        console.log('Cloodoo: Failed to get tags:', error);
        sendResponse({ success: false, error: error.message, tags: [] });
      }
    })();
    return true;
  }
});

// Try to sync pending TODOs periodically
chrome.alarms.create('syncPending', { periodInMinutes: 5 });

chrome.alarms.onAlarm.addListener((alarm) => {
  if (alarm.name === 'syncPending') {
    syncPendingTodos().then(result => {
      if (result.synced > 0) {
        console.log(`Cloodoo: Synced ${result.synced} pending TODOs`);
      }
    });
  }
});

// Try to sync when extension starts
syncPendingTodos();

// Reset native messaging check on startup (in case cloodoo was installed)
nativeMessagingAvailable = null;
