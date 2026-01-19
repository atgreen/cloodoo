// Cloodoo Gmail Extension - Content Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(function() {
  'use strict';

  // Selectors for each field, ordered by reliability
  const SELECTORS = {
    subject: [
      'h2[data-thread-perm-id]',
      '[data-legacy-thread-id] h2',
      '.hP',
      'h2.hP',
      '[role="main"] h2[tabindex="-1"]',
      '.ha h2',
      '[data-thread-id] h2'
    ],
    sender: [
      '.gD[email]',
      '[email]',
      '.gD',
      '.go',
      '[data-hovercard-id]',
      '.yP',
      '.zF'
    ],
    body: [
      '.a3s.aiL',
      '[data-message-id] .ii.gt',
      '.ii.gt .a3s',
      '.ii.gt',
      '.a3s'
    ],
    date: [
      '.gH .g3',
      '.g3',
      'span[data-tooltip]',
      '.gH span[title]'
    ]
  };

  // Try multiple selectors until one works
  function queryWithFallbacks(selectors) {
    for (const selector of selectors) {
      try {
        const el = document.querySelector(selector);
        if (el) return el;
      } catch (e) {
        // Invalid selector, skip
      }
    }
    return null;
  }

  // Extract email data from the currently open email
  function extractEmailData() {
    const data = {
      subject: '',
      sender: '',
      snippet: '',
      date: '',
      url: window.location.href
    };

    // Extract subject
    const subjectEl = queryWithFallbacks(SELECTORS.subject);
    if (subjectEl) {
      data.subject = subjectEl.textContent.trim();
    }

    // Extract sender - prefer email attribute
    const senderEl = queryWithFallbacks(SELECTORS.sender);
    if (senderEl) {
      data.sender = senderEl.getAttribute('email') ||
                    senderEl.getAttribute('data-hovercard-id') ||
                    senderEl.textContent.trim();
    }

    // Extract body snippet
    const bodyEl = queryWithFallbacks(SELECTORS.body);
    if (bodyEl) {
      const text = bodyEl.textContent.trim();
      data.snippet = text.substring(0, 200) + (text.length > 200 ? '...' : '');
    }

    // Extract date
    const dateEl = queryWithFallbacks(SELECTORS.date);
    if (dateEl) {
      data.date = dateEl.getAttribute('title') ||
                  dateEl.getAttribute('data-tooltip') ||
                  dateEl.textContent.trim();
    }

    return data;
  }

  // Watch for email content to load using MutationObserver
  function observeAndExtract(timeout = 2000) {
    return new Promise((resolve) => {
      // First try immediate extraction
      const immediateData = extractEmailData();
      if (immediateData.subject) {
        console.log('Cloodoo: Immediate extraction succeeded');
        resolve(immediateData);
        return;
      }

      let resolved = false;
      let attempts = 0;
      const maxAttempts = 10;

      // Set up observer to watch for content changes
      const observer = new MutationObserver(() => {
        if (resolved) return;
        attempts++;

        const data = extractEmailData();
        if (data.subject) {
          console.log('Cloodoo: Observer extraction succeeded after', attempts, 'mutations');
          resolved = true;
          observer.disconnect();
          resolve(data);
        }
      });

      // Observe the main content area
      const container = document.querySelector('[role="main"]') || document.body;
      observer.observe(container, {
        childList: true,
        subtree: true
      });

      // Also retry periodically in case mutations don't trigger
      const interval = setInterval(() => {
        if (resolved) {
          clearInterval(interval);
          return;
        }
        const data = extractEmailData();
        if (data.subject) {
          console.log('Cloodoo: Interval extraction succeeded');
          resolved = true;
          observer.disconnect();
          clearInterval(interval);
          resolve(data);
        }
      }, 200);

      // Timeout fallback - return whatever we have
      setTimeout(() => {
        if (!resolved) {
          console.log('Cloodoo: Extraction timed out, returning partial data');
          resolved = true;
          observer.disconnect();
          clearInterval(interval);
          resolve(extractEmailData());
        }
      }, timeout);
    });
  }

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
    if (request.action === 'getEmailData') {
      console.log('Cloodoo: Popup requested email data');
      observeAndExtract(1500).then(emailData => {
        console.log('Cloodoo: Sending to popup:', emailData);
        sendResponse(emailData);
      });
      return true; // Keep channel open for async response
    }
    return false;
  });

  // Add a quick-add button to Gmail's interface
  function addQuickButton() {
    const toolbar = document.querySelector('.iH .G-Ni');
    if (!toolbar) return;

    if (document.querySelector('.cloodoo-quick-add')) return;

    const button = document.createElement('div');
    button.className = 'G-Ni J-J5-Ji cloodoo-quick-add';
    button.innerHTML = `
      <div class="T-I J-J5-Ji aap T-I-awG T-I-ax7 L3"
           role="button"
           tabindex="0"
           data-tooltip="Add to Cloodoo"
           aria-label="Add to Cloodoo">
        <span class="asa">
          <svg viewBox="0 0 24 24" width="20" height="20" fill="currentColor">
            <path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10h-4v4h-2v-4H7v-2h4V7h2v4h4v2z"/>
          </svg>
        </span>
      </div>
    `;

    button.addEventListener('click', async () => {
      const emailData = await observeAndExtract(1000);
      chrome.runtime.sendMessage({
        action: 'openPopupWithData',
        emailData: emailData
      });
    });

    toolbar.appendChild(button);
  }

  // Observe DOM changes to add button when email is opened
  const observer = new MutationObserver(() => {
    addQuickButton();
  });

  // Start observing once Gmail is loaded
  function startObserver() {
    const container = document.querySelector('[role="main"]');
    if (container) {
      observer.observe(container, { childList: true, subtree: true });
      addQuickButton();
    } else {
      setTimeout(startObserver, 1000);
    }
  }

  console.log('Cloodoo: Content script loaded');
  startObserver();

})();
