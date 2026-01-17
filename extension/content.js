// Cluedo Gmail Extension - Content Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

// Extract email data from the currently open email in Gmail
function extractEmailData() {
  const data = {
    subject: '',
    sender: '',
    snippet: '',
    date: ''
  };

  // Try to get the email subject from the h2 element
  const subjectEl = document.querySelector('h2[data-thread-perm-id]');
  if (subjectEl) {
    data.subject = subjectEl.textContent.trim();
  }

  // Alternative: try the subject line in email view
  if (!data.subject) {
    const altSubject = document.querySelector('[data-legacy-thread-id] h2, .hP');
    if (altSubject) {
      data.subject = altSubject.textContent.trim();
    }
  }

  // Get sender from the email header
  const senderEl = document.querySelector('[email], .gD');
  if (senderEl) {
    data.sender = senderEl.getAttribute('email') || senderEl.textContent.trim();
  }

  // Get a snippet of the email body
  const bodyEl = document.querySelector('.a3s.aiL, [data-message-id] .ii.gt');
  if (bodyEl) {
    // Get first 200 chars of text content
    const text = bodyEl.textContent.trim();
    data.snippet = text.substring(0, 200) + (text.length > 200 ? '...' : '');
  }

  // Get date
  const dateEl = document.querySelector('.g3, [data-tooltip]');
  if (dateEl) {
    data.date = dateEl.getAttribute('data-tooltip') || dateEl.textContent.trim();
  }

  return data;
}

// Listen for messages from the popup
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  if (request.action === 'getEmailData') {
    const emailData = extractEmailData();
    sendResponse(emailData);
  }
  return true;
});

// Add a quick-add button to Gmail's interface (optional enhancement)
function addQuickButton() {
  // Check if we're in an email view
  const toolbar = document.querySelector('.iH .G-Ni');
  if (!toolbar) return;

  // Don't add if already exists
  if (document.querySelector('.cluedo-quick-add')) return;

  const button = document.createElement('div');
  button.className = 'G-Ni J-J5-Ji cluedo-quick-add';
  button.innerHTML = `
    <div class="T-I J-J5-Ji aap T-I-awG T-I-ax7 L3"
         role="button"
         tabindex="0"
         data-tooltip="Add to Cluedo"
         aria-label="Add to Cluedo">
      <span class="asa">
        <svg viewBox="0 0 24 24" width="20" height="20" fill="currentColor">
          <path d="M19 3H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm-2 10h-4v4h-2v-4H7v-2h4V7h2v4h4v2z"/>
        </svg>
      </span>
    </div>
  `;

  button.addEventListener('click', () => {
    const emailData = extractEmailData();
    // Open popup with pre-filled data
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
function init() {
  const container = document.querySelector('[role="main"]');
  if (container) {
    observer.observe(container, { childList: true, subtree: true });
    addQuickButton();
  } else {
    setTimeout(init, 1000);
  }
}

// Initialize when page loads
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}
