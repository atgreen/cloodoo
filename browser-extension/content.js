// Cloodoo Browser Extension - Content Script
// SPDX-License-Identifier: MIT
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(function() {
  'use strict';

  // Site-specific selectors for each field, ordered by reliability
  const SITE_SELECTORS = {
    gmail: {
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
    },
    outlook: {
      subject: [
        // Stable: title attribute on span inside subject container
        '[id$="_SUBJECT"] span[title]',
        // Stable: heading role with subject aria
        '[role="heading"][aria-level="3"] span[title]',
        // Less stable: specific class with title
        'span[title][class]'
      ],
      sender: [
        // Stable: aria-label starting with "From:"
        '[aria-label^="From:"]',
        // Stable: ID pattern ending in _FROM, find nested span
        '[id$="_FROM"] span[class]'
      ],
      body: [
        // Stable: aria-label for message body
        '[aria-label="Message body"]',
        // Stable: ID pattern for message body
        '[id^="UniqueMessageBody"]'
      ],
      date: [
        // Stable: data-testid is usually not minified
        '[data-testid="SentReceivedSavedTime"]',
        // Stable: ID pattern ending in _DATETIME
        '[id$="_DATETIME"]'
      ]
    }
  };

  // Known email sites for DOM recording and selector lookup
  const KNOWN_EMAIL_SITES = {
    'mail.google.com': 'gmail',
    'outlook.live.com': 'outlook',
    'outlook.office.com': 'outlook',
    'outlook.office365.com': 'outlook',
    'mail.yahoo.com': 'yahoo',
    'mail.proton.me': 'protonmail',
    'mail.zoho.com': 'zoho'
  };

  // Check if we're on a known email site
  function getEmailSite() {
    const hostname = window.location.hostname;
    for (const [domain, site] of Object.entries(KNOWN_EMAIL_SITES)) {
      if (hostname.includes(domain)) {
        return site;
      }
    }
    return null;
  }

  // Get selectors for the current site, with fallback to gmail (most complete)
  function getSelectors() {
    const site = getEmailSite();
    return SITE_SELECTORS[site] || SITE_SELECTORS.gmail;
  }

  // Check if extraction result has useful data
  function hasUsefulData(data) {
    return data && (data.subject || data.sender);
  }

  // Record DOM for later analysis when extraction fails.
  // Gated behind the domRecordingEnabled storage flag (disabled by default).
  function recordDomForAnalysis(extractionResult) {
    const site = getEmailSite();
    console.log('Cloodoo: recordDomForAnalysis called, site:', site, 'extractionResult:', extractionResult);
    if (!site) return; // Only record for known email sites

    chrome.storage.local.get('domRecordingEnabled', (result) => {
      if (!result.domRecordingEnabled) {
        console.log('Cloodoo: DOM recording disabled, skipping');
        return;
      }

      console.log('Cloodoo: Recording DOM for analysis (extraction failed)');

      // Get the main content area HTML to avoid capturing the entire page
      const mainContent = document.querySelector('[role="main"]');
      const html = mainContent ? mainContent.outerHTML : document.body.innerHTML;

      // Limit size to avoid huge payloads (max 500KB)
      const maxSize = 500 * 1024;
      const truncatedHtml = html.length > maxSize
        ? html.substring(0, maxSize) + '\n<!-- TRUNCATED -->'
        : html;

      chrome.runtime.sendMessage({
        action: 'recordDom',
        site: site,
        url: window.location.href,
        html: truncatedHtml,
        extractionResult: extractionResult
      }).catch(err => {
        console.log('Cloodoo: Failed to record DOM:', err);
      });
    });
  }

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
    const selectors = getSelectors();
    const data = {
      subject: '',
      sender: '',
      snippet: '',
      date: '',
      url: window.location.href
    };

    // Extract subject - for Outlook, prefer title attribute
    const subjectEl = queryWithFallbacks(selectors.subject);
    if (subjectEl) {
      const subject = (subjectEl.getAttribute('title') ||
                      subjectEl.textContent || '').trim();
      // Only set if non-empty after trimming
      if (subject.length > 0) {
        data.subject = subject;
      }
    }

    // Extract sender - prefer email attribute, then parse aria-label, then text
    const senderEl = queryWithFallbacks(selectors.sender);
    if (senderEl) {
      const ariaLabel = senderEl.getAttribute('aria-label') || '';
      // Try to extract email from aria-label like "From: Name <email>"
      const emailMatch = ariaLabel.match(/<([^>]+)>/);
      // Try to extract name from aria-label like "From: Name" or "From: Name <email>"
      const fromMatch = ariaLabel.match(/^From:\s*([^<]+)/);
      const nameFromLabel = fromMatch ? fromMatch[1].trim() : null;

      const sender = (senderEl.getAttribute('email') ||
                     senderEl.getAttribute('data-hovercard-id') ||
                     (emailMatch ? emailMatch[1] : null) ||
                     nameFromLabel ||
                     senderEl.textContent || '').trim();
      if (sender.length > 0) {
        data.sender = sender;
      }
    }

    // Extract body snippet
    const bodyEl = queryWithFallbacks(selectors.body);
    if (bodyEl) {
      const text = (bodyEl.textContent || '').trim();
      if (text.length > 0) {
        data.snippet = text.substring(0, 200) + (text.length > 200 ? '...' : '');
      }
    }

    // Extract date
    const dateEl = queryWithFallbacks(selectors.date);
    if (dateEl) {
      const date = (dateEl.getAttribute('title') ||
                   dateEl.getAttribute('data-tooltip') ||
                   dateEl.textContent || '').trim();
      if (date.length > 0) {
        data.date = date;
      }
    }

    return data;
  }

  // Check if data has meaningful content (not just whitespace)
  function hasRealContent(data) {
    return data.subject && data.subject.trim().length > 0;
  }

  // Watch for email content to load using MutationObserver
  function observeAndExtract(timeout = 2000) {
    return new Promise((resolve) => {
      // First try immediate extraction
      const immediateData = extractEmailData();
      if (hasRealContent(immediateData)) {
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
        if (hasRealContent(data)) {
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
        if (hasRealContent(data)) {
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
          const finalData = extractEmailData();
          console.log('Cloodoo: finalData:', finalData, 'hasUsefulData:', hasUsefulData(finalData), 'emailSite:', getEmailSite());
          // Record DOM if we couldn't extract useful data from a known email site
          if (!hasUsefulData(finalData) && getEmailSite()) {
            recordDomForAnalysis(finalData);
          }
          resolve(finalData);
        }
      }, timeout);
    });
  }

  // Listen for messages from the popup
  chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
    if (request.action === 'getEmailData') {
      console.log('Cloodoo: Popup requested email data');
      // Use 4-second timeout to handle slow-loading email content
      observeAndExtract(4000).then(emailData => {
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

  console.log('Cloodoo: Content script loaded on', window.location.hostname);
  console.log('Cloodoo: Detected email site:', getEmailSite());
  startObserver();

})();
