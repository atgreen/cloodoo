// extension.js
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

import GLib from 'gi://GLib';
import Gio from 'gi://Gio';
import Shell from 'gi://Shell';
import Meta from 'gi://Meta';
import * as Main from 'resource:///org/gnome/shell/ui/main.js';
import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import * as Screenshot from 'resource:///org/gnome/shell/ui/screenshot.js';

export default class CloodooExtension extends Extension {
    enable() {
        this._settings = this.getSettings();

        // Register keyboard shortcut: Super+Shift+T
        Main.wm.addKeybinding(
            'capture-todo-screenshot',
            this._settings,
            Meta.KeyBindingFlags.NONE,
            Shell.ActionMode.NORMAL,
            () => this._onShortcutPressed()
        );
    }

    disable() {
        Main.wm.removeKeybinding('capture-todo-screenshot');
        this._settings = null;
    }

    async _onShortcutPressed() {
        try {
            log('[Cloodoo] Shortcut pressed!');
            // 1. Capture screenshot using gnome-screenshot
            const screenshotPath = await this._captureScreenshot();
            log(`[Cloodoo] Screenshot path: ${screenshotPath}`);
            if (!screenshotPath) {
                log('[Cloodoo] No screenshot path, exiting');
                return;
            }

            // 2. Show dialog for title and metadata
            log('[Cloodoo] Showing dialog...');
            const result = await this._showDialog(screenshotPath);
            log(`[Cloodoo] Dialog result: ${JSON.stringify(result)}`);
            if (!result) {
                // User cancelled - clean up temp file
                try {
                    GLib.unlink(screenshotPath);
                } catch (e) {
                    // Ignore cleanup errors
                }
                return;
            }

            // 3. Call cloodoo CLI to create todo
            log('[Cloodoo] Creating todo...');
            await this._createTodo(screenshotPath, result);
            log('[Cloodoo] _createTodo completed successfully');

            // 4. Show success notification
            log(`[Cloodoo] Showing notification for: ${result.title}`);
            Main.notify('Cloodoo', `Created: ${result.title}`);
        } catch (e) {
            logError(e, '[Cloodoo] Error in _onShortcutPressed');
            Main.notifyError('Cloodoo', `Error: ${e.message}`);
        }
    }

    async _captureScreenshot() {
        return new Promise((resolve) => {
            try {
                log('[Cloodoo] Opening Screenshot UI...');

                // Get Pictures directory
                const picturesDir = GLib.get_user_special_dir(GLib.UserDirectory.DIRECTORY_PICTURES)
                    || GLib.build_filenamev([GLib.get_home_dir(), 'Pictures']);
                log(`[Cloodoo] Pictures directory: ${picturesDir}`);

                // Record timestamp before screenshot
                const beforeTime = GLib.DateTime.new_now_local();

                // Create Screenshot UI
                const shooter = new Screenshot.ScreenshotUI();

                // Monitor for when the UI closes
                shooter.connect('closed', () => {
                    log('[Cloodoo] Screenshot UI closed, looking for screenshot file...');

                    // Give GNOME Shell a moment to finish writing the file
                    GLib.timeout_add(GLib.PRIORITY_DEFAULT, 500, () => {
                        try {
                            // Find newest screenshot in Pictures directory
                            const screenshot = this._findNewestScreenshot(picturesDir, beforeTime);
                            if (screenshot) {
                                log(`[Cloodoo] Found screenshot: ${screenshot}`);
                                resolve(screenshot);
                            } else {
                                log('[Cloodoo] No screenshot found (user likely cancelled)');
                                resolve(null);
                            }
                        } catch (e) {
                            logError(e, '[Cloodoo] Error finding screenshot');
                            resolve(null);
                        }
                        return GLib.SOURCE_REMOVE;
                    });
                });

                // Open the screenshot UI
                shooter.open();

            } catch (e) {
                logError(e, '[Cloodoo] Error in _captureScreenshot');
                resolve(null);
            }
        });
    }

    _findNewestScreenshot(directory, sinceTime) {
        try {
            // Search both Screenshots subdirectory and Pictures directory
            const searchDirs = [
                GLib.build_filenamev([directory, 'Screenshots']),  // Primary location
                directory  // Fallback to Pictures directly
            ];

            let newestFile = null;
            let newestTime = sinceTime.to_unix();

            for (const searchDir of searchDirs) {
                try {
                    const dir = Gio.File.new_for_path(searchDir);
                    if (!dir.query_exists(null)) {
                        continue;
                    }

                    const enumerator = dir.enumerate_children(
                        'standard::name,time::modified',
                        Gio.FileQueryInfoFlags.NONE,
                        null
                    );

                    let fileInfo;
                    while ((fileInfo = enumerator.next_file(null)) !== null) {
                        const name = fileInfo.get_name();
                        const nameLower = name.toLowerCase();

                        // Look for screenshot files (case-insensitive: "Screenshot from" or "Screenshot From")
                        if (nameLower.startsWith('screenshot from ') && nameLower.endsWith('.png')) {
                            const modTime = fileInfo.get_modification_date_time();
                            if (modTime && modTime.to_unix() > newestTime) {
                                newestTime = modTime.to_unix();
                                newestFile = GLib.build_filenamev([searchDir, name]);
                            }
                        }
                    }
                } catch (e) {
                    // Continue to next directory if this one fails
                    log(`[Cloodoo] Could not search ${searchDir}: ${e.message}`);
                }
            }

            return newestFile;
        } catch (e) {
            logError(e, '[Cloodoo] Error enumerating files');
            return null;
        }
    }

    async _showDialog(_screenshotPath) {
        log('[Cloodoo] _showDialog called');
        return new Promise(resolve => {
            // Use zenity for the dialog (simpler than creating custom GTK dialog)
            const argv = [
                'zenity',
                '--forms',
                '--title=Create TODO from Screenshot',
                '--text=Enter TODO details',
                '--add-entry=Title',
                '--add-combo=Priority',
                '--combo-values=high|medium|low',
                '--add-entry=Tags (comma-separated)'
            ];

            try {
                log('[Cloodoo] Launching zenity...');
                const proc = Gio.Subprocess.new(
                    argv,
                    Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_PIPE
                );

                log('[Cloodoo] Zenity subprocess created, waiting for response...');
                proc.communicate_utf8_async(null, null, (proc, result) => {
                    try {
                        const [, stdout, stderr] = proc.communicate_utf8_finish(result);
                        log(`[Cloodoo] Zenity finished. Success: ${proc.get_successful()}`);
                        log(`[Cloodoo] Zenity stdout: ${stdout}`);
                        if (stderr) log(`[Cloodoo] Zenity stderr: ${stderr}`);

                        if (!proc.get_successful()) {
                            log('[Cloodoo] User cancelled dialog');
                            resolve(null);  // User cancelled
                            return;
                        }

                        // Parse zenity output: title|priority|tags
                        const parts = stdout.trim().split('|');
                        if (parts.length >= 1 && parts[0]) {
                            resolve({
                                title: parts[0],
                                priority: parts[1] || 'medium',
                                tags: parts[2] || ''
                            });
                        } else {
                            resolve(null);
                        }
                    } catch (e) {
                        logError(e, '[Cloodoo] Error in zenity callback');
                        resolve(null);
                    }
                });
            } catch (e) {
                logError(e, '[Cloodoo] Error launching zenity');
                resolve(null);
            }
        });
    }

    async _createTodo(screenshotPath, todoData) {
        const {title, priority, tags} = todoData;
        const cloodooPath = this._settings.get_string('cloodoo-path') || 'cloodoo';

        const argv = [
            cloodooPath, 'add',
            title,
            '--attachment', screenshotPath,
            '--priority', priority,
            '--schedule', 'today'
        ];

        // Add tags
        if (tags) {
            const tagList = tags.split(/[,\s]+/).filter(t => t.length > 0);
            tagList.forEach(tag => {
                argv.push('--tag', tag.trim());
            });
        }

        log(`[Cloodoo] Launching cloodoo: ${argv.join(' ')}`);

        return new Promise((resolve, reject) => {
            try {
                const proc = Gio.Subprocess.new(
                    argv,
                    Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_PIPE
                );

                log('[Cloodoo] Subprocess created, waiting for completion...');

                proc.wait_async(null, (source, result) => {
                    log('[Cloodoo] wait_async callback triggered');
                    try {
                        source.wait_finish(result);
                        const success = source.get_successful();
                        log(`[Cloodoo] Subprocess completed. Success: ${success}`);

                        if (success) {
                            log('[Cloodoo] Resolving promise with success');
                            resolve();
                        } else {
                            log('[Cloodoo] Rejecting promise - subprocess failed');
                            reject(new Error('Failed to create todo'));
                        }
                    } catch (e) {
                        logError(e, '[Cloodoo] Error in wait_async callback');
                        reject(e);
                    }
                });
            } catch (e) {
                logError(e, '[Cloodoo] Error creating subprocess');
                reject(e);
            }
        });
    }
}
