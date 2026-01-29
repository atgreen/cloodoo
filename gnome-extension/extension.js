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
            // 1. Capture screenshot using gnome-screenshot
            const screenshotPath = await this._captureScreenshot();
            if (!screenshotPath) {
                return;
            }

            // 2. Show dialog for title and metadata
            const result = await this._showDialog(screenshotPath);
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
            await this._createTodo(screenshotPath, result);

            // 4. Show success notification
            Main.notify('Cloodoo', `Created: ${result.title}`);
        } catch (e) {
            Main.notifyError('Cloodoo', `Error: ${e.message}`);
        }
    }

    async _captureScreenshot() {
        return new Promise((resolve, reject) => {
            const timestamp = GLib.DateTime.new_now_local().format('%Y%m%d-%H%M%S');
            const filename = `cloodoo-screenshot-${timestamp}.png`;
            const filepath = GLib.build_filenamev([GLib.get_tmp_dir(), filename]);

            // Use gnome-screenshot for area selection
            // -a = area selection, -f = file output
            const argv = ['gnome-screenshot', '-a', '-f', filepath];

            try {
                const proc = Gio.Subprocess.new(
                    argv,
                    Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_PIPE
                );

                proc.wait_async(null, (proc, result) => {
                    try {
                        proc.wait_finish(result);
                        if (proc.get_successful() && GLib.file_test(filepath, GLib.FileTest.EXISTS)) {
                            resolve(filepath);
                        } else {
                            resolve(null);  // User cancelled
                        }
                    } catch (e) {
                        reject(e);
                    }
                });
            } catch (e) {
                reject(e);
            }
        });
    }

    async _showDialog(_screenshotPath) {
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
                const proc = Gio.Subprocess.new(
                    argv,
                    Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_PIPE
                );

                proc.communicate_utf8_async(null, null, (proc, result) => {
                    try {
                        const [, stdout, _stderr] = proc.communicate_utf8_finish(result);

                        if (!proc.get_successful()) {
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
                        resolve(null);
                    }
                });
            } catch (e) {
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
            '--priority', priority
        ];

        // Add tags
        if (tags) {
            const tagList = tags.split(/[,\s]+/).filter(t => t.length > 0);
            tagList.forEach(tag => {
                argv.push('--tag', tag.trim());
            });
        }

        return new Promise((resolve, reject) => {
            try {
                const proc = Gio.Subprocess.new(
                    argv,
                    Gio.SubprocessFlags.STDOUT_PIPE | Gio.SubprocessFlags.STDERR_PIPE
                );

                proc.wait_async(null, (proc, result) => {
                    try {
                        proc.wait_finish(result);
                        if (proc.get_successful()) {
                            resolve();
                        } else {
                            reject(new Error('Failed to create todo'));
                        }
                    } catch (e) {
                        reject(e);
                    }
                });
            } catch (e) {
                reject(e);
            }
        });
    }
}
