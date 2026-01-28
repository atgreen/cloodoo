import Adw from 'gi://Adw';
import {ExtensionPreferences} from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export default class CloodooPreferences extends ExtensionPreferences {
    fillPreferencesWindow(window) {
        const settings = this.getSettings();

        const page = new Adw.PreferencesPage();
        const group = new Adw.PreferencesGroup({
            title: 'Cloodoo Settings',
            description: 'Configure the Cloodoo screenshot extension'
        });
        page.add(group);

        // Cloodoo path setting
        const pathRow = new Adw.EntryRow({
            title: 'Cloodoo Executable Path',
            text: settings.get_string('cloodoo-path')
        });
        pathRow.connect('changed', () => {
            settings.set_string('cloodoo-path', pathRow.text);
        });
        group.add(pathRow);

        window.add(page);
    }
}
