# Disable debug packages and stripping since this is a Lisp binary with dumped image
%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

Name:           cloodoo
Version:        %{version}
Release:        1%{?dist}
Summary:        Personal TODO system with a retro TUI
License:        MIT
URL:            https://github.com/atgreen/cloodoo
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  sbcl
BuildRequires:  gcc
BuildRequires:  make
BuildRequires:  git
BuildRequires:  sqlite-devel
BuildRequires:  openssl-devel
BuildRequires:  libzstd-devel
BuildRequires:  libfixposix-devel

Requires:       sqlite-libs
Requires:       libfixposix
Requires:       openssl-libs
Requires:       libzstd
Suggests:       gnome-shell

%description
Cloodoo is a personal TODO system featuring a retro TUI interface,
multi-device sync via gRPC with mTLS, LLM enrichment, and companion
Android and browser extension apps.

%prep
%setup -q

%install
mkdir -p %{buildroot}%{_bindir}
install -m 755 cloodoo %{buildroot}%{_bindir}/cloodoo

# GNOME Shell extension
mkdir -p %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/schemas
install -m 644 gnome-extension/extension.js %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
install -m 644 gnome-extension/prefs.js %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
install -m 644 gnome-extension/stylesheet.css %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
install -m 644 gnome-extension/metadata.json %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
install -m 644 gnome-extension/schemas/org.gnome.shell.extensions.cloodoo.gschema.xml %{buildroot}%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/schemas/

# GSettings schema (system-wide)
mkdir -p %{buildroot}%{_datadir}/glib-2.0/schemas
install -m 644 gnome-extension/schemas/org.gnome.shell.extensions.cloodoo.gschema.xml %{buildroot}%{_datadir}/glib-2.0/schemas/

# Man page
install -D -m 644 releng/cloodoo.1 %{buildroot}%{_mandir}/man1/cloodoo.1

# Native messaging host (system-wide, so no per-user setup-extension)
install -D -m 755 releng/native-messaging/cloodoo-native-host %{buildroot}%{_bindir}/cloodoo-native-host
install -D -m 644 releng/native-messaging/com.cloodoo.native.chrome.json \
  %{buildroot}%{_sysconfdir}/opt/chrome/native-messaging-hosts/com.cloodoo.native.json
install -D -m 644 releng/native-messaging/com.cloodoo.native.chrome.json \
  %{buildroot}%{_sysconfdir}/chromium/native-messaging-hosts/com.cloodoo.native.json
install -D -m 644 releng/native-messaging/com.cloodoo.native.firefox.json \
  %{buildroot}%{_libdir}/mozilla/native-messaging-hosts/com.cloodoo.native.json

%if 0%{?with_crx}
# Packed extension + external-extension prefs so Chrome/Chromium
# offer/install it for every user
install -D -m 644 cloodoo.crx %{buildroot}%{_datadir}/cloodoo/cloodoo.crx
install -D -m 644 external-extension.json \
  %{buildroot}%{_datadir}/google-chrome/extensions/lkagblncncheiiddbnpnoodghgjgagde.json
install -D -m 644 external-extension.json \
  %{buildroot}%{_datadir}/chromium/extensions/lkagblncncheiiddbnpnoodghgjgagde.json
%endif

# Browser extension
mkdir -p %{buildroot}%{_datadir}/cloodoo/browser-extension/icons
mkdir -p %{buildroot}%{_datadir}/cloodoo/browser-extension/popup
mkdir -p %{buildroot}%{_datadir}/cloodoo/browser-extension/options
install -m 644 browser-extension/manifest.json %{buildroot}%{_datadir}/cloodoo/browser-extension/
install -m 644 browser-extension/background.js %{buildroot}%{_datadir}/cloodoo/browser-extension/
install -m 644 browser-extension/content.js %{buildroot}%{_datadir}/cloodoo/browser-extension/
install -m 644 browser-extension/content.css %{buildroot}%{_datadir}/cloodoo/browser-extension/
install -m 644 browser-extension/icons/* %{buildroot}%{_datadir}/cloodoo/browser-extension/icons/
install -m 644 browser-extension/popup/* %{buildroot}%{_datadir}/cloodoo/browser-extension/popup/
install -m 644 browser-extension/options/* %{buildroot}%{_datadir}/cloodoo/browser-extension/options/

# No %post/%postun schema compilation: the glib2 package's RPM file
# triggers compile /usr/share/glib-2.0/schemas automatically.

%files
%license LICENSE
%license THIRD-PARTY-LICENSES
%doc README.md
%{_mandir}/man1/cloodoo.1*
%{_bindir}/cloodoo
%{_bindir}/cloodoo-native-host
%{_sysconfdir}/opt/chrome/native-messaging-hosts/com.cloodoo.native.json
%{_sysconfdir}/chromium/native-messaging-hosts/com.cloodoo.native.json
%{_libdir}/mozilla/native-messaging-hosts/com.cloodoo.native.json
%if 0%{?with_crx}
%{_datadir}/cloodoo/cloodoo.crx
%{_datadir}/google-chrome/extensions/lkagblncncheiiddbnpnoodghgjgagde.json
%{_datadir}/chromium/extensions/lkagblncncheiiddbnpnoodghgjgagde.json
%endif
%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
%{_datadir}/glib-2.0/schemas/org.gnome.shell.extensions.cloodoo.gschema.xml
%{_datadir}/cloodoo/browser-extension/

%changelog
