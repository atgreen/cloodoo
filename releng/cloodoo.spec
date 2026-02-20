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

%post
glib-compile-schemas %{_datadir}/glib-2.0/schemas/ || true

%postun
glib-compile-schemas %{_datadir}/glib-2.0/schemas/ || true

%files
%license LICENSE
%{_bindir}/cloodoo
%{_datadir}/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
%{_datadir}/glib-2.0/schemas/org.gnome.shell.extensions.cloodoo.gschema.xml
%{_datadir}/cloodoo/browser-extension/

%changelog
