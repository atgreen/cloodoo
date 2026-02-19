%global debug_package %{nil}

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

%description
Cloodoo is a personal TODO system featuring a retro TUI interface,
multi-device sync via gRPC with mTLS, LLM enrichment, and companion
Android and browser extension apps.

%prep
%setup -q

%install
mkdir -p %{buildroot}%{_bindir}
install -m 755 cloodoo %{buildroot}%{_bindir}/cloodoo

%files
%license LICENSE
%{_bindir}/cloodoo

%changelog
