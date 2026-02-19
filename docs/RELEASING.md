# Release Process for Cloodoo

This document describes how to create a new release of Cloodoo with packages for Linux, macOS, and Android.

## Prerequisites

- Push access to the Cloodoo repository
- Git configured with your credentials

## Release Steps

### 1. Update Version Numbers

Update the version in the following files:

- `cloodoo.asd` - Update the `:version` field (primary source of truth)
- `README.md` - Update version references if needed

The following files have their version substituted automatically by CI:

- `releng/cloodoo.spec` - Version passed via `--define` during rpmbuild
- `releng/debian/control` - `@VERSION@` placeholder replaced during deb build

### 2. Create Release Notes

Create a new release notes file in `docs/release-notes/` named `RELEASE-NOTES-X.Y.Z.md` documenting:
- Summary of changes
- Bug fixes
- New features
- Breaking changes (if any)

If no release notes file exists, the GitHub Release will use auto-generated notes.

### 3. Commit Changes

```bash
git add cloodoo.asd README.md docs/release-notes/RELEASE-NOTES-X.Y.Z.md
git commit -m "Boost version to X.Y.Z"
```

### 4. Create and Push Tag

```bash
git tag vX.Y.Z
git push origin main
git push origin vX.Y.Z
```

### 5. GitHub Actions Builds Packages

Once you push the tag, GitHub Actions will automatically build packages for all platforms:

#### Linux Packages

1. **Linux tarball** (`cloodoo-X.Y.Z-linux-x86_64.tar.gz`)
   - Built from source on Ubuntu
   - Contains binary, LICENSE, and README.md

2. **RPM package** (`cloodoo-X.Y.Z-1.fc*.x86_64.rpm`)
   - Built from source in Fedora container
   - Uses `releng/cloodoo.spec`

3. **DEB package** (`cloodoo_X.Y.Z-1_amd64.deb`)
   - Built from source on Ubuntu
   - Uses `releng/debian/` packaging files

#### macOS Packages

4. **macOS ARM64** (`cloodoo-X.Y.Z-macos-arm64.tar.gz`)
   - For Apple Silicon Macs (M1/M2/M3/M4)

5. **macOS x64** (`cloodoo-X.Y.Z-macos-x64.tar.gz`)
   - For Intel Macs

#### Android

6. **Android APK** (`cloodoo-X.Y.Z-android.apk`)
   - Debug-signed APK (release signing is a future improvement)

#### GitHub Release

7. **Create GitHub Release**
   - Attaches all artifacts to the release
   - Release can be found at: `https://github.com/atgreen/cloodoo/releases/tag/vX.Y.Z`

### 6. Verify Release

Check the GitHub Actions workflow at: `https://github.com/atgreen/cloodoo/actions`

Verify the release includes all packages:
- Linux: `.tar.gz`, `.rpm`, `.deb`
- macOS: `-macos-arm64.tar.gz`, `-macos-x64.tar.gz`
- Android: `.apk`

### 7. Test Packages (Recommended)

#### Test Linux tarball:
```bash
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo-X.Y.Z-linux-x86_64.tar.gz
tar xzf cloodoo-X.Y.Z-linux-x86_64.tar.gz
./cloodoo-X.Y.Z-linux-x86_64/cloodoo --version
```

#### Test RPM on Fedora:
```bash
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo-X.Y.Z-1.fc*.x86_64.rpm
sudo dnf install ./cloodoo-X.Y.Z-1.fc*.x86_64.rpm
cloodoo --version
```

Runtime dependencies (installed automatically by dnf): `sqlite-libs`, `libfixposix`, `openssl-libs`, `zstd-libs`.

#### Test DEB on Debian/Ubuntu:
```bash
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo_X.Y.Z-1_amd64.deb
sudo apt install ./cloodoo_X.Y.Z-1_amd64.deb
cloodoo --version
```

Runtime dependencies (installed automatically by apt): `libsqlite3-0`, `libfixposix4|libfixposix3`, `libssl3|libssl1.1`, `libzstd1`.

#### Test on macOS:
```bash
# ARM64 (Apple Silicon)
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo-X.Y.Z-macos-arm64.tar.gz
tar xzf cloodoo-X.Y.Z-macos-arm64.tar.gz
./cloodoo-X.Y.Z-macos-arm64/cloodoo --version

# x64 (Intel)
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo-X.Y.Z-macos-x64.tar.gz
tar xzf cloodoo-X.Y.Z-macos-x64.tar.gz
./cloodoo-X.Y.Z-macos-x64/cloodoo --version
```

macOS requires `libfixposix` (install via `brew install libfixposix`).

#### Test Android APK:
```bash
curl -LO https://github.com/atgreen/cloodoo/releases/download/vX.Y.Z/cloodoo-X.Y.Z-android.apk
adb install cloodoo-X.Y.Z-android.apk
```

## Package Details

### RPM Package (`releng/cloodoo.spec`)
- Pre-built binary packaged via rpmbuild
- Runtime dependencies: `sqlite-libs`, `libfixposix`, `openssl-libs`, `zstd-libs`
- Installs binary to `/usr/bin/cloodoo`

### DEB Package (`releng/debian/`)
- Pre-built binary packaged via dpkg-deb
- Runtime dependencies: `libsqlite3-0`, `libfixposix4|libfixposix3`, `libssl3|libssl1.1`, `libzstd1`
- Installs binary to `/usr/bin/cloodoo`

### macOS Packages
- Tarballs with binary, LICENSE, and README.md
- Requires `libfixposix` from Homebrew

### Android APK
- Debug-signed APK built with Gradle
- Future improvement: release signing with keystore in GitHub secrets

## Directory Structure

```
releng/
├── debian/               # Debian package files
│   ├── changelog
│   ├── compat
│   ├── control
│   ├── copyright
│   └── rules
└── cloodoo.spec          # RPM spec file
```

## Troubleshooting

### Build fails in GitHub Actions
- Check the Actions tab for error logs
- Common issues:
  - SBCL build errors (check ocicl dependencies)
  - Version string mismatches
  - macOS: Homebrew SBCL or libfixposix issues
  - RPM: Fedora container missing build deps
  - Android: Gradle or SDK version issues

### Package installation fails
- Verify runtime dependencies are installed
- Check architecture (packages are platform-specific)
- macOS: Ensure `libfixposix` is installed via Homebrew

## Future Improvements

- [ ] Add Linux ARM64 builds
- [ ] Release-signed Android APK
- [ ] Automate Homebrew formula updates
- [ ] Sign RPM packages with GPG
- [ ] Sign DEB packages with GPG
