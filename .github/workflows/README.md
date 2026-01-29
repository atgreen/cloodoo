# GitHub Actions Workflows

This directory contains CI/CD workflows for the Cloodoo project.

## Workflows

### ci.yml - Common Lisp Tests and Build
Runs on every push and PR to main:
- Installs SBCL and ocicl
- Installs project dependencies
- Generates protobuf code from proto definitions
- Runs FiveAM test suite
- Builds the `cloodoo` executable
- Uploads the Linux x86_64 binary as an artifact (7-day retention)

### android.yml - Android Build
Builds the Android APK:
- Sets up JDK 21 and Android SDK
- Caches Gradle dependencies for faster builds
- Builds debug APK using Gradle
- Runs Android unit tests
- Uploads both APK and test results as artifacts

### lint.yml - Code Quality Checks
Lints JavaScript code:
- **GNOME Extension**: Runs ESLint on gnome-extension/*.js
- **Browser Extension**: Validates manifest.json and runs npm lint if available

## Artifacts

Built artifacts are available for download from the Actions tab:
- `cloodoo-linux-x86_64`: The main TUI executable
- `cloodoo-debug-apk`: Android debug APK
- `android-test-results`: Android test reports

All artifacts are retained for 7 days.

## Local Testing

To test builds locally before pushing:

```sh
# Lisp tests
make cloodoo

# Android build
make android

# Linting
make lint-gnome-extension
```
