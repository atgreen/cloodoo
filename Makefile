.PHONY: android android-install install-gnome-extension clean

# Default target: build the TUI application
cloodoo: src/grpc-proto.lisp src/*.lisp *.asd
	sbcl --eval "(asdf:make :cloodoo)" --quit

# Build ag-protoc tool from ag-gRPC
ag-protoc: ocicl/ag-gRPC-*/ag-proto-cli/*.lisp ocicl/ag-gRPC-*/ag-proto/*.lisp
	@echo "Building ag-protoc tool..."
	@sbcl --non-interactive \
		--eval "(require 'asdf)" \
		--eval "(asdf:load-system :ag-proto-cli)" \
		--eval "(asdf:make :ag-proto-cli)" \
		--quit 2>&1 | grep -E "(Writing|saving)" || true
	@mv ocicl/ag-gRPC-*/ag-proto-cli/ag-protoc .

# Generate Lisp code from Protocol Buffer definition
# Uses ag-protoc with --class-prefix PROTO- to avoid conflicts with model classes
src/grpc-proto.lisp: ag-protoc android/app/src/main/proto/cloodoo_sync.proto
	@echo "Generating src/grpc-proto.lisp from proto file..."
	@./ag-protoc \
		-p cloodoo \
		--class-prefix PROTO- \
		-o $@ \
		android/app/src/main/proto/cloodoo_sync.proto

# Build Android app (output: android/app/build/outputs/apk/debug/app-debug.apk)
android:
	@echo "Building Android app..."
	@if [ -n "$$JAVA_HOME" ] && $$JAVA_HOME/bin/java -version 2>&1 | grep -q "version \"21"; then \
		echo "Using existing JAVA_HOME: $$JAVA_HOME"; \
		cd android && ./gradlew assembleDebug; \
	elif [ -d /usr/lib/jvm/java-21-openjdk ]; then \
		echo "Setting JAVA_HOME to /usr/lib/jvm/java-21-openjdk"; \
		cd android && JAVA_HOME=/usr/lib/jvm/java-21-openjdk ./gradlew assembleDebug; \
	else \
		echo "Error: Java 21 not found. Please install JDK 21 or set JAVA_HOME"; \
		exit 1; \
	fi

# Install Android app to connected device via ADB
# Set DEVICE to target a specific device: make android-install DEVICE=58181FDCQ003T4
android-install: android
	@echo "Installing Android app..."
	@if [ -n "$(DEVICE)" ]; then \
		adb -s $(DEVICE) install -r android/app/build/outputs/apk/debug/app-debug.apk; \
	elif adb devices | grep -q "device$$"; then \
		adb install -r android/app/build/outputs/apk/debug/app-debug.apk; \
	else \
		echo "No Android device connected"; \
		exit 1; \
	fi

# Install GNOME Shell extension
install-gnome-extension:
	@echo "Installing GNOME Shell extension..."
	@cd gnome-extension && glib-compile-schemas schemas/
	@mkdir -p ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com
	@cp -r gnome-extension/* ~/.local/share/gnome-shell/extensions/cloodoo-screenshot@moxielogic.com/
	@echo "Extension installed. Please restart GNOME Shell:"
	@echo "  X11: Press Alt+F2, type 'r', press Enter"
	@echo "  Wayland: Log out and log back in"
	@echo "Then enable with: gnome-extensions enable cloodoo-screenshot@moxielogic.com"

clean:
	rm -rf *~ cloodoo src/grpc-proto.lisp
