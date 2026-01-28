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

cloodoo: src/grpc-proto.lisp src/*.lisp *.asd
	sbcl --eval "(asdf:make :cloodoo)" --quit

clean:
	rm -rf *~ cloodoo src/grpc-proto.lisp
