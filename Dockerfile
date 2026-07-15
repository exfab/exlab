# Stage 1: Build the application
FROM docker.io/ocaml/opam:debian-12-ocaml-5.3 AS build

# Install system dependencies
# (binutils is added to strip the final binary and reduce size)
RUN sudo apt-get -o Acquire::ForceIPv4=true update && \
    sudo apt-get -o Acquire::ForceIPv4=true install -y --no-install-recommends \
        pkg-config libpq-dev libssl-dev libgmp-dev libev-dev binutils wget && \
    sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN sudo chown opam:opam /app

# Copy the opam file first to cache dependencies
COPY --chown=opam:opam exlab.opam ./

# Update opam and install dependencies using all CPU cores with verbose output.
# We include --with-test so bisect_ppx is installed, preventing dune deadlocks.
RUN opam update && \
    opam install . --deps-only --with-test -y -j $(nproc) --verbose

# Copy the rest of the application source code
COPY --chown=opam:opam . .

# Build the project using the release profile
RUN eval $(opam env) && dune build --profile release

# Strip debugging symbols from the compiled binary to drastically reduce its size
RUN sudo strip _build/default/bin/main.exe

# Stage 2: Create the lean runtime image
FROM debian:12-slim

# Create a non-root user for security
RUN groupadd -r exlab && useradd -r -g exlab exlab

# Install only the necessary runtime dependencies
RUN apt-get -o Acquire::ForceIPv4=true update && \
    apt-get -o Acquire::ForceIPv4=true install -y --no-install-recommends \
        libpq5 libgmp10 libev4 ca-certificates && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the stripped executable and static assets
COPY --from=build --chown=exlab:exlab /app/_build/default/bin/main.exe ./exlab
COPY --from=build --chown=exlab:exlab /app/src/server/static ./src/server/static
COPY --from=build --chown=exlab:exlab /app/docs ./docs

# Set environment variables
ENV OCAMLRUNPARAM=b
ENV DREAM_VERBOSITY=info
ENV DEFAULT_ADMIN_EMAIL="admin@exlab.com"
ENV DEFAULT_ADMIN_PASSWORD="admin123"

# Switch to the non-root user
USER exlab

EXPOSE 8080

CMD ["./exlab"]
