FROM --platform=linux/amd64 debian:trixie-slim AS build

WORKDIR /src

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    jq \
    make \
    unzip \
    xz-utils \
    zstd \
    && rm -rf /var/lib/apt/lists/*

# Install GHC wasm toolchain (auto-detects host architecture).
RUN curl -sSf https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh \
    | FLAVOUR=9.6 sh

# Copy only the cabal file first so that the expensive dependency build is
# cached by Docker unless the package manifest changes.
COPY ocelot.cabal ./
RUN . /root/.ghc-wasm/env && \
    wasm32-wasi-cabal update && \
    wasm32-wasi-cabal build --only-dependencies exe:ocelot-web -f -desktop -f wasm-reactor

# Build the emulator.
COPY src/ src/
COPY app-web/ app-web/
RUN . /root/.ghc-wasm/env && \
    wasm32-wasi-cabal build exe:ocelot-web -f -desktop -f wasm-reactor && \
    cp "$(wasm32-wasi-cabal list-bin exe:ocelot-web -f -desktop -f wasm-reactor)" ocelot.wasm

# Pinned to a specific minor for reproducibility; bump deliberately when needed.
FROM nginx:1.27-alpine

COPY web/ /usr/share/nginx/html/
COPY --from=build /src/ocelot.wasm /usr/share/nginx/html/

# Pre-compress static assets so nginx can serve them via gzip_static. Keeps the
# original alongside (-k) so clients without gzip support still work.
RUN find /usr/share/nginx/html -type f \( \
        -name "*.wasm" -o -name "*.js" -o -name "*.html" -o -name "*.ttf" \
    \) -exec gzip -9 -k -f {} \;

RUN printf '%s\n' \
    'server {' \
    '    listen 80;' \
    '    server_name _;' \
    '    root /usr/share/nginx/html;' \
    '    index index.html;' \
    '    gzip_static on;' \
    '    location / { try_files $uri $uri/ =404; }' \
    '}' > /etc/nginx/conf.d/default.conf

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --spider -q http://localhost/ || exit 1

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
