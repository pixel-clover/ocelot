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

ARG WASM_META_REV=ce99eb6bc65d935ee5cb0e7563deb893793dc947
ARG WASM_FLAVOUR=9.12
RUN curl -sSf "https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/${WASM_META_REV}/bootstrap.sh" \
    | FLAVOUR="${WASM_FLAVOUR}" sh

COPY ocelot.cabal ./
COPY src/ src/
RUN . /root/.ghc-wasm/env && \
    wasm32-wasi-cabal update && \
    wasm32-wasi-cabal build --only-dependencies exe:ocelot-web -f -desktop -f wasm-reactor

COPY app-web/ app-web/
RUN . /root/.ghc-wasm/env && \
    wasm32-wasi-cabal build exe:ocelot-web -f -desktop -f wasm-reactor && \
    cp "$(wasm32-wasi-cabal list-bin exe:ocelot-web -f -desktop -f wasm-reactor)" ocelot.wasm && \
    wasm-opt -O3 ocelot.wasm -o ocelot.wasm

FROM nginx:1.27-alpine

COPY web/ /usr/share/nginx/html/
COPY --from=build /src/ocelot.wasm /usr/share/nginx/html/

RUN find /usr/share/nginx/html -type f \( \
        -name "*.wasm" -o -name "*.js" -o -name "*.html" -o -name "*.ttf" \
        -o -name "*.gb" -o -name "*.gbc" \
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
