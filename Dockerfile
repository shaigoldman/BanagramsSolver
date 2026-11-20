FROM node:20-bookworm-slim AS frontend
WORKDIR /app/web
COPY web/package*.json ./
RUN npm install --no-audit --no-fund
COPY web/ ./
RUN npm run build

FROM haskell:9.6 AS backend
WORKDIR /app
COPY banana-web.cabal ./
RUN cabal update && cabal build --only-dependencies
COPY src/ ./src/
COPY server/ ./server/
RUN cabal build exe:banana-web && cp "$(cabal list-bin exe:banana-web)" /app/banana-web

FROM debian:bookworm-slim
WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates libgmp10 && rm -rf /var/lib/apt/lists/*
COPY --from=backend /app/banana-web ./banana-web
COPY --from=frontend /app/web/dist ./web/dist
COPY words.txt ./words.txt
ENV PORT=8080
EXPOSE 8080
CMD ["./banana-web", "+RTS", "-N2", "-RTS"]
