# Banana Board web app

A React + TypeScript interface backed by the original Haskell solver. Enter up to 25 English letters and select **Find my board** or press Enter. The server returns the solver's actual board. The search is bounded (20 iterations, 20 candidate states per iteration), so “couldn't find a board” doesn't prove no board exists. Each search has a 20-second timeout.

## Run with Docker

From the repository root:

```sh
docker build -t banana-board .
docker run --rm -p 8080:8080 banana-board
```

Visit http://localhost:8080. Run `python3 tests/smoke.py` while it is running for basic API checks. The container serves both frontend and API from one origin; no external API or secret is required.

## Local development

Install GHC/Cabal, Node 20, and the system dependencies needed to build the Haskell packages. Run `cabal update && cabal run exe:banana-web` from the repository root. In another terminal, run `cd web && npm install && npm run dev`. Vite proxies `/api` to port 8080. For a production-style local run, first build the frontend with `npm run build` in `web`.

## Hosting

`render.yaml` describes a Docker web service for Render. Connect this repository and deploy it as a Blueprint, or deploy the Dockerfile to another container host. The server listens on the host-provided `PORT`. The container is required: static-only hosting cannot run the Haskell backend. A small host may need additional memory for difficult searches; the limit and timeout can be adjusted in `server/Main.hs`.

The original solver and dictionary remain unchanged. The API accepts `POST /api/solve` with `{"letters":"cat"}` and responds with a solved grid, `not_found`, or an error. `GET /api/health` is a health check.
