# Build & runtime image for the McFarland Node/React application

FROM node:20-bookworm-slim AS builder
WORKDIR /app

# Install workspace dependencies (with hoisted node_modules)
COPY package.json package-lock.json ./
COPY client/package.json client/
COPY server/package.json server/
COPY tests/e2e/package.json tests/e2e/
RUN npm ci

# Copy the remainder of the repository and build
COPY . .
RUN npm run build

# Prune devDependencies to slim the runtime layer
RUN npm prune --omit=dev

FROM node:20-bookworm-slim AS runner
ENV NODE_ENV=production
WORKDIR /app

# Copy the built application (including pruned node_modules)
COPY --from=builder /app ./

# Ensure Render/other platforms know which port we listen on
EXPOSE 3000

CMD ["node", "server/dist/index.js"]
