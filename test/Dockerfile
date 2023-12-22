# Add 2 language servers for manual testing.

# using specific version for repeatibility
FROM python:3.12-rc

RUN python -m pip install \
    python-lsp-server

RUN apt-get update && apt-get install -y \
    clangd \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
