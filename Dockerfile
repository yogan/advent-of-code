FROM ubuntu:23.10

RUN apt-get update && apt-get install -y \
    curl unzip locales build-essential \
    fish leiningen pypy3

# Bun - https://bun.sh/
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# DDP - https://ddp.le0n.dev/Bedienungsanleitung/DE/Einstieg/Installation
RUN cd /usr/local/share/ && \
    curl -OL https://github.com/DDP-Projekt/Kompilierer/releases/latest/download/DDP-v0.1.0-alpha-linux-amd64.tar.gz && \
    tar -xzf ./DDP-v0.1.0-alpha-linux-amd64.tar.gz && \
    rm ./DDP-v0.1.0-alpha-linux-amd64.tar.gz && \
    mv DDP-v0.1.0-alpha-linux-amd64 ddp && \
    cd ddp && \
    # --force only partially works, still needs some 'y' inputs, see:
    # https://github.com/DDP-Projekt/Installer/issues/1#issuecomment-1810635337
    yes | ./ddp-setup --force
ENV PATH="/usr/local/share/ddp/bin:${PATH}"
ENV DDPPATH="/usr/local/share/ddp"

# Rust - https://www.rust-lang.org/tools/install
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Zig - https://ziglang.org/learn/getting-started/
RUN cd /usr/local/share && \
    curl -OL https://ziglang.org/builds/zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    tar -xJf ./zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    rm ./zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    mv zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e zig
ENV PATH="/usr/local/share/zig:${PATH}"

WORKDIR /app
COPY . /app
