FROM ubuntu:23.10

RUN apt-get update && apt-get install -y \
    bc curl ca-certificates gnupg unzip locales build-essential cmake \
    libcurl3-gnutls \
    vim fish leiningen nim r-base r-cran-testthat ruby3.1 \
    elixir erlang-base erlang-dev erlang-eunit rebar3 \
    python3 python3-pip python3-pytest python3-pytest-subtests python3-pytest-pylint \
    pypy3 pypy3-venv \
    dotnet8 dotnet-sdk-7.0

# Bun - https://bun.sh/
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# Node.js - https://github.com/nodesource/distributions/?tab=readme-ov-file#installation-instructions
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_21.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get install -y nodejs

# Crystal - https://crystal-lang.org
RUN curl -fsSL https://crystal-lang.org/install.sh | bash

# DDP - https://ddp.le0n.dev/Bedienungsanleitung/DE/Einstieg/Installation
RUN cd /usr/local/share/ && \
    curl -OL https://github.com/DDP-Projekt/Kompilierer/releases/download/v0.2.0-alpha/DDP-v0.2.0-alpha-linux-amd64.tar.gz && \
    tar -xzf ./DDP-v0.2.0-alpha-linux-amd64.tar.gz && \
    rm ./DDP-v0.2.0-alpha-linux-amd64.tar.gz && \
    mv DDP-v0.2.0-alpha-linux-amd64 ddp && \
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

# Common LISP via Roswell
# https://github.com/roswell/roswell/wiki/Installation
# https://github.com/roswell/roswell/releases
RUN curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb && \
    dpkg -i roswell.deb && \
    rm roswell.deb && \
    ros install quicklisp

# Julia via Juliaup
# https://julialang.org/downloads/
# https://github.com/JuliaLang/juliaup
RUN curl -fsSL https://install.julialang.org | sh -s -- --yes
ENV PATH="/root/.juliaup/bin:${PATH}"

# Add Ruby Minitest gem
RUN gem install minitest

# Gleam
# https://gleam.run/getting-started/installing
# Gleam requires Erlang, so we install erlang-base above.
# (elixir would already pull it in as a dependency, but it seems clearer to install it explicitly)
# We need erlang-eunit for unit tests, and also erlang-dev, otherwise eunit.hrl is missing
# Gleam itself is a single static binary, so we just download it and put it in the PATH.
# Exercism tests require rebar3, which is installed via apt as well.
RUN curl -fsSL https://github.com/gleam-lang/gleam/releases/download/v1.0.0-rc2/gleam-v1.0.0-rc2-x86_64-unknown-linux-musl.tar.gz | tar -xzf - -C /usr/local/bin
