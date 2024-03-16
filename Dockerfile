FROM ubuntu:23.10

RUN apt-get update && apt-get install -y \
    # base/util packages
    bc curl ca-certificates gnupg unzip locales build-essential cmake libcurl3-gnutls \
    # various languages directly from apt
    vim fish leiningen nim r-base r-cran-testthat ruby3.1 polyml \
    elixir erlang-base erlang-dev erlang-eunit rebar3 openjdk-8-jdk openjdk-22-jdk \
    # Python
    python3 python3-pip python3-pytest python3-pytest-subtests python3-pytest-pylint \
    pypy3 pypy3-venv \
    # .NET
    dotnet8 dotnet-sdk-7.0 \
    # Swift deps, see: https://www.swift.org/install/linux/#installation-via-tarball
    binutils git gnupg2 libc6-dev libcurl4-openssl-dev libedit2 libgcc-9-dev \
    libpython3.8 libsqlite3-0 libstdc++-9-dev libxml2-dev libz3-dev pkg-config \
    tzdata zlib1g-dev unzip

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
    curl -fsSLO https://github.com/DDP-Projekt/Kompilierer/releases/download/v0.2.0-alpha/DDP-v0.2.0-alpha-linux-amd64.tar.gz && \
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
RUN curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs | sh -s -- -y --no-modify-path
ENV PATH="/root/.cargo/bin:${PATH}"

# Zig - https://ziglang.org/learn/getting-started/
RUN cd /usr/local/share && \
    curl -fsSLO https://ziglang.org/builds/zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    tar -xJf ./zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    rm ./zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e.tar.xz && \
    mv zig-linux-x86_64-0.12.0-dev.1753+a98d4a66e zig
ENV PATH="/usr/local/share/zig:${PATH}"

# Common LISP via Roswell
# https://github.com/roswell/roswell/wiki/Installation
# https://github.com/roswell/roswell/releases
RUN curl -fsSL https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb && \
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

# Swift
# https://www.swift.org/install/linux/#installation-via-tarball
# tarball has a usr/ directory, so we strip that and extract to /usr
RUN curl -fsSL https://download.swift.org/swift-5.9.2-release/ubuntu2204/swift-5.9.2-RELEASE/swift-5.9.2-RELEASE-ubuntu22.04.tar.gz | tar -xzf - -C /usr --strip-components=2

# PowerShell universal package
# https://learn.microsoft.com/en-us/powershell/scripting/install/install-ubuntu?view=powershell-7.4#installation-via-direct-download
RUN curl -fsSL https://github.com/PowerShell/PowerShell/releases/download/v7.4.1/powershell_7.4.1-1.deb_amd64.deb --output powershell.deb && \
    dpkg -i powershell.deb && \
    rm powershell.deb && \
    pwsh -c "Install-Module -Name Pester -Force -SkipPublisherCheck -Scope AllUsers"

# Haskell via GHCup - https://www.haskell.org/ghcup
# Docker steps as recommended by ghcup maintainer here: https://stackoverflow.com/a/71513191/183582
# (adapted to remove gpg verification, which did not work for some reason)
RUN curl -fsSL https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup
ARG GHC=recommended
ARG CABAL=latest
ARG STACK=recommended
RUN ghcup -v install ghc   --isolate /usr/local     --force ${GHC}   && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK}

# Scala via cs setup - https://www.scala-lang.org/download
RUN curl -fsSL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | \
    gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --dir /usr/local/share/scala
ENV PATH="/usr/local/share/scala:${PATH}"
RUN sbt --script-version

# vim: tw=0
