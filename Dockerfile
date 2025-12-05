FROM ubuntu:24.04

# See https://docs.docker.com/build/building/best-practices/#apt-get
# TL;DR:
# - always combine RUN apt-get update with apt-get install in the same RUN statement
# - add rm -rf /var/lib/apt/lists/* to the end (next apt-get update will re-fetch anyway)
RUN apt-get update && apt-get install -y --no-install-recommends \
    # base/util packages
    bc curl ca-certificates gnupg unzip locales build-essential cmake libcurl3-gnutls jq \
    # this brings add-apt-repository, needed later:
    software-properties-common \
    # various languages directly from apt
    vim fish bats leiningen r-base r-cran-testthat ruby3.2 polyml golang-go gawk \
    guile-3.0 crystal elixir erlang-base erlang-dev erlang-eunit rebar3 nim \
    openjdk-8-jdk-headless swi-prolog-nox tcl gfortran octave octave-symbolic \
    # shellcheck and fd for the shellcheck script
    shellcheck fd-find \
    # Python
    python3 python3-pip python3-pytest python3-pytest-subtests python3-pytest-pylint \
    pypy3 pypy3-venv pypy3-dev \
    # Build deps for Pillow (PIL fork), required by matplotlib
    # see: https://pillow.readthedocs.io/en/latest/installation/building-from-source.html#building-from-source
    libtiff5-dev libjpeg8-dev libopenjp2-7-dev zlib1g-dev libfreetype6-dev liblcms2-dev \
    libwebp-dev tcl8.6-dev tk8.6-dev python3-tk libharfbuzz-dev libfribidi-dev libxcb1-dev \
    # Lua and busted as testing framework
    lua5.4 lua-busted \
    # Swift deps, see: https://www.swift.org/install/linux/#installation-via-tarball
    binutils git gnupg2 libc6-dev libcurl4-openssl-dev libedit2 libgcc-9-dev \
    libpython3.8 libsqlite3-0 libstdc++-9-dev libxml2-dev libz3-dev pkg-config \
    tzdata zlib1g-dev \
    # Crystal deps
    libssl-dev \
    # Haskell (GHC) deps
    libncurses-dev \
    # Idris deps; GMP is needed for Pack, see
    # https://github.com/stefan-hoeck/idris2-pack/blob/main/INSTALL.md#1-preparations
    chezscheme libgmp3-dev && \
    rm -rf /var/lib/apt/lists/*

# Bun - https://bun.sh/
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# Node.js - https://github.com/nodesource/distributions/?tab=readme-ov-file#installation-instructions
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_21.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

# DDP - https://doku.ddp.im/Einstieg/Installation/
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
RUN curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs | sh -s -- -y --no-modify-path && \
    # Remove docs (~ 700 MB), not needed in container
    rm -rf /root/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc
ENV PATH="/root/.cargo/bin:${PATH}"

# Zig - https://ziglang.org/learn/getting-started/
RUN cd /usr/local/share && \
    curl -fsSLO https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz && \
    tar -xJf ./zig-linux-x86_64-0.13.0.tar.xz && \
    rm ./zig-linux-x86_64-0.13.0.tar.xz && \
    mv zig-linux-x86_64-0.13.0 zig
ENV PATH="/usr/local/share/zig:${PATH}"

# Common LISP via Roswell
# https://github.com/roswell/roswell/wiki/Installation
# https://github.com/roswell/roswell/releases
# NOTE: without an explicit sbcl version, `ros install quicklisp` errors, at least
# with sbcl v2.5.11, see:
# https://github.com/yogan/advent-of-code/actions/runs/19966484132/job/57259512968
RUN curl -fsSL https://github.com/roswell/roswell/releases/download/v24.10.115/roswell_24.10.115-1_amd64.deb --output roswell.deb && \
    dpkg -i roswell.deb && \
    rm roswell.deb && \
    ros install sbcl-bin/2.3.2 && \
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
RUN curl -fsSL https://github.com/gleam-lang/gleam/releases/download/v1.12.0/gleam-v1.12.0-x86_64-unknown-linux-musl.tar.gz | tar -xzf - -C /usr/local/bin

# Swift
# https://www.swift.org/install/linux/#installation-via-tarball
# tarball has a usr/ directory, so we strip that and extract to /usr
RUN curl -fsSL https://download.swift.org/swift-5.9.2-release/ubuntu2204/swift-5.9.2-RELEASE/swift-5.9.2-RELEASE-ubuntu22.04.tar.gz | tar -xzf - -C /usr --strip-components=2

# PowerShell
# https://learn.microsoft.com/en-us/powershell/scripting/install/install-ubuntu?view=powershell-7.4
RUN curl -fsSL https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb --output packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    rm packages-microsoft-prod.deb && \
    apt-get update && \
    apt-get install -y powershell && \
    rm -rf /var/lib/apt/lists/* && \
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
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK} && \
    # Remove docs (~ 600 MB), not needed in container
    rm -rf /usr/local/share/doc/ghc-9.4.8

# .NET
# https://devblogs.microsoft.com/dotnet/whats-new-for-dotnet-in-ubuntu-2404/
RUN add-apt-repository ppa:dotnet/backports && \
    apt-get update && \
    # .NET 7 (required for Exercism stuff, coming from ppa:dotnet/backports)
    # .NET 8 (straight from the default Ubuntu repos)
    # .NET 9 (latest shit, from ppa:dotnet/backports)
    apt-get install -y --no-install-recommends \
    dotnet-sdk-7.0 dotnet-sdk-8.0 dotnet-sdk-9.0 && \
    rm -rf /var/lib/apt/lists/*

# Java - we need a JDK19 for Exercism stuff. Ubuntu 24.04 does not have a package anymore,
# but the old .deb from Oracle seems to do it, as long as we provide the right libc, etc.
RUN apt-get update && \
    apt-get install -y libc6-i386 libc6-x32 libasound2t64 && \
    rm -rf /var/lib/apt/lists/* && \
    curl -fsSL https://download.oracle.com/java/19/archive/jdk-19.0.2_linux-x64_bin.deb --output jdk19.deb && \
    dpkg -i jdk19.deb && \
    rm jdk19.deb && \
    update-alternatives --install /usr/bin/java java /usr/lib/jvm/jdk-19/bin/java 1 && \
    update-alternatives --install /usr/bin/javac javac /usr/lib/jvm/jdk-19/bin/javac 1 && \
    update-alternatives --set java /usr/lib/jvm/jdk-19/bin/java && \
    update-alternatives --set javac /usr/lib/jvm/jdk-19/bin/javac

# Scala via cs setup - https://www.scala-lang.org/download
RUN curl -fsSL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | \
    gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --dir /usr/local/share/scala
ENV PATH="/usr/local/share/scala:${PATH}"
RUN sbt --script-version

# Elm and elm-test
# https://github.com/elm/compiler/blob/master/installers/linux/README.md
RUN curl -fsSL https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz | \
    gunzip --stdout > /usr/local/bin/elm \
    && chmod +x /usr/local/bin/elm \
    && npm install --global elm-test@latest-0.19.1

# Perl Test2::V0
RUN cpan Test2::V0

# Dart
RUN curl -fsSL https://dl-ssl.google.com/linux/linux_signing_key.pub \
    | gpg --dearmor -o /usr/share/keyrings/dart.gpg && \
    echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' \
    | tee /etc/apt/sources.list.d/dart_stable.list && \
    apt-get update && \
    apt-get install -y dart && \
    rm -rf /var/lib/apt/lists/*

# Idris2 via Pack
# https://github.com/idris-lang/Idris2/blob/main/INSTALL.md
# https://github.com/stefan-hoeck/idris2-pack
ENV PACK_BIN_DIR="/usr/local/bin"
ENV PACK_USER_DIR="/usr/local/share/pack"
ENV PACK_STATE_DIR="/usr/local/state/pack"
ENV PACK_CACHE_DIR="/usr/local/cache/pack"
RUN bash -c "echo '' | bash <(curl -fsSL https://raw.githubusercontent.com/stefan-hoeck/idris2-pack/main/install.bash)"

# OCaml via opam
# https://ocaml.org/docs/installing-ocaml#install-opam
RUN apt-get update && apt-get install -y --no-install-recommends opam && \
    rm -rf /var/lib/apt/lists/* && \
    opam init --auto-setup --disable-sandboxing --yes && \
    opam install --yes dune ocaml ounit2

# This is basically the output of `opam env`. Ideally, this should be done by
# a `eval $(opam env)` command from either .profile or .bashrc, but for
# whatever reason, this only works when running the Docker image locally, but
# not on GitHub Actions. So we just put everything right here. It's not pretty,
# but it works.
ENV OPAM_SWITCH_PREFIX="/root/.opam/default"
ENV OCAMLTOP_INCLUDE_PATH="/root/.opam/default/lib/toplevel"
ENV CAML_LD_LIBRARY_PATH="/root/.opam/default/lib/stublibs:/root/.opam/default/lib/ocaml/stublibs:/root/.opam/default/lib/ocaml"
ENV OCAML_TOPLEVEL_PATH="/root/.opam/default/lib/toplevel"
ENV PATH="/root/.opam/default/bin:${PATH}"

# Install Hex for Elixir (so that `mix deps.get` will not prompt)
ENV MIX_HOME=/opt/mix
RUN mix local.hex --force

# vim: tw=0
