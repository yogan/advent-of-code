FROM ubuntu:24.04

RUN apt-get update && apt-get install -y \
    # base/util packages
    bc curl ca-certificates gnupg unzip locales build-essential cmake libcurl3-gnutls \
    # this brings add-apt-repository, needed later:
    software-properties-common \
    # various languages directly from apt
    vim fish bats leiningen r-base r-cran-testthat ruby3.2 polyml golang-go gawk guile-3.0 \
    crystal elixir erlang-base erlang-dev erlang-eunit rebar3 nim openjdk-8-jdk swi-prolog \
    # Python
    python3 python3-pip python3-pytest python3-pytest-subtests python3-pytest-pylint \
    pypy3 pypy3-venv \
    # Swift deps, see: https://www.swift.org/install/linux/#installation-via-tarball
    binutils git gnupg2 libc6-dev libcurl4-openssl-dev libedit2 libgcc-9-dev \
    libpython3.8 libsqlite3-0 libstdc++-9-dev libxml2-dev libz3-dev pkg-config \
    tzdata zlib1g-dev unzip

# Remove fonts to save space (~ 260 MB).
# This will also remove ghostscript, libgs10, and some libjs-* and r-cran-* packages,
# but this is actually fine, as we don't need them either.
RUN apt-get remove -y fonts-dejavu-core fonts-dejavu-extra fonts-dejavu-mono \
    fonts-droid-fallback fonts-font-awesome fonts-glyphicons-halflings \
    fonts-lato fonts-liberation fonts-liberation-sans-narrow fonts-mathjax \
    fonts-noto-mono fonts-urw-base35

# Bun - https://bun.sh/
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# Node.js - https://github.com/nodesource/distributions/?tab=readme-ov-file#installation-instructions
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_21.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get install -y nodejs

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
# Remove docs (~ 700 MB), not needed in container
RUN rm -rf /root/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc

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
RUN curl -fsSL https://github.com/gleam-lang/gleam/releases/download/v1.0.0/gleam-v1.0.0-x86_64-unknown-linux-musl.tar.gz | tar -xzf - -C /usr/local/bin

# Swift
# https://www.swift.org/install/linux/#installation-via-tarball
# tarball has a usr/ directory, so we strip that and extract to /usr
RUN curl -fsSL https://download.swift.org/swift-5.9.2-release/ubuntu2204/swift-5.9.2-RELEASE/swift-5.9.2-RELEASE-ubuntu22.04.tar.gz | tar -xzf - -C /usr --strip-components=2

# PowerShell universal package
# https://learn.microsoft.com/en-us/powershell/scripting/install/install-ubuntu?view=powershell-7.4#installation-via-direct-download
# Using this temp fix until the powershell package can deal with libicu74, the only version of
# libicu available in Ubuntu 24.04. See: https://github.com/PowerShell/PowerShell/issues/21385
RUN curl -fsSL https://launchpad.net/ubuntu/+archive/primary/+files/libicu72_72.1-3ubuntu3_amd64.deb --output libicu72.deb && \
    dpkg -i libicu72.deb && \
    rm libicu72.deb
RUN curl -fsSL https://github.com/PowerShell/PowerShell/releases/download/v7.4.3/powershell_7.4.3-1.deb_amd64.deb --output powershell.deb && \
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
    ghcup -v install stack --isolate /usr/local/bin --force ${STACK} && \
# Remove docs (~ 600 MB), not needed in container
    rm -rf /usr/local/share/doc/ghc-9.4.8

# .NET 7 and 8
# https://devblogs.microsoft.com/dotnet/whats-new-for-dotnet-in-ubuntu-2404/
RUN add-apt-repository ppa:dotnet/backports && \
    # .NET 7 (required for Exercism stuff, coming from ppa:dotnet/backports)
    # .NET 8 (straight from the default Ubuntu repos)
    apt-get install -y dotnet-sdk-7.0 dotnet-sdk-8.0

# Java - we need a JDK19 for Exercism stuff. Ubuntu 24.04 does not have a package anymore,
# but the old .deb from Oracle seems to do it, as long as we provide the right libc.
RUN apt-get install -y libc6-i386 libc6-x32 && \
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

# vim: tw=0
