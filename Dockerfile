FROM ubuntu:23.10

RUN apt-get update && apt-get install -y curl unzip build-essential locales leiningen

# install Bun
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# install DDP
RUN cd /usr/local/share/ && \
    curl -OL https://github.com/DDP-Projekt/Kompilierer/releases/latest/download/DDP-v0.0.1-alpha-linux-amd64.tar.gz && \
    tar -xzf ./DDP-v0.0.1-alpha-linux-amd64.tar.gz && \
    mv DDP-v0.0.1-alpha-linux-amd64 ddp && \
    cd ddp && \
    # --force only partially works, still needs some 'y' inputs, see:
    # https://github.com/DDP-Projekt/Installer/issues/1#issuecomment-1810635337
    yes | ./ddp-setup --force
ENV PATH="/usr/local/share/ddp/bin:${PATH}"
ENV DDPPATH="/usr/local/share/ddp"

WORKDIR /app
COPY . /app

CMD ["bash", "./test-templates.sh"]
