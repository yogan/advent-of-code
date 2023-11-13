FROM ubuntu:23.10

# install packages required for Bun installer
RUN apt-get update && apt-get install -y curl unzip

# install Bun
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

WORKDIR /app
COPY . /app

CMD ["bash", "./run_tests.sh"]
