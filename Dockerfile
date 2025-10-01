# @info
# build:
#   docker build --platform linux/amd64 -t sm64 .
# run:
#   docker run --rm -it -v "$PWD":/sm64 sm64 make VERSION=us -j4

FROM archlinux:base AS build

RUN pacman -Syu --noconfirm && \
  pacman -S --noconfirm \
  util-linux \
  base-devel \
  capstone \
  pkgconf \
  python \
  python-pip \
  git \
  wget

RUN python -m venv /opt/pyenv
RUN /opt/pyenv/bin/pip install pypng bitstring
ENV PATH="/opt/pyenv/bin:${PATH}"
ENV VIRTUAL_ENV="/opt/pyenv"

# Create a non-root user for building AUR packages
RUN useradd -m -G wheel builder && \
    echo 'builder ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

WORKDIR /tmp
RUN git clone https://aur.archlinux.org/yay.git yay
RUN chown -R builder:builder yay
WORKDIR /tmp/yay
# @warn separate these steps to maximize build cache hits
RUN sudo -u builder makepkg -si --noconfirm
RUN sudo -u builder yay -S --noconfirm mips64-elf-newlib
RUN sudo -u builder yay -S --noconfirm mips64-elf-toolchain
RUN sudo -u builder yay -S --noconfirm mips64-elf-binutils

RUN mkdir /sm64
WORKDIR /sm64
ENV PATH="/sm64/tools:${PATH}"

CMD echo 'usage: docker run --rm --mount type=bind,source="$(pwd)",destination=/sm64 sm64 make VERSION=us -j4\n' \
         'see https://github.com/n64decomp/sm64/blob/master/README.md for advanced usage'
