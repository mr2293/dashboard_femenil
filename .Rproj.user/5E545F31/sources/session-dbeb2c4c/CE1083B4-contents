# ---------------------------
# Dockerfile for dashboard_femenil
# ---------------------------

FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    libcairo2-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglib2.0-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libv8-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libfontconfig1-dev \
    gfortran \
    g++ \
    default-jdk \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Ensure pkg-config can find harfbuzz & fribidi
ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:$PKG_CONFIG_PATH

# Set working directory
WORKDIR /home/dashboard_femenil

# Copy project files
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# Install renv first
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Optional: pre-install packages that require compilation
RUN R -e "install.packages(c('sf', 'units', 'V8', 'ragg'), repos='https://cloud.r-project.org')"

# Restore project library with renv
RUN R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE)"

# Default command
CMD ["Rscript", "deploy.R"]
