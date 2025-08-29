# ===============================
# Dockerfile for dashboard_femenil
# ===============================

FROM rocker/shiny:4.4.1

# -----------------------------
# 1. System dependencies
# -----------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    libharfbuzz-dev \
    libfribidi-dev \
    libcairo2-dev \
    libfreetype6-dev \
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

# -----------------------------
# 2. Environment variables for pkg-config (fix textshaping build)
# -----------------------------
ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig

# -----------------------------
# 3. Working directory
# -----------------------------
WORKDIR /home/dashboard_femenil

# -----------------------------
# 4. Copy project files
# -----------------------------
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# -----------------------------
# 5. Install renv and restore project library
# -----------------------------
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE)"

# -----------------------------
# 6. Expose Shiny port
# -----------------------------
EXPOSE 3838

# -----------------------------
# 7. Run Shiny app
# -----------------------------
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
