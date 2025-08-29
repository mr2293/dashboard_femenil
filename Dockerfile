# ===============================
# Dockerfile for dashboard_femenil
# ===============================
FROM rocker/shiny:4.4.1

# ------------------------------
# 1. Install system dependencies
# ------------------------------
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

# ------------------------------
# 2. Set environment variables
# ------------------------------
# Fix pkg-config path so R packages like textshaping can find harfbuzz & fribidi
ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig

# ------------------------------
# 3. Set working directory
# ------------------------------
WORKDIR /home/dashboard_femenil

# ------------------------------
# 4. Copy project files
# ------------------------------
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# ------------------------------
# 5. Install renv
# ------------------------------
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# ------------------------------
# 6. Restore renv environment
# ------------------------------
RUN R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE)"

# ------------------------------
# 7. Expose Shiny port
# ------------------------------
EXPOSE 3838

# ------------------------------
# 8. Launch Shiny app
# ------------------------------
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
