# ---------------------------
# Dockerfile for dashboard_femenil
# ---------------------------

# ---------------------------
# 1. Base image
# ---------------------------
FROM rocker/shiny:4.4.1

# ---------------------------
# 2. Install system dependencies including textshaping/build requirements
# ---------------------------
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

# ---------------------------
# 3. Configure Java for R
# ---------------------------
RUN R CMD javareconf

# ---------------------------
# 4. Set working directory
# ---------------------------
WORKDIR /home/dashboard_femenil

# ---------------------------
# 5. Copy project files
# ---------------------------
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# ---------------------------
# 6. Install renv first
# ---------------------------
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# ---------------------------
# 7. Pre-install packages that compile (optional, speeds up renv restore)
# ---------------------------
RUN R -e "install.packages(c('sf', 'units', 'V8', 'ragg'), repos='https://cloud.r-project.org')"

# ---------------------------
# 8. Restore project library using renv
# ---------------------------
RUN R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE)"

# ---------------------------
# 9. Default command to deploy the app
# ---------------------------
CMD ["Rscript", "deploy.R"]
