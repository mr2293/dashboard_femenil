# Dockerfile for dashboard_femenil

# Use a newer R version compatible with renv.lock
FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libv8-dev \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /home/dashboard_femenil

# Copy project files
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# Install renv and restore project library
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE)"

# Default command to deploy app
CMD ["Rscript", "deploy.R"]