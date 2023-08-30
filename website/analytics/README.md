# Download analytics

This directory contains tools to measure how often quick-lint-js is downloaded.

These analytics tools are tightly coupled to quick-lint-js's infrastructure.

To set up analytics, copy `config.example.json` to `config.json` and edit as
appropriate. Then run `yarn install` to install third-party dependencies.

## Workflow

1. Collect data
2. Import data
3. Present data

## 1. Collect data

Configure Apache httpd with the following log format:

    %v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"

Configure Matomono Analytics. (This is not needed if using Apache logs.)

## 2. Import data

Importing will update the database specified by `db.file` in `config.json`.

Import data from Apache access logs (idempotent):

    $ node src/import-apache-logs.mjs /var/log/apache2/access.log*

Import data from Matomo's MySQL database (idempotent):

    $ node src/import-matomo-logs.mjs

## 3. Present data

Run the following command to create `chart.directory` (`config.json`) and put
HTML and helper files inside that directory:

    $ node src/make-charts.mjs

Then view `${chart.directory}/index.html` with your web browser.
