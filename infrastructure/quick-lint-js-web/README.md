# quick-lint-js-web

This directory contains sources for quick-lint-js.com's web server.

## Developing

1. Add the following entries to your hosts file (`/etc/hosts` on Linux):

    127.0.0.1 quick-lint-js.com.developer.app
    127.0.0.1 c.quick-lint-js.com.developer.app
    127.0.0.1 admin.quick-lint-js.com.developer.app
    127.0.0.1 quicklintjs.com.developer.app

2. Install the [mkcert][] tool.
3. Run the following command to generate HTTPS certificates for development and
   install a root CA in your browser:

    $ infrastructure/quick-lint-js-web/create-dev-certificates.sh

4. Build the Docker container:

    $ cd infrastructure/quick-lint-js-web/
    $ docker build . --tag quick-lint-js-web

5. Run the Docker container:

    $ cd infrastructure/quick-lint-js-web/
    $ docker run --rm --name quick-lint-js-web -it -p 80:80 -p 443:443 \
        --volume "${PWD}/../../website/www:/var/www/quick-lint-js.com/www" \
        --volume "${PWD}/apache2/options-ssl-apache.conf:/etc/letsencrypt/options-ssl-apache.conf" \
        --volume "${PWD}/dev-certificates:/etc/letsencrypt/live" \
        quick-lint-js-web

6. Visit <http://quick-lint-js.com.developer.app/> in your web browser.

### Matomo

If you want to check that Matomo works, follow these steps:

1. Install and run MySQL on your Linux host.
2. Run the following commands on your MySQL instance:

    CREATE DATABASE matomo;
    CREATE USER 'matomo'@'localhost' IDENTIFIED WITH mysql_native_password BY 'hello';
    GRANT SELECT, INSERT, UPDATE, DELETE, CREATE, INDEX, DROP, ALTER, CREATE TEMPORARY TABLES, LOCK TABLES ON matomo.* TO 'matomo'@'localhost';

3. Start a quick-lint-js-web container per the instructions above, but also with
   the `--volume /run/mysqld/mysqld.sock:/run/mysqld/mysqld.sock` option.
4. Visit <https://admin.quick-lint-js.com.developer.app/matomo/>.
5. Follow the instructions on the page to set up Matomo. Use the following
   database settings:
   * host: `/run/mysqld/mysqld.sock`
   * user: `matomo`
   * password: `hello`
   * database: `matomo`

## Deploying

After changing the Dockerfile or any script, build then push the new image:

    $ cd infrastructure/quick-lint-js-web/
    $ docker build . --tag quick-lint-js-web
    $ docker image save quick-lint-js-web | pv | ssh root@c.quick-lint-js.com 'docker image load'

Then, restart the web server:

    $ ssh root@c.quick-lint-js.com systemctl restart quick-lint-js-web-docker.service

[mkcert]: https://github.com/FiloSottile/mkcert
