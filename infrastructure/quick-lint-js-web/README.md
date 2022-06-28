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

## Deploying

After changing the Dockerfile or any script, build then push the new image:

    $ cd infrastructure/quick-lint-js-web/
    $ docker build . --tag quick-lint-js-web
    $ docker image save quick-lint-js-web | pv | ssh root@c.quick-lint-js.com 'docker image load'

Then, restart the web server:

    $ ssh root@c.quick-lint-js.com systemctl restart quick-lint-js-web-docker.service

[mkcert]: https://github.com/FiloSottile/mkcert
