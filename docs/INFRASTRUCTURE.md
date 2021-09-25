# quick-lint-js infrastructure

This document describes the infrastructure used for managing builds, releases,
and the website.

## Hosted services

### c.quick-lint-js.com

c.quick-lint-js.com is a VPS on strager's [Vultr][] account.

* vhost: https://c.quick-lint-js.com/
* vhost: http://c.quick-lint-js.com/
* vhost: https://admin.quick-lint-js.com/
* vhost: http://admin.quick-lint-js.com/
* vhost: https://quicklintjs.com/
* vhost: http://quicklintjs.com/
* Matomo analytics
* MariaDB database

### vhost: https://c.quick-lint-js.com/

Data updated manually and by GitHub Actions.

* Data: /mnt/blockstorage/c.quick-lint-js.com/ (Vultr block storage)
* TLS certificates: Let's Encrypt
* Cron: `/home/github-ci/prune-old-builds.py` run by `github-ci`

### vhost: http://c.quick-lint-js.com/

Redirects to https://c.quick-lint-js.com/

### vhost: https://admin.quick-lint-js.com/

Serves several services for administration:

* Matomo: <https://admin.quick-lint-js.com/matomo/>
* phpMyAdmin: <https://admin.quick-lint-js.com/phpmyadmin/>

* TLS certificates: Let's Encrypt

### vhost: http://admin.quick-lint-js.com/

Redirects to https://admin.quick-lint-js.com/

### vhost: https://quicklintjs.com/

Redirects to https://quick-lint-js.com/

* TLS certificates: Let's Encrypt

### vhost: http://quicklintjs.com/

Redirects to https://quick-lint-js.com/

### Matomo analytics

Analytics for the website is self-hosted with Matomo.

* Database: MariaDB matomo_analytics@c.quick-lint-js.com
* Admin URL: <https://admin.quick-lint-js.com/matomo/>
* Cron: `/root/update-analytics.sh` run by `root`

## Cloud services

* HTTP: https://quick-lint-js.com/
* Domains
* DNS
* Source code
* Issue tracking
* Continuous integration/building
* Open VSX Registry
* Visual Studio Marketplace
* npm

### HTTP: https://quick-lint-js.com/

<https://quick-lint-js.com/> is the main website for users.

* Server: GitHub Pages
* Repo: <https://github.com/quick-lint/quick-lint-js.com>

### Domains

Domains were purchased through strager's [Namecheap][] account.

### DNS

DNS nameservers are hosted through strager's [DNS Made Easy][] account.

### Source code hosting

GitHub hosts quick-lint-js' repositories: <https://github.com/quick-lint>

### Issue tracking

GitHub hosts quick-lint-js' issue tracker:
<https://github.com/quick-lint/quick-lint-js/issues>

### Continuous integration/building

quick-lint-js uses GitHub Actions for continuous integration (aka automated
building): <https://github.com/quick-lint/quick-lint-js/actions>

### Open VSX Registry

strager owns the [quick-lint-js
package](https://open-vsx.org/extension/quick-lint/quick-lint-js) on the Open
VSX registry

### Visual Studio Marketplace

strager owns the [quick-lint publisher
account](https://marketplace.visualstudio.com/publishers/quick-lint) on the
Visual Studio Marketplace.

### npm

strager owns the [quick-lint-js
package](https://www.npmjs.com/package/quick-lint-js) on the npm registry.

[DNS Made Easy]: https://dnsmadeeasy.com/
[Namecheap]: https://www.namecheap.com/
[Vultr]: https://www.vultr.com/
