# quick-lint-js-web

This directory contains Ansible scripts for configuring the quick-lint-js.com
server.

## Production

If you want to run Ansible for the production server, run these commands:

    $ touch vault.password
    $ chmod 600 vault.password
    $ $EDITOR vault.password
    # Write a new secret password for the vault.

    $ ansible-vault encrypt --vault-password-file vault.password vault-dev.yml --output vault.yml
    $ ansible-vault edit --vault-password-file vault.password vault.yml
    # Change the passwords.

    $ LC_ALL=C ansible-playbook -i inventory.yml prod.yml --vault-password-file vault.password

Then, open <https://admin.quick-lint-js.com/matomo/> to configure
Matomo:

* Database Server: `localhost`
* (Database) Login: `matomo_analytics`
* (Database) Password: (see `vault.yml`)
* Database Name: `matomo_analytics`
* Table Prefix: `matomo_` (default)

## Development (VM)

If you want to test on a development VM, run these commands:

    $ cp inventory.yml inventory-dev.yml
    $ $EDITOR inventory-dev.yml
    # Change the ansible_host to your VM's IP address.

    # NOTE(strager): The following command will edit your system's hosts
    # file, and will install a root certificate in your web browsers.
    $ LC_ALL=C ansible-playbook -i inventory-dev.yml mkcert.yml --ask-become-pass

    $ LC_ALL=C ansible-playbook -i inventory-dev.yml dev.yml --ask-become-pass

(If Apache spits out the wrong TLS certs, run `sudo systemctl restart apache2`
in your VM.)

Then, open <https://admin.quick-lint-js.com.developer.app/matomo/> to configure
Matomo:

* Database Server: `localhost`
* (Database) Login: `matomo_analytics`
* (Database) Password: `hunter12`
* Database Name: `matomo_analytics`
* Table Prefix: `matomo_` (default)
