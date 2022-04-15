
### Instuctions for generating a Personal access token:

1. Navigate to your GitHub account settings.
2. Then Developer Settings (bottom-most option in the panel on the left).
3. Click the Personal access tokens menu.
4. Then click Generate new token.

Under `repo` permissions for the token you only need to select/checkbox scope for:

    public_repo Access public repositories

Store the access token in a file and variable of your choosing using cat.

    $ myToken=$(cat example.txt)

### Running quick-release-notes.

    $ go run main.go -Repo=quck-lint/quick-lint-js -TagsRepo=quick-lint/quick-lint-js -AuthToken=$myToken

Three flags:

- -Repo (optional): where you want the release notes to be released.
- -TagsRepo (optional): where you want to get the GitHub tags that correspond to the releases from.
- -AuthToken (required): the token that is required to make releases using the GitHub releases
API.
- -IsDraft (optional): creates releases as drafts.

