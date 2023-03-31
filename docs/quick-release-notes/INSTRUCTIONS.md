
### Instuctions for generating a Personal access token:

Link: https://github.com/settings/tokens
1. Click Generate new token.

2. Under `repo` permissions for the token you only need to select/checkbox scope for:

    public_repo Access public repositories

3. Store the access token in file (token.txt)

### Running quick-release-notes.

    $ go run main.go -Repo=quick-lint/quick-lint-js -TagsRepo=quick-lint/quick-lint-js -AuthToken=$(cat token.txt) isDraft=false 

Flags:

- -Repo (optional): where you want the release notes to be released.
- -TagsRepo (optional): where you want to get the GitHub tags that correspond to the releases from.
- -AuthToken (required): the token that is required to make releases using the GitHub releases
API.
