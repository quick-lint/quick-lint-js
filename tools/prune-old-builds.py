import os
import shutil
import argparse
import subprocess
from pathlib import Path
from datetime import datetime


FOURTEEN_DAYS = 14


def how_old(date: datetime) -> int:
    """Return a integer that represents how many days has passed given a date"""
    return (datetime.now() - date).days


if __name__ == '__main__':
    parser = argparse.ArgumentParser("Prune old builds")
    parser.add_argument(
        "builds_path",
        help="The path where the builds are stored.",
        nargs=1,
        type=str)
    parser.add_argument(
        "repository_url",
        help="The url to the repository.",
        nargs=1,
        type=str)
    args, _ = parser.parse_known_args()

    builds_path = Path(args.builds_path[0])
    # https://github.com/quick-lint/quick-lint-js.git
    repository_url = args.repository_url[0]
    repo_name = 'quick-lint-js'

    if not os.path.exists(builds_path):
        print(
            f"error: The directory \033[1m{builds_path}\033[0m does not exists.")
        exit(1)

    if not os.path.exists(repo_name):
        print("Cloning repository")
        subprocess.run(['git', 'clone', '-q', repository_url])

    print('Getting all commits of the repository\n')
    commits = subprocess.getoutput(
        f"cd {repo_name} \
        && git pull origin master \
        && git fetch origin 'refs/pull/*/head:refs/remotes/github-pr/*' \
        && git rev-list --all --remotes"
    ).split('\n')

    for folder in os.listdir(builds_path):
        folder_path = builds_path / folder
        created_at = datetime.fromtimestamp(os.path.getctime(folder_path))

        if not folder in commits and how_old(created_at) > FOURTEEN_DAYS:
            print(
                f"""Build \033[93m{folder}\033[0m accomplish the following criteria:\n* Is not part of the repo/PR\n* Has more then fourteen days\n\033[1;31mThis build will be deleted.\033[0m\n""")
            try:
                shutil.rmtree(folder_path)
            except OSError as e:
                print("Error: %s : %s" % (folder_path, e.strerror))
