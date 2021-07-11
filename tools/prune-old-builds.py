import os
import shutil
import subprocess
from pathlib import Path
from datetime import datetime

FOURTEEN_DAYS = 14


def how_old(date: str) -> int:
    """Return a integer that represents how many days has passed given a date"""
    return (datetime.now() - datetime.strptime(date, '%Y-%m-%d')).days


if __name__ == '__main__':
    builds_path = Path('builds/')
    repo_url = 'https://github.com/quick-lint/quick-lint-js.git'
    repo_name = 'quick-lint-js'

    if os.path.exists(repo_name):
        shutil.rmtree(repo_name)

    subprocess.run(['git', 'clone', '-q', repo_url])

    print('Getting all commits of the repository')
    commits = subprocess.getoutput(
        f"cd {repo_name} && git rev-list --all --remotes").split('\n')

    builds_path = Path('builds/')

    for folder in os.listdir(builds_path):
        folder_path = builds_path / folder
        created_at = datetime.fromtimestamp(
            os.path.getctime(folder_path)).strftime('%Y-%m-%d')

        if not folder in commits:
            print(
                f'The build of hash {folder} is not related to a branch anymore and will be deleted')
            shutil.rmtree(folder_path)

        if how_old(created_at) > FOURTEEN_DAYS:
            print(
                f'The build {folder} take more then fourteen days, then it will be deleted')
            try:
                shutil.rmtree(folder_path)
            except OSError as e:
                print("Error: %s : %s" % (folder_path, e.strerror))

    print('Removing quick-lint-js repository')
    shutil.rmtree(repo_name)