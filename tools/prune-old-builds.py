import os
from datetime import datetime
from pathlib import Path
import shutil

FOURTEEN_DAYS = 14


def how_old(date: str) -> int:
    """Return a integer that represents how many days has passed given a date"""
    return (datetime.now() - datetime.strptime(date, '%Y-%m-%d')).days


if __name__ == '__main__':
    builds_path = Path('builds/')

    for folder in os.listdir(builds_path):
        folder_path = builds_path / folder
        created_at = datetime.fromtimestamp(
            os.path.getctime(folder_path)).strftime('%Y-%m-%d')

        if how_old(created_at) > FOURTEEN_DAYS:
            try:
                shutil.rmtree(folder_path)
            except OSError as e:
                print("Error: %s : %s" % (folder_path, e.strerror))