#!/usr/bin/python3

from github import Github
from datetime import datetime, timedelta
import subprocess
import argparse
import json
import sys
import os

repositories_to_clean = ["batch_zkapp_txn_tool",
        "block-archiver",
        "mina-batch-txn",
        "mina-daemon",
        "mina-archive",
        "mina-daemon-baked",
        "mina-daemon-lightnet",
        "mina-daemon-devnet",
        "mina-daemon-instrumented",
        "mina-snapp-test-transaction"
]
project = "gcr.io/o1labs-192920"
github_repo = "minaProtocol/mina"

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')


parser = argparse.ArgumentParser()
parser.add_argument('age', type=int, help='eligible age for cleaning (expressed in days)')
parser.add_argument('dryrun', type=str2bool, nargs='?',const=True, default=False,
                        help='if true, script will not perform any actions, just reporting')

args = parser.parse_args()

gcr_repositories = list(map(lambda x: project + "/" + x,repositories_to_clean))

g = Github()
repo = g.get_repo(github_repo)
commits_to_skip = list(map(lambda x: x.commit.sha[0:7], repo.get_tags()))


def delete_image(tag,dryrun,reason):
    image_prefix = tag["digest"][:15]
    if dryrun:
        print(f'[DRYRUN]: {gcr_repository}: deleting {image_prefix} because {reason}')
    else:
        id = f'{gcr_repository}@{tag["digest"]}'
        print(f'{gcr_repository}: deleting {image_prefix} because ${reason}')
        cmd = ["gcloud", "container", "images", "delete", id,"--quiet", "--force-delete-tags"]
        subprocess.run(cmd)


for gcr_repository in gcr_repositories:
    print(f"Cleaning {gcr_repository} repository... it may take a while")
    cmd = ["gcloud", "container", "images", "list-tags", gcr_repository,"--format", "json"]
    result = subprocess.check_output(cmd)
    tags = json.loads(result)

    threshold = datetime.now().date() - timedelta(days=args.age)
    deleted = 0
    for tag in tags:
        github_tags = tag["tags"]
        if not any(any(commit_to_skip in x for x in github_tags) for commit_to_skip in commits_to_skip):
            timestamp = tag["timestamp"]
            if timestamp is None:
                delete_image(tag,args.dryrun,f"image doesn't have timestamp property (which means it's very old) and not tagged with version")
                deleted += 1
            else:
                date = datetime.strptime(timestamp["datetime"],"%Y-%m-%d %H:%M:%S%z")
                if date.date() < threshold:
                    delete_image(tag,args.dryrun,f"is older than {args.age} days and not tagged with version")
                    deleted += 1

    total = len(tags)
    
    suffix = "will be deleted in standard run" if args.dryrun else "deleted"
    print(f"{gcr_repository} repository cleaning completed. {deleted} out of {total} images {suffix}")