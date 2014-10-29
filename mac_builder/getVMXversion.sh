#!/bin/sh
# This script returns the version of the current repository
#
# It uses the versioning scheme:
#
# When the most recent tag (like "v0.1.2") matches master, the version is
#   VMXmiddle_Mac_v0.1.2
#
# If we are in at non-master hash c934fsd, the version is
#   VMXmiddle_Mac_2014-10-31_c934fsd
#
# If we are also in another branch, the version has the branch name:
#   VMXmiddle_Mac_2014-10-31_fixbug123_c934fsd
#
# Copyright 2013-2014 vision.ai, LLC


BRANCH_NAME=`git branch | grep "*" | awk '{print($2)}'`
if [ $BRANCH_NAME == "master" ]; then
    BRANCH_NAME=""
else
    BRANCH_NAME=$BRANCH_NAME"-"
fi
DATER=`date "+%Y-%m-%d"`
HASH=`git --no-pager log --format='%h' -n 1`
GIT_LIST=`git show-ref | grep refs/heads/master | awk '{print($1)}'`
GITTAG=`git show-ref | grep ${GIT_LIST} | grep refs/tags/ | awk '{print($2)}' | sed 's/refs\/tags\///'`
HASH=$DATER"_"$BRANCH_NAME$HASH
if [ "$GITTAG" != "" ]; then
    if [ "$BRANCH_NAME" == "" ]; then
        HASH=$GITTAG
    else
        echo 'hi' > /dev/null
    fi
else
    echo 'hi' > /dev/null
fi
echo $HASH
