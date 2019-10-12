#!/usr/bin/env bash

set -ueEo pipefail
IFS=$'\n\t'

action=${1:?"You must provide an action"}; shift

if [[ -f .env ]]; then
  . .env
fi

now=$(date +%Y-%m-%dT%H:%M)

latest_image_tag="$APP_NAME:latest"

function doBuild {

    local output_file="/tmp/$APP_NAME-$now.txt"
    trap 'postBuild ${output_file} EXIT'

    time "$@" | tee "$output_file"

}

function postBuild {
    result=$?
    if [[ ${result} -eq 0 ]]
    then echo "********* SUCCESSFUL *********"
    else echo "!!!!!!!!! FAILED !!!!!!!!!!!!!"
    fi
    echo "Output can be found at $1"
    exit ${result}
}

function build {
  docker build -t "$latest_image_tag" .
}

function run {
  docker run --rm "$latest_image_tag" "$@"
}

function buildAndRun {
  doBuild build
  run "$@"
}

case ${action} in

  "build")
    doBuild build
    ;;

  "buildAndRun")
    buildAndRun "$@"
    ;;

  "run")
    run "$@"
    ;;

  *)
    echo "Unknown action '$action'"
    echo "Usage:"
    echo "$0 build|buildAndRun|run|runLatest"
    ;;
esac
