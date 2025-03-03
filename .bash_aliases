ARTIFACTORY_USER=dcervone
ARTIFACTORY_PASSWORD=AP6tcciBgtNxaVFmLDVB3HFULVj

release-tag() {
  GIT_TIMESTAMP=$(date -u "+%Y%m%d.%H%M%S");
  if [[ $# -eq 1 ]] ; then
     echo "Tagging and pushing with release-$1-$GIT_TIMESTAMP"
     sleep 3
     git tag release-$1-$GIT_TIMESTAMP
     git push origin release-$1-$GIT_TIMESTAMP
  else
     echo "Error: requires exactly one argument"
  fi
}

alias emcas="emacs"
