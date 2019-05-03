#!/usr/bin/bash
usage="$(basename $0) COMMAND [DIR..]

COMMAND
  clean  Clean up symlinks from passed packages
  list   List files belonging to passed packages
  help   Show help message"

with_links() {
    cmd="$1 ;"
    shift
    if [[ -z "$@" ]]; then
	find $HOME -type l -lname "*/$(basename `pwd`)/*" -exec $cmd
    else
	for pkg in $@; do
	    find $HOME -type l -lname "*/$(basename `pwd`)/$pkg/*" -exec $cmd
	done
    fi
}

case $1 in
    clean)
	shift
	with_links "rm {}" $@
	;;
    list)
	shift
	with_links "echo {}" $@
	;;
    help|*)
	echo "$usage"
	;;
esac
