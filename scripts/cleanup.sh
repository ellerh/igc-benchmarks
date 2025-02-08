
declare -a CLEANUP_ACTIONS=()

function cleanup {
    for I in $(seq $(( ${#CLEANUP_ACTIONS[@]} - 1 )) -1 0) ; do
	local ACTION=${CLEANUP_ACTIONS[$I]}
	eval "$ACTION"
    done
}

trap cleanup EXIT
