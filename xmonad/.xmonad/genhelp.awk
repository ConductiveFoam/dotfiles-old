#! /usr/bin/gawk -f

function ltrim(s) {
    gsub(/^\s*/, "", s)
    return s
}

function bindmodifiers(s) {
    switch (s) {
        case "myModMask": return "mod-"
        case "myShiftMask": return "mod-shift-"
        case "myControlMask": return "mod-control-"
        case "myShiftControlMask": return "mod-shift-control-"
	case "shiftMask": return "shift-"
	case "controlMask": return "control-"
        case "0": return "none-"
        default: return s
    }
}

function printbind(bind, desc) {
    printf "%25s  %s\n", bind, desc
}

#-- %% Are comments without binding
/^\s*--\s+%%/ {
    $1 = $2 = ""
    if ($3 == "!") { #-- %% ! Are section headers
	print ""
	$3 = ""
    }
    print ltrim($0)
}

#-- %! Are bindings with their combination in line
/^.*--\s+%!/ {
    mod = bindmodifiers(substr($2, 3, length($2) - 3))
    if ($3 ~ /^xK_/) { #
        bind = mod substr($3, 4, length($3) - 5)
    } else {
        bind = mod substr($3, 8, length($3) - 9)
    }
    printbind(bind, substr($0, index($0, "%! ") + 3))
}

#-- [bind] %! Are bindings with their combination elsewhere
/^.*--\s+[^[:blank:]]+\s+%!/ {
    if ($1 == "--") {
	bind = ltrim($2)
	$1 = $2 = $3 = ""
	printbind(bind, ltrim($0))
    } else {
	i = index($0, "-- ") + 3
	j = index($0, "%! ")
	printbind(substr($0, i, j - i), substr($0, j + 3))
    }
}
