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
    # Every "!! " in the describing comment will indent the combination and
    # comment by a bit
    n_indent = gsub("!!", " ", desc)
    for (i = 0; i < n_indent; i++) {
        bind = "  " bind
    }
    printf "  %-25s  %s\n", bind, desc
}

#-- %% Are comments without binding
/^\s*--\s+%%/ {
    $1 = $2 = ""
    if ($3 == "!") { #-- %% ! Are section headers
        # that get preceded by a blank line
	print ""
	$3 = ""
    }
    print ltrim($0)
}

#-- %! Are bindings with their combination in line
/^.*--\s+%!/ {
    mod = bindmodifiers(substr($2, 3, length($2) - 3))
    i_underscore = index($3, "_")
    # Remove the "xK_" or "xF86XK_" prefixes
    bind = mod substr($3, i_underscore + 1, length($3) - 2 - i_underscore)
    printbind(bind, substr($0, index($0, "%! ") + 3))
}

#-- [bind] %! Are comments for binds that are defined elsewhere
/^.*--\s+[^[:blank:]]+\s+%!/ {
    # Need to treat comments at the beginning of a line a bit differently
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
