#! /usr/bin/gawk -f

function ltrim(s) {
    gsub(/^\s*/, "", s)
    return s
}

function bindmodifiers(s) {
    switch (s) {
        case "myModMask": return "mod-"
        case "maskS": return "mod-shift-"
        case "maskC": return "mod-control-"
        case "maskCS": return "mod-shift-control-"
	case "shiftMask": return "shift-"
	case "controlMask": return "control-"
        case "0": return "none-"
        default: return s
    }
}

#-- %% Are comments without binding
/^\s*--\s+%%/ {
    $1 = $2 = ""
    if ($3 == "!") {
	print ""
	$3 = ""
    }
    print ltrim($0)
}

#-- %! Are bindings with their combination in line
/^.*--\s+%!/ {
    mod = bindmodifiers(substr($2, 3, length($2) - 3))
    bind = mod substr($3, 4, length($3) - 5)
    desc = substr($0, index($0, "%! ") + 3)

    print "\t" bind "\t" desc
}

#-- [bind] %! Are bindings with their combination elsewhere
/^.*--\s+[^\s]+\s+%!/ {
    if ($1 == "--") {
	$1 = $3 = ""
	$4 = "\t" ltrim($4)
	print "\t" ltrim($0)
    } else {
	i = index($0, "--") + 3
	j = index($0, "%! ")
	bind = substr($0, i, j - i) 
	print "\t" bind "\t" substr($0, j + 3)
    }
}
