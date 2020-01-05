#! /usr/bin/bash
# Identities needed to pull all git repos
identity_files=("$HOME/.ssh/id_rsa.pub")

tput bold
# 80 characters wide separator in the style of topgrade's
echo "―― $(date +%H:%M:%S) - Check necessary SSH identities ―――――――――――――――――――――――――――――――――――"
tput sgr0
# Add keys that aren't already in the agent
#  This assumes the corresponding private files are without suffix ".pub"
for fn in $identity_files; do
    [[ -n $(ssh-add -L | grep -F "$(cat $fn)") ]] || ssh-add "${fn%.*}"
done

topgrade
