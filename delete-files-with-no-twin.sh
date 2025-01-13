# PURPOSE:
# This deletes any file in the mirror (git-formatted) repo
# to which no file in the original (org-formatted) repo
# -- that is, none with the same relative path --
# corresponds. Without this step, each time I e.g. rename a file
# in the org-roam repo, it would persist in this repo.

ORIGINAL=/home/jeff/org-roam
MIRROR=/home/jeff/ugh/une/github-exported-org-roam

# Find all ".org" files in /b
find "$MIRROR" -type f -name "*.org" | while read -r mirrored_file; do
    # Compute the relative path of the file in /b
    relative_path="${mirrored_file#$MIRROR/}"

    # Check if the corresponding file exists in /a
    if [ ! -e "$ORIGINAL/$relative_path" ]; then
        # If it doesn't exist, delete the file from /b
        echo "Deleting: $mirrored_file"
        rm "$mirrored_file"
	git rm --cached "$mirrored_file"
    fi
done
