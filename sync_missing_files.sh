#!/bin/bash
# Copies files that exist in the external gemmini-rocc-tests but not in the
# in-tree software copy. Source this or run it directly.

SRC_BASE="/bwrcq/B/mshi/chipyard/cy_gpu/gemmini-rocc-tests"
DST_BASE="/bwrcq/B/mshi/chipyard/cy_gpu/chipyard/generators/gemmini/software/gemmini-rocc-tests"

for subdir in bareMetalC include; do
    src="$SRC_BASE/$subdir"
    dst="$DST_BASE/$subdir"

    if [ ! -d "$src" ] || [ ! -d "$dst" ]; then
        echo "Skipping $subdir: directory not found"
        continue
    fi

    missing=$(comm -23 <(ls "$src" | sort) <(ls "$dst" | sort))

    if [ -z "$missing" ]; then
        echo "[$subdir] No missing files."
    else
        for f in $missing; do
            cp "$src/$f" "$dst/$f"
            echo "[$subdir] Copied: $f"
        done
    fi
done