#!/bin/bash
set -euo pipefail

SCRATCH="$HOME/.dotfiles/emacs-profiles/scratch"
REPO="$SCRATCH/straight/repos/cook-mode"
GRAMMAR_DIR="$HOME/Documents/Dev/Tools/tree-sitter-cooklang"
DYLIB_DST="$SCRATCH/tree-sitter/libtree-sitter-cooklang.dylib"

# Build and install the cooklang grammar from source.
# Emacs cannot reload dylibs, so this must run before Emacs starts.
(cd "$GRAMMAR_DIR" && tree-sitter generate && tree-sitter build)
cp "$GRAMMAR_DIR/cooklang.dylib" "$DYLIB_DST"

SETUP="(setq treesit-extra-load-path (list \"$SCRATCH/tree-sitter\"))"

case "${1:-}" in
  --batch)
    # Run ERT tests non-interactively.
    emacs -Q --batch \
      -L "$REPO" -L "$REPO/tests" \
      --eval "$SETUP" \
      --eval "(require 'ert)" \
      --eval "(require 'ert-x)" \
      --eval "(require 'cook-ts-mode-test)" \
      --eval "(ert-run-tests-batch-and-exit)"
    ;;
  --grammar-test)
    # Run tree-sitter corpus tests for the grammar itself.
    (cd "$GRAMMAR_DIR" && tree-sitter test)
    ;;
  *)
    # Interactive mode: open the example file for manual inspection.
    emacs -Q \
      -L "$REPO" \
      --eval "$SETUP" \
      --eval "(require 'cook-ts-mode) (find-file \"$REPO/example/brigadeiro.cook\")"
    ;;
esac
