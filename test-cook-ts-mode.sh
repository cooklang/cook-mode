#!/bin/bash
set -euo pipefail

# Required environment variables:
#   COOKLANG_GRAMMAR_REPO  path to the tree-sitter-cooklang source repository
#   TREESIT_LOAD_PATH      directory where Emacs loads tree-sitter grammars

if [ -z "${COOKLANG_GRAMMAR_REPO:-}" ]; then
  echo "error: COOKLANG_GRAMMAR_REPO is not set" >&2
  echo "  Set it to the path of your tree-sitter-cooklang source repository." >&2
  exit 1
fi

if [ -z "${TREESIT_LOAD_PATH:-}" ]; then
  echo "error: TREESIT_LOAD_PATH is not set" >&2
  echo "  Set it to the directory where Emacs loads tree-sitter grammars." >&2
  exit 1
fi

REPO=$(cd "$(dirname "$0")" && pwd)
DYLIB_DST="$TREESIT_LOAD_PATH/libtree-sitter-cooklang.dylib"

# Build and install the cooklang grammar from source.
# Emacs cannot reload dylibs, so this must run before Emacs starts.
(cd "$COOKLANG_GRAMMAR_REPO" && tree-sitter generate && tree-sitter build)
cp "$COOKLANG_GRAMMAR_REPO/cooklang.dylib" "$DYLIB_DST"

SETUP="(setq treesit-extra-load-path (list \"$TREESIT_LOAD_PATH\"))"

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
    (cd "$COOKLANG_GRAMMAR_REPO" && tree-sitter test)
    ;;
  *)
    # Interactive mode: open the example file for manual inspection.
    emacs -Q \
      -L "$REPO" \
      --eval "$SETUP" \
      --eval "(require 'cook-ts-mode) (find-file \"$REPO/example/brigadeiro.cook\")"
    ;;
esac
