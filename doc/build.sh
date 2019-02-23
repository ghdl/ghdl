#!/bin/sh

set -e

cd $(dirname $0)/..

$(command -v winpty) docker run --rm -t \
  -v /$(pwd)://src \
  -w //src/doc \
  btdi/sphinx:py3-featured sh -c "$(cat <<-EOF
pip install -r requirements.txt
sphinx-build -T -b html -D language=en . _build/html
sphinx-build -T -b latex -D language=en . _build/latex
EOF
)"

$(command -v winpty) docker run --rm -t \
  -v /$(pwd)://src \
  -w //src/doc \
  btdi/latex sh -c "$(cat <<-EOF
cd _build/latex
FILE=\"\`ls *.tex | sed -e 's/\.tex//'\`\"
pdflatex -interaction=nonstopmode \$FILE.tex;
makeindex -s python.ist \$FILE.idx;
pdflatex -interaction=nonstopmode \$FILE.tex;
EOF
)"
