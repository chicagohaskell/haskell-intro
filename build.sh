#! /usr/bin/env bash

if [ ! -d build ]; then
  mkdir build
  reveal_root=build/reveal.js
  git clone "https://github.com/hakimel/reveal.js" $reveal_root 
  git --git-dir $reveal_root/.git --work-tree $reveal_root checkout 3.7.0
fi

pandoc --standalone \
  --self-contained \
  --highlight-style tango \
  -f markdown+lhs \
  -i run/Main.lhs \
  -o build/Main.html

pandoc --standalone \
  -V theme=serif \
  -f markdown \
  -t revealjs \
  -i slides.md \
  -o build/slides.html

pandoc --standalone \
  --self-contained \
  -f markdown \
  -i editor_setup.md \
  -o build/editor_setup.html
