pandoc -f markdown_strict+backtick_code_blocks \
       --template=template.html \
       --columns=100 \
       --metadata title="Interactive Future Interests: A Digital Textbook" \
       --lua-filter ./codeblocks.lua \
       -o textbook.html \
       textbook.md