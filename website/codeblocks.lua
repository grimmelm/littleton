counter = 0

function CodeBlock(block)
  div = pandoc.Div

  run = pandoc.RawBlock('html',
  [[<button type="button" class="btn btn-tutorial btn-run">
     Run
     <i class="fas fa-play"></i>
    </button>]]
  )

  edit = pandoc.RawBlock('html',
  [[<button type="button" class="btn btn-tutorial btn-editable">
     Edit
     <i class="fas fa-edit"></i>
   </button>]]
  )

  hide = pandoc.RawBlock('html',
  [[<button type="button" class="btn btn-tutorial btn-hide" hidden>
     Hide
     <i class="fas fa-chevron-up"></i>
   </button>]]
   )

  pre = pandoc.CodeBlock(block.text)
  pre.classes = {"code-tut-eg"}

  div = pandoc.Div({run, edit, hide, pre})
  div.classes = {"container-tut-eg"}
  div.identifier = "tut-eg-"..counter
  counter = counter + 1

  return div
end