function fcode -d "Fuzzy search to open in VS Code"
  if fzf > $TMPDIR/fzf.result
    code (cat $TMPDIR/fzf.result)
  end
end