use context dcic2024


files = table: filename, format
  row: "1.pdf", "pdf"
  row: "2.doc", "doc"
end

transform-column(files, "format", lam(o): string-replace(o, "pdf", "document") end)

build-column(files, "filename2", lam(r): case(r : Row) -> string: r["format"] + r["filename"] end)