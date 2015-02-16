## Huffman algorithm

# Usage

Compress file

```
cat somefile | huffman - > somefile.huf
```

Expand compressed
```
cat somefile.huf | huffman + > somefile
```
