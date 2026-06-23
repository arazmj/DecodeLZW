# DecodeLZW

A small Huffman codec in Haskell. It **compresses** plain text into a prefix-code
tree plus a stream of bits, and **decompresses** that stream back into the
original text.

## How it works

The decoding procedure is deceptively simple. Starting with the first bit in the
stream, one then uses successive bits from the stream to determine whether to go
left or right in the decoding tree. When we reach a leaf of the tree, we've
decoded a character, so we place that character onto the (uncompressed) output
stream. The next bit in the input stream is the first bit of the next character.

Compression is the inverse: count how often each character appears, build a
Huffman tree so that frequent characters get short bit strings, and emit each
character as the path from the root to its leaf.

## Format

Both programs read from `stdin` and write to `stdout`. The encoded form is plain
text:

* **Line 1** — the encoding tree in prefix notation. A `*` marks a branch
  (followed by its left and right sub-trees); any other character is a leaf.
  For example `*a*bc` is `Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))`.
* **Lines 2+** — one `0`/`1` bit string per message, where `0` means "go left"
  and `1` means "go right".

A single message (input line) is encoded to a single bit-string line, so empty
lines round-trip as empty lines.

## Usage

```sh
DecodingText compress     # plain text  -> tree + bit strings
DecodingText decompress   # tree + bit strings -> plain text
DecodingText              # decode (default, kept for backwards compatibility)
```

Round-trip example:

```sh
printf 'abracadabra\n' | DecodingText compress | DecodingText decompress
# -> abracadabra
```

## Build and run

With Cabal:

```sh
cabal build
cabal run DecodingText -- compress   < input.txt > encoded.txt
cabal run DecodingText -- decompress < encoded.txt
```

Or directly with GHC:

```sh
ghc -isrc -o DecodingText src/Main.hs
./DecodingText compress   < input.txt > encoded.txt
./DecodingText decompress < encoded.txt
```

## Source layout

* `src/Huffman.hs` — the codec: tree building, (de)serialization, `compress`,
  `decompress`.
* `src/Main.hs` — the command line front end.
