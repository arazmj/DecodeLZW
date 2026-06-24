# DecodeLZW

Two small text codecs in Haskell. Both **compress** plain text and
**decompress** it back to the original:

* **Huffman** — a prefix-code tree plus a stream of bits.
* **LZW** — Lempel-Ziv-Welch dictionary coding, emitting a stream of integer
  codes.

## How it works

### Huffman

The decoding procedure is deceptively simple. Starting with the first bit in the
stream, one then uses successive bits from the stream to determine whether to go
left or right in the decoding tree. When we reach a leaf of the tree, we've
decoded a character, so we place that character onto the (uncompressed) output
stream. The next bit in the input stream is the first bit of the next character.

Compression is the inverse: count how often each character appears, build a
Huffman tree so that frequent characters get short bit strings, and emit each
character as the path from the root to its leaf.

### LZW

LZW starts from a dictionary that holds every distinct character of the input.
Compression reads the longest run that is already in the dictionary, emits that
run's code, and then adds the run plus the next character as a new entry — so
repeated patterns collapse to a single code. Decompression rebuilds the very
same dictionary as it walks the codes, so no dictionary needs to be stored.

## Format

Both programs read from `stdin` and write to `stdout`. The encoded form is plain
text.

**Huffman**

* **Line 1** — the encoding tree in prefix notation. A `*` marks a branch
  (followed by its left and right sub-trees); any other character is a leaf.
  For example `*a*bc` is `Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))`.
* **Lines 2+** — one `0`/`1` bit string per message, where `0` means "go left"
  and `1` means "go right".

A single message (input line) is encoded to a single bit-string line, so empty
lines round-trip as empty lines.

**LZW**

* **Line 1** — the alphabet: the ordinals of the distinct characters in the
  input, sorted ascending and space separated. Code `i` denotes the `i`-th of
  these characters; new dictionary codes are assigned from the alphabet size up.
* **Line 2** — the LZW code stream as space-separated integers.

The whole input (newlines included) is treated as one stream, so LZW round-trips
any text exactly.

## Usage

```sh
DecodingText huffman compress     # text -> tree + bit strings
DecodingText huffman decompress   # tree + bit strings -> text
DecodingText lzw     compress     # text -> alphabet + codes
DecodingText lzw     decompress   # alphabet + codes -> text

DecodingText compress             # Huffman compress (shorthand)
DecodingText decompress           # Huffman decompress (shorthand)
DecodingText                      # Huffman decompress (default, legacy)
```

Round-trip examples:

```sh
printf 'abracadabra\n'             | DecodingText huffman compress | DecodingText huffman decompress
printf 'TOBEORNOTTOBEORTOBEORNOT' | DecodingText lzw     compress | DecodingText lzw     decompress
```

## Build and run

With Cabal:

```sh
cabal build
cabal run DecodingText -- lzw compress   < input.txt > encoded.txt
cabal run DecodingText -- lzw decompress < encoded.txt
```

Or directly with GHC:

```sh
ghc -isrc -o DecodingText src/Main.hs
./DecodingText lzw compress   < input.txt > encoded.txt
./DecodingText lzw decompress < encoded.txt
```

## Source layout

* `src/Huffman.hs` — the Huffman codec: tree building, (de)serialization,
  `compress`, `decompress`.
* `src/LZW.hs` — the LZW codec: dictionary coding `compress` and `decompress`.
* `src/Main.hs` — the command line front end.
