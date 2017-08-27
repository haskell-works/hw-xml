# hw-xml
[![CircleCI](https://circleci.com/gh/haskell-works/hw-xml.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-xml)

`hw-xml` is a high performance XML parsing library. It uses
succinct data-structures to allow traversal of large XML
strings with minimal memory overhead.

For an example, see [app/Main.hs](../master/app/Main.hs)

# Notes
* [Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
* [Space-Efficient, High-Performance Rank & Select Structures on Uncompressed Bit Sequences](https://www.cs.cmu.edu/~dga/papers/zhou-sea2013.pdf)
