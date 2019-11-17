# symbiotic-haskell

An implementation of the [Symbiotic Data Standard](https://symbiotic-data.github.io/#/data/) for
interoperable serialization implementations, for various data types and platforms.

## Modules

### Primitives

- Unit - `Data.Symbiotic.Primitives.Unit`
- Boolean - _builtin `Data.Bool.Bool`_

#### Integral

##### Signed

- Int8 - _builtin `Data.Int.Int8`_
- Int16
  - Big-Endian - `Data.Symbiotic.Primitives.Integral.Int16.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Integral.Int16.LE`
- Int32
  - Big-Endian - `Data.Symbiotic.Primitives.Integral.Int32.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Integral.Int32.LE`
- Int64
  - Big-Endian - `Data.Symbiotic.Primitives.Integral.Int64.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Integral.Int64.LE`

##### Unsigned

- Uint8 - _builtin `Data.Word.Word8`_
- Uint16
  - Big-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint16.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint16.LE`
- Uint32
  - Big-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint32.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint32.LE`
- Uint64
  - Big-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint64.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Uintegral.Uint64.LE`

##### Multiple Precision

- Integer8 - `Data.Symbiotic.Primitives.Integral.Integer8`
- Integer16 - `Data.Symbiotic.Primitives.Integral.Integer16`
- Integer32 - `Data.Symbiotic.Primitives.Integral.Integer32`
- Integer64 - `Data.Symbiotic.Primitives.Integral.Integer64`
- Natural8 - `Data.Symbiotic.Primitives.Integral.Natural8`
- Natural16 - `Data.Symbiotic.Primitives.Integral.Natural16`
- Natural32 - `Data.Symbiotic.Primitives.Integral.Natural32`
- Natural64 - `Data.Symbiotic.Primitives.Integral.Natural64`

#### Floating Point

- Float32
  - Big-Endian - `Data.Symbiotic.Primitives.Floating.Float32.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Floating.Float32.LE`
- Float64
  - Big-Endian - `Data.Symbiotic.Primitives.Floating.Float64.BE`
  - Little-Endian - `Data.Symbiotic.Primitives.Floating.Float64.LE`
- Scientific - `Data.Symbiotic.Primitives.Floating.Scientific`
- Ratio - _builtin `Data.Ratio.Ratio`_

#### UTF-8 Strings

- Char - _builtin `Data.Char.Char`_
- String8 - `Data.Symbiotic.Primitives.UTF8Strings.String8`
- String16 - `Data.Symbiotic.Primitives.UTF16Strings.String16`
- String32 - `Data.Symbiotic.Primitives.UTF32Strings.String32`
- String64 - `Data.Symbiotic.Primitives.UTF64Strings.String64`

-----------------------

### Casual

#### Chronological

- Date - `Data.Symbiotic.Casual.Chronological.Date`
- Time - `Data.Symbiotic.Casual.Chronological.Time`
- DateTime - `Data.Symbiotic.Casual.Chronological.DateTime`

#### URI-Like

- IPV4 - `Data.Symbiotic.Casual.URILike.IPV4`
- IPV6 - `Data.Symbiotic.Casual.URILike.IPV6`
- URI - `Data.Symbiotic.Casual.URILike.URI`
- EmailAddress - `Data.Symbiotic.Casual.URILike.EmailAddress`

-----------------------

### Primitive Composites

#### Collections

- Array - _external `Data.Array.Array` from [array](https://hackage.haskell.org/package/array)_
- Vector8 - `Data.Symbiotic.PrimitiveComposites.Collections.Vector8`
- Vector16 - `Data.Symbiotic.PrimitiveComposites.Collections.Vector16`
- Vector32 - `Data.Symbiotic.PrimitiveComposites.Collections.Vector32`
- Vector64 - `Data.Symbiotic.PrimitiveComposites.Collections.Vector64`


- Maybe - _builtin `Data.Maybe.Maybe`_
- Tuple - _builtin `() :: * -> * -> *`_
- Either - `Data.Symbiotic.PrimitiveComposites.Either`

-----------------------

### Sophisticated Composites

#### Mappings

- StringMap8 - `Data.Symbiotic.SophisticatedComposites.Mappings.StringMap8`
- StringMap16 - `Data.Symbiotic.SophisticatedComposites.Mappings.StringMap16`
- StringMap32 - `Data.Symbiotic.SophisticatedComposites.Mappings.StringMap32`
- StringMap64 - `Data.Symbiotic.SophisticatedComposites.Mappings.StringMap64`
- Map8 - `Data.Symbiotic.SophisticatedComposites.Mappings.Map8`
- Map16 - `Data.Symbiotic.SophisticatedComposites.Mappings.Map16`
- Map32 - `Data.Symbiotic.SophisticatedComposites.Mappings.Map32`
- Map64 - `Data.Symbiotic.SophisticatedComposites.Mappings.Map64`

#### Tries

- StringTrie8 - `Data.Symbiotic.SophisticatedComposites.Triepings.StringTrie8`
- StringTrie16 - `Data.Symbiotic.SophisticatedComposites.Triepings.StringTrie16`
- StringTrie32 - `Data.Symbiotic.SophisticatedComposites.Triepings.StringTrie32`
- StringTrie64 - `Data.Symbiotic.SophisticatedComposites.Triepings.StringTrie64`
- Trie8 - `Data.Symbiotic.SophisticatedComposites.Triepings.Trie8`
- Trie16 - `Data.Symbiotic.SophisticatedComposites.Triepings.Trie16`
- Trie32 - `Data.Symbiotic.SophisticatedComposites.Triepings.Trie32`
- Trie64 - `Data.Symbiotic.SophisticatedComposites.Triepings.Trie64`
