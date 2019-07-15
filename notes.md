# Notes

Internet resource names are tricky. Important RFCs
and literature:

- [RFC 3986 3.2.2](https://tools.ietf.org/html/rfc3986#section-3.2.2)
  discusses the restrictions on a registered name in a URI. The registered
  name is a domain name subject to the restrictions in
  [RFC 1034 3.5](https://tools.ietf.org/html/rfc1034#section-3.5). However,
  domain names generally have no restrictions other than the total length
  and the label length. This is mandated in
  [RFC 2181 11](https://tools.ietf.org/html/rfc2181#section-11), which
  is careful to point out that various applications may add restrictions.
- [Length restrictions are weird](https://stackoverflow.com/questions/8724954/what-is-the-maximum-number-of-characters-for-a-host-name-in-unix).
  Although a hostname can techically be up to 255 bytes, in most contexts,
  this number is a little lower.
- [RFC 952](https://tools.ietf.org/html/rfc952) provides the
  original restrictions on a host name.
- [RFC 1132 2.1](https://tools.ietf.org/html/rfc1123#page-13)
  weakens restrictions on a host name.
- [Where can host names have digits?](https://serverfault.com/questions/638260/is-it-valid-for-a-hostname-to-start-with-a-digit)
  The RFCs do not actually make this totally clear. My reading is that
  `3foo.com`, `3foo`, `3x3`, and `300.foo` are all valid host names but
  that `33` and `foo.33`. There are a number of domain names (mostly
  used in China) that have a entirely numeric labels, so supporting
  this is actually important. This puts hostnames in a shaky situation
  where `300.bar` is a hostname but `300` is not. But hey, try `nslookup`.
  It seems to agree with this. Nevermind. I've changed my interpretation
  of this. All numeric hostnames are probably alright.

It may be worthwhile to consider a light form a subtyping with phantom
types (it's not technically subtyping, but I don't know the correct name
for it):

    data Host
    data Unknown
    data SomethingElse

    newtype Name :: Type -> Type where
      Name :: Bytes -> Name r

    weaken :: Name Host -> Name Unknown

But there is probably no tangible benefit to doing this.
