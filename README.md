# Emacs IPFS Project

[![badge](https://img.shields.io/badge/maturity-ideation-yellow.svg)](https://shields.io/)
![tests](https://github.com/mnp/ipfs-el/workflows/tests/badge.svg?event=push)

This project will create an Elisp package `ipfs-el` to access IPFS nodes, both with the [IPFS API
protocol](https://docs.ipfs.io/reference/api/http) as well as [retrieve-only HTTP
gateways](https://ipfs.github.io/public-gateway-checker/) for convenience. `ipfs-el` will also aim to
provide some integration for `ipfs://` and `ipns://` paths and others such as magic file names.

After that foundation, some applications will be explored such as `org-mode` IPFS links, an IPFS backend for
the `package` package manager, and structured programming.

## IPFS API Binding

If you're running [your own daemon locally](https://github.com/ipfs/go-ipfs) which is also easy to do now
[with a Docker container](https://hub.docker.com/r/ipfs/go-ipfs), it's assumed to be
http://127.0.0.1:5001. Configure via variable `ipfs-api-url`.

TBW: `add` and `cat`. 

TBD: To be most useful, file, name and key management will also be needed.

Following existing `url` package access patterns, synchronous and asynchronous versions will be given.

See also [implementation doc](https://github.com/ipfs/go-ipfs/blob/master/docs/implement-api-bindings.md).

## Retrieve Via Gateway - Read Only

See variable `ipfs-public-gateway`.

If a node API is not available, we can still offer synchronous and asynchronous `get` via public IPFS HTTP
gateways.  Local node daemons also offer a gateway interface but there's no point locally except testing.

TBD: Automatic fallback to gateway on API `get/cat` failure, vs explicit calls to distinguish.

## Publish Objects

This is a little complicated because saving any change to a file implies a new CID, which is returned. 

TBD: An IPFS name publish would make sense, eg, for a document, which will imply key management decisions.

# Applications to Explore

## Org mode link handling

Links in org mode are subject to edit, rot, etc. Supporting an ipfs://
link type may be the killer app allowing whole org trees to be
retrieved with graph-sync, or just referred to.

## Package Manager

This is also an IPFS ELPA/MELPA
[package implementation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Package-Archives.html#Package-Archives).
Package managers are a current focus of the IPFS team, which it's especially well suited for.
This will be the initial focus of this project.

## Structured Programming

A personal interest of mine. Immutable, composable, content addressable chunks of code can be assembled into
larger structures. In case you write the exact same normalized code as someone else, it will have the same
CID and be de-duped. Explore in Elisp.

# Roadmap

`ipfs-el` Emacs package - ideally provide only a package, defadvice and hooks, no patching of existing packages

- [ ] Add an ipns:// and ipfs:// URL scheme for retrieving
- [ ] Investigate what web3:// would mean
- [ ] `package-list`: resolve IPNS name and pull archive-contents from IPFS
- [ ] `package-install`: pull selected packages from IPFS
- [ ] magic file types
- [ ] org-mode links

Package Index Publishing

- [x] Initial experiments and learning
- [ ] Consider [Eldev](https://github.com/doublep/eldev) as a foundation instead of `Package`.
- [x] Retrieve the current public Melpa archive-contents and publish to default IPNS name
- [ ] Determine an IPNS key management plan
- [ ] Support other repos like ELPA

Package Publishing
- [ ] Fork https://github.com/melpa/package-build to pull packages git and publish each to IPFS
- [ ] Augment the archive-contents structure to hold IPFS CIDs to each package

Structured Programming
- [ ] Idea generation


# Other Resources

## URL
Many hooks and internals we may need to operate with: https://www.gnu.org/software/emacs/manual/html_mono/url.html

First good trick is to enable debugging like this `(let ((url-debug t)) (ipfs-add-string "foo"))` and then look in `*URL_DEBUG*` buffer for details.

## Addressing conventions

 https://github.com/ipfs/in-web-browsers/blob/master/ADDRESSING.md



-----

*Sic itur ad astra*
