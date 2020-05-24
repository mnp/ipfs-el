# Emacs Elisp Interface to IPFS

## Retrieve objects
This package allows Emacs to retrieve IPFS and IPNS objects over the
[HTTP API protocol](https://docs.ipfs.io/reference/api/http) or from
[IPFS HTTP Gateways](https://ipfs.github.io/public-gateway-checker/).

You can either run your own daemon locally or use a public gateway. The code will try local first.

If you're running [your own daemon locally](https://github.com/ipfs/go-ipfs) which is also easy to do now
[with a Docker container](https://hub.docker.com/r/ipfs/go-ipfs), it's assumed to be http://127.0.0.1:5001.

## Publish Objects

to be written

## Package Manager

This is also an IPFS ELPA/MELPA package implementation. Package managers are a
current focus of the IPFS team, which it's especially well suited for.
This will be the initial focus of this project.

# Roadmap

Package Index Publishing

- [x] Initial experiments and learning
- [x] Retrieve the current public Melpa archive-contents and publish to default IPNS name
- [ ] Determine an IPNS key management plan
- [ ] Support other repos like ELPA

Package Publishing
- [ ] Fork https://github.com/melpa/package-build to pull packages git and publish each to IPFS
- [ ] Augment the archive-contents structure to hold IPFS CIDs to each package

Emacs packackage - ideally provide only a package, defadvice and hooks, no patching of existing packages

- [ ] Add an ipns:// and ipfs:// URL scheme for retrieving
- [ ] Investigate what web3:// would mean
- [ ] `package-list`: resolve IPNS name and pull archive-contents from IPFS
- [ ] `package-install`: pull selected packages from IPFS

