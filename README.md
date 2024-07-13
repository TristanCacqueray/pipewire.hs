# pipewire.hs

> [Pipewire](https://pipewire.org) is a project that aims to greatly improve handling of audio and video under Linux.
> It provides a low-latency, graph-based processing engine on top of audio and video devices that can be used to support the use cases currently handled by both PulseAudio and JACK.

This project contains Haskell bindings to the libpipewire.
This allows you to leverage the multimedia framework directly from your Haskell program.

## Project Status

The bindings are experimental: only a small subset of the libpipewire is implemented.
A basic high level API is available (exported in camelCase).
Checkout the following examples:

- `pw-mon` to watch pipewire events: [PwMon.hs](./pipewire/examples/PwMon.hs)
- `pw-link` to list/create/delete links: [PwLink.hs](./pipewire/examples/PwLink.hs)
- `pw-metadata` to list/create/delete metadatas: [PwMetadata.hs](./pipewire/examples/PwMetadata.hs)
- `tutorial4` to play a tone: [Tutorial4.hs](./pipewire/examples/Tutorial4.hs)
- `video-src` to draw a video stream: [VideoSrc.hs](./pipewire/examples/VideoSrc.hs)
- `pw-play` to play an audio file: [PwPlay.hs](./pipewire/examples/PwPlay.hs)
- `pw-controller` to apply rules to pipewire links: [pw-controller](./pw-controller)

References:

- Documentation: <https://tristancacqueray.github.io/pipewire.hs/Pipewire.html>
- Source entrypoint: [Pipewire.hs](./pipewire/src/Pipewire.hs)
- Upstream doc: <https://docs.pipewire.org/topics.html>

## Nix
To use this library in your own Haskell projects that are built with [Nix](https://nixos.org/), this flake provides an output `haskellExtend`, which can be used to extend a collection of Haskell packages in nixpkgs. An example flake structure using this is shown below.

```nix
# flake.nix
{
    inputs = {
       ...
       pipewire-hs.url = "github:TristanCacqueray/pipewire.hs;
       ...
    };

    outputs = { self, nixpkgs, pipewire-hs, ...}: {
       ...
       haskellPackages = pkgs.haskellPackages.extend (pipewire-hs.haskellExtend { pipewire = pkgs.pipewire; });
       ...
    };
}

```

## Contribute

Contribution, bug reports and feedback are welcome.

Run the tests with:

```
just ci
```
