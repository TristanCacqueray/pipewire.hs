# pipewire.hs

> [Pipewire](https://pipewire.org) is a project that aims to greatly improve handling of audio and video under Linux.
> It provides a low-latency, graph-based processing engine on top of audio and video devices that can be used to support the use cases currently handled by both PulseAudio and JACK.

This project contains Haskell bindings to the libpipewire.
This allows you to leverage the multimedia framework directly from your Haskell program.

## Project Status

The bindings are experimental: only a small subset of the libpipewire is implemented.
A basic high level API is available (exported in camelCase).
Checkout the following examples:

- `pw-link` to list/create/delete links: [PwLink.hs](./pipewire/examples/PwLink.hs)
- `tutorial4` to play a tone: [Tutorial4.hs](./pipewire/examples/Tutorial4.hs)
- `video-src` to draw a video stream: [VideoSrc.hs](./pipewire/examples/VideoSrc.hs)
- `pw-play` to draw a video stream: [PwPlay.hs](./pipewire/examples/PwPlay.hs)
- `pw-controller` to apply rules to pipewire links: [pw-controller](./pw-controller)

References:

- Documentation: <https://tristancacqueray.github.io/pipewire.hs/Pipewire.html>
- Source entrypoint: [Pipewire.hs](./pipewire/src/Pipewire.hs)
- Upstream doc: <https://docs.pipewire.org/topics.html>


## Contribute

Contribution, bug reports and feedback are welcome.

Run the tests with:

```
just ci
```
