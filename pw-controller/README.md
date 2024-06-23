## pw-controller - apply rules to pipewire links

The goal of pw-controller is to ensure custom links are present.
pw-controller is similar to [WirePlumber][wireplumber] but it uses a simpler configuration which features higher level rules:

```
# When the MiniFuse is connected, setup Reaper i/o:
when device:r"MiniFuse 2"
  connect input  "REAPER:in(1|2)"  "MiniFuse 2:capture_(FL|FR)"
  connect output "REAPER:out(1|2)" "MiniFuse 2:playback_(FL|FR)"
  connect inout  "REAPER:MIDI .* 1" "Arturia MicroFreak:.*"

# When the headset is present
when device:r"Sennheiser USB headset"
  # Ensure VLC does not play in the headset
  connect output "VLC media player .*:output_(FL|FR)" "Speaker .*:playback_(FL|FR)"
  # Send visio app to the headet, other media are redirected to the speakers
  connect input "Sennheiser USB headset:playback_(FL|FR)" media:"(Meet .*|.* Jitsi Meet)" "Speaker .*:playback_(FL|FR)"
  # Ensure laptop mic is not used
  disconnect input ".*:capture_(FL|FR)"
```

pw-controller reacts to changes in pipewire state to continuously produce such actions:

- Ensure Reaper stays connected to the right interface (it sometimes reconnect to the default card when doing render).
- Ensure video calls are played to headset and not on speaker.
- Ensure that the laptop microphone is disconnected when a headset is plugged in.

### Rules

The rules are composed of matchers, actions and conditions:

### Matcher

Matcher identify objects using the following syntax: `object-type:match`:

- Object type can be any of: `port`, `node`, `media`, `device`. Object type can be omited by defaulting to `port`.
- Match can be a regex with `"regex"`, or a raw name with `r"a name"`. Raw names' quotes can be omited when the name doesn't contain spaces.

For example:

- `device:r"headset"` matches all the ports connected to the device named `headset`.
- `node:"VLC.*"` matches all the node with a name starting with `VLC`.
- `"capture_*F(L|R)"` matches all the port ending with `capture_FL` or `capture_FR`.

### Actions

#### `connect direction target with`

Connect a given target matcher with another matcher.
Direction must be one of `input`, `output` or `inout`.
All the ports are grouped by node so that multiple matching targets can be rewired at once.

For example:

- `connect output client sink` connects the client's output ports to the sink input ports.
- `connect input client source` connects the source output ports to the client's input ports.
- `connect inout client card` connect the client output to the card input and vice-versa.

Note that if the desired connections don't exist, any previous links are disconnected.
Once the rule is effective, the target ports can be connected to other nodes.

#### `connect direction target with redirect`

Same as the `connect` action, but instead of simply disconnecting previous links, they are
redirected to another node. For example:

- `connect input device:headset media:video-conf device:speaker` connects the media named `video-conf` to the headset, and redirect any other node connected to the headset to the speaker.

#### `disconnect direction target`

Disconnect a target matcher.
Direction must be one of `input`, `output` or `inout`

For example:

- `disconnect output device:microphone` disconnect any input ports connected to the microphone output.

### Conditions

Rules can be activated base on a condition using the following syntax: `when matcher [rules]`.

For example:

```
when device:headset
  connect ...
  disconnect ...
```

… performs the rules only when a device named headset is connected.

### Usage

```ShellSession
$ cabal run exe:pw-controller -- --help
Usage: pw-controller [--debug] [--dry] [--one-shot] [--config PATH]

Available options:
  --debug                  Debug internal state
  --dry                    Do not apply rules, just show the pw-link commands
  --one-shot               Do not monitor, just apply rules to the current state
  --config PATH            Config path, default to ~/.config/pw-controller.conf
```

## Contribute

The implementation works for the above config, adding new features is welcome, for example:

- [ ] Provide a GUI/web page to show the status.
- [ ] Use pw-metadata to move sink input for replacing microphones from stereo to mono.
- [ ] Add extra conditions and actions (e.g. delete to permanently remove nodes).

The code is implemented as follow:

- [Expr](./src/PwController/Expr.hs): define the rules.
- [Parser](./src/PwController/Parser.hs): parse the config file format.
- [Eval](./src/PwController/Eval.hs): create actions to apply the rules on a RegistryState.
- [PwController](./app/PwController.hs): the main app to listen for events and apply actions.

[wireplumber]: https://docs.pipewire.org/group__api__pw__core.html
