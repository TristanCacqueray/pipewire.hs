# pw-controller - apply rules to pipewire links

The goal of pw-controller is to ensure custom links are present.
pw-controller is similar to [WirePlumber][wireplumber] but it uses a simpler configuration which features higher level rules.
pw-controller monitors the pipewire state by running `pw-mon` and it issues `pw-link` commands based on such rules:

```
# When the MiniFuse is connected, setup Reaper i/o:
when device:"MiniFuse 2"
  set input  "REAPER:in{1,2}"  "MiniFuse 2:capture_{FL,FR}"
  set output "REAPER:out{1,2}" "MiniFuse 2:playback_{FL,FR}"
  set output "REAPER:MIDI Output 1" "Arturia MicroFreak:.*"
  set input  "REAPER:MIDI Input 1" "Arturia MicroFreak:.*"

# When the headset is present
when device:"Sennheiser USB headset"
  # Ensure VLC does not play in the headset
  set output "VLC media player .*:output_{FL,FR}" "Speaker .*:playback_{FL,FR}"
  # Send visio app to the headet, other media are redirected to the speakers
  media out "(Meet .*|.* Jitsi Meet)" "Sennheiser USB headset:playback_{FL,FR}" "Speaker .*:playback_{FL,FR}"
  # Ensure laptop mic is not used
  disconnect input ".*:capture_{FL,FR}"
```

pw-controller reacts to changes in pipewire state to continuously produce such actions:

- Ensure Reaper stays connected to the right interface (it sometimes reconnect to the default card when doing render).
- Ensure video calls are played to headset and not on speaker.
- Ensure that the laptop microphone is disconnected when a headset is plugged in.

## Usage

> Note: this requires the following change: [pipewire#1998](https://gitlab.freedesktop.org/pipewire/pipewire/-/merge_requests/1998)

```ShellSession
$ pw-controller --help
Usage: pw-controller [--debug] [--dry] [--status] [--config PATH]

Available options:
  --debug                  Debug internal state
  --dry                    Do not apply rules, just show the pw-link commands
  --status                 Print the current state
  --config PATH            Config path, default to ~/.config/pw-controller.conf
  -h,--help                Show this help text
```

## Contribute

The implementation works for the above config, adding new features is welcome, for example:

- Implement proper binding (started in [pipewire](./pipewire))
- Provide a GUI/web page to show the status
- Add more conditions and actions

The code is implemented as follow:

- [PwMonParser](./PwMonParser.hs): parse pw-mon output to produce PwEvent.
- [PwState](./PwState.hs): based on PwEvent, construct a State reprensentation.
- [PwConstraint](./PwConstraint.hs): handle the logic to apply the rules based config to the State and produce Action.
- [PwController](./PwController.hs): the main apps that runs pw-mon and performs Actions.

Run the tests with:

```
just ci
```

[wireplumber]: https://docs.pipewire.org/group__api__pw__core.html
