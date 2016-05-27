# LTSHaskell Slack Bot

[![Travis Status][travis-image]][travis-url]
[![AppVeyor Status][appveyor-image]][appveyor-url]
[![Release][release-image]][release-url]

## Installation

```bash
git clone https://github.com/uecmma/LTSHaskell-slack-bot
cd LTSHaskell-slack-bot
stack install
```

## Usage

```bash
> ltshaskell-slackbot --help
notification updating of LTSHaskell

Usage: ltshaskell-slackbot [--url|--webhook-url URL] [-t|--delay-time TIME]
                           [-c|--config FILE] [--snapshots-url URL]
  LTSHaskell Slack Bot

Available options:
  -h,--help                Show this help text
  --url,--webhook-url URL  WebHook URL for Slack Incoming
  -t,--delay-time TIME     Delay time (seconds)
  -c,--config FILE         config file
  --snapshots-url URL      snapshots url
```

[travis-image]: https://travis-ci.org/uecmma/LTSHaskell-slack-bot.svg?branch=master
[travis-url]: https://travis-ci.org/uecmma/LTSHaskell-slack-bot
[appveyor-image]: https://ci.appveyor.com/api/projects/status/2ddgphotdxmx5omt?svg=true
[appveyor-url]: https://ci.appveyor.com/project/uecmma/LTSHaskell-slack-bot
[release-image]: https://img.shields.io/github/release/uecmma/LTSHaskell-slack-bot.svg
[release-url]: https://github.com/uecmma/LTSHaskell-slack-bot/releases
