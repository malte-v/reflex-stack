Building this requires `webkit2gtk` to be installed locally. On Arch Linux, this
can be done via

```
pacman -S webkit2gtk
```

By default, the frontend will be served locally by a Warp server (`jsaddle-warp`).
It can also be turned into a GTK application (`jsaddle-webkit2gtk`). To do so, set
`flags.reflex-dom.use-warp` to `false` in `stack.yaml`.
