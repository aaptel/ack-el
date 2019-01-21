# ack.el

Yet another ack/ag front-end. See:

- [ack](http://beyondgrep.com/)
- [silver searcher (ag)](https://geoff.greer.fm/ag/)
- [Better than Ack](http://betterthanack.com/)

One notable difference with this front end is that the directory it
searches in is global. No matter what your buffer is and where the
corresponding file is location, this mode will always use the current
global ack dir.

# Usage

- `M-x ack RET your query RET`
- `C-u ack` will let you set the global search directory.
