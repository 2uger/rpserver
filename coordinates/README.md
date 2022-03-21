### Coordinates sharing server </br>
<sup>Web socket server to connect and share simple commands:</sup>
* Send raw coordinates
* Follow, to share your coordinates with others
* Unfollow, to stop sharing
### How to start?
* If you on m1 chip:
**stack build --compiler ghc-<<your ghc version>> --system-ghc && stack exec coordinates-exe --compiler ghc-<<your ghc version>> --system-ghc**
* Otherwise:
**stack build && stack exec coordinates-exe**
