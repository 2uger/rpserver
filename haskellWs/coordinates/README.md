# Coordinates
Haskell ws-server for riders to exchange their coordinates and share with others
Written in haskell because of speed bottleneck.
Hope that will help.

## Run with:
* stack build && stack exec coordinates-exe

## Usage
Run server with above command.<br>
Connect to ws://HOST:PORT/coord. Another path will be rejected.
## Then:
- Send raw coordinates in format (99.99; 99.00). It will rewrite your coordinates every time you send it<br>
- Send command <b>follow:user_id1, user_id2...</b> to share your coordinates with certain people<br>
- Send command <b>unfollow:user_ids</b> to not send your coordinates to that people anymore

