# Install
* Use recent nightly Emacs 27 with libjansson support (native JSON)
* Install Node v10 or later (with npm)
* Run this:
```
npm install
```

# Run test
`emacs -q -l test-init.el test.vue`
* In the buffer, go to the blank line in `foo` and type: `d3.` and wait
* You should see lots of completions in the popup, starting with `version`
* While the completion popup is there, type slowly: 's', 'c', 'a'. 
* Notice the completions narrow to the ones with those letters, and it gets fast.
* But now: backspace back to the "."
* and type 's', then 'c', then 'a' again -- you'll see the completions don't narrow anymore, 
  and each letter takes a long time to echo to the screen.
* Do `M-x normal-mode` to reset; repeat the test and it'll be OK again, until you backspace
  a few times, then it'll be slow again.