hpview README
=============

`hpview` is a GHC's `.hp` (heap profile) files viewer, implemented in Haskell + Gtk3.

For now, it is does not have much features; it may be nicer than use of hp2pretty because of

* No need to convert `.hp` to `.svg` and then open `.svg` with another program, just run `hpview file.hp`;
* Areas on plot are higlighted interactively (on mouse-over), and name of selected area is dislpalyed in the statusbar.

Other features may be added later.

![Screenshot](https://user-images.githubusercontent.com/284644/62837559-3c054d80-bc8a-11e9-8ca4-58caf132ab5f.png)

