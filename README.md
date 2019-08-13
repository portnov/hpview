hpview README
=============

`hpview` is a GHC's `.hp` (heap profile) files viewer, implemented in Haskell + Gtk3.

For now, it is does not have much features; it may be nicer than use of hp2pretty because of

* No need to convert `.hp` to `.svg` and then open `.svg` with another program, just run `hpview file.hp`;
* Areas on plot are higlighted interactively (on mouse-over), and name of selected area is dislpalyed in the statusbar. Statusbar also displays the timestamp pointed by cursor and amount of memory consumed by highlighted item at that moment.
* Some interactive filtering features. You may interactively change number of items to display (by default only 10 largest items are shown) and filter them by name, or module, or package name.

Other features may be added later.

![Screenshot](https://user-images.githubusercontent.com/284644/62966062-af8c9380-be1f-11e9-90c3-177f4be91f18.png)

