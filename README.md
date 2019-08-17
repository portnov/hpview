hpview README
=============

`hpview` is a GHC's `.hp` (heap profile) files viewer, implemented in Haskell + Gtk3.

It may be nicer than use of [hp2pretty][1] because of the follwing features:

* No need to convert `.hp` to `.svg` and then open `.svg` with another program,
  just run `hpview file.hp`.
* Areas on plot are higlighted interactively (on mouse-over), and name of
  selected area is dislpalyed in the statusbar. Statusbar also displays the
  timestamp pointed by cursor and amount of memory consumed by highlighted item
  at that moment.
* Interactive filtering by item name, module or package name (module and
  package filtering works for `-hc` mode; for all other modes, only name
  filtering is useful).
* Interactive zoom:
  * Select time interval by dragging mouse with left button held pressed;
  * Zoom selected part of the chart by clicking "Zoom In" button in the bottom
    right corner;
  * Return to previous zoom level by clicking "Zoom Out" button in the bottom
    right corner;
  * Return to original zoom level (to fit the whole chart into the window) by
    clicking "Reset Zoom" button in the bottom right corner.
* Displaying of "trace elements", i.e. items that occupy very small amounts of
  memory. Such items are merged into one band called `(trace elements)`, as
  `hp2pretty` does. There are several options for such elements:
  * To show or not to show trace elements at all
  * Number of biggest items to show (10 by default)
  * Two modes of trace elements detection:
    * either select items that occupy less than N percents of memory in total, or
    * select items, each of which occupies less than N percents of memory.
  * Percentage of memory for trace elements is configurable.
* Legend may be enabled (by default) or disabled in preferences dialog;
  preferences dialog is called with a button in right bottom corner of the
  window.
* Highlighting of the area under mouse cursor may be disabled in preferences dialog.

Other features may be added later.

![Screenshot](https://user-images.githubusercontent.com/284644/63210503-94b06c80-c108-11e9-95a3-0e503121bc17.png)

Installation
------------

Install it by stack:

```
$ sudo apt-get install stack
$ git clone https://github.com/portnov/hpview.git
$ cd hpview/
$ stack install
```

Note: `hpview` currently depends on master branch of [haskell-chart][2] and on
master branch of [formattable][3] packages; but `stack` will take care of that.

[1]: http://hackage.haskell.org/package/hp2pretty
[2]: https://github.com/timbod7/haskell-chart
[3]: https://github.com/portnov/formattable

