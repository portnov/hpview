hpview README
=============

`hpview` is a GHC's `.hp` (heap profile) files viewer, implemented in Haskell + Gtk3.

![Screenshot](https://user-images.githubusercontent.com/284644/63450688-f7cd3680-c45b-11e9-900a-245bfa9cb444.png)

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
* Optional downsampling. Downsampling algorithm is very simple (it just takes
  each N'ths item) and thus not very precise; but downsampling can make the
  resulting chart much cleaner and more readable, and also it helps with
  performance. Number of samples to use is specified in the preferences window.
  By default, downsampling to 500 samples is used; it means that `hpview` will
  draw not more than 500 samples along time axis on the screen. You may use
  interactive zooming on the area of interest if you wish to see more details.

Other features may be added later.

Performance: it takes about 25 seconds to display a `.hp` file of 181 Mb in
size on my machine (i5 2.8GHz), from start to displayed chart, without
downsampling. With downsampling to 500 samples, it takes about 17 seconds. In
my practice, usually smaller files give even more insight into what's going on
than bigger ones; so I am not going to invest a lot of time into this matter.
But PRs are welcome.

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

