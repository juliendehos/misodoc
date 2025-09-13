
# Usage

## How to use MisoDoc?

Don't.

MisoDoc dynamically renders static MD files, which is probably never a good
idea. :sweat_smile:

However, if you really want to use it, you have to:

- build the web app (see the [github
  repo](https://github.com/juliendehos/misodoc));

- write your MD files in the `book` folder (it must contain a `summary.md` file
  that links to the other files);

- deploy everything in a `public` folder and run a HTTP server.

Technically, we could simply write the MD files somewhere and have the MisoDoc
app get and render these files.

## What's in the web app?

Well, you are using it, actually. :scream:

On the left side, you have the summary, where you can choose the file to
render, on the right side. You also have three icons, at the bottom of the
page, to go to the previous page, to the top of the current page or to the next
page.

Finally, at the top of the page, you can click on the first icon to show/hide
the summary. There is also a second icon but you probably don't want to use it.
You've been warned.

