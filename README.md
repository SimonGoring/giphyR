# giphyR

The `giphy` package is a programmatic R interface to [Giphy](http://giphy.com).

### Development

*We welcome contributions from any individual, whether code, documentation, or issue tracking.  All participants are expected to follow the [code of conduct](https://github.com/SimonGoring/giphyR/blob/master/code_of_conduct.md) for this project.*

+ [Simon Goring](http://goring.org) - University of Wisconsin-Madison, Department of Geography

Package functions resolve various giphy APIs and enable plotting.  Note that the package requires the installation of [ImageMagick](https://www.imagemagick.org/script/download.php).  Otherwise it's really
hard to manage the data.  Also, check out the great [`magick` package](https://cran.r-project.org/web/packages/magick/vignettes/intro.html), by [Jeroen Ooms](https://github.com/jeroen).  More plugins will be available over time to that package.  Currently `giphyR` is a very simple wrapper.

### Currently implemented in `giphyR`

* `search` - Search all Giphy GIFs for a word or phrase. Punctuation will be stripped and ignored.
* `trending` - Fetch GIFs currently trending online. Hand curated by the Giphy editorial team.
* `translate` - The translate API draws on search, but uses the Giphy "special sauce" to handle translating from one vocabulary to another.
* `random` - Returns a random GIF from Giphy, which may be limited using tags.
* `gif_id` - Get GIF metadata based on the unique id.
* `plot` - Plot the GIF returned by any of the above calls.

### Install `giphyR`

+ Development version from GitHub:

```r
install.packages("devtools")
library(devtools)
install_github("rSimonGoring/giphyR")
library(giphyR)
```
