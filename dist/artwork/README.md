# Artwork

This directory contains artwork for quick-lint-js.

Artwork is licensed under Creative Commons Attribution-ShareAlike 4.0
International. See [LICENSE.txt](LICENSE.txt) for details.

### Files

* [`dusty-left.svg`](dusty-left.svg): quick-lint-js' mascot, Dusty, facing to
  the left.
* [`dusty-right.svg`](dusty-right.svg): quick-lint-js' mascot, Dusty, facing to
  the right.
* [`dusty-right-200x200.png`](dusty-right-200x200.png): `dusty-right.svg` padded
  and rasterized as a 200x200-pixel PNG. Used as quick-lint GitHub organization
  icon.
* [`dusty-right-256x256.png`](dusty-right-256x256.png): `dusty-right.svg` padded
  and rasterized as a 256x256-pixel PNG.
* [`dusty-favicon.svg`](dusty-favicon.svg): quick-lint-js' mascot, Dusty,
  optimized for small rasterization.
* [`dusty-favicon-16x16.png`](dusty-favicon-16x16.png): `dusty-favicon.svg`
  rasterized as a 16x16-pixel PNG.
* [`dusty-favicon-32x32.png`](dusty-favicon-32x32.png): `dusty-favicon.svg`
  rasterized as a 32x32-pixel PNG.
* [`dusty-color-guide.png`](dusty-color-guide.png): Dusty reference colors for
  color-matching.
* [`dusty-app.ico`](dusty-app.ico): Icon used for the Windows app. Generated
  with the following steps (needs
  [iconutils](https://www.nongnu.org/icoutils/) and [GIMP](https://www.gimp.org/)):

  - $ icotool --create dusty-right-256x256.png dusty-favicon-32x32.png dusty-favicon-16x16.png --out dusty-app.ico
  - Then compress it using GIMP
    1. Open dusty-app.ico in GIMP
    2. File > Export As
    3. Navigate to the artwork directory and select dusty-app.ico
    4. Click Export and after that click Replace
    5. Turn on "Compressed (PNG)" for every image <p><a target="_blank" rel="noopener noreferrer" href="https://user-images.githubusercontent.com/29011024/181103334-c38f9cd0-3eb2-482f-9b52-7dfc8c9fc2aa.png"><img src="https://user-images.githubusercontent.com/29011024/181103334-c38f9cd0-3eb2-482f-9b52-7dfc8c9fc2aa.png" alt="image" width="50%"></a></p>
    6. Click Export
