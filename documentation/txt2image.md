# Install desktop app

Got to `https://github.com/jgraph/drawio-desktop/releases/` and download the latest version for your OS.

To install it on wsl

```bash
sudo apt install /mnt/c/Users/llenezet/Dowlnoads/drawio-amd64-21.6.8.deb
```

To use drawio

```bash
drawio --version
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --output inst/figures/kinship2_diagram.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 0 --output inst/figures/plotting.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 1 --output inst/figures/shrinking.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 2 --output inst/figures/infos.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 3 --output inst/figures/pedigreeobj.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 4 --output inst/figures/alignment.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 5 --output inst/figures/checkingcol.png
drawio documentation/DiagramFunction.drawio --export --format png --page-index 0 --layers 6 --output inst/figures/legend.png
```
