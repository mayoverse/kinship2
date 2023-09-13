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
```
