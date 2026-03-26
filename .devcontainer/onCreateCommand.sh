# Ubuntu packages
#apt-get update
#apt-get install -y \
#    acl \
#    gfortran \
#    gnuplot-nox

# python modules
#python3 -m pip install -r .devcontainer/requirements.txt
#python3 -m gnuplot_kernel install

# acl
sudo apt-get update
sudo apt-get install -y acl
setfacl -bnR .

# Install Quarto
curl -fsSL https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.40/quarto-1.6.40-linux-amd64.tar.gz -o /tmp/quarto.tar.gz
tar -xzf /tmp/quarto.tar.gz -C /tmp
sudo mv /tmp/quarto-1.6.40 /usr/local/lib/quarto
sudo ln -sf /usr/local/lib/quarto/bin/quarto /usr/local/bin/quarto
rm /tmp/quarto.tar.gz
