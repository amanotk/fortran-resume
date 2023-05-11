echo "hello" > hoge.txt

# python modules
python3 -m pip install -r .devcontainer/requirements.txt

# gfortran
apt-get update && apt-get install -y gfortran
