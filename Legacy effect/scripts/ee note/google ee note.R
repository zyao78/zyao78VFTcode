## change directory
cd /d F:\VFT\VFT_github\zyao78VFTcode

#### check ee api version in Anaconda prompt
conda activate vft
python -c "import ee, sys; print('ee:', ee.__version__); print(sys.executable)" 

## create new env,(with updated python)
conda create -n vft-gee python=3.10 -c conda-forge

