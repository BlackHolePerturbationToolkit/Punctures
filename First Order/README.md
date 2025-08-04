# Generating first order punctures


1. Edit the "Parameters" section of h1P-exact-order-eps2.wl.
2. Copy h1P-exact-order-eps2.wl to the machine where it is to be run.
3. Start Mathematica and set a value for r0, then run h1P-exact-order-eps2.wl
   (the below SLURM script can be used on a cluster). This generates a set of 
   files, one for each i and separate files for the first and second derivatives.
4. Run process[r0] inside Process-h1P.nb. This consolidates everything into
   a single file.
5. Run h1S-exact-order-eps2.nb. This generates the HDF5 files for h1S, h1R and
   h1ret including second derivatives.

## Example SLURM script

```sh
#!/bin/bash -l
# Set the number of nodes
#SBATCH -N 1

# Set the number of tasks/cores per node required 
#SBATCH -n 44

# Set the walltime of the job to 1 hour (format is hh:mm:ss)
#SBATCH -t 240:00:00

# E-mail on begin (b), abort (a) and end (e) of job
#SBATCH --mail-type=ALL

# E-mail address of recipient
#SBATCH --mail-user=barry.wardell@ucd.ie

# Specifies the jobname
#SBATCH --job-name=r0_19.9

# Change working directory to current directory
cd $SLURM_SUBMIT_DIR

echo "Running on `hostname`"
echo "Started at `date`"

module load mathematica
time WolframKernel -run "r0=1990/100; Get[\"../h1P-exact-order-eps2.wl\"]; Quit[];" >& logfile

echo "Done at `date`"
````
