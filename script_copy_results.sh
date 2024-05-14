prefix=synth

mkdir "$prefix"_beta03_noise0
cd "$prefix"_beta03_noise0
cp -r /lustre/buiss/"$prefix"_beta03_noise0/results .
cp /lustre/buiss/"$prefix"_beta03_noise0/*.R* .
cp /lustre/buiss/"$prefix"_beta03_noise0/output.txt .
cp /lustre/buiss/"$prefix"_beta03_noise0/*.sh .
mv results/*/*/* results

cd ..

mkdir "$prefix"_beta03_noise01
cd "$prefix"_beta03_noise01
cp -r /lustre/buiss/"$prefix"_beta03_noise01/results .
cp /lustre/buiss/"$prefix"_beta03_noise01/*.R* .
cp /lustre/buiss/"$prefix"_beta03_noise01/output.txt .
cp /lustre/buiss/"$prefix"_beta03_noise01/*.sh .
mv results/*/*/* results

cd ..

mkdir "$prefix"_beta06_noise01
cd "$prefix"_beta06_noise01
cp -r /lustre/buiss/"$prefix"_beta06_noise01/results .
cp /lustre/buiss/"$prefix"_beta06_noise01/*.R* .
cp /lustre/buiss/"$prefix"_beta06_noise01/output.txt .
cp /lustre/buiss/"$prefix"_beta06_noise01/*.sh .
mv results/*/*/* results

cd ..

mkdir "$prefix"_beta06_noise0
cd "$prefix"_beta06_noise0
cp -r /lustre/buiss/"$prefix"_beta06_noise0/results .
cp /lustre/buiss/"$prefix"_beta06_noise0/*.R* .
cp /lustre/buiss/"$prefix"_beta06_noise0/output.txt .
cp /lustre/buiss/"$prefix"_beta06_noise0/*.sh .
mv results/*/*/* results

cd ..