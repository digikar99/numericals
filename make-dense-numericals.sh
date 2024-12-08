

# Make backup and copy from backup
mv dense-numericals-src dense-numericals-src.bak
mkdir dense-numericals-src
cp -r dense-numericals-src.bak/utils dense-numericals-src/

# Copy common files from numericals
# This excludes: utils basic-utils
for dir in basic-math transcendental linalg random statistics magicl
do
    cp -r src/$dir dense-numericals-src/
done

# lparallel.lisp is handled by more-utils in dense-numericals
rm dense-numericals-src/transcendental/lparallel.lisp
mv dense-numericals-src.bak/magicl/magicl-wrapper.lisp dense-numericals-src/magicl/

# Replace ':numericals' with ':dense-numericals'
find dense-numericals-src/*/*.lisp -maxdepth 1 -type f -exec sed -i 's/\:numericals/\:dense-numericals/g' {} \;

# Replace 'numericals:' with 'dense-numericals:'
find dense-numericals-src/*/*.lisp -maxdepth 1 -type f -exec sed -i 's/numericals\:/dense-numericals\:/g' {} \;
