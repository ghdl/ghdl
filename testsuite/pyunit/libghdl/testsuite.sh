for f in Initialize.py Comments.py; do
  PYTHONPATH=../../.. python3 -m unittest $f
done
