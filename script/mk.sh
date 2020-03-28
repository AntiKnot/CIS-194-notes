for c in {1..12}; do 
	mkdir Chapter-$c && cd Chapter-$c && \
	mkdir Week-$c && cd Week-$c $$ echo "# .gitkeep" > .gitkeep && cd .. && \
	mkdir Homework-$c && cd Homework-$c $$ echo "# .gitkeep">.gitkeep  && cd .. && \
	cd ..
done
