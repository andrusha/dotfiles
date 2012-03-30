install:
	for config in .*; do \
		ln -s `pwd`/$x ~/$x; \
	done
