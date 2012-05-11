install:
	for config in .*; do \
		ln -s `pwd`/$config ~/$config; \
	done
