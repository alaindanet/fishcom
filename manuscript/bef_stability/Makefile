################################################################################
#                           Routine for the cluster                            #
################################################################################

FILE?=manuscript

clean:
	ssh mesu "cd $(RUN_DIR)/run && rm temporal_networks.*"
	rm $(DEST)/run/temporal_networks.*

convert:
	latex $(FILE).tex
	bibtex $(FILE)
	mylatex2rtf $(FILE).tex
	





