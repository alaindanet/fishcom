################################################################################
#                           Routine for the cluster                            #
################################################################################

RUN=run
EXEC=my_job
DEST:=/home/alain/Documents/post-these/mnhn/fishcom

all: $(RUN)

run:
	ssh mesu "cd fishcom/run && qsub job_mesu_cluster.sh"

my_job: 
	# check if the job has finished every 300s (5min):
	#while [ ! -f temporal_networks.*]; do sleep 300; done

check: 
	ssh mesu "qstat -u danet"

clean:
	ssh mesu "cd fishcom/run && rm temporal_network.*"

import:
	scp danet@mesu.dsi.upmc.fr:/home/danet/fishcom/data/* $(DEST)/data/

export_raw_data:
	scp -r $(DEST)/data-raw/* danet@mesu.dsi.upmc.fr:/home/danet/fishcom/data-raw/
