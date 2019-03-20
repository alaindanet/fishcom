################################################################################
#                           Routine for the cluster                            #
################################################################################

EXEC=my_job

all: $(EXEC)

my_job: qsub job_mesu_cluster.sh
	# check if the job has finished every 300s (5min):
	#while [ ! -f temporal_networks.*]; do sleep 300; done


clean:
	- rm temporal_network.*

import:
	scp -r danet@mesu.dsi.upmc.fr:/home/danet/fishcom/data/ ../data/
