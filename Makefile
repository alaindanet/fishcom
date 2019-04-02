################################################################################
#                           Routine for the cluster                            #
################################################################################

RUN=analysis
EXEC=my_job
RUN_DIR:=/home/danet/fishcom
DEST:=/home/alain/Documents/post-these/mnhn/fishcom

all: $(RUN)

dataset:
	ssh mesu "cd $(RUN_DIR)/run && qsub build_dataset_mesu_job.sh"
analysis:
	ssh mesu "cd $(RUN_DIR)/run && qsub job_mesu_cluster.sh"

my_job: 
	# check if the job has finished every 300s (5min):
	#while [ ! -f temporal_networks.*]; do sleep 300; done

check: 
	ssh mesu "qstat -u danet"
report:
	scp danet@mesu.dsi.upmc.fr:$(RUN_DIR)/run/temporal_networks.* $(DEST)/run/

clean:
	ssh mesu "cd $(RUN_DIR)/run && rm temporal_networks.*"
	rm $(DEST)/run/temporal_networks.*

import:
	scp danet@mesu.dsi.upmc.fr:$(RUN_DIR)/data/* $(DEST)/data/

export_raw_data:
	scp -r $(DEST)/data-raw/* danet@mesu.dsi.upmc.fr:$(RUN_DIR)/data-raw/
	# Replace with rsync

git_update:
	ssh mesu "cd $(RUN_DIR) &&\
	    git checkout master && git pull origin master &&\
	    git checkout new_db && git pull origin new_db"
