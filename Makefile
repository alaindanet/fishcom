################################################################################
#                           Routine for the cluster                            #
################################################################################

CLUSTER?=migale
RUN=analysis
EXEC=my_job
RUN_DIR:=~/fishcom
DEST:=/home/alain/Documents/post-these/mnhn/fishcom
RSYNC_OPT=--exclude={'*.R','*.Rmd'}

all: $(RUN)

dataset:
	ssh $(CLUSTER) "cd $(RUN_DIR)/run && qsub build_dataset_$(CLUSTER)_job.sh"
analysis:
	ssh $(CLUSTER) "cd $(RUN_DIR)/run && qsub job_$(CLUSTER)_cluster.sh"

my_job: 
	# check if the job has finished every 300s (5min):
	#while [ ! -f temporal_networks.*]; do sleep 300; done

check: 
	ssh $(CLUSTER) "qstat -u danet"
report:
	scp $(CLUSTER):$(RUN_DIR)/run/temporal_networks.* $(DEST)/run/

clean:
	ssh mesu "cd $(RUN_DIR)/run && rm temporal_networks.*"
	rm $(DEST)/run/temporal_networks.*

import_data:
	rsync -e ssh -r -avz $(RSYNC_OPT) $(CLUSTER):$(RUN_DIR)/data/* $(DEST)/data/

import_raw_data:
	rsync -e ssh -r -avz $(RSYNC_OPT) $(CLUSTER):$(RUN_DIR)/data-raw/* $(DEST)/data-raw/

export_raw_data:
	rsync -e ssh -r -avz $(RSYNC_OPT) $(DEST)/data-raw/* $(CLUSTER):$(RUN_DIR)/data-raw/

export_data:
	rsync -e ssh -r -avz $(RSYNC_OPT) $(DEST)/data/* $(CLUSTER):$(RUN_DIR)/data/

git_update:
	ssh $(CLUSTER) "cd $(RUN_DIR) &&\
	    git checkout master && git pull origin master &&\
	    git checkout new_db && git pull origin new_db"
