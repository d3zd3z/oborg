#! /bin/bash

# borg=~davidb/wd/borg-env/bin/borg
borg=borg

env BORG_PASSPHRASE=kao3ohBae0quaMohzu5eemaeghei3Gox8zu \
	BORG_REPO=/lint/borgs/test \
	$borg "$@"
