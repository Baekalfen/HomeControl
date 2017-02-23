#!/bin/bash
rsync -avh ~/Git/GitHub/HomeControl/ pi:~/HomeControl/ --exclude='.git/' --exclude='_build/' --exclude='rebar.lock'; rsync -avh ~/Git/GitHub/HomeControl/ homecontrol:~/homecontrol/ --exclude='.git/' --exclude='_build/' --exclude='rebar.lock'
