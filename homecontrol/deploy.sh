# Zero error tolerance
set -e

NODE_A=homecontrol
NODE_B=pi

IP_A=$(ssh -G "$NODE_A" | grep hostname | head -n1 | cut -d " " -f 2)
IP_B=$(ssh -G "$NODE_B" | grep hostname | head -n1 | cut -d " " -f 2)

pushd $(pwd)

WD="/Users/mads/Git/GitHub/HomeControl/homecontrol/"
cd $WD

# Clear everything
# rebar3 clean
# rm -rf _build

# Build
rebar3 compile
# rebar3 dialyzer

tmux new -d -s my-session \
    "rsync -avh $WD pi:HomeControl --exclude='*.o' --exclude='*.so' && ssh -t pi 'tmux new -d -s homecontrol \"./HomeControl/compile_homecontrol.sh pi $IP_B\"; tmux attach; sleep 3'"\; \
    split-window -d \
    "rsync -avh $WD homecontrol:HomeControl --exclude='*.o' --exclude='*.so' && ssh -t homecontrol 'tmux new -d -s homecontrol \"./HomeControl/compile_homecontrol.sh homecontrol $IP_A\"; tmux attach; sleep 3'"\
    ; tmux attach

tmux kill-session -t my-session

popd
