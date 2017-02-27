# Zero error tolerance
set -e

NODE_A=homecontrol
NODE_B=pi

tmux new -d -s my-session \
    "ssh -t $NODE_B 'tmux attach; sleep 10'"\; \
    split-window -d \
    "ssh -t $NODE_A 'tmux attach; sleep 10'"\
    ; tmux attach

tmux kill-session -t my-session
