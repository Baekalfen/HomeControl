wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

sudo apt-get update
sudo apt-get install erlang

git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
cd -

git clone https://github.com/tonyg/erlang-serial
cd erlang-serial
make
DESTDIR=/usr/lib make install
cd -

