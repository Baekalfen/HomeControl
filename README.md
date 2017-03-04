HomeControl
===========
This project aims to merge together all electronic devices in my home. This makes it possible to set up schedules, and run routines based on time, event or manual triggers.

The following devices are implemented:

* Philips Hue - Lights (REST API)
* Rotel RA-11 - Stereo amplifier (RS232)
* Optoma HD131Xe - Projector (RS232)
* Elite Screens Electric84H - Projector screen (Arduino 5V trigger)
* Generic DVB-C tuner - TV output (Arduino IR remote)
* Apple TV - TV output (Arduino IR remote)
* Generic HDMI switch (Arduino IR remote)

The code is still very specific to my set up, and assumes alot of details about the computers and which devices are present.

Installation
------------
To make the code run, you will have to add a `config.hrl` file which defines the following:

    % Nodes
    -define(HOMECONTROLVM, 'homecontrol@[ip]').
    -define(PI, 'pi@[ip]').
    -define(MACBOOK, 'macbook@[ip]').

    % Hue
    -define (HUE_ADDR, "http://[hue bridge ip]/api/").
    -define (USERNAME, "[username for hue bridge]").
    -define (LIGHT_DELAY, 10000).

    % Stereo
    -define(ALIVE_DELAY, 1000*120).

With a little effort, this could get you up and running, but the project just isn't quite ready for general use.

