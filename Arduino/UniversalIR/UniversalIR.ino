#include <IRremote.h>

IRsend irsend;

#define ScreenPin 4

int screenStatus = 0;
void setup()
{
    Serial.begin(9600);
    Serial.println("Begin");
    /* Serial.setTimeout((long) 1000); */
    pinMode(ScreenPin, OUTPUT);
    digitalWrite(ScreenPin, screenStatus);
}

unsigned long HDMI[] = {
    (unsigned long)0xFF10EF, // 1
    (unsigned long)0xFF50AF, // 2
    (unsigned long)0xFF30CF, // 3
    (unsigned long)0xFF708F, // 4
    (unsigned long)0xFF18E7, // 2CH
    (unsigned long)0xFF00FF, // Off/On
};

unsigned long TVTuner[] = {
    (unsigned long)0x80BF3BC4, // Off/On
    (unsigned long)0x80BF49B6, // 1
    (unsigned long)0x80BFC936, // 2
    (unsigned long)0x80BF33CC, // 3
    (unsigned long)0x80BF718E, // 4
    (unsigned long)0x80BFF10E, // 5
    (unsigned long)0x80BF13EC, // 6
    (unsigned long)0x80BF51AE, // 7
    (unsigned long)0x80BFD12E, // 8
    (unsigned long)0x80BF23DC, // 9
    (unsigned long)0x80BFE11E, // 0
    (unsigned long)0x80BFB34C, // REC
};

unsigned long AppleTV[] = {
    // Apple TV
    (unsigned long)0x77E1BAED, // Ok - start
    (unsigned long)0x77E17AED, // Play - start
    (unsigned long)0x77E120ED // stop
};

unsigned long Projector[] = {
    // Projector
    (unsigned long)0x4CB340BF, // On
    (unsigned long)0x4CB3748B // Off
};

unsigned long c;
byte readByte;
byte readByte2;

void loop() {
    // Packages are prefixed with $ and ended with !
    // For example: $a1!
    if (Serial.available() > 3){
        if (Serial.read() == '$'){
            code();
        }
    }
}

void code(){
    readByte = Serial.read();
    readByte2 = Serial.read();
    if (Serial.read() != '!'){
        Serial.println("Wrong code pattern");
        return;
    }

    switch (readByte){
        case 'a':
            // HDMI switch
            c = HDMI[readByte2 - '1'];
            irsend.sendNEC(c, 32);
            Serial.println("HDMI");
            break;
        case 'b':
            // TV Tuner
            c = TVTuner[readByte2 - '1'];
            irsend.sendNEC(c, 32);
            Serial.println("TV Tuner");
            break;
        case 'c':
            // Apple TV
            c = AppleTV[readByte2 - '1'];
            irsend.sendNEC(c, 32);
            delay(250);
            c = AppleTV[2]; // stop
            irsend.sendNEC(c, 32);
            Serial.println("Apple TV");
            break;
        case 'd':
            // Projector
            c = Projector[readByte2 - '1'];
            irsend.sendNEC(c, 32);
            Serial.println("Projector");
            break;
        case 'e':
            // Screen
            screenStatus = readByte2 - '0';
            digitalWrite(ScreenPin, screenStatus);
            Serial.println("Screen");
            break;
        default:
            /* c = (unsigned long)0xC18801FE; // Stereo Off */
            Serial.println("Default");
            break;
    }
    delay(250);
}
