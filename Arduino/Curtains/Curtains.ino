void setup() {
  #define DIR A0
  #define EN A1
  #define CP A2
  pinMode(DIR, OUTPUT);
  pinMode(EN, OUTPUT);
  pinMode(CP, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  digitalWrite(DIR, HIGH);
  digitalWrite(EN, HIGH);

  digitalWrite(CP, HIGH);
  delayMicroseconds(350);

  digitalWrite(CP, LOW);
  delayMicroseconds(350);
}
