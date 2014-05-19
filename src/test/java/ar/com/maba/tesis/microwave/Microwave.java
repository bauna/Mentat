package ar.com.maba.tesis.microwave;


public interface Microwave {

    void start();

    void start(Integer time);

    void start(Integer time, Integer power);
    
    void stop();

    void pause();
    
    void openDoor();

    void closeDoor();

    boolean isDoorOpened();

    long getPower();

    long getTime();

    boolean isOn();
}