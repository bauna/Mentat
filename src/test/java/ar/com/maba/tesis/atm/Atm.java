package ar.com.maba.tesis.atm;


public interface Atm {

    void authenticate();

    void finish();

    void insertCard();
    
    void removeCard();

    void operate();
    
    void printTicket();

}