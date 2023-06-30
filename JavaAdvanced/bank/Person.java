package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {

    String getName() throws RemoteException;

    String getSurname() throws RemoteException;

    String getPassportId() throws RemoteException;

    Account getAccount(String subId) throws RemoteException;

    Account createAccount(String subId) throws RemoteException;
}
