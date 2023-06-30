package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {

    /**
     * Creates a {@link Person} entity if it does not already exist, or retrieves the existing one.
     * The new entity is stored in the persons map and exported as a remote object.
     *
     * @param name the name of the person.
     * @param surname the surname of the person.
     * @param passportId the passport id of the person.
     * @return created or retrieved person.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    Person createPerson(String name, String surname, String passportId) throws RemoteException;

    /**
     * Creates an {@link Account} entity if it does not already exist, or retrieves the existing one.
     * The new entity is stored in the accounts map and exported as a remote object.
     *
     * @param person the person to whom the account is to be associated.
     * @param subId the sub-identifier of an account.
     * @return created or retrieved account.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    Account createAccount(Person person, String subId) throws RemoteException;

    /**
     * Retrieves a {@link Person} entity by passportId from the persons map and returns it as a remote object.
     * Prints a message to the standard output if the person does not exist.
     *
     * @param passportId the passport id of the person.
     * @return retrieved person or null if absent.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    Person getRemotePerson(final String passportId) throws RemoteException;

    /**
     * Retrieves a {@link Person} entity by passportId from the persons map and returns it as a local object.
     * The method also associates all accounts belonging to the person (identified by the starting part of
     * the composite key in the accounts map) with the local person object.
     * Prints a message to the standard output if the person does not exist.
     *
     * @param passportId the passport id of the person.
     * @return retrieved person or null if absent.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    Person getLocalPerson(final String passportId) throws RemoteException;

    /**
     * Retrieves an {@link Account} entity by the composite key (person's passportId and subId) from the accounts map
     * and returns it as a remote object.
     * Prints a message to the standard output if the account does not exist.
     *
     * @param person the person to whom the account is associated.
     * @param subId the sub-identifier of an account.
     * @return retrieved account or null if absent.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    Account getAccount(Person person, String subId) throws RemoteException;
}
